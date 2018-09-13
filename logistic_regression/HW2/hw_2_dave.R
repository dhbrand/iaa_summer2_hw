library(tidyverse)
library(haven)
library(broom)
#library(MASS)
library(visreg)
library(brglm)
library(mgcv)
library(car)

# read the sas dataset into an R dataframe
train <- read_sas("logistic_regression/insurance_t.sas7bdat")

fit <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
            + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
            data = train, family = binomial(link = "logit"))
summary(fit)


#Check for any variables to transform
#Removing Branch from the list in order to plot histograms
train_no_branch <- train %>%
  dplyr::select(INS, DDA, DDABAL, DEP, CHECKS, TELLER, SAV, SAVBAL, ATM, ATMAMT)


#Plotting histograms of the variables selected for the model, not including Branch.
ggplot(gather(train_no_branch, key, value), aes(value)) + geom_histogram(color = "blue") +
  facet_wrap(~ key, scales = "free") + theme_bw() +
  labs(y = "Frequncy of Each Variable", x = "Multiple Variables Defined Above", title = "Histograms for Model Variables")


#Check separation
#Based on coefficient estimates and standard errors, don't see separation

#Check linearity
#partial residuals plots vs continuous predictors plots
visreg(fit, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDABAL",
       x = "DDABAL", y = "partial (deviance) residuals")

visreg(fit, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")


visreg(fit, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ATMAMT",
       x = "ATMAMT", y = "partial (deviance) residuals")


visreg(fit, "CHECKS", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for CHECKS",
       x = "CHECKS", y = "partial (deviance) residuals")

visreg(fit, "TELLER", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for TELLER",
       x = "TELLER", y = "partial (deviance) residuals")

visreg(fit, "DEP", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DEP",
       x = "DEP", y = "partial (deviance) residuals")
#####SAVBAL, ATMAMT not linear. DEP: a few very large values that are causing issues

dfbetasPlots(fit, terms = "SAVBAL", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))
# 180 1260 1958 5993 7441
train_reduced <- train %>% 
  filter(SAVBAL <= mean(SAVBAL) + 3 * sd(SAVBAL))
  
fit_no_if <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
             + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
             data = train_reduced, family = binomial(link = "logit"))
summary(fit_no_if)

visreg(fit_no_if, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")
#additive model with smoothing for SAVBAL and ATMAMT
fit.gam <- gam(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
               + s(SAVBAL) + ATM + s(ATMAMT) + BRANCH,
               data = train, family = binomial, method = "REML")
summary(fit.gam)
plot(fit.gam, ylab = "f(SAVBAL)", shade = TRUE, main = "effect of SAVBAL", jit = TRUE,seWithMean = TRUE)

plot(fit.gam, ylab = "f(ATMAMT)", shade = TRUE, main = "effect of ATMAMT", jit = TRUE,seWithMean = TRUE)

#not sure how to interpret these plots or what to do from here with SAVBAL and ATMAMT. Remove from the model? Try
#interaction terms/higher order terms? Not sure how to test after adding these terms if they are appropriate.

#adding in squared term for SAVBAL:
train_1 <- train %>%
  mutate(SAVBAL_SQ=SAVBAL^2) %>%
  mutate(ATM_SQ=ATMAMT^2) %>%
  mutate(DDABAL_SQ=DDABAL^2)

fit_1 <- glm(INS ~ DDA + DDABAL + DEP + TELLER + CHECKS
          + SAV + SAVBAL + SAVBAL_SQ + ATM + ATMAMT + BRANCH,
           data = train_1, family = binomial(link = "logit"))
summary(fit_1)
#AIC is lower with this term added

#Interaction term: SAVBAL and ATMAMT
fit_2 <- glm(INS ~ DDA + DDABAL + DEP + TELLER + CHECKS
             + SAV + SAVBAL + ATM + ATMAMT + BRANCH + (SAVBAL*ATMAMT),
             data = train_1, family = binomial(link = "logit"))
summary(fit_2) 
#AIC not as good as model with square term

#Interaction term: SAVBAL and ATMAMT
fit_3 <- glm(INS ~ DDA + DDABAL + DEP + TELLER + CHECKS
             + SAV + SAVBAL + SAVBAL_SQ + ATM + ATMAMT + BRANCH + ATM_SQ,
             data = train_1, family = binomial(link = "logit"))
summary(fit_3) 
#AIC lower with both SAVBAL and ATMAMT square terms added


### diagnostics ###

### plot Cook's distance
plot(fit_3, 4, id.n = 5) # id.n = #points identified on the plot
#2 very influential points!!! 6150 and 7325. 

### dfbetas plots
# col is coloring the outcome=1 points red and the no outcome blue


dfbetasPlots(fit_3, terms = "ATMAMT", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))
#Highly influential point 7325


dfbetasPlots(fit_3, terms = "SAVBAL", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))
#Highly influential points 7325, 6150, 7504


