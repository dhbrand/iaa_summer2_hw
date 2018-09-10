library(tidyverse)
library(haven)
library(broom)
library(MASS)
library(visreg)
library(brglm)
library(mgcv)
library(car)

# read the sas dataset into an R dataframe
train <- read_sas("C:\\Users\\Melissa Sandahl\\OneDrive\\Documents\\School\\MSA courses\\AA502\\Logistic regression\\data\\insurance_t.sas7bdat")

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


fit.gam <- gam(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
               + s(SAVBAL) + ATM + s(ATMAMT) + BRANCH,
               data = train, family = binomial, method = "REML")
summary(fit.gam)
plot(fit.gam, ylab = "f(SAVBAL)", shade = TRUE, main = "effect of SAVBAL", jit = TRUE,seWithMean = TRUE)

plot(fit.gam, ylab = "f(ATMAMT)", shade = TRUE, main = "effect of ATMAMT", jit = TRUE,seWithMean = TRUE)

#interaction terms tested:
#SAVBAL*DEP: pvalue 0.00035
#SAVBAL*DDABAL: pvalue 2.95e-08
#SAVBAL*CHECKS: not signif
#SAVBAL*TELLER: not signif
#SAVBAL*ATMAMT: 0.02


fit_1 <- glm(INS ~ DDA + DDABAL + DEP + TELLER + CHECKS
          + SAV + ATM + ATMAMT + BRANCH,
           data = train, family = binomial(link = "logit"))
summary(fit_1)



#Re Check linearity after removing SAVBAL
#partial residuals plots vs continuous predictors plots
visreg(fit_1, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDABAL",
       x = "DDABAL", y = "partial (deviance) residuals")


visreg(fit_1, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ATMAMT",
       x = "ATMAMT", y = "partial (deviance) residuals")

visreg(fit_1, "TELLER", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for TELLER",
       x = "TELLER", y = "partial (deviance) residuals")

visreg(fit_1, "DEP", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DEP",
       x = "DEP", y = "partial (deviance) residuals")

visreg(fit_1, "CHECKS", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for CHECKS",
       x = "CHECKS", y = "partial (deviance) residuals")

### diagnostics ###
# you can get the different types of residuals with the resid() function:
# resid(fit, type = c("deviance", "pearson", "working", "response", "partial"))

# influence.measures() gives dfbetas, Cook's D, leverage, and all that fun stuff
# you can also call these individually if you want
# using functions like dfbetas(), cooks.distance(), etc.
influence.measures(fit)

### plot Cook's distance
plot(fit, 4, id.n = 5) # id.n = #points identified on the plot
##5 observations from highest Cook's D values: 1721, 4601, 5400, 1547, 4769

### dfbetas plots
# col is coloring the outcome=1 points red and the no outcome blue

# checking account balance:
dfbetasPlots(fit, terms = "DDABAL", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))
#2221 4975 4176 6257 6739

# savings account balance:
dfbetasPlots(fit, terms = "SAVBAL", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))
#180 1935 1260 1958 5993

# ATM amount:
dfbetasPlots(fit, terms = "ATMAMT", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))
#1505 3832 7496 5400 3936


### partial residuals ###
# age:
visreg(fit, "age", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for age",
       x = "age", y = "partial (deviance) residuals")

# lwt:
visreg(fit, "lwt", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for lwt",
       x = "lwt", y = "partial (deviance) residuals")

# you could also use the following, which are equivalent:
# termplot(fit, terms = "age", partial.resid = TRUE, se = TRUE, smooth = panel.smooth)
# crPlot(fit, "age")

# i don't think this ultimately matters much (at least for logistic regression),
# but partial residuals are based on the working residuals, and that's what
# crPlot(), termplot(), and resid(..., type = "partial") use. visreg bases them
# on the deviance residuals

### GAMs ###
# fit model as a GAM:
# gam() from the mgcv package uses basically the same syntax as glm()
# s() tells it to use a spline for this variable
fit.gam <- gam(low ~ s(age) + lwt + smoke + race,
               data = lowbwt, family = binomial, method = "REML")
summary(fit.gam)
# in this output, the "significance of smooth terms" is testing whether or not
# the age effect = 0, NOT if it's linear!

# plot estimated effect of age
plot(fit.gam, ylab = "f(age)", shade = TRUE, main = "effect of age", jit = TRUE,
     seWithMean = TRUE)

### simulation ###
# following the steps in the slides:
# 1. we've already done step 1 by fitting with glm() on actual lowbwt data
# 2. likewise with step 2 using gam()
# 3. difference in deviance
d_obs <- fit$deviance - fit.gam$deviance
# 4. get predicted probs from the regular logistic regression glm
# type = "response" returns the predicted probability
phat <- predict(fit, newdata = lowbwt, type = "response")
# 5. generate new outcomes using the predicted probabilites from glm()
# using the same sample size as original data
# our logistic regression model from glm() is the "truth" now
sim_outcome <- rbinom(n = nrow(lowbwt), size = 1, prob = phat)
sim_data <- data.frame(low = sim_outcome, lowbwt[,-1])
# 6: fit glm() and gam() to this simulated data using the SAME predictor values
# as the real data
sim.glm <- glm(low ~ age + lwt + smoke + race,
               data = sim_data, family = binomial(link = "logit"))
sim.gam <- gam(low ~ s(age) + lwt + smoke + race,
               data = sim_data, family = binomial, method = "REML")
# 7: take the difference in deviance between the two models on the simulated data
d_sim <- sim.glm$deviance - sim.gam$deviance

# 8: now i'm writing a function to do steps 5-7 a bunch of times
# so don't worry about it if you can't follow this
sim_models <- function(glm_model, gam_model){
  if(any(class(glm_model) == "gam"))
    stop("Error: glm_model must be fit by glm()")
  n_obs <- nrow(glm_model$data) # sample size
  y_name <- all.vars(glm_model$formula)[1] # get name of response
  y_index <- match(y_name, colnames(glm_model$data)) # get index of y
  
  phat <- predict(glm_model, type = "response") # predicted probabilities
  sim_data <- glm_model$data
  # generate new response
  sim_data[,y_index] <- rbinom(n = n_obs, size = 1, prob = phat)
  
  # fit glm to fake data and get deviance
  sim_glm_dev <- glm(glm_model$formula, family = binomial(link = "logit"),
                     data = sim_data)$deviance
  # fit gam to fake data and get deviance
  sim_gam_dev <- gam(gam_model$formula, family = binomial(link = "logit"), 
                     data = sim_data, method = "REML")$deviance
  # take the difference in deviance
  d_sim <- sim_glm_dev - sim_gam_dev
  return(d_sim)
}

set.seed(9418)
d_sim <- replicate(200, sim_models(fit, fit.gam)) # do this 200 times
paste("p-value = ", mean(d_obs <= d_sim))

# plot
hist(d_sim, breaks = 40, main = "distribution of D_sim", xlab = "D_sim")
abline(v = d_obs, col = "red")

### observed calibration curve

obs.phat <- data.frame(y = fit$y, phat = fitted(fit))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()
