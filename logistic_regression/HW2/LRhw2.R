library(tidyverse)
library(haven)
library(broom)
library(MASS)
library(visreg)
library(brglm)
library(mgcv)


# read the sas dataset into an R dataframe
train <- read_sas("C:/Users/thebi/OneDrive/Documents/Logistic Regression/data/insurance_t.sas7bdat")

# look at the structure of the dataframe to see the class of each variable
str(train)
p_summary <- psych::describe(train)
h_summary <- Hmisc::describe(train)
summary(train)

# create a character vector which counts all the missing values for variables
missing <- train %>% 
  dplyr::select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.))))

# create a new dataframe without the variables with missing values
train_no_NAs <- train %>% 
  dplyr::select(-!! names(missing))

# create a dataframe which creates the 5 number summary of the variables
train_summary <- tidy(summary(train_no_NAs))

#Histograms of selected variables without missing values
ggplot(gather(train_no_NAs, key, value, -c(BRANCH, RES)), aes(value)) +
  geom_histogram(color = "blue") +
  facet_wrap(~ key, scales = "free") + theme_bw() +
  labs(y = "Frequncy of Each Variable", x = "Multiple Variables Defined Above", title = "Histograms for the Variables with no Missing Observations")

# filtering binary variables where one count is less than 1000
check <- train_no_NAs %>%
  # select columns which only have 2 distint values (ie 0 and 1)
  select_if(~ n_distinct(.) ==2) %>% 
  # find sum of those columns which should be count of 1's
  map_dbl(~ sum(.))

# find the names of the columns above that are under 100
low_count_var <- names(which(check < 1000, useNames = TRUE))
low_count_var

train_reduced <- train_no_NAs %>% 
  # drop the columns from above
  dplyr::select(-c(!! low_count_var)) 



# checking the histograms of the new reduced df
ggplot(gather(train_reduced, key, value, -c(BRANCH, RES)), aes(value)) +
  geom_histogram(color = "blue") +
  facet_wrap(~ key, scales = "free") +
  theme_bw()



# Histograms of the selected variables
ggplot(gather(train_reduced, key, value, -c(BRANCH, RES)), aes(value)) +
  geom_histogram() +  facet_wrap(~ key, scales = "free")




#Run logistic regression with all train_reduced variables included
fit <- glm(INS ~ DDA + DDABAL + DEP + DEPAMT + CHECKS + 
             DIRDEP + TELLER + SAV + SAVBAL + ATM + 
             ATMAMT + RES + BRANCH,
           data = train_reduced, family = binomial(link = "logit"))
summary(fit)
exp(confint(fit))

#Remove variables with pvalues above 0.05 in "fit". Rerun logistic regression.
# Removed were: DEPTAMT, DIRDEP, RES, BRANCH
############## FINAL MODEL
fit2 <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
            + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
            data = train_reduced, family = binomial(link = "logit"))
summary(fit2) 
sort(abs(fit2$coefficients))
exp(confint(fit2))
diff(exp(fit2$coefficients))
exp(fit2$coefficients)
AIC(fit2)
plot(fit2, 4, n.id=4)
#Influential Obs 1721, 4601, 1547

par(mfrow=c(2,4))
dfbetasPlots(fit2, terms = "DDA", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))   #2033 3794 5727 5465 7828
dfbetasPlots(fit2, terms = "DDABAL", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))   #147, 1848, 4975, 4176, 6739
dfbetasPlots(fit2, terms = "DEP", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))   #1856 5465 5767 6018 8259
dfbetasPlots(fit2, terms = "CHECKS", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))   #2467
dfbetasPlots(fit2, terms = "TELLER", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))
dfbetasPlots(fit2, terms = "SAV", ID.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))
dfbetasPlots(fit2, terms = "SAVBAL", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))
dfbetasPlots(fit2, terms = "ATM", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))   #5465
dfbetasPlots(fit2, terms = "ATMAMT", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))
dfbetasPlots(fit2, terms = "BRANCH", id.n = 5,
             col = ifelse(fit2$y == 1, "red", "blue"))

train_reduced[5465,]



#CHECK LINEARITY
visreg(fit2, "DDABAL", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

visreg(fit2, "DEP", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red") + theme_bw() +
  labs(title = "partial residual plot for DDBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

#NONLINEAR
visreg(fit2, "SAVBAL", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

#weird towards end of graph
visreg(fit2, "ATMAMT", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

#GAM to check Linearity...Spline on nonlinear term
fit.gam<- gam(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
              + SAV + s(SAVBAL) + ATM + ATMAMT + BRANCH,
              data = train_reduced, family = binomial,method = "REML")
summary(fit.gam)
AIC(fit.gam)
plot(fit.gam, ylab = "f(SAVBAL)", shade = TRUE, main = "effect of SAVBAL", jit = TRUE,seWithMean = TRUE)
#GAM has better AIC than fit2

fit3 <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
            + SAV + SAVBAL + ATM + ATMAMT + BRANCH + (SAVBAL*BRANCH),
            data = train_reduced, family = binomial(link = "logit"))
AIC(fit3)
#interactions not improving AIC

#Second smoothing term on ATMAMT bc partial residual plot looks weird
fit.gam1<- gam(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
              + SAV + s(SAVBAL) + ATM + s(ATMAMT) + BRANCH,
              data = train_reduced, family = binomial,method = "REML")
summary(fit.gam)
AIC(fit.gam1)
plot(fit.gam1, ylab = "f(SAVBAL)", shade = TRUE, main = "effect of SAVBAL", jit = TRUE,seWithMean = TRUE)

#fit.gam1 calibration curve
obs.phat <- data.frame(y = fit.gam1$y, phat = fitted(fit.gam1))
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

#rerun model without influential obs
train_remove <- train[-c(1721, 1547, 4601, 5400),]
fit_remove = glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
                         + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
                         data = train_remove, family = binomial(link = "logit"))
AIC(fit_remove) #AIC better than original fit model
AIC(fit2)

#CHECK LINEARITY with new model now that influential obs are taken out
visreg(fit_remove, "DDABAL", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

visreg(fit_remove, "DEP", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red") + theme_bw() +
  labs(title = "partial residual plot for DDBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

#NONLINEAR
visreg(fit_remove, "SAVBAL", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

#weird towards end of graph
visreg(fit_remove, "ATMAMT", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

###########After removing influential points, all variables appear linear

#Calibration curve w/ model once influential points removed
obs.phat1 <- data.frame(y = fit_remove$y, phat = fitted(fit_remove))
obs.phat1 <- arrange(obs.phat1, phat)
ggplot(data = obs.phat1) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()

#BRIER SCORE
brier_score <- function(obj, new_x = NULL, new_y = NULL){
  
  if(is.null(new_y)){
    y <- obj$y
  } else {
    y <- new_y
  }
  
  p_obs <- mean(y)
  
  if(any(class(obj) == "glm")){
    if(is.null(new_x)){
      p <- predict(obj, newdata = new_x, type = "response")
      lp <- predict(obj, newdata = new_x, type = "link")
    } else {
      lp <- obj$linear
      p <- fitted(obj)
    }
  } else if(is.null(obj$p)) {
    lp <- obj$lp
    p <- fitted(obj)
  } else {
    p <- obj$p
    lp <- obj$linear
  }
  
  # brier score
  brier_score <- mean((y - p)^2)
  
  # max brier score is just the observed proportion
  brier_max <- p_obs*((1 - p_obs)^2) + (1 - p_obs)*(p_obs^2)
  
  # scaled brier score
  # ranges from 0 to 1---lower is better
  brier_scaled <- brier_score/brier_max
  # essentially, 1 - brier_scaled is the %improvement over null model
  
  res <- data.frame(brier_score = brier_score,
                    brier_max = brier_max,
                    brier_scaled = brier_scaled)
  res
}

brier_score(fit_remove)
