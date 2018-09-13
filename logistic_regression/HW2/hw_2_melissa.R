library(tidyverse)
library(haven)
library(broom)
library(MASS)
library(visreg)
library(brglm)
library(mgcv)
library(car)
library(ROCR)
library(DescTools)
library(Hmisc)


# read the sas dataset into an R dataframe
train <- read_sas("C:\\Users\\Melissa Sandahl\\OneDrive\\Documents\\School\\MSA courses\\AA502\\Logistic regression\\data\\insurance_t.sas7bdat")



######################### ORIGINAL MODEL FROM HW1 IS CALLED "FIT" #################################################
fit <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
            + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
            data = train, family = binomial(link = "logit"))
summary(fit)
vif(fit)

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


#####SAVBAL, ATMAMT not linear. NEXT: TEST spline function with SAVBAL. Confirm non-linearity of SAVBAL.



##################### DEALING WITH NON-LINEARITY: CHANGING "FIT" MODEL, USING GAM to do smoothing for SAVBAL ############################################################################


fit.gam <- gam(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
               + s(SAVBAL) + ATM + ATMAMT + BRANCH,
               data = train, family = binomial, method = "REML")
summary(fit.gam)
AIC(fit.gam)
plot(fit.gam, ylab = "f(SAVBAL)", shade = TRUE, main = "effect of SAVBAL", jit = TRUE,seWithMean = TRUE)

# SOME CURVATURE IN SAVBAL, GENERALLY ONLY OUT AT LARGE VALUES WITH FEW OBSERVATIONS.
# FIT.GAM AIC better than "FIT" model





#################### CONTINUE WITH "FIT" MODEL: Try interaction terms involving SAVBAL ############################################

#Interaction term: SAVBAL and ATMAMT
fit_2 <- glm(INS ~ DDA + DDABAL + DEP + TELLER + CHECKS
             + SAV + SAVBAL + ATM + ATMAMT + BRANCH + (SAVBAL*ATMAMT),
             data = train, family = binomial(link = "logit"))
summary(fit_2) 
#AIC higher than fit.gam

train <- train %>%
  mutate(SAVBAL_SQ=SAVBAL**2)

#Interaction term: SAVBAL and ATMAMT plus SAVBAL squared term
fit_3 <- glm(INS ~ DDA + DDABAL + DEP + TELLER + CHECKS
             + SAV + SAVBAL + SAVBAL_SQ + ATM + ATMAMT + BRANCH + SAVBAL*ATMAMT,
             data = train, family = binomial(link = "logit"))
summary(fit_3) 
# Warning message: fitted probabilities numerically 0 or 1 occurred


#Interaction term: SAVBAL and DDABAL
fit_4 <- glm(INS ~ DDA + DDABAL + DEP + TELLER + CHECKS
             + SAV + SAVBAL + ATM + ATMAMT + BRANCH + SAVBAL*DDABAL,
             data = train, family = binomial(link = "logit"))
summary(fit_4) 
#AIC higher than fit.gam

############################################# FIT.GAM MODEL WITH A SECOND SMOOTHING TERM: ATMAMT ############################

fit.gam_1 <- gam(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
               + s(SAVBAL) + ATM + s(ATMAMT) + BRANCH,
               data = train, family = binomial, method = "REML")
summary(fit.gam_1)
AIC(fit.gam_1)
plot(fit.gam_1, ylab = "f(ATMAMT)", shade = TRUE, main = "effect of ATMAMT", jit = TRUE,seWithMean = TRUE)

plot(fit.gam_1, ylab = "f(SAVBAL)", shade = TRUE, main = "effect of SAVBAL", jit = TRUE,seWithMean = TRUE)
rcorr.cens(fitted(fit.gam_1), fit.gam_1$y)[-c(5, 6, 9)]
# AIC better than fit.gam

### So far fit.gam_1 has lowest AIC: THIS IS "FIT" MODEL WITH SAVBAL and ATMAMT as NON-LINEAR TERMS. 
### NOTE THAT THIS IS A GAM NOT GLM, NOT ABLE TO USE SOME OF THE GLM MODEL CHECKING AND ASSESSING ######



### FIT.GAM_1 calibration curve

obs.phat <- data.frame(y = fit.gam_1$y, phat = fitted(fit.gam_1))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()

######## LOOKS BETTER......


###### Predicted Probability Histograms for FIT.GAM_1
df <- data.frame(y = fit.gam_1$y,
                 phat = fitted(fit.gam_1))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")

########## Not great......






################################## REMOVING INFLUENTIAL POINTS AND RE-RUNNING "FIT" MODEL: "FIT_REMOVE" ###############################

# Cook's D plot to pick out influencers:
plot(fit, 4, id.n=4)

# 4 highly influential points
train[1721,] # SAVBAL 61,000 / INS = 0
train[1547,] # ATMAMT 96,370 / INS = 0
train[4601,] # SAVBAL 41,000 / INS = 0
train[5400,] # ATMAMT 94,645 / INS = 0

train_remove <- train[-c(1721, 1547, 4601, 5400),]

# RE-RUN ORIGINAL "FIT" MODEL WITH INFLUENCERS REMOVED:

fit_remove <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
                  + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
                  data = train_remove, family = binomial(link = "logit"))
summary(fit_remove)
vif(fit_remove)
# AIC better than original "fit" model now that influencers have been removed
# Continue with analysis with these influencers removed


######### Check linearity with influential points removed

visreg(fit_remove, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDABAL",
       x = "DDABAL", y = "partial (deviance) residuals")

visreg(fit_remove, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")

visreg(fit_remove, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ATMAMT",
       x = "ATMAMT", y = "partial (deviance) residuals")

visreg(fit_remove, "CHECKS", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for CHECKS",
       x = "CHECKS", y = "partial (deviance) residuals")

visreg(fit_remove, "TELLER", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for TELLER",
       x = "TELLER", y = "partial (deviance) residuals")

visreg(fit_remove, "DEP", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DEP",
       x = "DEP", y = "partial (deviance) residuals")

##### REMOVING THE INFLUENTIAL POINTS: THE "FIT_REMOVE" VARIABLES LOOK LINEAR


#Calibration curve for fit_remove model
obs.phat <- data.frame(y = fit_remove$y, phat = fitted(fit_remove))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()

# LOOKS OKAY... Not great. FIT.GAM_1 LOOKED BETTER...

# Predicted probabilities for fit_remove:

df <- data.frame(y = fit_remove$y,
                 phat = fitted(fit_remove))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")

# Not great....



###############################################################################################################


############################### STARTING OVER!! NEW MODEL. ALLOWING VARIABLES WITH MISSING VALUES ##############################

# RAN SOME TRIAL AND ERROR OF VARIOUS COMBINATIONS OF VARIABLES
# THIS MODEL LOOKED THE BEST IN TERMS OF AIC AND MINIMIZING NUMBER OF MISSING OBS
# MISSING ~1500 OBS

fit_5 <- glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
           + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC,
           data = train, family = binomial(link = "logit"))
summary(fit_5)
vif(fit_5)
#AIC SUBSTANTIALLY IMPROVED OVER OTHER MODELS TESTED SO FAR.


#Calibration curve for fit_5 model
obs.phat <- data.frame(y = fit_5$y, phat = fitted(fit_5))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()

# A BIT WONKY... 



# Predicted probabilities for fit_5
df <- data.frame(y = fit_5$y,
                 phat = fitted(fit_5))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")
rcorr.cens(fitted(fit_5), fit_5$y)[-c(5, 6, 9)]

#HISTOGRAMS LOOK OKAY....


#Check linearity of fit_5 continuous variables
#partial residuals plots vs continuous predictors plots

visreg(fit_5, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDABAL",
       x = "DDABAL", y = "partial (deviance) residuals")

visreg(fit_5, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")

visreg(fit_5, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ATMAMT",
       x = "ATMAMT", y = "partial (deviance) residuals")

visreg(fit_5, "CHECKS", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for CHECKS",
       x = "CHECKS", y = "partial (deviance) residuals")

visreg(fit_5, "TELLER", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for TELLER",
       x = "TELLER", y = "partial (deviance) residuals")


######### SAME ISSUE AS "FIT" MODEL: A FEW HIGHLY INFLUENTIAL POINTS DRIVING THE MODEL
############### RE-RUN THIS MODEL WIHOUT THESE INFLUENCERS





################## Remove influential points from fit_5 #########################################################


fit_remove_2 <- glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
             + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC,
             data = train_remove, family = binomial(link = "logit"))
summary(fit_remove_2)
vif(fit_remove_2)
# AIC Improved

#Recheck for linearity of continuous variables

visreg(fit_remove_2, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDABAL",
       x = "DDABAL", y = "partial (deviance) residuals")

visreg(fit_remove_2, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")

visreg(fit_remove_2, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ATMAMT",
       x = "ATMAMT", y = "partial (deviance) residuals")

visreg(fit_remove_2, "CHECKS", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for CHECKS",
       x = "CHECKS", y = "partial (deviance) residuals")

visreg(fit_remove_2, "TELLER", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for TELLER",
       x = "TELLER", y = "partial (deviance) residuals")

visreg(fit_remove_2, "ACCTAGE", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ACCTAGE",
       x = "ACCTAGE", y = "partial (deviance) residuals")

###### LINEARITY LOOKS OKAY 

#Calibration curve for fit_remove_2 model
obs.phat <- data.frame(y = fit_remove_2$y, phat = fitted(fit_remove_2))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()

# Not as wonky as the fit_5 model with the influential points included


# Predicted probabilities for fit_remove_2
df <- data.frame(y = fit_remove_2$y,
                 phat = fitted(fit_remove_2))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")
# Not great....




################################ BRIER SCORES ##############################################

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




brier_score(fit)
brier_score(fit_5)

brier_score(fit_remove)
brier_score(fit_remove_2) # Best Brier score of the models

AIC(fit)
AIC(fit_5)
AIC(fit.gam)
AIC(fit.gam_1)
AIC(fit_remove)
AIC(fit_remove_2) # Best AIC of the models
