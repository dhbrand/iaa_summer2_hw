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


fit_int <- glm(INS ~ DDA + DDABAL + DEP + CHECKS : TELLER 
           + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
           data = train, family = binomial(link = "logit"))
summary(fit_int)

fit_poly <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
               + poly(SAVBAL,2) + ATM + ATMAMT + BRANCH,
               data = train_reduced, family = binomial(link = "logit"))
summary(fit_poly)

visreg(fit_poly, "SAVBAL", gg = TRUE,  points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")







#  Final Models #####################

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
test <- read_sas("logistic_regression/insurance_v.sas7bdat")

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


######################## CONTINUING WITH FIT5 / FIT_REMOVE_2 #####################################################


# FIT5 MODEL VARIABLES: SHOULD WE TAKE ANY OUT???


# LOOK AT HISTOGRAMS FOR THESE VARIABLES


train_new_model <- train %>%
  dplyr::select(INS, DDA, DDABAL, CHECKS, TELLER, SAV, SAVBAL, ATM, ATMAMT, ACCTAGE, IRA, CD, MM, MTG, CC)
ggplot(gather(train_new_model, key, value), aes(value)) + geom_histogram(color = "blue") +
  facet_wrap(~ key, scales = "free") + theme_bw() +
  labs(y = "Frequncy of Each Variable", x = "Multiple Variables Defined Above", title = "Histograms for Model Variables")

# very few have IRA, MM, MTG, CD accounts. Could these variables still provide valuable information? 
# Check to see if differing percentages of INS = 0 vs INS = 1 customers have these accounts

table1 <- table(train$INS, train$IRA) # 3.5% of INS = 0 have IRA, 10.2 % of INS=1 have IRA
prop.table(table1, 1) 

table2 <- table(train$INS, train$MM) # 7.9% of INS = 0 have IRA, 19.7 % of INS=1 have MM
prop.table(table2, 1) 

table3 <- table(train$INS, train$MTG) # 4.7% of INS = 0 have IRA, 5.0 % of INS=1 have MTG. Probably want to remove this one.
prop.table(table3, 1) 

table4 <- table(train$INS, train$CD) # 7.2% of INS = 0 have IRA, 21.2 % of INS=1 have CD. 
prop.table(table4, 1) 

# TRYING OTHER VARIABLES:
# Removing MTG actually increases AIC slightly
# HMOWN adds another 1300 missing obs
# POS doesn't change AIC, is not significant according to pvalue
# DEPAMT not significant
# DIRDEP not significant
# NSF not significant
# PHONE is significant 
# IRABAL not significant
# INV is significant 
# ILS is significant
# CCPURC not significant
# INCOME too many additional missing values
# SDB not significant

#######################################################################################################################

#### NEW MODEL TO USE : FIT6 ##########################################################################################

###########################################################################################################################


fit_6 <- glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
             + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC + PHONE + INV + ILS,
             data = train, family = binomial(link = "logit"))
summary(fit_6)
vif(fit_6)
#AIC 7736.9



##### FIT6 MODEL ASSESSMENTS #############################################

# check for separation
# check linearity assumptions
# check interactions between most important main effects
# check for influential obs
# select your model


#########################################################################




#Check linearity of fit_6 continuous variables
#partial residuals plots vs continuous predictors plots

visreg(fit_6, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDABAL",
       x = "DDABAL", y = "partial (deviance) residuals")

visreg(fit_6, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")

visreg(fit_6, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ATMAMT",
       x = "ATMAMT", y = "partial (deviance) residuals")

visreg(fit_6, "CHECKS", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for CHECKS",
       x = "CHECKS", y = "partial (deviance) residuals")

visreg(fit_6, "TELLER", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for TELLER",
       x = "TELLER", y = "partial (deviance) residuals")

visreg(fit_6, "ACCTAGE", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ACCTAGE",
       x = "ACCTAGE", y = "partial (deviance) residuals")

visreg(fit_6, "PHONE", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for PHONE",
       x = "PHONE", y = "partial (deviance) residuals")


######### SAVBAL, ATMAMT are pulled by outliers ################################
############# REDO FIT6 REMOVING THESE OUTLIERS ####################################

fit_remove_3 <- glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
                    + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC + PHONE + INV + ILS,
                    data = train_remove, family = binomial(link = "logit"))
summary(fit_remove_3)
vif(fit_remove_3)
#AIC 7623
# pvalues all still significant



# Recheck linearity of continuous variables
#partial residuals plots vs continuous predictors plots

visreg(fit_remove_3, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDABAL",
       x = "DDABAL", y = "partial (deviance) residuals")

visreg(fit_remove_3, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")

visreg(fit_remove_3, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ATMAMT",
       x = "ATMAMT", y = "partial (deviance) residuals")

visreg(fit_remove_3, "CHECKS", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for CHECKS",
       x = "CHECKS", y = "partial (deviance) residuals")

visreg(fit_remove_3, "TELLER", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for TELLER",
       x = "TELLER", y = "partial (deviance) residuals")

visreg(fit_remove_3, "ACCTAGE", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ACCTAGE",
       x = "ACCTAGE", y = "partial (deviance) residuals")

visreg(fit_remove_3, "PHONE", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for PHONE",
       x = "PHONE", y = "partial (deviance) residuals")

####################### LOOK MUCH BETTER WITHOUT THOSE FOUR OUTLIERS #################



####################### CONTINUE WITH FIT6 AND FIT3_REMOVE ###########################




############ INTERACTIONS ###########################

# model with interactions
fit6_int <- glm(INS ~ DDA + DDABAL + DDABAL*SAVBAL + CHECKS + TELLER 
                + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC + PHONE + INV + ILS,
                data = train, family = binomial(link = "logit"))
summary(fit6_int)
aic(fit6_int)
# Interaction term significant, AIC 7724.6

# Interaction term on model with outliers removed
fit6_int_remove <- glm(INS ~ DDA + DDABAL + DDABAL*SAVBAL + CHECKS + TELLER 
                       + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC + PHONE + INV + ILS,
                       data = train_remove, family = binomial(link = "logit"))
summary(fit6_int_remove)
# AIC 7607.9

##################################################################################################
########################## KEEPING INTERACTION TERM DDABAL*SAVBAL #################################
########################## MODEL FIT6_INT and FIT6_INT_REMOVE ####################################
#######################################################################################################

#Calibration curve for fit6_int model
obs.phat <- data.frame(y = fit6_int$y, phat = fitted(fit6_int))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()



#Calibration curve for fit6_int_remove model
obs.phat <- data.frame(y = fit6_int_remove$y, phat = fitted(fit6_int_remove))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()



# Probability density fit6_int model

df <- data.frame(y = fit6_int$y,
                 phat = fitted(fit6_int))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")


# Probability density fit6_int_remove model

df <- data.frame(y = fit6_int_remove$y,
                 phat = fitted(fit6_int_remove))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")


###########################################################################################################
# with final model:
#youden index to find optimal threshold for classification
#on the validation data report the coefficient of discrimination, brier score
# c-stat, and show ROC curve and classification table. present these first in the report


# DescTools::PseudoR2 has various pseudo-Rsq
PseudoR2(fit6_int, which = c("Cox", "Nagelkerke", "McFadden"))
PseudoR2(fit6_int_remove, which = c("Cox", "Nagelkerke", "McFadden"))
# McFadden's Rsq is the most useful of them in my opinion, but like regular Rsq,these all increase with added predictors

### Brier score function ###
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

brier_score(fit6_int)
brier_score(fit6_int_remove)
pred <- predict(fit6_int,newdata = test, type = "response")
brier_pred <- mean((test$INS - pred)^2, na.rm = TRUE)

### discrimination slope = mean(p1) - mean(p0) ###
mean(fitted(fit6_int)[fit6_int$y == 1]) - mean(fitted(fit6_int)[fit6_int$y == 0])
mean(fitted(fit6_int_remove)[fit6_int_remove$y == 1]) - mean(fitted(fit6_int_remove)[fit6_int_remove$y == 0])

pred_df <- pred %>% 
  as_tibble %>% 
  mutate(bin = ifelse(pred >= 0.5, 1, 0))

mean(pred_df$value[pred_df$bin == 1], na.rm = TRUE) - mean(pred_df$value[pred_df$bin == 0], na.rm = TRUE) 
# Predicted probabilities for fit6_int
df <- data.frame(y = fit6_int$y,
                 phat = fitted(fit6_int))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")


# Predicted probabilities for fit6_int_remove
df <- data.frame(y = fit6_int_remove$y,
                 phat = fitted(fit6_int_remove))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")



### c-statistic and Somers' D ###
# predicted prob goes first, outcome second
rcorr.cens(fitted(fit6_int), fit6_int$y)[-c(5, 6, 9)] 
rcorr.cens(fitted(fit6_int_remove), fit6_int_remove$y)[-c(5, 6, 9)]

### ROC curves ###
pred <- prediction(fitted(fit6_int), factor(fit6_int$y))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)
auc <- performance(pred, measure = "auc")@y.values

### classification table ###
classif_table <- data.frame(threshold = perf@alpha.values[[1]],
                            tpr = perf@y.values[[1]],
                            tnr = 1 - perf@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table$youdenJ <- with(classif_table, tpr + tnr - 1)
# find row with max
classif_table[which.max(classif_table$youdenJ),]
