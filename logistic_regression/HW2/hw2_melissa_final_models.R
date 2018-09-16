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

### discrimination slope = mean(p1) - mean(p0) ###
mean(fitted(fit6_int)[fit6_int$y == 1]) - mean(fitted(fit6_int)[fit6_int$y == 0])
mean(fitted(fit6_int_remove)[fit6_int_remove$y == 1]) - mean(fitted(fit6_int_remove)[fit6_int_remove$y == 0])




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

