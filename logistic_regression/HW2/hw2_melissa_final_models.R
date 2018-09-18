library(tidyverse)
library(haven)
library(car)
library(visreg)
library(Hmisc)


# read the training dataset into an R dataframe
train <- read_sas("C:\\Users\\Melissa Sandahl\\OneDrive\\Documents\\School\\MSA courses\\AA502\\Logistic regression\\data\\insurance_t.sas7bdat")

###############################################################################################################

################ MODEL FROM HW 1 ##############################################################################

###############################################################################################################

fit <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
           + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
           data = train, family = binomial(link = "logit"))
summary(fit)
vif(fit)

###########################################################################################################
### MAKING ADJUSTMENTS TO THE MODEL #######################################################################
##### ADDING IN VARIABLES THAT WERE PREVIOUSLY DROPPED ####################################################
######### THINKING THAT BINARY VARIABLES FOR DIFFERENT ACCOUNT TYPES COULD BE IMPORTANT FOR PREDICTING ####
###########################################################################################################
################# NEW MODEL BASED ON ADDING IN IRA, MM, MTG, CC. BRANCH, DEP NO LONGER SIGNIF. ############
###########################################################################################################

fit_5 <- glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
             + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC,
             data = train, family = binomial(link = "logit"))
summary(fit_5)
vif(fit_5)
#AIC 7777.2


######################## CONTINUING WITH FIT5 ###############################################
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

# CONTINUING TO TEST ADDING/DROPPING VARIABLES:

# Removing MTG actually increases AIC slightly
# HMOWN adds another 1300 missing obs
# POS doesn't change AIC, is not significant according to pvalue
# DEPAMT not significant
# DIRDEP not significant
# NSF not significant
# PHONE is significant - ADDING IN 
# IRABAL not significant
# INV is significant  - ADDING IN
# ILS is significant  - ADDING IN
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




############################## FIT6 MODEL DIAGNOSTICS #############################################

# check for separation
# check linearity assumptions
# check interactions between most important main effects
# check for influential obs
# select your model


###################################################################################################

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


######### SAVBAL, ATMAMT have 4 HIGHLY INFLUENTIAL POINTS ##########################
############# REDO FIT6 REMOVING THESE POINTS. #####################################



################## Remove influential points from fit_6 ############################

# Cook's D plot to pick out influencers:
plot(fit_6, 4, id.n=4)

# 4 highly influential points
l1 <- train[1721,] # SAVBAL 609,000 / INS = 0
l1 <- train[1547,] # ATMAMT 96,370 / INS = 0
l1 <- train[4601,] # SAVBAL 410,000 / INS = 0
l1 <- train[5400,] # ATMAMT 94,645 / INS = 0

train_filter <- train %>%
  filter(SAVBAL > 50000) 

ggplot(train_filter, aes(SAVBAL)) + geom_histogram()

train_filter_2 <- train %>%
  filter(ATMAMT > 10000) 

ggplot(train_filter_2, aes(ATMAMT)) + geom_histogram()


## Removing extreme values of SAVBAL, ATMAMT from model
train_remove <- train %>% 
  mutate(SAVBAL = ifelse(SAVBAL >= 100000, 100000, SAVBAL)) %>%
  mutate(ATMAMT = ifelse(ATMAMT >= 25000, 25000, ATMAMT))

#Re-run fit_6 model with these influencers removed to see if its linear
fit6_remove <- glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
                    + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC + PHONE + INV + ILS,
                    data = train_remove, family = binomial(link = "logit"))
summary(fit6_remove)
vif(fit6_remove)
#AIC 7643.1
# pvalues all still significant



# Recheck linearity of continuous variables
#partial residuals plots vs continuous predictors plots

visreg(fit6_remove, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDABAL",
       x = "DDABAL", y = "partial (deviance) residuals")

visreg(fit6_remove, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for SAVBAL",
       x = "SAVBAL", y = "partial (deviance) residuals")

visreg(fit6_remove, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ATMAMT",
       x = "ATMAMT", y = "partial (deviance) residuals")

visreg(fit6_remove, "CHECKS", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for CHECKS",
       x = "CHECKS", y = "partial (deviance) residuals")

visreg(fit6_remove, "TELLER", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for TELLER",
       x = "TELLER", y = "partial (deviance) residuals")

visreg(fit6_remove, "ACCTAGE", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for ACCTAGE",
       x = "ACCTAGE", y = "partial (deviance) residuals")

visreg(fit6_remove, "PHONE", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for PHONE",
       x = "PHONE", y = "partial (deviance) residuals")

####################### LOOK MUCH BETTER WITHOUT EXTREME VALUES #################

####################### CONTINUE WITH FIT6_REMOVE ####################################




############################## LOOK FOR INTERACTIONS #######################################


# Interaction term on model with outliers removed
fit6_int <- glm(INS ~ DDA + DDABAL + DDABAL*SAVBAL + CHECKS + TELLER 
                       + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC + PHONE + INV + ILS,
                       data = train_remove, family = binomial(link = "logit"))
summary(fit6_int)
# AIC 7629.7

##################################################################################################
########################## KEEPING INTERACTION TERM DDABAL*SAVBAL #################################
########################## MODEL FIT6_INT##########################################################
###################################################################################################

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



# Probability density fit6_int model

df <- data.frame(y = fit6_int$y,
                 phat = fitted(fit6_int))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")




###########################################################################################################
# with final model:
#youden index to find optimal threshold for classification

### ROC curves ### Plot of true pos rate vs false pos rate
# for every possible classification threshold
# tpr: when the actual classification is positive, how often does it predict positive
# fpr: when it is actually negative how often does it incorrectly predict positive
# evaluates a classifier
# visualizes all possible classification thresholds
# This is telling how well you have separated the two classes for predictions
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

## threshold: where to separate the 2 classes based on predicted probability: 0.311
# True positive rate: 0.738
# True negative rate: 0.700
# YoudenJ: 0.438


###################################################################################################
########## Continue with fit6_int #################################################################
###################################################################################################
###################################################################################################

###################################################################################################
#################### VALIDATION DATA ##############################################################
###################################################################################################

#on the validation data report the coefficient of discrimination, brier score
# c-stat, and show ROC curve and classification table. present these first in the report


# Validation dataset
valid <- read_sas("C:\\Users\\Melissa Sandahl\\OneDrive\\Documents\\School\\MSA courses\\AA502\\Logistic regression\\data\\insurance_v.sas7bdat")


# change extreme values
valid_remove <- valid %>% 
  mutate(SAVBAL = ifelse(SAVBAL >= 100000, 100000, SAVBAL)) %>%
  mutate(ATMAMT = ifelse(ATMAMT >= 25000, 25000, ATMAMT))
  
  

# Run validation data in fit6_int model
pred <- predict(fit6_int, newdata = valid_remove, type = "response")

# Create dataframe of predicted probabilities using 0.31 threshold for predicting INS = 0 or 1. 
pred_df <- pred %>% 
  as_tibble %>% 
  mutate(bin = ifelse(pred >= 0.311, 1, 0))


# Using cutoff of probability 0.5, coefficient of discrimination
# this is the difference in avg predicted probabilities between 1s and 0s. ability to separate the 1s and 0s. 
mean(pred_df$value[pred_df$bin == 1], na.rm = TRUE) - mean(pred_df$value[pred_df$bin == 0], na.rm = TRUE) 
#0.341


# Brier score
mean((valid_remove$INS - pred)^2, na.rm = TRUE)
#0.189


### c-statistic and Somers' D ###
## interpretation: for all possible pairs of event 0 and event 1, the model assigned the 
# higher predicted probability to the event 1 c% of the time. If just guessing c=50%
rcorr.cens(pred, valid_remove$INS)[-c(5, 6, 9)] 
# c-stat: 0.774





### ROC curves ###
pred_v <- prediction(pred, factor(valid_remove$INS))
perf_v <- performance(pred_v, measure = "tpr", x.measure = "fpr")
plot(perf_v, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)
performance(pred_v, measure = "auc")@y.values

#AUC: 0.774

### classification table ###
classif_table_v <- data.frame(threshold = perf_v@alpha.values[[1]],
                            tpr = perf_v@y.values[[1]],
                            tnr = 1 - perf_v@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table_v$youdenJ <- with(classif_table_v, tpr + tnr - 1)
# find row with max
classif_table_v[which.max(classif_table_v$youdenJ),]
## threshold 0.322
## TPR: 0.759
## TNR: 0.694
## YoudenJ: 0.453






