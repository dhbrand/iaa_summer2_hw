library(tidyverse)
library(brglm2)
library(brglm)
library(modelr)
library(broom)
library(car)
library(mgcv)
library(DescTools)
library(psych)
library(visreg)
library(haven)
library(Hmisc)

con <- read_sas("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\orange_5_hw\\logistic_regression\\final_proj\\construction.sas7bdat")
#Bid history from previous 3 years. No time information.  

# variables: 
str(con)

# Win_Bid: char "Yes" "No" Yes if IAA Construction won the project #### Y VARIABLE ####

# Estimated_Cost_Millions
# Estimated_Years_to_Complete
# Bid_Price__Millions
# Sector (numeric) 10 different sectors, not sure what the numbers correspond to
# Region_of_Country (char)
# Number_of_Competitor_Bids
# Competitor_A through Competitor_J: numeric binary 1 or 0
# Winning_Bid_Price__Millions Price of the winning bid (for whoever won)
# Cost_After_Engineering_Estimate (in thousands) 

# continuous vars are: 
# Estimated_Cost_Millions
# Estimated_Years_to_Complete
# Bid_Price__Millions
# Number_of_Competitor_Bids
# Winning_Bid_Price__Millions Price of the winning bid (for whoever won)
# Cost_After_Engineering_Estimate (in thousands)



# categorical vars are:
# Win_Bid (char)
# Sector (numeric) 
# Region_of_Country (char)


# convert Y variable to binary 1 or 0 instead of "Yes" "No"
con_2 <- con %>% 
  modify_at("Win_Bid", as.character) %>% 
  modify_at("Win_Bid", as_factor) %>% 
  modify_at("Win_Bid", as.integer) %>% 
  mutate( Win_Bid = if_else(Win_Bid == 1, 1, 0)) %>% 
  mutate(id = row_number())

# check for missing values
missing <- con_2 %>% 
  dplyr::select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.))))
# no missing values

# check summary stats of numeric vars
con_2 %>% 
  select_if(is.numeric) %>% 
  summary()



# check distributions of numeric variables
con_numeric <- con_2 %>%
  dplyr::select(-Sector, -Region_of_Country, -id)


ggplot(gather(con_numeric, key, value), aes(value)) + geom_histogram(color = "blue") +
  facet_wrap(~ key, scales = "free") + theme_bw() +
  labs(y = "Frequncy of Each Variable", x = "Multiple Variables Defined Above", title = "Histograms for Numeric Variables")




table(con$Win_Bid)
# How many construction bids were won? 456 No, 87 Yes




# basic model
fit <- glm(Win_Bid ~ ., data = con_2, 
           family = binomial("logit"))
# warning message: algorithm did not converge
# fitted probabilities 0 or 1 occurred
# Possibly due to Winning_Bid_Price in the model? Remove this variable!
# Other variables that would not be available when placing a bid: 
# Number of Competitor bids

con_3 <- con_2 %>%
  dplyr::select(-Winning_Bid_Price__Millions_, -Number_of_Competitor_Bids)


# Rerun basic model:
fit_2 <- glm(Win_Bid ~ ., data = con_3, 
             family = binomial("logit"))
# no warning message

# check separation again. 
fit_sep <- glm(Win_Bid ~ ., data = con_3, 
               family = binomial("logit"),
               method = "detect_separation")
fit_sep
# Separation: NO


# Look at plots of each variable vs Win_Bid

ggplot(gather(con_3, key, value, -Win_Bid), aes(x = Win_Bid, y = value)) +
  geom_jitter() +
  facet_wrap(~ key, scales = "free_y")




# make training and testing data
set.seed(1234)
train <- con_3  %>% 
  sample_frac(.8)
test <- anti_join(con_3, train, by = "id")
train <- train %>% 
  select(-id)
test <- test %>% 
  select(-id)


# Basic model on training data

fit_train <- glm(Win_Bid ~ ., data = train, 
                 family = binomial("logit"))
summary(fit_train)
#AIC 250.36


# check for multicollinearity
vif(fit_train)
#estimated cost millions and bid price millions are highly correlated
# need to remove one


# Rmove Bid_Price__Millions_
fit_train_2 <- glm(Win_Bid ~  Estimated_Cost__Millions_+ Estimated_Years_to_Complete + Sector +
                     Region_of_Country + Competitor_A + Competitor_B + Competitor_C + Competitor_D +
                     Competitor_E + Competitor_F + Competitor_G + Competitor_H + Competitor_J, data = train, family = binomial("logit"))
summary(fit_train_2)
#aic 276
vif(fit_train_2)
#Looks good


############### Remove insignificant variables: Sector, Comp A, D, G, H,  Region, Estimated Years to complete


fit_train_3 <- glm(Win_Bid ~  Estimated_Cost__Millions_+ Competitor_B + Competitor_C +
                     Competitor_E + Competitor_F + Competitor_J, data = train, family = binomial("logit"))
summary(fit_train_3)
#AIC 275.46
#most impactful variables: Competitor C, F, E, B. Cost has very small effect. 


# From Dave: trying some different combinations of predictors
fit_train_2b <- glm(Win_Bid ~  Estimated_Cost__Millions_+ Estimated_Years_to_Complete + Sector +
                      Region_of_Country, data = train, family = binomial("logit"))
summary(fit_train_2b)

fit_train_2c <- glm(Win_Bid ~  ((Estimated_Cost__Millions_ + Sector + Estimated_Years_to_Complete) ^2 +
                                  Region_of_Country, data = train, family = binomial("logit")))
summary(fit_train_2c)

fit_train_2d <- glm(Win_Bid ~  Estimated_Cost__Millions_ * Estimated_Years_to_Complete  + Region_of_Country , 
                    data = train, family = binomial("logit"))
summary(fit_train_2d)

fit_train_2e <- glm(Win_Bid ~  Estimated_Cost__Millions_ * Estimated_Years_to_Complete  + Region_of_Country + 
                      Competitor_A + Competitor_B + Competitor_C + Competitor_D + Competitor_E + 
                      Competitor_F + Competitor_G + Competitor_H + Competitor_J, 
                    data = train, family = binomial("logit"))
summary(fit_train_2e)

# making a significant competitor variable to see if it works
train <- train %>% 
  mutate(sig_comp = ifelse(Competitor_B==1 | Competitor_C==1 | Competitor_E==1 | Competitor_F==1 | Competitor_J==1,1,0))

# best model by aic so far
fit_train_2f <- glm(Win_Bid ~  Estimated_Cost__Millions_ * Estimated_Years_to_Complete  + Region_of_Country + 
                      Competitor_B + Competitor_C + Competitor_E + Competitor_F + Competitor_J, 
                    data = train, family = binomial("logit"))
summary(fit_train_2f)
# AIC: 269.15

#################### Moving forward with fit_train_2f #################################

###############################################
# Check for influential points

# fit diagnostics
influence.measures(fit_train_2f)

# Cooks distance
plot(fit_train_2f, 4, id.n = 5) 
# obs 200, 71, 137, 389, 87
# values all at 0.08 or below


cd_check <- slice(train, c(200, 71, 137, 389, 87))
# 200 has very high estimated cost
# 71 has very low estimated cost
# 71 137 389 87 were winning bids
# 137 389 87 were in southwest or west region, few winning bids there



# DFbetas
dfbetas(fit_train_2f)
dfbetasPlots(fit_train_3, id.n = 5)
dfb_check <- slice(train, c(43, 29, 160, 311, 417))
#43 outlier on multiple plots. This was not bid on by Comp B, E, J, all
# three of these tend to bid on nearly everything. Unusual for a project to not
# be bid on by any of these 3. 
# 29, 160, 311, 417 for compC variable. Comp_C had very few bids
# these were ones that Comp_C bid on that were winning bids for IAA



##################### Check linearity ######################################### 
# Only two continuous variables in the model, used in an interaction term
# check if interaction term is improvement over model without interaction term:

visreg(fit_train_2f, "Estimated_Cost__Millions_", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for Estimated_Cost__Millions_",
       x = "Estimated Cost (Millions)", y = "partial (deviance) residuals")

visreg(fit_train_2f, "Estimated_Years_to_Complete", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for Estimated_Years_to_Complete",
       x = "Estimated Years to Complete", y = "partial (deviance) residuals")

# Model without interaction terms
fit_train_no_int <- glm(Win_Bid ~  Estimated_Cost__Millions_ +  Estimated_Years_to_Complete  + Region_of_Country + 
                          Competitor_B + Competitor_C + Competitor_E + Competitor_F + Competitor_J, 
                        data = train, family = binomial("logit"))
summary(fit_train_no_int)
#AIC 274.25

#ROC curves: With and without interactions
pred <- prediction(fitted(fit_train_2f), factor(fit_train_2f$y))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

pred_no_int <- prediction(fitted(fit_train_no_int), factor(fit_train_no_int$y))
perf_no_int <- performance(pred_no_int, measure = "tpr", x.measure = "fpr")

plot(perf, col = "red")
abline(0, 1, lty = 2)
title("ROC Curve for Final Model")

#Check BIC
BIC(fit_train_2f)
BIC(fit_train_no_int)

#####################################################################################################
### ROC Curves: With and without interactions look similar, slight improvement with interactions#####
# at the highest point of the curve, and lower AIC and BIC with interactions#########################
# will continue to use model with interactions. #####################################################
#####################################################################################################



#Calibration curve
obs.phat <- data.frame(y = fit_train_2f$y, phat = fitted(fit_train_2f))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()

# looks pretty good



# Probability density 

df <- data.frame(y = fit_train_2f$y,
                 phat = fitted(fit_train_2f))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "win")

#youden's index
pred <- prediction(fitted(fit_train_2f), factor(fit_train_2f$y))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)
auc <- performance(pred, measure = "auc")@y.values
auc #0.898




### classification table ###
classif_table <- data.frame(threshold = perf@alpha.values[[1]],
                            tpr = perf@y.values[[1]],
                            tnr = 1 - perf@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec), more weight towards picking up true neg
classif_table$youdenJ <- with(classif_table, (0.35*tpr) + (0.65*tnr) - 1)
# find row with max
classif_table[which.max(classif_table$youdenJ),]

## threshold: where to separate the 2 classes based on predicted probability: 0.232
# True positive rate: 0.797
# True negative rate: 0.869
# YoudenJ: -0.156


# discrimination slope = mean(p1) - mean(p0) ###
mean(fitted(fit_train_2f)[fit_train_2f$y == 1]) - mean(fitted(fit_train_2f)[fit_train_3$y == 0])
#0.399


#######################################################################################################
######################################## Now Use Validation data ######################################
#######################################################################################################

#use model on validation data 
pred <- predict(fit_train_2f, newdata = test, type = "response")
test$pred <- pred
# get the actual prediction based on the probabilities of the predicted values
# using threshold from youden's index

pred_2 <- pred %>% 
  as_tibble() %>% 
  add_column(pred = rep(0, nrow(.))) %>% 
  mutate(pred = if_else(value > 0.232, 1, 0))


# coefficient of discrimination
mean(pred_2$value[pred_2$pred == 1]) - mean(pred_2$value[pred_2$pred == 0]) 
#0.386

#Calibration curve
obs.phat <- data.frame(y = test$Win_Bid, phat = test$pred)
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()

# Brier score
mean((test$Win_Bid - pred)^2)
#0.065


### c-statistic and Somers' D ###
## interpretation: for all possible pairs of event 0 and event 1, the model assigned the 
# higher predicted probability to the event 1 c% of the time. If just guessing c=50%
rcorr.cens(pred, test$Win_Bid)[-c(5, 6, 9)] 
# c-stat: 0.927





### ROC curves ###
pred_v <- prediction(pred, factor(test$Win_Bid))
perf_v <- performance(pred_v, measure = "tpr", x.measure = "fpr")
plot(perf_v, colorize = TRUE)
title("ROC Curve for Final Model on Validation Data")
abline(a = 0, b = 1, lty = 2)
performance(pred_v, measure = "auc")@y.values

#AUC: 0.927

### classification table ###
classif_table_v <- data.frame(threshold = perf_v@alpha.values[[1]],
                              tpr = perf_v@y.values[[1]],
                              tnr = 1 - perf_v@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table_v$youdenJ <- with(classif_table_v, (0.35*tpr) + (0.65*tnr) - 1)
## Find Validation Data TPR, TNR: Use 0.232 threshold??? Closest is 0.211
classif_table_v[20,]

## TPR: 0.769
## TNR: 0.906
## YoudenJ: -0.142


# youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table_v$youdenJ <- with(classif_table_v, (0.35*tpr) + (0.65*tnr) - 1)
# find row with max
classif_table_v[which(classif_table_v$youdenJ >= -0.1558071),]
classif_table_v[which.max(classif_table_v$youdenJ),]




