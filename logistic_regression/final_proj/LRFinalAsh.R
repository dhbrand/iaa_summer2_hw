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
               family = binomial("logit"))
               # ,               method = "detect_separation")
fit_sep
# Separation: NO


# Look at plots of each variable vs Win_Bid.... gotta be a better way to do this but I don't know it!
ggplot(con_3, aes(x=Win_Bid, y=Estimated_Cost__Millions_)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Estimated_Years_to_Complete)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Bid_Price__Millions_)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Sector)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Region_of_Country)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Cost_After_Engineering_Estimate_)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Competitor_A)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Competitor_B)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Competitor_C)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Competitor_D)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Competitor_E)) + geom_jitter()  
ggplot(con_3, aes(x=Win_Bid, y=Competitor_F)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Competitor_G)) + geom_jitter()
ggplot(con_2, aes(x=Win_Bid, y=Competitor_H)) + geom_jitter()
ggplot(con_2, aes(x=Win_Bid, y=Competitor_I)) + geom_jitter()
ggplot(con_2, aes(x=Win_Bid, y=Competitor_J)) + geom_jitter()






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

fit_train_4 <- glm(Win_Bid ~  Estimated_Cost__Millions_+ Competitor_B + Competitor_C  + 
                     Competitor_E + Competitor_F + Competitor_J + Competitor_H, data = train, family = binomial("logit"))
summary(fit_train_4)
#AIC 273.83



###############################################
# Check for influential points

# fit diagnostics
influence.measures(fit_train_3)

# Cooks distance
plot(fit_train_4, 4, n.id = 5) 
# obs 57, 71, 200
# values all at 0.06 or below, very low!
# Not concerned about influential points

cd_check <- slice(train, c(57, 71, 200))



# DFbetas
dfbetas(fit_train_4)
dfbetasPlots(fit_train_4, id.n = 5)
dfb_check <- slice(train, c(43 ))


# Check linearity of continuous variables 
visreg(fit_train_4, "Estimated_Cost__Millions_", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for Est Cost",
       x = "Estimated_Cost__Millions_", y = "partial (deviance) residuals")




# Check interactions
#most impactful variables: Competitor C, F, E, B. Cost has very small effect.
fit_int <- brglm(Win_Bid ~  Estimated_Cost__Millions_+ Competitor_B + Competitor_C +
                   Competitor_E + Competitor_F + Competitor_J + Competitor_H, Competitor_C*Competitor_F*Competitor_E*Competitor_B, 
                 data = train, family = binomial("logit"))
summary(fit_int)
#getting errors with interaction terms

fit_int1 <- glm(Win_Bid ~  Estimated_Cost__Millions_+ Competitor_B + Competitor_C*Competitor_F + 
                    Competitor_E + Competitor_J + Competitor_H, 
                 data = train, family = binomial("logit"))
summary(fit_int1)




#Calibration curve
obs.phat <- data.frame(y = fit_train_3$y, phat = fitted(fit_train_3))
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

df <- data.frame(y = fit_train_3$y,
                 phat = fitted(fit_train_3))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "win")

#youden's index
pred <- prediction(fitted(fit_train_3), factor(fit_train_3$y))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)
auc <- performance(pred, measure = "auc")@y.values
auc #0.873

### classification table ###
classif_table <- data.frame(threshold = perf@alpha.values[[1]],
                            tpr = perf@y.values[[1]],
                            tnr = 1 - perf@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec), more weight towards picking up true neg
classif_table$youdenJ <- with(classif_table, (0.35*tpr) + (0.65*tnr) - 1)
# find row with max
classif_table[which.max(classif_table$youdenJ),]

## threshold: where to separate the 2 classes based on predicted probability: 0.291
# True positive rate: 0.649
# True negative rate: 0.922
# YoudenJ: -0.174


# discrimination slope = mean(p1) - mean(p0) ###
mean(fitted(fit_train_3)[fit_train_3$y == 1]) - mean(fitted(fit_train_3)[fit_train_3$y == 0])
#0.369


#######################################################################################################
######################################## Now Use Validation data ######################################
#######################################################################################################

#use model on validation data 
pred <- predict(fit_train_3, newdata = test, type = "response")

# get the actual prediction based on the probabilities of the predicted values
# using threshold from youden's index

pred_2 <- pred %>% 
  as_tibble() %>% 
  add_column(pred = rep(0, nrow(.))) %>% 
  mutate(pred = if_else(value > 0.291, 1, 0))


# coefficient of discrimination
mean(pred_2$value[pred_2$pred == 1]) - mean(pred_2$value[pred_2$pred == 0]) 
#0.367


# Brier score
mean((test$Win_Bid - pred)^2)
#0.074


### c-statistic and Somers' D ###
## interpretation: for all possible pairs of event 0 and event 1, the model assigned the 
# higher predicted probability to the event 1 c% of the time. If just guessing c=50%
rcorr.cens(pred, test$Win_Bid)[-c(5, 6, 9)] 
# c-stat: 0.902





### ROC curves ###
pred_v <- prediction(pred, factor(test$Win_Bid))
perf_v <- performance(pred_v, measure = "tpr", x.measure = "fpr")
plot(perf_v, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)
performance(pred_v, measure = "auc")@y.values

#AUC: 0.902

### classification table ###
classif_table_v <- data.frame(threshold = perf_v@alpha.values[[1]],
                              tpr = perf_v@y.values[[1]],
                              tnr = 1 - perf_v@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table_v$youdenJ <- with(classif_table_v, (0.35*tpr) + (0.65*tnr) - 1)
# find row with max
classif_table_v[which.max(classif_table_v$youdenJ),]
## threshold 0.284
## TPR: 0.769
## TNR: 0.938
## YoudenJ: -0.121




