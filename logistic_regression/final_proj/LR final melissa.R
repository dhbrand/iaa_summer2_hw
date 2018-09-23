library(tidyverse)
install.packages('brglm2')
library(brglm2)
library(modelr)
library(broom)
library(car)
library(mgcv)
library(DescTools)
library(psych)
install.packages('psych')

con <- haven::read_sas("C:\\Users\\Melissa Sandahl\\OneDrive\\Documents\\School\\MSA courses\\AA502\\Logistic regression\\data\\construction.sas7bdat")
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

con_3 <- con_2 %>%
  dplyr::select(-Winning_Bid_Price__Millions_)


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


# Look at plots of each variable vs Win_Bid.... gotta be a better way to do this but I don't know it!

ggplot(con_3, aes(x=Win_Bid, y=Estimated_Cost__Millions_)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Estimated_Years_to_Complete)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Bid_Price__Millions_)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Sector)) + geom_jitter()
ggplot(con_3, aes(x=Win_Bid, y=Number_of_Competitor_Bids)) + geom_jitter()
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
#AIC 213.71



########### Removing variables from model. Remove cost after engineering estimate, this info is not available during bid process

fit_train_2 <- glm(Win_Bid ~ Estimated_Cost__Millions_ + Estimated_Years_to_Complete + Bid_Price__Millions_ + Sector +
                       Region_of_Country + Number_of_Competitor_Bids + Competitor_A + Competitor_B + Competitor_C + Competitor_D +
                       Competitor_E + Competitor_F + Competitor_G + Competitor_H + Competitor_J, data = train, family = binomial("logit"))
summary(fit_train_2)
#AIC 211.14

#check for multicollinearity
vif(fit_train_2)
#estimated cost millions and bid price millions are highly correlated
# need to remove one


# Rmove Bid_Price__Millions_, this model should be used before setting a bid price so this should be removed anyway
fit_train_3 <- glm(Win_Bid ~  Estimated_Cost__Millions_+ Estimated_Years_to_Complete + Sector +
                       Region_of_Country + Number_of_Competitor_Bids + Competitor_A + Competitor_B + Competitor_C + Competitor_D +
                       Competitor_E + Competitor_F + Competitor_G + Competitor_H + Competitor_J, data = train, family = binomial("logit"))
summary(fit_train_3)
#aic 239.7
vif(fit_train_3)

############### Remove insignificant variables Estimated_Cost__Millions_, Estimated_Years_to_Complete, Sector, Comp A, G, D, J, Region

 
fit_train_4 <- glm(Win_Bid ~  Number_of_Competitor_Bids + Competitor_B + Competitor_C + 
                     Competitor_E + Competitor_F + Competitor_H, data = train, family = binomial("logit"))
summary(fit_train_4)
#AIC 235.41
#most impactful variables: competitor C, competitor F, competitor E
# this only leaves information about competitor bids in the model. Is this information available when making the bid???


# what if you don't have competitor bid information?

fit_train_5 <- glm(Win_Bid ~  Estimated_Cost__Millions_+ Estimated_Years_to_Complete + Sector +
                     Region_of_Country, data = train, family = binomial("logit"))
summary(fit_train_5)
# in this case estimated cost and region are significant




#################### What model to continue with??? Sector not significant with and without competitor bid information. Remove this. 

fit_train_7 <- glm(Win_Bid ~  Estimated_Cost__Millions_+ Estimated_Years_to_Complete + 
                     Region_of_Country + Number_of_Competitor_Bids + Competitor_A + Competitor_B + Competitor_C + Competitor_D +
                     Competitor_E + Competitor_F + Competitor_G + Competitor_H + Competitor_J, data = train, family = binomial("logit"))
summary(fit_train_7)
#AIC 237.92


############## STOP HERE FOR NOW #################################










###############################################
# Check for influential points

# fit diagnostics
influence.measures(fit_train_3)

# Cooks distance
plot(fit_train_3, 4, n.id = 5) 
# obs 20, 137, 327
# values all at 0.06 or below, very low!

cd_check <- slice(train, c(20, 137, 327))
 
#20 and 327 were high bids
# 137? 

# DFbetas
dfbetas(fit_train_3)
dfbetasPlots(fit_train_3, id.n = 5)
dfb_check <- slice(train, c(76, 80, 109, 137, 245, 307, 319, 373, 389 ))

# most of these are high bids
# 109: high years to complete
# don't see anything problematic








pred <- predict(fit_train_3, newdata = test, type = "response")

# get the actual prediction based on the probabilities of the predicted values
pred_2 <- pred %>% 
  as_tibble() %>% 
  add_column(pred = rep(0, nrow(.))) %>% 
  mutate(pred = if_else(value > 0.5, 1, 0))

# checking the accuracy of results
MLmetrics::Accuracy(pred_2$pred, test$Win_Bid)





# 5fold cv
set.seed(10392)
con_2 %>% crossv_kfold(5)

fit <- brglm(Win_Bid ~ ., data=con_2)
augment(fit) %>% select(.fitted, everything()) %>% head()


con_2 %>%
  crossv_kfold(5) %>%
  mutate(model = purrr::map(train, ~brglm(Win_Bid ~ ., data=.))) -> trained.models
trained.models

map2_dbl(trained.models$model, trained.models$test, rmse) -> test.rmse
test.rmse

## Convert to data frame and plot:
as.data.frame(test.rmse) %>% ggplot(aes(x="", y=test.rmse)) + geom_boxplot()


map2_dbl(trained.models$model, trained.models$train, rmse) -> train.rmse
## Convert these to **vectors** to run the test below
as.numeric(test.rmse) -> test.rmse2
as.numeric(train.rmse) -> train.rmse2

## Run a test on train/test rmse, remembering that these are PAIRED by k-fold!
wilcox.test(test.rmse2, train.rmse2, paired=T)

trained.models %>%
  unnest( pred = map2( model, test, ~predict( .x, .y, type = "response")) ) -> test.predictions
test.predictions


trained.models %>%
  unnest( fitted = map2(model, test, ~augment(.x, newdata = .y)),
          pred = map2( model, test, ~predict( .x, .y, type = "response")) ) -> test.predictions
test.predictions %>% select(.id, Win_Bid, pred )

test.predictions %>%
  group_by(.id) %>%
  summarize(auc = pROC::roc(Win_Bid, .fitted)$auc) %>%
  select(auc)


train.predictions <- trained.models %>% 
  unnest( fitted = map2(model, train, ~augment(.x, newdata = .y)),
          pred = map2( model, train, ~predict( .x, .y, type = "response")) )
train.predictions %>%
  group_by(.id) %>%
  summarize(auc = pROC::roc(Win_Bid, .fitted)$auc) %>% ### outcome from the true data, .fitted from augment's output. Run roc() on these columns and pull out $auc!
  select(auc)


## How to change pred column from probability to real prediction
test.predictions %>%
  select(.id, Win_Bid, pred ) %>%
  mutate(pred = ifelse(pred >= 0.5, 1, 0))

## Tally it all up by fold
test.predictions %>%
  select(.id, Win_Bid, pred ) %>%
  mutate(pred = ifelse(pred >= 0.5, 1, 0)) %>%
  group_by(.id, Win_Bid, pred) %>% 
  tally()


## Create a dataframe confusion matrix
test.predictions %>%
  select(.id, Win_Bid, pred ) %>%
  mutate(pred = ifelse(pred >= 0.5, 1, 0)) %>%
  group_by(.id, Win_Bid, pred) %>%
  tally() %>%
  mutate(class = case_when(Win_Bid == pred & pred == 1 ~ "TP",
                           Win_Bid != pred & pred == 1 ~ "FP",
                           Win_Bid == pred & pred == 0 ~ "TN",
                           TRUE ~ "FN")) %>%
  ungroup() %>% ### We want to ditch the `outcome` column, so remove it from grouping
  select(.id, n, class) %>% ### Retain only columns of interest; use spread to get a column per classification type
  spread(class, n) -> confusion
confusion


## Some classifer metric across all folds
confusion %>%
  mutate_all(funs(replace_na(.,0))) %>% 
  group_by(.id) %>%
  summarize(TPR = TP/(TP+FN),
            Accuracy = (TP+TN)/(TP+TN+FP+FN),
            PPV = TP/(TP+FP)) -> fold.metrics
fold.metrics

## Finally we can summarize:
fold.metrics %>% summarize(meanTPR = mean(TPR), meanAcc = mean(Accuracy), meanPPV=mean(PPV))



# looking at GAMs
iv_string <- paste(names(train)[-1], collapse=" + ")
gam_formula <- as.formula(Win_Bid ~ s(Estimated_Cost__Millions_) + UQ(iv_string))

fit.gam <- gam(gam_formula,
               data = train, family = binomial, method = "REML")
summary(fit.gam)


#more model assessment
PseudoR2(fit_train, which = c("Cox", "Nagelkerke", "McFadden"))

brier_score(fit_train)

### discrimination slope = mean(p1) - mean(p0) ###
D <- mean(fitted(fit_train)[fit_train$y == 1]) - mean(fitted(fit_train)[fit_train$y == 0])


