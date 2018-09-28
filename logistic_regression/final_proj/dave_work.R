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
library(ROCR)
library(haven)

library(Hmisc)

con <- haven::read_sas("logistic_regression/final_proj/construction.sas7bdat")

# check summary stats of numeric vars
summ_stat <- con %>% 
  select_if(is.numeric) %>% 
  psych::describe()
table(con$Win_Bid)


# convert response to binary
con_2 <- con %>% 
  modify_at("Win_Bid", as.character) %>% 
  modify_at("Win_Bid", as_factor) %>% 
  modify_at("Win_Bid", as.integer) %>% 
  mutate( Win_Bid = if_else(Win_Bid == 1, 1, 0)) %>% 
  mutate(id = row_number())


# basic model
fit <- glm(Win_Bid ~ ., data = con_2, 
           family = binomial("logit"))

# perfect seperation
check_infinite_estimates(fit)

fit_sep <- glm(Win_Bid ~ ., data = con, 
           family = binomial("logit"),
           method = "detect_separation")
fit_sep

# using bias reduction
fit_br <- brglm(Win_Bid ~ ., data = con, family = binomial("logit"))
summary(fit_br)


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
fit_2_train <- glm(Win_Bid ~ ., data = train, 
             family = binomial("logit"))
# no warning message

# check separation again. 
fit_sep <- glm(Win_Bid ~ ., data = con_3, 
               family = binomial("logit"),
               method = "detect_separation")
fit_sep
# Separation: NO


# Look at plots of each variable vs Win_Bid.... gotta be a better way to do this but I don't know it!

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


# trying some different combinations of predictors
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

# checing var swap for multicollinearity
fit_train_2g <- glm(Win_Bid ~  Bid_Price__Millions_ * Estimated_Years_to_Complete  + Region_of_Country + 
                      Competitor_B + Competitor_C + Competitor_E + Competitor_F + Competitor_J, 
                    data = train, family = binomial("logit"))
summary(fit_train_2g)
# AIC: 269.15


fit_train_3 <- glm(Win_Bid ~  Estimated_Cost__Millions_+ Competitor_B + Competitor_C +
                     Competitor_E + Competitor_F + Competitor_J, data = train, family = binomial("logit"))
summary(fit_train_3)
#AIC 275.46
#most impactful variables: Competitor C, F, E, B. Cost has very small effect. 





###############################################
# Check for influential points

# fit diagnostics
influence.measures(fit_train_3)

# Cooks distance
plot(fit_train_2f, 4, n.id = 5) 
# obs 71, 137, 200
# values all at 0.08 or below, very low!
# Not concerned about influential points

cd_check <- slice(train, c(71,137, 200))



# DFbetas
dfbetas(fit_train_2f)
dfbetasPlots(fit_train_2f, id.n = 5)
dfb_check <- slice(train, c(43 ))


# Check linearity of continuous variables 
visreg(fit_train_2f, "Estimated_Cost__Millions_", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for Est Cost",
       x = "Estimated_Cost__Millions_", y = "partial (deviance) residuals")




# Check interactions
#most impactful variables: Competitor C, F, E, B. Cost has very small effect.
fit_int <- brglm(Win_Bid ~  Estimated_Cost__Millions_+ Competitor_B + Competitor_C +
                   Competitor_E + Competitor_F + Competitor_J, Competitor_C*Competitor_F*Competitor_E*Competitor_B, data = train, family = binomial("logit"))
summary(fit_int)
#getting errors with interaction terms




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
auc #0.898048

### classification table ###
classif_table <- data.frame(threshold = perf@alpha.values[[1]],
                            tpr = perf@y.values[[1]],
                            tnr = 1 - perf@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec), more weight towards picking up true neg
classif_table$youdenJ <- with(classif_table, (0.35*tpr) + (0.65*tnr) - 1)
# find row with max
classif_table[which.max(classif_table$youdenJ),]

## threshold: where to separate the 2 classes based on predicted probability: 0.291
# True positive rate: 0.7972973
# True negative rate: 0.8694444
# YoudenJ: -0.1558071


# discrimination slope = mean(p1) - mean(p0) ###
mean(fitted(fit_train_2f)[fit_train_2f$y == 1]) - mean(fitted(fit_train_2f)[fit_train_2f$y == 0])
#0.3987412


#######################################################################################################
######################################## Now Use Validation data ######################################
#######################################################################################################

#use model on validation data 
pred <- predict(fit_train_2f, newdata = test, type = "response")

# get the actual prediction based on the probabilities of the predicted values
# using threshold from youden's index

pred_2 <- pred %>% 
  as_tibble() %>% 
  add_column(pred = rep(0, nrow(.))) %>% 
  mutate(pred = if_else(value > 0.2315702, 1, 0))


# coefficient of discrimination
mean(pred_2$value[pred_2$pred == 1]) - mean(pred_2$value[pred_2$pred == 0]) 
#0.3857614


# Brier score
brier_score <- mean((test$Win_Bid - pred)^2)
#0.0651423

brier_max <- mean((test$Win_Bid - mean(test$Win_Bid))^2)

# scaled brier score
# this is basically the %improvement over null/intercept-only model
brier_scaled <- 1 - (brier_score/brier_max)

### c-statistic and Somers' D ###
## interpretation: for all possible pairs of event 0 and event 1, the model assigned the 
# higher predicted probability to the event 1 c% of the time. If just guessing c=50%
rcorr.cens(pred, test$Win_Bid)[-c(5, 6, 9)] 
# c-stat: 0.927





### ROC curves ###
pred_v <- prediction(pred, factor(test$Win_Bid))
perf_v <- performance(pred_v, measure = "tpr", x.measure = "fpr")
plot(perf_v, colorize = TRUE)
abline(a = 0, b = 1, lty = 2)
auc <- performance(pred_v, measure = "auc")@y.values
auc
#AUC: 0.9270833

### classification table ###
classif_table_v <- data.frame(threshold = perf_v@alpha.values[[1]],
                              tpr = perf_v@y.values[[1]],
                              tnr = 1 - perf_v@x.values[[1]])

# youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table_v$youdenJ <- with(classif_table_v, (0.35*tpr) + (0.65*tnr) - 1)
# find row with max
classif_table_v[which(classif_table_v$youdenJ >= -0.1558071),]
## threshold 0.2676357
## TPR: 0.9230769
## TNR: 0.8020833
## YoudenJ: -0.1558071 from the highest max training class table

### compute a bunch of stuff ###
fitstat <- function(obj, new_x = NULL, new_y = NULL){
  # this function computes a bunch of fitstats.
  #
  # inputs:
  # 1. obj: a data frame or a model object from glm() or gam()
  #         the data frame must have a vector of responses "y" AND a vector of
  #         either probabilities "p" or linear predictor "lp"
  #         (linear predictors are obtained with predict(..., type = "link"))
  # 2. new_x: specify new dataset to get predictions for new obs. if NULL,
  #           results will be computed using original obj data
  # 3. new_y: use new responses. if NULL, results will be computed using 
  #           original obj data
  #
  # output: a list of two elements
  #   1. fitstats = coefficient of discrimination, raw/max/scaled brier scores,
  #      c-statistic, Somers' D, stdev of c/Somers, Nagelkerke R^2
  #   2. est = a data frame with y (new_y), p (predicted probabilities for
  #      new_y), and lp (predicted linear predictor for new_y). these will be
  #      same as those in obj if no new data is given
  
  # check structure
  if(is.null(obj$y)){
    stop("obj must be a model object or a data frame with arguments y and either
         p or lp")
  }
  
  # if no new data given, use old response
  if(is.null(new_y)){
    new_y <- obj$y
  }
  
  # get predicted probabilities and linear predictors for data
  if(any(class(obj) == "glm")){
    # predict from model object
    p_hat <- predict(obj, newdata = new_x, type = "response")
    lp <- predict(obj, newdata = new_x, type = "link")
  } else if(is.null(obj$lp)){
    # predict from data frame
    p_hat <- obj$p
    lp <- qlogis(p_hat)
  } else {
    lp <- obj$lp
    p_hat <- plogis(lp)
  }
  
  # remove missing values
  new_y <- new_y[!is.na(p_hat)]
  p_hat <- p_hat[!is.na(p_hat)]
  lp <- lp[!is.na(p_hat)]
  
  ### compute coefficient of discrimination ###
  # D = 0.5(r2res + r2mod), where r2res = 1 - (brier/brier_max), and
  # r2mod = sum(((p_hat - p_obs)^2))/sum(((new_y - p_obs)^2))
  p1 <- p_hat[new_y == 1]
  p0 <- p_hat[new_y == 0]
  coef_discrim <- mean(p1) - mean(p0)
  
  ### compute [scaled] brier score ###
  # raw brier score
  brier_score <- mean((new_y - p_hat)^2)
  
  # max brier score is just the observed proportion
  # brier_max <- p_obs*((1 - p_obs)^2) + (1 - p_obs)*(p_obs^2)
  brier_max <- mean((new_y - mean(new_y))^2)
  
  # scaled brier score
  # this is basically the %improvement over null/intercept-only model
  brier_scaled <- 1 - (brier_score/brier_max)
  
  ### compute c-statistic/Somers' Dxy ###
  rank_stats <- as.numeric(Hmisc::rcorr.cens(p_hat, new_y))
  somers <- rank_stats[2] # Somers' D
  c_stat <- rank_stats[1] # c-statistic
  rankSD <- rank_stats[3] # standard deviation for c & somers'
  
  ### compute nagelkerke r2 ###
  # like scaled brier score, indicates %improvement over null model
  p_obs <- mean(new_y) # mean/observed proportion of data
  mod_dev <- -2*sum(new_y*lp - log(1 + exp(lp))) # model deviance
  nullmod_int <- qlogis(p_obs) # intercept for null model
  null_dev <- -2*sum(new_y*nullmod_int - log(1 + exp(nullmod_int))) # null dev
  LRstat <- null_dev - mod_dev # likelihood ratio statistic
  n_obs <- length(new_y) # sample size
  R2nagelkerke <- (1 - exp(-LRstat/n_obs))/(1 - exp(-null_dev/n_obs))
  
  fitstats <- data.frame(coef_discrim = coef_discrim,
                         brier_score = brier_score,
                         brier_max = brier_max,
                         brier_scaled = brier_scaled,
                         somers = somers,
                         c_stat = c_stat,
                         rankSD = rankSD,
                         R2nagelkerke = R2nagelkerke)
  
  predictions <- data.frame(y = new_y,
                            p = p_hat,
                            lp = lp)
  
  res <- list("fitstats" = fitstats,
              "est" = predictions)
  return(res)
  }


fitstat(fit_train_2f, new_y = test$Win_Bid)

# playing with 5fold cv
# make training and testing data
train <- con_2  %>% 
  sample_frac(.8)
test <- anti_join(con_2, train, by = "id")
train <- train %>% 
  select(-id)
test <- test %>% 
  select(-id)
# test brglm on train and predict on test
set.seed(1234)
fit_train <- brglm(Win_Bid ~ ., data = train, 
                   family = binomial("logit"))
summary(fit_train)


pred <- predict(fit_train, newdata = test, type = "response")

# get the actual prediction based on the probabilities of the predicted values
pred_2 <- pred %>% 
  as_tibble() %>% 
  add_column(pred = rep(0, nrow(.))) %>% 
  mutate(pred = if_else(value > 0.5, 1, 0))

# checking the accuracy of results
MLmetrics::Accuracy(pred_2$pred, test$Win_Bid)


# fit diagnostics
influence.measures(fit_train)

# Cooks distance
plot(fit_train, 4, n.id = 5) # n.id = #points identified on the plot
# obs 100, 431 432

cd_check <- slice(train, c(69, 137, 241))

# DFbetas
dfbetas(fit_train)
dfbetasPlots(fit_train, id.n = 5)
# multiple points worth investigating further


# 5fold cv
set.seed(10392)
con_2 %>% crossv_kfold(5)

fit <- brglm(Win_Bid ~ ., data=con_2)
augment(fit) %>% select(.fitted, everything()) %>% head()


train %>%
  crossv_kfold(5) %>%
  mutate(model = purrr::map(train, ~glm(Win_Bid ~  Estimated_Cost__Millions_ * Estimated_Years_to_Complete  + Region_of_Country + 
                                          Competitor_B + Competitor_C + Competitor_E + Competitor_F + Competitor_J, 
                                        data = ., family = binomial("logit")))) -> trained.models
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
  summarise(auc = pROC::roc(Win_Bid, .fitted)$auc) %>%
  select(auc)


train.predictions <- trained.models %>% 
  unnest( fitted = map2(model, train, ~augment(.x, newdata = .y)),
          pred = map2( model, train, ~predict( .x, .y, type = "response")) )
train.predictions %>%
  group_by(.id) %>%
  summarise(auc = pROC::roc(Win_Bid, .fitted)$auc) %>% ### outcome from the true data, .fitted from augment's output. Run roc() on these columns and pull out $auc!
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
  summarise(TPR = TP/(TP+FN),
            TNR = TN/(TN+FP),
            Accuracy = (TP+TN)/(TP+TN+FP+FN),
            PPV = TP/(TP+FP)) -> fold.metrics
fold.metrics

## Finally we can summarize:
mean_metrics <- fold.metrics %>% summarise(maxTPR = max(TPR), maxTNR = max(TNR), meanAcc = mean(Accuracy), meanPPV=mean(PPV))

# calculating youdens index
you_j <-  mean_metrics$maxTPR + mean_metrics$maxTNR - 1
w <- .25
you_jw <-  2 * (w * mean_metrics$meanTPR + (1  -w) * mean_metrics$meanTNR) - 1


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


MLmetrics::Accuracy(pred, test$Win_Bid)
