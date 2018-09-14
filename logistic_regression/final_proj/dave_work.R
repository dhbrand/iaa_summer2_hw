library(tidyverse)
library(brglm)
library(modelr)
library(broom)
library(car)
library(mgcv)
library(DescTools)

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
