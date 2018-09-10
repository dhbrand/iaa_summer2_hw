library(tidyverse)
library(brglm2)

con <- haven::read_sas("logistic_regression/final_proj/construction.sas7bdat")

# check summary stats of numeric vars
summ_stat <- con %>% 
  select_if(is.numeric) %>% 
  map_df(psych::describe)


# convert response to binary
con_2 <- con %>% 
  modify_at("Win_Bid", as.character) %>% 
  modify_at("Win_Bid", as_factor) %>% 
  modify_at("Win_Bid", as.integer) %>% 
  mutate( Win_Bid = if_else(Win_Bid == 1, 1, 0))


# basic model
fit <- glm(Win_Bid ~ ., data = con, 
           family = binomial("logit"))

# perfect seperation
check_infinite_estimates(fit)

fit_sep <- glm(Win_Bid ~ ., data = con, 
           family = binomial("logit"),
           method = "detect_separation")
fit_sep

# using bias reduction
fit_br <- brglm(Win_Bid ~ ., data = con, 
               family = binomial("logit"))
summary(fit_br)

# make training and testing data
train <- con_2 %>% 
  sample_frac(.8)
test <- anti_join(con_2, train)

# test brglm on train and predict on test

fit_train <- brglm(Win_Bid ~ ., data = train, 
                family = binomial("logit"))
summary(fit_train)

pred <- predict(fit_train, newdata = test, type = "response")

pred_2 <- pred %>% 
  as_tibble() %>% 
  add_column(pred = rep(0, nrow(.))) %>% 
  mutate(pred = if_else(value > 0.5, 1, 0))

MLmetrics::Accuracy(pred_2$pred, test$Win_Bid)
