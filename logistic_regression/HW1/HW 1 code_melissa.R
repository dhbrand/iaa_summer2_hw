library(tidyverse)
library(haven)
library(broom)
library(MASS)
library(visreg)
library(brglm)


# read the sas dataset into an R dataframe
train <- read_sas("C:\\Users\\Melissa Sandahl\\OneDrive\\Documents\\School\\MSA courses\\AA502\\Logistic regression\\data\\insurance_t.sas7bdat")

# look at the structure of the dataframe to see the class of each variable
str(train)
p_summary <- psych::describe(train)
h_summary <- Hmisc::describe(train)
summary(train)

# create a character vector which counts all the missing values for variables
missing <- train %>% 
  dplyr::select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.))))

# create a new dataframe without the variables with missing values
train_no_NAs <- train %>% 
  dplyr::select(-!! names(missing))

# create a dataframe which creates the 5 number summary of the variables
train_summary <- tidy(summary(train_no_NAs))

#Histograms of selected variables without missing values
ggplot(gather(train_no_NAs, key, value, -c(BRANCH, RES)), aes(value)) +
  geom_histogram(color = "blue") +
  facet_wrap(~ key, scales = "free") + theme_bw() +
  labs(y = "Frequncy of Each Variable", x = "Multiple Variables Defined Above", title = "Histograms for the Variables with no Missing Observations")

# filtering binary variables where one count is less than 1000
check <- train_no_NAs %>%
  # select columns which only have 2 distint values (ie 0 and 1)
  select_if(~ n_distinct(.) ==2) %>% 
  # find sum of those columns which should be count of 1's
  map_dbl(~ sum(.))

# find the names of the columns above that are under 100
low_count_var <- names(which(check < 1000, useNames = TRUE))
low_count_var

train_reduced <- train_no_NAs %>% 
  # drop the columns from above
  dplyr::select(-c(!! low_count_var)) 

# checking the histograms of the new reduced df
ggplot(gather(train_reduced, key, value, -c(BRANCH, RES)), aes(value)) +
  geom_histogram(color = "blue") +
  facet_wrap(~ key, scales = "free") +
  theme_bw()



# Histograms of the selected variables
ggplot(gather(train_reduced, key, value, -c(BRANCH, RES)), aes(value)) +
  geom_histogram() +  facet_wrap(~ key, scales = "free")




#Run logistic regression with all train_reduced variables included
fit <- glm(INS ~ DDA + DDABAL + DEP + DEPAMT + CHECKS + 
             DIRDEP + TELLER + SAV + SAVBAL + ATM + 
             ATMAMT + RES + BRANCH,
           data = train_reduced, family = binomial(link = "logit"))
summary(fit)
exp(confint(fit))

#Remove variables with pvalues above 0.05 in "fit". Rerun logistic regression.
# Removed were: DEPTAMT, DIRDEP, RES, BRANCH

fit2 <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
            + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
            data = train_reduced, family = binomial(link = "logit"))
summary(fit2)
sort(abs(fit2$coefficients))
exp(confint(fit2))
diff(exp(fit2$coefficients))
exp(fit2$coefficients)

# Comparing the AIC for removal of columns that might seem redundant
fit3 <- glm(INS ~ DDABAL + DEP + CHECKS + TELLER 
            + SAVBAL + ATMAMT + BRANCH,
            data = train_reduced, family = binomial(link = "logit"))
summary(fit3)

# adding the credit score back in because it only had 195 missing obs
fit4 <- glm(INS ~ DDABAL + DEP + CHECKS + TELLER 
            + SAVBAL + ATMAMT + BRANCH + CRSCORE,
            data = cbind(train_reduced, CRSCORE = train$CRSCORE), family = binomial(link = "logit"))
summary(fit4)


# Checking distributions of variables with missing information
train_missing <- train %>% 
  dplyr::select(!! names(missing))


#Plot of variables with missing values
ggplot(gather(train_missing, key, value), aes(value)) +
  geom_histogram(color = "blue") +facet_wrap(~ key, scales = "free") + theme_bw() +
  labs(y = "Frequncy of Each Variable", x = "Multiple Variables Defined Above", title = "Histograms for the Variables with Missing Observations")




#Dataset with only the variables in the model
train_model_vars <- train %>%
  dplyr::select(INS, DDA, DDABAL, DEP, CHECKS, TELLER, SAV, SAVBAL, ATM, ATMAMT, BRANCH)

#Removing Branch from the list in order to plot histograms
train_model_no_branch <- train %>%
  dplyr::select(INS, DDA, DDABAL, DEP, CHECKS, TELLER, SAV, SAVBAL, ATM, ATMAMT)


#Plotting histograms of the variables selected for the model, not including Branch.
ggplot(gather(train_model_no_branch, key, value), aes(value)) + geom_histogram(color = "blue") +
  facet_wrap(~ key, scales = "free") + theme_bw() +
  labs(y = "Frequncy of Each Variable", x = "Multiple Variables Defined Above", title = "Histograms for Model Variables")

#Count the number of customers that purchased the insurance product and those that didn't.
counts <- train %>%
  count(INS) %>%
  mutate(prop = prop.table(n))
counts
