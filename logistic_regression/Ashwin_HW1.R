library(tidyverse)
library(haven)
library(broom)

# Possible libraries needed for logistic regression
library(MASS)
library(visreg)
library(brglm)
# read the sas dataset into an R dataframe
train <- read_sas("logistic_regression//insurance_t.sas7bdat")

# look at the structure of the dataframe to see the class of each variable
str(train)


# create a character vector which counts all the missing values for variables
missing <- train %>% 
  dplyr::select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.))))

# create a new dataframe without the variables with missing values
train_no_NAs <- train %>% 
  dplyr::select(-!! names(missing))

# create a dataframe which creates the 5 number summary of the variables
train_summary <- tidy(summary(train_no_NAs))

# kinda of just eyeballing quickly
# vars with lots of 0 to 1 range
# DDA, DEP, CASHBK, DIRDEP, NSF, NSFAMT, TELLER, SAV,
# SAVBAL, ATM, CD, CDBAL, IRA, IRABAL, LOC, LOCBAL, ILS, 
# ILSBAL, MM, MMBAL, MMCRED, MTG, MTGBAL, SDB, MOVED, 
# INAREA, INS

# reduced dataframe based on the train_summary output
# train_reduced <- train_no_NAs %>% 
#   select(-c(DDA, DEP, CASHBK, DIRDEP, NSF, NSFAMT, TELLER, SAV,
#             SAVBAL, ATM, CD, CDBAL, IRA, IRABAL, LOC, LOCBAL, ILS, 
#             ILSBAL, MM, MMBAL, MMCRED, MTG, MTGBAL, SDB, MOVED, 
#             INAREA, INS))

# create histograms for all the variables except branch and res because they are character
# the gather function puts all of the variable names into one column called key and all of their 
# values ( the actual numbers) into a column called value
# the only aesthetic is the values since were using histograms
ggplot(gather(train_no_NAs, key, value, -c(BRANCH, RES)), aes(value)) +
  # call to the histrogram function
  geom_histogram(color = "blue") +
  # facet wrap creates a plot for each of the variables defined, here were using key to contain the variable 
  # names so we say "by row" using the tilda, scales need to be free meaning let the x and y axes be scaled 
  # according to the variable in the plot instead all the same
  facet_wrap(~ key, scales = "free") +
  # better looking theme
  theme_bw()

# filtering binary variables where one count is less than 1000
check <- train_no_NAs %>%
  # select columns which only have 2 distint values (ie 0 and 1)
  select_if(~ n_distinct(.) ==2) %>% 
  # find sum of those columns which should be count of 1's
  map_dbl(~ sum(.))

# find the names of the columns above that are under 100
low_count_var <- names(which(check < 1000, useNames = TRUE))


train_reduced <- train_no_NAs %>% 
  # drop the columns from above
  dplyr::select(-c(!! low_count_var)) 

# checking the histograms of the new reduced df
ggplot(gather(train_reduced, key, value, -c(BRANCH, RES)), aes(value)) +
  geom_histogram(color = "blue") +
  facet_wrap(~ key, scales = "free") +
  theme_bw()


# reduced dataframe based on the histograms. Deleted these variables due to 
# minimal variation in the data
# train_reduced <- train_no_NAs %>% 
#   dplyr::select(-c(CASHBK, NSF, NSFAMT,
#                    CD, CDBAL, IRA, IRABAL, LOC, LOCBAL, ILS, 
#                    ILSBAL, MM, MMBAL, MMCRED, MTG, MTGBAL, SDB, MOVED, 
#                    INAREA))

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
set.seed(1234)
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
# All variables in "fit2" model are significant. These are:
# ATM
# DEP: checking deposits
# ATMAMT: ATM withdrawal amount
# SAVBAL: saving balance
# SAV: saving account
# Teller: teller visits
# Checks: # checks
# DDABAL: checking balance
# DDA: checking account



# Checking distributions of variables with missing information
train_missing <- train %>% 
  dplyr::select(!! names(missing))


ggplot(gather(train_missing, key, value), aes(value)) +
  # call to the histrogram function
  geom_histogram(color = "blue") +
  # facet wrap creates a plot for each of the variables defined, here were using key to contain the variable 
  # names so we say "by row" using the tilda, scales need to be free meaning let the x and y axes be scaled 
  # according to the variable in the plot instead all the same
  facet_wrap(~ key, scales = "free") +
  # better looking theme
  theme_bw()
