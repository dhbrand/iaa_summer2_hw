library(tidyverse)
library(haven)
library(broom)

# read the sas dataset into an R dataframe

train <- read_sas("C:\\Users\\Melissa Sandahl\\OneDrive\\Documents\\School\\MSA courses\\AA502\\Logistic Regression\\data\\insurance_t.sas7bdat")

# look at the structure of the dataframe to see the class of each variable
str(train)

# create a character vector which counts all the missing values for variables
missing <- train %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.))))

# create a new dataframe without the variables with missing values
train_no_NAs <- train %>% 
  select(-!! names(missing))

# create a dataframe which creates the 5 number summary of the variables
train_summary <- tidy(summary(train_no_NAs))

# create histograms for all the variables except branch and res
ggplot(gather(train_no_NAs, key, value, -c(BRANCH, RES)), aes(value)) +
  geom_histogram() + facet_wrap(~ key, scales = "free")

# reduced dataframe based on the histograms. Deleted these variables due to 
# minimal variation in the data
train_reduced <- train_no_NAs %>% 
  dplyr::select(-c(CASHBK, NSF, NSFAMT,
           CD, CDBAL, IRA, IRABAL, LOC, LOCBAL, ILS, 
            ILSBAL, MM, MMBAL, MMCRED, MTG, MTGBAL, SDB, MOVED, 
            INAREA))

# Histograms of the selected variables
ggplot(gather(train_reduced, key, value, -c(BRANCH, RES)), aes(value)) +
  geom_histogram() +  facet_wrap(~ key, scales = "free")


# Possible libraries needed for logistic regression
library(MASS)
library(visreg)
library(brglm)

#Run logistic regression with all train_reduced variables included
fit <- glm(INS ~ DDA + DDABAL + DEP + DEPAMT + CHECKS + DIRDEP + TELLER 
                       + SAV + SAVBAL + ATM + ATMAMT + RES + BRANCH,
           data = train_reduced, family = binomial(link = "logit"))
summary(fit)
exp(confint(fit))

#Remove variables with pvalues above 0.05 in "fit". Rerun logistic regression.
# Removed were: DEPTAMT, DIRDEP, RES, BRANCH
fit2 <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
           + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
           data = train_reduced, family = binomial(link = "logit"))
summary(fit2)
exp(confint(fit2))

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




