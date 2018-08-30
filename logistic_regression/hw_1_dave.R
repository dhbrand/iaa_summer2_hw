library(tidyverse)
library(haven)
library(broom)

# read the sas dataset into an R dataframe
train <- read_sas("data/insurance_t.sas7bdat")

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

# kinda of just eyeballing quickly
# vars with lots of 0 to 1 range
# DDA, DEP, CASHBK, DIRDEP, NSF, NSFAMT, TELLER, SAV,
# SAVBAL, ATM, CD, CDBAL, IRA, IRABAL, LOC, LOCBAL, ILS, 
# ILSBAL, MM, MMBAL, MMCRED, MTG, MTGBAL, SDB, MOVED, 
# INAREA, INS

# reduced dataframe based on the train_summary output
train_reduced <- train_no_NAs %>% 
  select(-c(DDA, DEP, CASHBK, DIRDEP, NSF, NSFAMT, TELLER, SAV,
            SAVBAL, ATM, CD, CDBAL, IRA, IRABAL, LOC, LOCBAL, ILS, 
            ILSBAL, MM, MMBAL, MMCRED, MTG, MTGBAL, SDB, MOVED, 
            INAREA, INS))

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
  select(-c(!! low_count_var)) 

# checking the histograms of the new reduced df
ggplot(gather(train_reduced, key, value, -c(BRANCH, RES)), aes(value)) +
  geom_histogram(color = "blue") +
  facet_wrap(~ key, scales = "free") +
  theme_bw()


#im adding new code
A = matrix(c(1,1,1,4))

