library(tidyverse)
library(haven)
library(broom)
library(MASS)
library(visreg)
library(brglm)
library(mgcv)


# read the sas dataset into an R dataframe
train <- read_sas("C:/Users/thebi/OneDrive/Documents/Logistic Regression/data/insurance_t.sas7bdat")

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
############## FINAL MODEL
fit2 <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
            + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
            data = train_reduced, family = binomial(link = "logit"))
summary(fit2) 
sort(abs(fit2$coefficients))
exp(confint(fit2))
diff(exp(fit2$coefficients))
exp(fit2$coefficients)

plot(fit2, 4, n.id=5)

#CHECK LINEARITY
visreg(fit2, "DDABAL", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

##### WHY do the lines cross / dont really follow each other
visreg(fit2, "DEP", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

#NONLINEAR
visreg(fit2, "SAVBAL", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

#weird towards end of graph
visreg(fit2, "ATMAMT", gg = TRUE, points=list(col="black")) + 
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for DDBAL",
       x = "DDBAL", y = "partial (deviance) residuals")

#GAMs to check Linearity
fit.gam<- gam(INS ~ DDA + s(DDABAL) + DEP + CHECKS + TELLER 
              + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
              data = train_reduced, family = binomial,method = "REML")
summary(fit.gam)
plot(fit.gam, shade=TRUE, jit=TRUE, seWithMean = TRUE)

