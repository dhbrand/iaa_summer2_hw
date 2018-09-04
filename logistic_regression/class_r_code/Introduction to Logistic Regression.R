#################################
#                               #
#       MSA Class of 2019       #
#                               #
#      Logistic Regression:     #
#  Intro to Logistic Regression #
#                               #
#        Matthew Austin         #
#                               #
#################################

# need these packages #
library(MASS)
library(visreg)
library(brglm)

# read in data=
lowbwt <- read.csv("lowbwt.csv", header = TRUE)

# some initial data summaries
rs_table <- with(lowbwt,
                 table(factor(smoke, labels = c("non-smoker", "smoker")),
                       race))
addmargins(rs_table) # add row and column totals
hist(lowbwt$age, breaks = 20, col = "gray", xlab = "age", main = "")
hist(lowbwt$lwt, breaks = 25, col = "gray", xlab = "lwt", main = "")


# fit GLMs using glm()
# family specifies data distribution, and link function goes in parentheses
fit <- glm(low ~ age + lwt + smoke + race,
           data = lowbwt, family = binomial(link = "logit"))
summary(fit)

# get profile likelihood CI. if you load the MASS package, it will get the
# profile likelihood CIs by default
exp(confint(fit)) # since we're exponentiating, these are the CIs for the odds
# ratios

### making predictions
# create a new dataset of the subjects we'd like to compare
# here, we're looking two 30-y/o, 130lb women, one is Black and smokes, the
# other is White and doesn't smoke
newdata <- data.frame(age = c(30, 30),
                      lwt = c(130, 130),
                      race = c("black", "white"),
                      smoke = c(1, 0))
# type = "link" will return the predicted log(odds) for each of these subjects
predict(fit, newdata = newdata, type = "link")
# so to get the odds ratio, we need to exponentiate that
exp(predict(fit, newdata = newdata, type = "link"))
# the diff() function is the second value minus the first
# so the following statement is comparing the White non-smoker
# to the Black smoker
exp(diff(predict(fit, newdata = newdata, type = "link")))
#Low birth weight for a white non-smoker mom is 0.102times as likely as the black smoker mom

# for probabilities, we need to use type = "response"
predict(fit, newdata = newdata, type = "response")
diff(predict(fit, newdata = newdata, type = "response"))

### plotting predicted probabilities
# visreg() will plot predicted probabilities for one predictor at whatever
# levels of the other predictors you set
# the first argument is your model
# the second is the predictor you're interested in
# the "by" argument will plot separate lines for each level of the "by" variable,
# and the "overlay" option will put them all on the same graph. so here, we'll
# get a line for each race
# scale = "response" tells it to plot the probabilities rather than log(odds)
# the "cond" argument is where you specify the levels of the other predictors.
# by default, if left unspecified, it will use the median of the continuous
# predictors and the most frequent category of categorical predictors
visreg(fit, "age", by = "race", scale = "response",
       cond = list(smoke = 0, lwt = 130),
       overlay = TRUE)


### likelihood ratio test
# let's do the LRT comparing our original model to one without smoking and race
fit2 <- glm(low ~ age + lwt, data = lowbwt, family = binomial)
anova(fit, fit2, test = "LRT")

### separation ###
### NOTE: seems like my code from last year for this section might be out of
# date. I'll look into the brglm2 package and let you know about any changes
# here
### complete separation
x <- c(1, 2, 3, 4, 5, 6)
y <- c(0, 0, 0, 1, 1, 1)
summary(glm(y ~ x, family = binomial(link = "logit")))
# use brglm() for Firth/penalized likelihood
# same structure as glm()
summary(brglm(y ~ x, family = binomial(link = "logit")))

### quasi-complete separation
x <- c(1, 2, 3, 4, 4, 5, 6, 7)
y <- c(0, 0, 0, 0, 1, 1, 1, 1)
fit.quasisep <- glm(y ~ factor(x), family = binomial(link = "logit"))
summary(fit.quasisep) # no warnings, but look at those standard errors!
summary(brglm(y ~ factor(x), family = binomial(link = "logit"))) # penalized likelihood
# can also check for separation using separation.detection()
# each column shows convergence of standard errors for betas
# if any keep changing, then it didn't converge
separation.detection(fit.quasisep)
