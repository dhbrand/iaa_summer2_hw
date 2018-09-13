#############################
#                           #
#     MSA Class of 2019     #
#                           #
#    Logistic Regression:   #
#      Model Assessment     #
#                           #
#       Matthew Austin      #
#                           #
#############################

# need these packages #
library(tidyverse)
library(MASS)
library(ROCR)
library(DescTools)
library(Hmisc)

# read in data
data_dir <- "/wherever/your/data/is"
input_file <- "lowbwt.csv"
lowbwt <- read.csv(paste(data_dir, input_file, sep = ""), header = TRUE)

# fit models
fit <- glm(low ~ age + lwt + smoke + race, data = lowbwt,
           family = binomial(link = "logit"))
# model with interactions
fit_int <- glm(low ~ age*lwt + lwt*smoke + race, data = lowbwt,
               family = binomial(link = "logit"))

# AIC and BIC
AIC(fit)
BIC(fit)

# DescTools::PseudoR2 has various pseudo-Rsq
# "Cox" is the Rsq SAS reports
# "Nagelkerke" is the "Max-rescaled" in SAS
PseudoR2(fit, which = c("Cox", "Nagelkerke", "McFadden"))
# McFadden's Rsq is the most useful of them in my opinion, but like regular Rsq,
# these all increase with added predictors

### Brier score function ###
brier_score <- function(obj, new_x = NULL, new_y = NULL){
  # computes [scaled] brier score
  #
  # inputs:
  # 1. obj: either a model from glm() or a data frame.
  #         the data frame must have a vector responses "y" and a vector of
  #         either probabilities "p" or linear predictor "lp".
  # 2. new_x: specify new dataset to get predicted probabilities for new obs.
  #             if NULL, the estimated probabilities from original obs will
  #             be used.
  # 3. new_y: use new responses. if NULL, original ones will be used.
  #
  # output:
  #   brier score, scaled brier score
  
  if(is.null(new_y)){
    y <- obj$y
  } else {
    y <- new_y
  }
  
  p_obs <- mean(y)
  
  if(any(class(obj) == "glm")){
    if(is.null(new_x)){
      p <- predict(obj, newdata = new_x, type = "response")
      lp <- predict(obj, newdata = new_x, type = "link")
    } else {
      lp <- obj$linear
      p <- fitted(obj)
    }
  } else if(is.null(obj$p)) {
    lp <- obj$lp
    p <- fitted(obj)
  } else {
    p <- obj$p
    lp <- obj$linear
  }
  
  # brier score
  brier_score <- mean((y - p)^2)
  
  # max brier score is just the observed proportion
  brier_max <- p_obs*((1 - p_obs)^2) + (1 - p_obs)*(p_obs^2)
  
  # scaled brier score
  # ranges from 0 to 1---lower is better
  brier_scaled <- brier_score/brier_max
  # essentially, 1 - brier_scaled is the %improvement over null model
  
  res <- data.frame(brier_score = brier_score,
                   brier_max = brier_max,
                   brier_scaled = brier_scaled)
  res
}

brier_score(fit)

### discrimination slope = mean(p1) - mean(p0) ###
D <- mean(fitted(fit)[fit$y == 1]) - mean(fitted(fit)[fit$y == 0])
# alternatively:
# D <- diff(aggregate(fitted(fit), by = list(fit$y == 0, fit$y == 1), FUN = mean)$x)

# histogram of predicted probabilities by outcome
# create data frame of outcome and predicted probabilities
df <- data.frame(y = fit$y,
                 phat = fitted(fit))
ggplot(df, aes(phat, fill = factor(y))) +
  geom_density(alpha = 0.2) +
  labs(x = "predicted probability",
       fill = "low")

### c-statistic and Somers' D ###
# predicted prob goes first, outcome second
rcorr.cens(fitted(fit), fit$y)[-c(5, 6, 9)] # ignoring output i don't need

### ROC curves ###
# the predicted probabilities go first, the actual outcomes (as a factor) second
pred <- prediction(fitted(fit), factor(fit$y))
# then in performance, "measure" is the y-axis, and "x.measure" is the x-axis
# for a roc curve, we want tpr vs. fpr. "sens" and "spec" also work
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# then we can plot
plot(perf, colorize = TRUE)
# add 45-degree line (random guessing)
abline(a = 0, b = 1, lty = 2)
# AUC
auc <- performance(pred, measure = "auc")@y.values

### classification table ###
# NOTE that SAS basically does leave-one-observation-out with the predicted
# probabilities, so the results here (and youden index) are different than
# the SAS results
classif_table <- data.frame(threshold = perf@alpha.values[[1]],
                         tpr = perf@y.values[[1]],
                         tnr = 1 - perf@x.values[[1]])
# youden's index: add weights for tpr (sens) and tnr (spec) if desired
classif_table$youdenJ <- with(classif_table, tpr + tnr - 1)
# find row with max
classif_table[which.max(classif_table$youdenJ),]

# compare the model with no interactions to the interaction model
pred_int <- prediction(fitted(fit_int), factor(fit_int$y))
perf_int <- performance(pred_int, measure = "tpr", x.measure = "fpr")
plot(perf, col = "red")
abline(0, 1, lty = 2)
plot(perf_int, col = "purple", add = TRUE)
legend("bottomright", inset = 0.03, col = c("red", "purple"),
       legend = c("no int", "with int"), lwd = 1)

# plot precision-recall curves
# we want precision (y-axis) vs. recall (x-axis)
perf_pr <- performance(pred, measure = "prec", x.measure = "rec")
plot(perf_pr, col = "red", xlim = c(0, 1), ylim = c(0, 1))
perf_pr_int <- performance(pred_int, measure = "prec", x.measure = "rec")
plot(perf_pr_int, col = "purple", add = TRUE)
# baseline is observed proportion
abline(h = mean(fit$y), lty = 2)
legend("bottomleft", inset = 0.03, col = c("red", "purple"),
       legend = c("no int", "with int"), lwd = 1)

# f-score
# ROCR uses a different formula than what's on my slides
# they have alpha = 1/(1 + w^2), where w is as defined on the slides
w <- 1 # weight
f1_scores <- performance(pred, measure = "f", alpha = 1/(1 + w^2))
f1_table <- data.frame(threshold = perf_pr@alpha.values[[1]],
                       precision = perf_pr@y.values[[1]],
                       recall = perf_pr@x.values[[1]],
                       f_score = f1_scores@y.values[[1]])
