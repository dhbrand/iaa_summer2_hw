##############################
#                            #
#     MSA Class of 2019      #
#                            #
#    Logistic Regression:    #
#  Model Fit & Diagnostics   #
#                            #
#       Matthew Austin       #
#                            #
##############################

# need these packages #
# (install them if you don't have them)
library(haven)
library(tidyverse)
#ibrary(MASS)
library(mgcv)
library(visreg)
library(car)
library(rlang)

# read in data
data_dir <- "data/"
input_file <- "lowbwt.csv"
lowbwt <- read.csv(paste(data_dir, input_file, sep = ""), header = TRUE)

mod <- low ~ age + lwt + smoke + race
fit <- glm(mod, data = lowbwt,
           family = binomial(link = "logit"))

### diagnostics ###
# you can get the different types of residuals with the resid() function:
# resid(fit, type = c("deviance", "pearson", "working", "response", "partial"))

# influence.measures() gives dfbetas, Cook's D, leverage, and all that fun stuff
# you can also call these individually if you want
# using functions like dfbetas(), cooks.distance(), etc.
influence.measures(fit)

### plot Cook's distance
plot(fit, 4, n.id = 5) # n.id = #points identified on the plot

### dfbetas plots
# age:
dfbetasPlots(fit, terms = "age", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))
# col is just me coloring the outcome points red and the no outcome blue

# lwt:
dfbetasPlots(fit, terms = "lwt", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))

# smoke:
dfbetasPlots(fit, terms = "smoke", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))

# race
dfbetasPlots(fit, terms = "race", id.n = 5,
             col = ifelse(fit$y == 1, "red", "blue"))

### partial residuals ###
# age:
visreg(fit, "age", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for age",
       x = "age", y = "partial (deviance) residuals")

# lwt:
visreg(fit, "lwt", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red") + theme_bw() +
  labs(title = "partial residual plot for lwt",
       x = "lwt", y = "partial (deviance) residuals")

# you could also use the following, which are equivalent:
# termplot(fit, terms = "age", partial.resid = TRUE, se = TRUE, smooth = panel.smooth)
# crPlot(fit, "age")

# i don't think this ultimately matters much (at least for logistic regression),
# but partial residuals are based on the working residuals, and that's what
# crPlot(), termplot(), and resid(..., type = "partial") use. visreg bases them
# on the deviance residuals

### GAMs ###
# fit model as a GAM:
# gam() from the mgcv package uses basically the same syntax as glm()
# s() tells it to use a spline for this variable
fit.gam <- gam(low ~ s(age) + lwt + smoke + race,
               data = lowbwt, family = binomial, method = "REML")
summary(fit.gam)
# in this output, the "significance of smooth terms" is testing whether or not
# the age effect = 0, NOT if it's linear!

# plot estimated effect of age
plot(fit.gam, ylab = "f(age)", shade = TRUE, main = "effect of age", jit = TRUE,
     seWithMean = TRUE)

### simulation ###
# following the steps in the slides:
# 1. we've already done step 1 by fitting with glm() on actual lowbwt data
# 2. likewise with step 2 using gam()
# 3. difference in deviance
d_obs <- fit$deviance - fit.gam$deviance
# 4. get predicted probs from the regular logistic regression glm
# type = "response" returns the predicted probability
phat <- predict(fit, newdata = lowbwt, type = "response")
# 5. generate new outcomes using the predicted probabilites from glm()
# using the same sample size as original data
# our logistic regression model from glm() is the "truth" now
sim_outcome <- rbinom(n = nrow(lowbwt), size = 1, prob = phat)
sim_data <- data.frame(low = sim_outcome, lowbwt[,-1])
# 6: fit glm() and gam() to this simulated data using the SAME predictor values
# as the real data
sim.glm <- glm(low ~ age + lwt + smoke + race,
               data = sim_data, family = binomial(link = "logit"))
sim.gam <- gam(low ~ s(age) + lwt + smoke + race,
               data = sim_data, family = binomial, method = "REML")
# 7: take the difference in deviance between the two models on the simulated data
d_sim <- sim.glm$deviance - sim.gam$deviance

# 8: now i'm writing a function to do steps 5-7 a bunch of times
# so don't worry about it if you can't follow this
sim_models <- function(glm_model, gam_model){
  if(any(class(glm_model) == "gam"))
    stop("Error: glm_model must be fit by glm()")
  n_obs <- nrow(glm_model$data) # sample size
  y_name <- all.vars(glm_model$formula)[1] # get name of response
  y_index <- match(y_name, colnames(glm_model$data)) # get index of y
  
  phat <- predict(glm_model, type = "response") # predicted probabilities
  sim_data <- glm_model$data
  # generate new response
  sim_data[,y_index] <- rbinom(n = n_obs, size = 1, prob = phat)
  
  # fit glm to fake data and get deviance
  sim_glm_dev <- glm(glm_model$formula, family = binomial(link = "logit"),
                        data = sim_data)$deviance
  # fit gam to fake data and get deviance
  sim_gam_dev <- gam(gam_model$formula, family = binomial(link = "logit"), 
                   data = sim_data, method = "REML")$deviance
  # take the difference in deviance
  d_sim <- sim_glm_dev - sim_gam_dev
  return(d_sim)
}

set.seed(9418)
d_sim <- replicate(200, sim_models(fit, fit.gam)) # do this 200 times
paste("p-value = ", mean(d_obs <= d_sim))

# plot
hist(d_sim, breaks = 40, main = "distribution of D_sim", xlab = "D_sim")
abline(v = d_obs, col = "red")

### observed calibration curve

obs.phat <- data.frame(y = fit$y, phat = fitted(fit))
obs.phat <- arrange(obs.phat, phat)
ggplot(data = obs.phat) +
  geom_point(mapping = aes(x = phat, y = y), color = "black") +
  geom_smooth(mapping = aes(x = phat, y = y), color = "red") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "black") +
  labs(x = "predicted probability", y = "observed frequency",
       title = "calibration curve") +
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  lims(x = c(0, 0.8), y = c(0, 1)) +
  theme_bw()
