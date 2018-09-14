install.packages('brglm')

library(tidyverse)
library(haven)
library(broom)
library(MASS)
library(visreg)
library(brglm)
library(mgcv)
library(car)


train <- read_sas("C:\\Users\\yuanm\\OneDrive - North Carolina State University\\Logistic reg\\Class Data\\MSA2019LogisticData\\data\\insurance_t.sas7bdat")

fit <- glm(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
           + SAV + SAVBAL + ATM + ATMAMT + BRANCH,
           data = train, family = binomial(link = "logit"))
summary(fit)



#Checking linearity. DEP, SAVBAL, ATMAMT do not look linear
visreg(fit, "DDABAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red", se = FALSE) + theme_bw() +
  labs(title = "partial residual plot for check account bal",
       x = "checking account bal", y = "partial residuals")

visreg(fit, "DEP", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red", se = FALSE) + theme_bw() +
  labs(title = "partial residual plot for #check acc deposit",
       x = "DEP", y = "partial residuals")

visreg(fit, "CHECKS", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red", se = FALSE) + theme_bw() +
  labs(title = "partial residual plot for #checks",
       x = "CHECKS", y = "partial residuals")


visreg(fit, "TELLER", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red", se = FALSE) + theme_bw() +
  labs(title = "partial residual plot for #teller visits",
       x = "#teller visits", y = "partial residuals")

visreg(fit, "SAVBAL", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red", se = FALSE) + theme_bw() +
  labs(title = "partial residual plot for savings account bal",
       x = "savings account balance", y = "partial residuals")

visreg(fit, "ATMAMT", gg = TRUE, points = list(col = "black")) +
  geom_smooth(col = "red", fill = "red", se = FALSE) + theme_bw() +
  labs(title = "partial residual plot for atm withdrawal amt",
       x = "atmamt", y = "partial residuals")

########################################################
#additive model with smoothing 
fit.gam <- gam(INS ~ DDA + DDABAL + DEP + CHECKS + TELLER 
               + S(SAVBAL) + ATM + S(ATMAMT) + BRANCH,
               data = train, family = binomial, method = "REML")
summary(fit.gam)
AIC(fit.gam)
#9507. Var lengths differ for DEP

#plot estimated effects of these variables
plot(fit.gam, ylab = "f(ATMAMT)", shade = TRUE, main = "effect of ATMAMT", jit = TRUE,seWithMean = TRUE)
plot(fit.gam, ylab = "f(SAVBAL)", shade = TRUE, main = "effect of SAVBAL", jit = TRUE,seWithMean = TRUE)



####################testing potential interactions

#transform non-linear vars
fit1 <- glm(INS ~ DDA + DDABAL + DEP**2 + TELLER + CHECKS + SAV + SAVBAL**2 + ATM + ATMAMT**2 + BRANCH + (SAVBAL*DDABAL), 
            data = train, family = binomial(link = "logit"))
summary(fit1) 
#9821.4

#Checking and saving
savbal_ddabal <- glm(INS ~ DDA + DDABAL + DEP + TELLER + CHECKS + SAV + SAVBAL + ATM + ATMAMT + BRANCH + (SAVBAL*DDABAL), 
             data = train, family = binomial(link = "logit"))
summary(savbal_ddabal) 
#AIC (9821.4)

checks_teller <- glm(INS ~ DDA + DDABAL + DEP + TELLER + CHECKS + SAV + SAVBAL + ATM + ATMAMT + BRANCH + (CHECKS*TELLER),
                     data = train, family = binomial(link = "logit"))
summary(checks_teller) 
#9835.8

dep_checks <- glm(INS ~ DDA + DDABAL + DEP + TELLER + CHECKS + SAV + SAVBAL + ATM + ATMAMT + BRANCH + (DEP*CHECKS),
                     data = train, family = binomial(link = "logit"))
summary(dep_checks) 
#9836.3


#######################################################
#Look for 
colSums(is.na(train))

#Melissa's Model
fit_5 <- glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
             + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC,
             data = train, family = binomial(link = "logit"))
summary(fit_5)
vif(fit_5)
#7777.2


train_remove <- train[-c(1721, 1547, 4601, 5400),]

fit_remove_2 <- glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
                    + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG + CC,
                    data = train_remove, family = binomial(link = "logit"))
summary(fit_remove_2)
vif(fit_remove_2)
#7661.3

fit_cc <- glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
                    + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + CC,
                    data = train_remove, family = binomial(link = "logit"))
summary(fit_cc)
vif(fit_cc)

fit_nocc <-  glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
                 + SAV + SAVBAL + ATM + ATMAMT + ACCTAGE + IRA + CD + MM + MTG,
                 data = train_remove, family = binomial(link = "logit"))
summary(fit_nocc)
vif(fit_nocc)


fit_2 <- glm(INS ~ DDA + DDABAL + CHECKS + TELLER 
             + SAV + SAVBAL + ATM + ATMAMT + CRSCORE,
             data = train, family = binomial(link = "logit"))
summary(fit_2)
vif(fit_2)

####################################

#searching for influential observations
influence.measures(fit)
plot(fit, 4, id.n=4)
#1721, 1547, 4601, 5400##


