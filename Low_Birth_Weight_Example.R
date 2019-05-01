### Example of Logistic Regression Using dataset from class
require(ResourceSelection)
require(gmodels)
require(multcomp)

## Read in data
lbw <- read.table("~/Downloads/LBW.txt", quote="\"", comment.char="")
colnames(lbw) <- c("low", "age", "wt", "race", "smoke", "hyp", "ui", "ftv", "ptl", "bwt")

lbw$racew[lbw$race == "white"] <- 1
lbw$raceb[lbw$race == "black"] <- 1
lbw[is.na(lbw)] <- 0


## Question 1 ##
## a) Compute descriptive statistics on all variables
summary(lbw$age)
summary(lbw$wt)
summary(lbw$bwt)

table(lbw$low)
table(lbw$smoke)
table(lbw$hyp)
table(lbw$ui)
table(lbw$ftv)
table(lbw$ptl)

## b) Fit main effects model based on all covariates
lbw.mod1 <- glm(low ~ age + wt + racew + raceb + smoke + hyp + ui + ftv + ptl,family = binomial(link="logit") ,data = lbw)
summary(lbw.mod1)

## c) Carry out the Hosmer-Lemeshow goodness of fit test
hl.test <- hoslem.test(lbw.mod1$y, fitted(lbw.mod1), g = 10)
# The model appears to fit the data well. We get a p = 0.87. 
# Thus we do not reject the null that the logistic regression model fits the data well

## d) Re-fit the model. Which covariates are predictive of low birth weight?
# Backwards selection
backwards.sel <-step(lbw.mod1)
formula(backwards.sel)

nothing <- glm(low ~ 1, family = binomial(link="logit"), data = lbw)
# Forwards selection
forwards.sel <- step(nothing,
                scope=list(lower=formula(nothing),upper=formula(lbw.mod1)), direction="forward")
formula(forwards.sel)
#Both ways
bothways <- step(nothing, list(lower=formula(nothing),upper=formula(lbw.mod1)),
         direction="both",trace=0)
formula(bothways)

# Ending model
lbw.mod2 <- glm(low ~ ptl + hyp + wt + racew + smoke + ui, family = binomial(link="logit"), data = lbw)

## e) Re-fit model with age, race, ftv removed. Interprete the smoke and wt parameters and B_0

lbw.mod3 <- glm(low ~ ptl + hyp + wt + smoke + ui, family = binomial(link=logit), data = lbw)
summary(lbw.mod3)

# B_smoke: 0.5035 -> The effect of smoking on the log odds of having low birth weight 
# holding all other covariates constant

# B_weight: -0.0154 -> the effect of a pound increase in weight on the log odds of having low birth weight
# holding all other covariates constant

# B_0: 0.47 -> the estimated log odds of having low birth weight when all covariates are equal to zero

## f) How would you restructure the model to make the intercept interpretation better?
## Compare B_0

lbw$wt_centered <- lbw$wt -  mean(lbw$wt)
lbw.mod4 <- glm(low ~ ptl + hyp + wt_centered + smoke + ui, family = binomial(link=logit), data = lbw)
summary(lbw.mod4)

# B_0 under the centered model is -1.52 vs. 0.47
# B_wt and B_wtcentered are the same value

## g) Carry out a test of whether the effect of smoke depends on either UI or PTL
lbw$smoke_ui <- as.factor(lbw$smoke * lbw$ui)
lbw$smoke_ptl <- as.factor(lbw$smoke * lbw$ptl)


lbw.mod5 <- glm(low ~ ptl + hyp + wt_centered + smoke + ui + smoke_ui + smoke_ptl, family = binomial(link="logit"), data = lbw)
summary(lbw.mod5)

fit.contrast(lbw.mod5, "smoke_ui",c(1,-1))



