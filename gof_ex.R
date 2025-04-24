##########################################################
#
# Demos used in lectures for MTH2006
# (c) 2018 d.b.stephenson@exeter.ac.uk
#
# 3.3 NLM evaluation and selection
#
# Thanks to Stefan Siegert for sharing his R code
#####################################################

#####################################################
# Factory production example shown in the lecture slides

# Load the prod data frame
prod <- structure(list(temp = c(35.3, 29.7, 30.8, 58.8, 61.4, 71.3, 74.4, 
                                76.7, 70.7, 57.5, 46.4, 28.9, 28.1, 39.1, 46.8, 48.5, 59.3, 70, 
                                70, 74.5, 72.1, 58.1, 44.6, 33.4, 28.6), output = c(10.98, 11.13, 
                                                                                    12.51, 8.4, 9.27, 8.73, 6.36, 8.5, 7.82, 9.14, 8.24, 12.19, 11.88, 
                                                                                    9.57, 10.94, 9.58, 10.09, 8.11, 6.83, 8.88, 7.68, 8.47, 8.86, 
                                                                                    10.36, 11.08), days = c(20, 20, 23, 20, 21, 22, 11, 23, 21, 20, 
                                                                                                            20, 21, 21, 19, 23, 20, 22, 22, 11, 23, 20, 21, 20, 20, 22)), .Names = c("temp", 
                                                                                                                                                                                     "output", "days"), row.names = c(NA, 25L), class = "data.frame")

# Fit null and NLM models to data
fit.null <- lm(output ~1, data=prod)
fit.lm <- lm(output ~ temp + days, data=prod)

fit.lm.s <- summary(fit.lm)

# Some summary measures of goodness of fit
fit.lm.s$r.squared
fit.lm.s$adj.r.squared
AIC(fit.lm)

AIC(fit.null)

# ANOVA tables
anova(fit.null)
anova(fit.lm)
anova(fit.null,fit.lm)

#####################################################
# Example of sequential selection of variables on the "cement" data set

y <- c( 78.5,104.3,95.9,102.7,93.1,83.8,109.4,74.3,87.6,109.2,72.5,115.9,113.3 )
x1 <- c( 7, 11, 7, 3, 2, 1, 10, 1, 11, 11, 1, 21, 11 )
x2 <- c( 26, 56, 52, 71, 54, 40, 68, 29, 31, 55, 31, 47, 66 )
x3 <- c(6, 8, 6,17,18,23,8,15,8,9,22,4,9)
x4 <- c( 60,20,33,6,22,34,12,52,47,22,44,26,12 )
cement <- data.frame(y,x1,x2,x3,x4)

# forward selection 
step(lm(y~1, data=cement), scope=y~x1+x2+x3+x4, direction="forward")

# backward elimination
step(lm(y~., data=cement), direction="backward")

# stepwise regression starting with the null model
step(lm(y~1, data=cement), scope=y~x1+x2+x3+x4, direction="both")

# stepwise regression starting with the full model
step.mod <- step(lm(y~., data=cement), direction="both")

# compare with the full model by anova 
full.mod <- lm(y~., data=cement)
anova(step.mod, full.mod)

#####################################################
# Example of various models fitted to the biometric weight height data

# Load workspace containing data sets for topic 3
load("lm.RData")

# fit nested linear models
mod0 <- lm(Weight~1, data=bio)
mod1 <- lm(Weight~Height, data=bio)
mod2 <- lm(Weight~Height+Sex, data=bio)

# we add an obviously useless predictor, say the last two digit of students
# phone number (simulate as random number between 10 and 99)
set.seed(12345)
bio["Tel"] <- sample(x=10:99, size=nrow(bio), replace=TRUE)
head(bio)

# fit a linear model that includes the students phone number
mod3 <- lm(Weight~Height+Sex+Tel, data=bio)

# save model summaries to access r.squared and adj.r.squared
mod0s <- summary(mod0)
mod1s <- summary(mod1)
mod2s <- summary(mod2)
mod3s <- summary(mod3)

# calculate r-squared, the best R^2 is achieved by model 3 (which includes
# Tel!!!)
mod0s$r.squared
mod1s$r.squared
mod2s$r.squared
mod3s$r.squared

# calculate adjusted r-squared; the best R_a^2 is achieved by model 2
mod0s$adj.r.squared
mod1s$adj.r.squared
mod2s$adj.r.squared
mod3s$adj.r.squared

# calculate AIC; model 2 has lowest AIC so is the "best" model
AIC(mod0, mod1, mod2, mod3)

# Anova F test to see if model 3 is significantly better than model 2
anova(mod2, mod3)

# anova F-test comparing model 3 to model 2, calculated "by hand"
RSS.M2 <- sum(residuals(mod2)^2)
RSS.M3 <- sum(residuals(mod3)^2)
n <- nrow(bio) # number of students
p.M2 <- 2 # number of expl. var. in model 2
p.M3 <- 3 # number of expl. var. in model 3
df1 <- p.M3 - p.M2 # degrees of freedom of the f-distribution
df2 <- n - p.M3 - 1 # degrees of freedom of the f-distribution
Fstat <- ((RSS.M2 - RSS.M3) / df1) / (RSS.M3 / df2)
pf(Fstat, df1, df2, lower.tail=FALSE)


###################### end of file ###############################
