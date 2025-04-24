##########################################################
#
# Demos used in lectures for MTH2006
# (c) 2018 d.b.stephenson@exeter.ac.uk
#
# 3.2 NLM specification and inference
#
# Thanks to Ben Youngman for sharing his R code

# Load libraries that should be installed if tidyverse package has been installed correctly
library(ggplot2)
library(gridExtra)

# Set up gg graphics theme to be clearer and less cluttered with bigger labels
theme_set(theme_classic(base_size=22))

#####################################################
# Topic 3.2 NLM specification and inference

# Load the prod data frame
prod <- structure(list(temp = c(35.3, 29.7, 30.8, 58.8, 61.4, 71.3, 74.4, 
                                76.7, 70.7, 57.5, 46.4, 28.9, 28.1, 39.1, 46.8, 48.5, 59.3, 70, 
                                70, 74.5, 72.1, 58.1, 44.6, 33.4, 28.6), output = c(10.98, 11.13, 
                                                                                    12.51, 8.4, 9.27, 8.73, 6.36, 8.5, 7.82, 9.14, 8.24, 12.19, 11.88, 
                                                                                    9.57, 10.94, 9.58, 10.09, 8.11, 6.83, 8.88, 7.68, 8.47, 8.86, 
                                                                                    10.36, 11.08), days = c(20, 20, 23, 20, 21, 22, 11, 23, 21, 20, 
                                                                                                            20, 21, 21, 19, 23, 20, 22, 22, 11, 23, 20, 21, 20, 20, 22)), .Names = c("temp", 
                                                                                                                                                                                     "output", "days"), row.names = c(NA, 25L), class = "data.frame")

n <- nrow(prod)
p <- ncol(prod)-1			# Number of explanatory variables (temp and days)	

head(prod)					# Take a look at the values in the top of the data frame
pairs(prod)					# Look at scatter between different pairs of variables

# Look at some simple linear regression fits to the two explanatory variables
ggplot(prod,aes(x=temp,y=output))+geom_point(size=2)+stat_smooth(method="lm")
ggplot(prod,aes(x=days,y=output))+geom_point(size=2)+stat_smooth(method="lm")

# Fit a NLM to the data: output~ N(beta_0 + beta_1 * temp + beta_2 * days, sigma^2) 
fit.lm <- lm(output ~ temp + days, data=prod)
fit.lm 			# Show model and point estimates
summary(fit.lm) # More complete summary of all the estimated values



####################################################
# Parameter estimation: using matrix algebra and comparing with lm() function

X1 <- cbind(rep(1, n), temp, days) # forming the design matrix directly
X2 <- model.matrix(fit.lm) # using R's formulae for `lm' 
# check X1 - X2
X1-X2
X <- X1

# set the response variable
attach(prod)		# attach the data frame so its variables can be found without having to extract them
y <- output

# computing X'X
XtX1 <- t(X) %*% X # using R's matrix transposition and multiplication
XtX2 <- crossprod(X, X) # `crossprod' does both at once, so is more efficient
# check XtX1 - XtX2 which should be zero
XtX <- XtX2

# computing X'y
Xty <- crossprod(X, y) # computes X'y, preferred to t(X) %*% y

# computing `betahat' = (X'X)^{-1}X'y
betahat1 <- solve(XtX) %*% Xty # using R's matrix inversion function, and matrix multiplication
betahat2 <- solve(XtX, Xty) # solving a system of linear equations Ax = b, which is more stable
# check betahat1 - betahat2
# a logical check considering only the first 10 decimal places is round(betahat1, 10) - round(betahat2, 10)
betahat <- betahat2
betahat
fit.lm # compare with betahat to see that the estimates match

# estimating variance of residuals `sigmahat ^ 2'
H <- X %*% solve(XtX, t(X)) # obtain the `hat matrix'
yhat <- H %*% y # use H to give fitted values
epsilonhat <- y - yhat # obtain observed residuals
RSS <- crossprod(epsilonhat, epsilonhat) 
sigmasqhat <- RSS / (n - p - 1)
sigmasqhat  

yhat - fitted(fit.lm) # should all be zero; they are subject to computer rounding
epsilonhat - residuals(fit.lm) # as above
sigmahatsq <- sum(residuals(fit.lm)^2) / (n - p - 1)
sigmahatsq 
summary(fit.lm) # all quantities calculated can be seen using the `summary' function

# Var(betahat)=sigma^2 (X'X)^{-1}
varbeta <- sigmahatsq * solve(XtX)
sqrt(diag(varbeta))			# standard error of the parameter estimates

####################################################
# 3D plot of the data and planes of best fit
#install.packages("scatterplot3d") # then select a repository; you should only need to do this once
attach(prod)  # Make prods variables available without having to extract them e.g. temp rather than prod$temp
library(scatterplot3d) # but you'll need to do this each time you start R

s3d.mlr <- scatterplot3d(temp, days, output, pch = 19, scale.y = 0.5, cex.lab = 1.5, cex.axis = 1.2) # plot the observations; `scale.y' changes the vertical angle from which the plot is viewed
s3d.mlr$plane3d(fit.lm$coef[1], fit.lm$coef[2], fit.lm$coef[3], lwd = 2) # add fitted MLR model; note that the order for the explanatory variables must be the same as above, but with the intercept included first

s3d.mlr <- scatterplot3d(days, temp, output, pch = 19, scale.y = 0.5, cex.lab = 1.5, cex.axis = 1.2) # plot the observations from the other angle
s3d.mlr$plane3d(fit.lm$coef[1], fit.lm$coef[3], fit.lm$coef[2], lwd = 2) # add fitted MLR model; note that betahat[2][ and betahat[3] are reversed

s3d.null <- scatterplot3d(temp, days, output, pch = 19, scale.y = 0.5, cex.lab = 1.5, cex.axis = 1.2) # plot the observations again
s3d.null$plane3d(mean(output), 0, 0, col = "red", lwd = 2) # superimpose the fitted `null model'
s3d.null$plane3d(fit.lm$coef[1], fit.lm$coef[2], fit.lm$coef[3], pch = 19, lwd = 2) # superimpose fitted MLR model

####################################################
# Model checking

# Calculate leverages
leverages <- diag(X %*% solve(crossprod(X), t(X)))
leverages <- hat(X) # R function to give diagonal elements of hat matrix
leverages <- hatvalues(fit.lm) # Another R function to give diagonal elements of hat matrix
qplot(temp, leverages)+geom_hline(yintercept=2*(p+1)/n)
qplot(days, leverages)+geom_hline(yintercept=2*(p+1)/n)
qplot(yhat, leverages)+geom_hline(yintercept=2*(p+1)/n)

# Calculate standardised residuals 
epsilonstd=epsilonhat/sqrt(sigmahatsq*(1-leverages))
epsilonrstd=rstandard(fit.lm) # from the lm fit
epsilonstd-epsilonrstd

# Is the distribution of the residuals independent of the fitted value?
qplot(yhat,epsilonstd)+geom_hline(yintercept=0, col="red", linetype="dashed")
ggplot(fit.lm, aes(.fitted, .stdresid))+geom_point()+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")+xlab("Fitted values")+ylab("Standardised residuals")+ggtitle("Std Residual vs Fitted Plot")

ggplot(fit.lm,aes(sample=.stdresid))+geom_qq()+geom_abline()+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")+ggtitle("Normal Q-Q")

ggplot(fit.lm, aes(.fitted, abs(.resid)))+geom_point()+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")+xlab("Fitted values")+ylab("|Residuals|")+ggtitle("Abs Residual vs Fitted Plot")

ggplot(fit.lm,aes(sample=.stdresid))+geom_qq()+geom_abline()+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")+ggtitle("Normal Q-Q")

# How influential are the data points?
qplot(leverages,epsilonstd)+geom_vline(xintercept=2*(p+1)/n)+geom_hline(yintercept=0)

cooks_direct=(epsilonstd^2)*leverages/((p+1)*(1-leverages))
cooks=cooks.distance(fit.lm)
cooks-cooks_direct
qplot(days,cooks)+geom_hline(yintercept=4/n)

ggplot(fit.lm, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)+xlab("Leverage")+ylab("Standardized Residuals")+ggtitle("Residual vs Leverage Plot")+scale_size_continuous("Cook's Distance", range=c(0.1,1.5))

cd_cont_pos <- function(leverage, level, model) {sqrt(level*length(coef(model))*(1-leverage)/leverage)}
cd_cont_neg <- function(leverage, level, model) {-cd_cont_pos(leverage, level, model)}

ggplot(fit.lm, aes(.hat, .stdresid))+geom_point()+stat_function(fun = cd_cont_pos, args = list(level = 4/n, model = fit.lm))+stat_function(fun = cd_cont_neg, args = list(level = 4/n, model = fit.lm))+xlab("Leverage")+ylab("Standardized Residuals")+ggtitle("Residual vs Leverage Plot")

####################################################
# Model Prediction

prod.new <- data.frame(temp = 50, days = 20) # create new data frame with observation having temperature of 50 deg Fahrenheit and 20 days' operation
predict(fit.lm, prod.new, interval = "confidence", level = 0.95) # 95% condfidence interval
predict(fit.lm, prod.new, interval = "prediction", level = 0.95) # 95% prediction interval

X <- model.matrix(fit.lm)
mlr.df <- nrow(X) - ncol(X)
sighatsq <- sum((prod$output - fitted(fit.lm))^2)/mlr.df
colnames(X)
X.pred <- c(1, 50, 20)
se.ci <- sighatsq * crossprod(X.pred, solve(crossprod(X, X), X.pred))
se.pi <- sighatsq * (1 + crossprod(X.pred, solve(crossprod(X, X), X.pred)))

muhat <- crossprod(X.pred, coef(fit.lm))
muhat + qt(.975, mlr.df) * c(-1, 1) * sqrt(se.ci)
muhat + qt(.975, mlr.df) * c(-1, 1) * sqrt(se.pi)


# Example of confidence and prediction bands using Weight~Height simple linear regression
load("lm.RData")

model=lm(Weight~Height, data=bio)

# Plot Weight against Height and add confidence band
ggplot(bio, aes(x = Height, y = Weight)) + geom_point(size=2) + stat_smooth(method=lm, se=TRUE,level=0.95)

# Calculate 95% confidence and prediction intervals for 3m high students
predict(model,data.frame(Height=300),interval="confidence",level=0.95)

predict(model,data.frame(Height=300),interval="prediction",level=0.95)


# Plot Weight against Height and add confidence band and prediction band
pinterval <- predict(model, interval="prediction",level=0.99)
bioint=cbind(bio,pinterval)

ggplot(bioint, aes(x = Height, y = Weight)) + geom_point(size=2) + stat_smooth(method=lm, se=TRUE,level=0.95)+ geom_line(aes(y=lwr), color = "red", linetype = "dashed")+ geom_line(aes(y=upr), color = "red", linetype = "dashed")

# But should we trust the model? Let's do some model checking diagnostics ...
## Residuals vs Fitted values
ggplot (data =  model, mapping = aes (x =  .fitted, y =  .stdresid)) +
  geom_point () + geom_smooth (method = "loess" , se = FALSE ) +
  geom_hline (yintercept = 0 , linetype = "dotted" ) +
  labs (x = "Fitted values" , y = "Standardised residuals" )

## Quantile-Quantile plot
ggplot (data =  model, mapping = aes (sample =  .stdresid)) +
  geom_qq () + geom_qq_line () +
  labs (x = "Theoretical quantiles" , y = "Standardised residuals" )

## Standardised residuals vs Leverage
cd_cont_pos <- function (leverage, level, model)
{sqrt (level*length (coef (model))* (1- leverage)/ leverage)}
cd_cont_neg <- function (leverage, level, model)
{-cd_cont_pos (leverage, level, model)}
level <- 4/nrow (model$ model)

ggplot (data =  model, mapping = aes (x =  .hat, y =  .stdresid)) +
  geom_point () + geom_smooth (method = "loess" , se = FALSE ) +
  stat_function (fun =  cd_cont_pos, args = list (level =  level, model =  model)) +
  stat_function (fun =  cd_cont_neg, args = list (level =  level, model =  model)) +
  geom_hline (yintercept = 0 , linetype = "dotted" ) +
  labs (x = "Leverage" , y = "Standardised residuals" ) +
  lims (x = c (0 ,NA ), y = c (-3 ,+3 ))


##################### end of file ########################################


