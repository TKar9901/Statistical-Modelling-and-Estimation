### MTH2006 T1-CW
## SETUP
load("Aut2024.RData")
library(tidyverse, tune)


# PART 1
# exploring the data
head(alfheim)
nrow(alfheim)

ggplot(alfheim) +
  geom_histogram(aes(x=y, y=..density..), binwidth=3) +
  labs(x="Daily Rainfall Totals (mm)", y="Density")


# a)
ggplot(alfheim) +
  geom_point(aes(x=x, y=y)) +
  labs(x="Day", y="Rainfall Total (mm)")

ggplot(data=alfheim, aes(x=y)) +
  stat_boxplot(geom = "errorbar", width=0.3) +
  geom_boxplot() +
  labs(x="Daily Rainfall Totals (mm)")

alfheim$above1 = ifelse(alfheim$y>1, "Yes", "No")
alfheim$above1 <- as.factor(alfheim$above1)


ggplot(data=alfheim[alfheim$above1=="No", ], aes(x=y)) +
  stat_boxplot(geom = "errorbar", width=0.3) +
  geom_boxplot() +
  labs(x="Daily Rainfall Totals (mm)")

ggplot(data=alfheim[alfheim$above1=="No", ]) +
  geom_histogram(aes(x=y, y=..density..)) +
  geom_vline(xintercept=mean(alfheim[alfheim$above1=="No", ]$y), colour="red") +
  labs(x="Daily Rainfall Totals (mm)", y="Density")


ggplot(data=alfheim[alfheim$above1=="Yes", ], aes(x=y)) +
  stat_boxplot(geom = "errorbar", width=0.3) +
  geom_boxplot() +
  labs(x="Daily Rainfall Totals (mm)")

ggplot(data=alfheim[alfheim$above1=="Yes", ], aes(x=y, y=..density..)) +
  geom_histogram() +
  geom_vline(xintercept=mean(alfheim[alfheim$above1=="Yes", ]$y), colour="red") +
  labs(x="Daily Rainfall Totals (mm)", y="Density")


# d)
n = nrow(alfheim)
m = length(alfheim[alfheim$above1=="Yes", ]$y)
y2_mean = mean(alfheim[alfheim$above1=="Yes", ]$y)

# MLEs
phi_hat = 2-m/n
theta_hat = n/(m*(y2_mean)-m)


# e)
loglik = function(p, data) {
  n = nrow(alfheim)
  m = length(alfheim[alfheim$above1=="Yes", ]$y)
  y2_mean = mean(alfheim[alfheim$above1=="Yes", ]$y)
  (n-m)*log(1-p[1]) + n*log(p[1]) + n*log(p[2]) + m*p[2]*(1-y2_mean)
}

optim(c(0.1, 100), loglik, control=list(fnscale=-1), data=alfheim)



# PART 2
# exploring the data
head(whra)
nrow(whra)

# a)
ggplot(whra, aes(x=GDP,y=Happiness)) +
  geom_point() +
  stat_smooth(method="lm", se=TRUE, level=0.95)


# b)
model = lm(Happiness~GDP, data=whra)
null_model = lm(Happiness~1, data=whra)

summary(model)
summary(null_model)

AIC(model, null_model)


# c)
# standardized residuals vs fitted values

ggplot (model, mapping=aes(x=.fitted, y=.stdresid)) +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  geom_hline(yintercept=0, linetyp="dotted") +
  labs(x="Fitted values", y="Standardised residuals")

# justifying suggesting of outliers:

# q-q plot to compare standardised residuals to the expected standard normal model
ggplot(model, mapping= aes(sample=.stdresid)) +
  geom_qq_line(colour="red", linewidth=1) +
  geom_qq() +
  labs(x="Theoretical quantiles", y="Standardised residuals") +
  tune::coord_obs_pred()

# histogram to compare with standard normal curve to show this
ggplot(model) +
  geom_histogram(aes(x = .stdresid, y = ..density..)) +
  stat_function(geom="line", fun=dnorm, colour="red") +
  labs(x="Standardised Residuals", y="Density")

# finding those outliers

# standardised residuals vs leverage
cd_cont_pos <- function (leverage, level, model)
{sqrt (level*length (coef (model))* (1- leverage)/ leverage)}

cd_cont_neg <- function (leverage, level, model)
{-cd_cont_pos (leverage, level, model)}

level <- 4/nrow(model$model)

ggplot (data=model, mapping=aes(x=.hat, y=.stdresid)) +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  stat_function(fun=cd_cont_pos, args=list(level=level, model=model), colour="red") +
  stat_function(fun=cd_cont_neg, args=list(level=level, model=model), colour="red") +
  geom_hline(yintercept=0, linetype="dotted") +
  labs(x="Leverage", y="Standardised residuals")


# d)
newUK_GDP = whra[whra$Country=="United Kingdom", ]$GDP * 1.1

predict(model, data.frame(GDP=newUK_GDP), interval="prediction", level=0.95)


# e)
best_stepwise_model = step(lm(Happiness~1, data=whra), scope=y~GDP+SocialSupport+LifeExpectancy+Freedom+Generosity+Corruption, direction="both")

anova(best_stepwise_model, model)
