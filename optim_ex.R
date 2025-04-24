# Demos for section 2 likelihood

# load graphics library and set text large for visibility in lectures
library(ggplot2)
theme_set(theme_grey(base_size = 20))

# Plot likelihood function for Bernoulli data Y~Ber(theta)
y=c(1,1,0,0,1,0,1,1,1,1)		# some binary data
n=length(y)						# sample size
m=sum(y)							# number of 1's

thetavals=seq(0.01,0.99,0.01)			# vector of theta values
lik=thetavals^m*(1-thetavals)^(n-m) 	# likelihood function

qplot(thetavals,lik)					# plot the pairs of points					

# Now let's do it by making a function for calculating likelihood
lik=function(theta,data)
{
  # Function to calculate likelihood function for iid Bernoulli data Y~Ber(theta)
  n=length(data)						# sample size
  m=sum(data)							# number of 1's
  theta^m*(1-theta)^(n-m) 	# likelihood function
}

# Plot the function
ggplot(data.frame(thetavals), aes(thetavals)) + stat_function(fun=lik,args=list(data=y))

# What's likelihood look like for a bigger sample size?
ybig=rbinom(100,1,0.7)		# Simulate a 1000 independent binary values with Pr(Y=1)=0.7
ggplot(data.frame(thetavals), aes(thetavals)) + stat_function(fun=lik,args=list(data=ybig))


# Now make natural (base e) logarithm of the likelihood function
loglik=function(theta,data)
{
  n=length(data)						# sample size
  m=sum(data)							# number of 1's
  m*log(theta)+(n-m)*log(1-theta) 	# log-likelihood function
}

# Plot the log-likelihood function
ggplot(data.frame(thetavals), aes(thetavals)) + stat_function(fun=loglik,args=list(data=y))

# Now find arg max of the log likelihood function (or arg min of - log likelihood)
out=optimize(f=loglik,interval=c(0,1),maximum=TRUE,data=y)
out
thetahat=out$maximum	# Maximum Likelihood Estimate (MLE) of probability theta

# And if you want to see which values were chosen put print in function
loglik=function(theta,data)
{
  print(theta)
  n=length(data)						# sample size
  m=sum(data)							# number of 1's
  m*log(theta)+(n-m)*log(1-theta) 	# log-likelihood function
}
out=optimize(f=loglik,interval=c(0,1),maximum=TRUE,data=y)

# Function to find subintervals for general section method where 0.5<r<1 
interval=function(x,f,r=0.75,data){
  a=x[1]  # lower limit of interval
  b=x[2]  # upper limit of interval
  if (f(a+r*(b-a),data)>f(a+(1-r)*(b-a),data)) {newx=c(a+(1-r)*(b-a),b)} else {newx=c(a, a+r*(b-a))}
  newx
}

x=c(0,1)  # Start with full range for parameter and watch the intervals narrow 
x=interval(x,loglik,data=y)
x
x=interval(x,loglik,data=y)
x
x=interval(x,loglik,data=y)
x
x=interval(x,loglik,data=y)
x
x=interval(x,loglik,data=y)
x




# Examples  of numerical optimization in R

#################################################
# Example 1: Estimating theta for Y~Bin(theta)
loglik <- function(theta, y) {
  print(theta)	 # Print out theta value when this function called
  # log likelihood for Y~Ber(theta)
  sum(y) * log(theta) + (length(y) - sum(y)) * log(1 - theta)
  # Or you could use the pmf in R:
  #sum(log(dbinom(y,1,theta)))
}

theta=0.3			# Set the true parameter
estimate=numeric()

for (k in 1:1000)
{
  data <- rbinom(12,1,theta)	# Generate some data Y~Ber(theta)
  cat("Estimating for sample ",k,"\n")
  out=optimize(loglik, c(0, 1), maximum = TRUE, y = data)
  estimate[k]=out$maximum
}

hist(estimate)
abline(v=theta,lwd=2)

# Sample estimates of MSE, Bias and Efficiency
MSE=mean((estimate-theta)^2)
bias=mean(estimate-theta)
efficiency=var(estimate)

cat(MSE,bias,efficiency,bias^2+efficiency)

#################################################
# Example 2: Estimating mu for Y~N(mu,100) 
sigma=sqrt(100)

loglik <- function(mu, y) {
  print(mu)	 # Print out mu value when this function called
  # log likelihood for Y~N(mu,sigma^2)
  sum(log(dnorm(y,mean=mu,sd=sigma)))
}

mu=150			# Set the true parameter
estimate=numeric()

for (k in 1:1000)
{
  data <- rnorm(150,mu,sigma)	# Generate some data Y~Ber(theta)
  cat("Estimating for sample ",k,"\n")
  out=optimize(loglik, c(0, 1000), maximum = TRUE, y = data)
  estimate[k]=out$maximum
}

hist(estimate)
abline(v=mu,lwd=2)

#################################################
# Example 3: Estimating mu and sigma for Y~N(mu,sigma^2)
loglik <- function(p, y) {
  # log likelihood for Y~N(mu,sigma^2)
  # p[1]=mu
  # p[2]=log(sigma) to ensure sigma>0
  sum(log(dnorm(y,mean=p[1],sd=exp(p[2]))))
}

mu=150			# Set the true parameters
sigma=10
muest=numeric()
sdest=numeric()

for (k in 1:1000)
{
  data <- rnorm(150,mu,sigma)		# Generate some data Y~N(mu,sigma^2)
  cat("Estimating for sample ",k,"\n")
  out=optim(c(100, 5), loglik, control = list(fnscale = -1), y = data)
  muest[k]=out$par[1]
  sdest[k]=exp(out$par[2])
}

plot(muest,sdest)
abline(v=mu)
abline(h=sigma)

