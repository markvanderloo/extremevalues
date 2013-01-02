
# Estimating parameters: normal distribution.
x <- rnorm(100,mean=3, sd=1.5)
# realized mean and standard deviation:
c( mean=mean(x), sd=sd(x) )
# estimate with the parameters function
parameters(x, qnorm, mean=0, sd=1)
# now using only the middle 80 percent of observations
parameters(x, qnorm, Flim=c(0.1,0.9), mean=0, sd=1)
# now letting optim use the BFGS method
parameters(x, qnorm, mean=0, sd=1, optimpar=list(method='BFGS'))



