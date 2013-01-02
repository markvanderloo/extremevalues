
# Estimating parameters: normal distribution.
x <- rnorm(100,mean=3, sd=1.5)
# realized mean and standard deviation:
c( mean=mean(x), sd=sd(x) )
# estimate with the parameters function
parameters(x, qnorm, mean=0, sd=1)


