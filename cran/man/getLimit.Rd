\name{getLimit}
\keyword{internal}
\alias{getNormalLimit}
\alias{getLognormalLimit}
\alias{getExponentialLimit}
\alias{getParetoLimit}
\alias{getWeibullLimit}
\title{Determine outlier limit}
\description{
Determine outlier limit. These functions are called by the
wrapper function getOutliers
}
\usage{
getExponentialLimit(y, p, N, rho)
getLognormalLimit(y, p, N, rho)
getNormalLimit(y, p, N, rho)
getParetoLimit(y, p, N, rho)
getWeibullLimit(y, p, N, rho)
}

\arguments{
\item{y}{Vector of one-dimensional nonnegative data}
\item{p}{Corresponding quantile values}
\item{N}{Number of observations}
\item{rho}{Limiting expected value}
}
\value{
\item{limit}{The y-value above which less then rho observations are expected}
\item{R2}{R-squared value for the fit}
\item{nFit}{Number of values used in fit (length(y))}
\item{lamda}{(exponential only) Estimated location (and spread) parameter for \eqn{f(y)=\lambda\exp(-\lambda  y)}}
\item{mu}{(lognormal only) Estimated \eqn{{\sf E}(\ln(y))} for lognormal distribution}
\item{sigma}{(lognormal only) Estimated Var(ln(y)) for lognormal distribution}
\item{ym}{(pareto only) Estimated location parameter (mode) for pareto distribution} 
\item{alpha}{(pareto only) Estimated spread parameter for pareto distribution}
\item{k}{(weibull only) estimated power parameter \eqn{k} for weibull distribution}
\item{lambda}{(weibull only) estimated scaling parameter \eqn{\lambda} for weibull distribution}
   }

\details{
The functions fit a model cdf to the observed y and p and returns the
y-value above which less than rho values are expected, given N observations.
See getOutlierLimit for a complete explanation. 
}

\references{
M.P.J. van der Loo, Distribution based outlier detection for univariate
data. Discussion paper 10003, Statistics Netherlands, The Hague (2010).
Available from www.markvanderloo.eu or www.cbs.nl.
}
\author{Mark van der Loo, see www.markvanderloo.eu}
\examples{
y <- sort(exp(rnorm(100)));
p <- seq(1,100)/100;
II <- seq(10,90)
L <- getExponentialLimit(y[II],p[II],100,1.0);
}

