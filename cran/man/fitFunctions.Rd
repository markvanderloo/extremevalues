\name{fitFunctions}
\keyword{internal}
\alias{fitNormal}
\alias{fitLognormal}
\alias{fitExponential}
\alias{fitPareto}
\alias{fitWeibull}
\title{Fit model distributions}
\description{
Fit model distribution to a set of observations.
}
\usage{
fitNormal(y, p)
fitLognormal(y, p)
fitPareto(y, p)
fitExponential(y, p)
fitWeibull(y, p)
}

\arguments{
\item{y}{Vector of one-dimensional nonnegative data}
\item{p}{Corresponding quantile values}
}

\value{
\item{R2}{R-squared value for the fit}
\item{lamda}{(exponential only) Estimated location (and spread) parameter for \eqn{f(y)=\lambda*exp(-\lambda * y)}}
\item{mu}{(lognormal only) Estimated \eqn{{\sf E}(\ln(y))} for lognormal distribution}
\item{sigma}{(lognormal only) Estimated Var(ln(y)) for lognormal distribution}
\item{ym}{(pareto only) Estimated location parameter (mode) for pareto distribution} 
\item{alpha}{(pareto only) Estimated spread parameter for pareto distribution}
\item{k}{(weibull only) estimated power parameter \eqn{k} for weibull distribution}
\item{lambda}{(weibull only) estimated scaling parameter \eqn{\lambda} for weibull distribution}
}

\details{
The function sorts the values of y and uses (log)linear regression to fit
the values between the pmin and pmax quantile to the cdf
of a model distribution. 
}

\references{
M.P.J. van der Loo, Distribution based outlier detection for univariate
data. Discussion paper 10003, Statistics Netherlands, The Hague (2010).
Available from www.markvanderloo.eu or www.cbs.nl.
}
\author{Mark van der Loo, see www.markvanderloo.eu}
\examples{
y = 10^rnorm(50);
L <- getOutliers(y,rho=0.5);
}

