\name{getQQLimit}
\keyword{internal}
\alias{qqLognormalLimit}
\alias{qqExponentialLimit}
\alias{qqParetoLimit}
\alias{qqWeibullLimit}
\alias{qqNormalLimit}
\title{Determine outlier limit}
\description{
Determine outlier limit. These functions are called by the
wrapper function getOutliersII
}
\usage{
qqExponentialLimit(y, p, iLambda, alpha)
qqLognormalLimit(y, p , iLambda, alpha)
qqParetoLimit(y, p , iLambda, alpha)
qqWeibullLimit(y, p , iLambda, alpha)
qqNormalLimit(y, p , iLambda, alpha)
}

\arguments{
\item{y}{Vector of real numbers}
\item{p}{Corresponding quantile values}
\item{N}{Index vector, indicating which elements of y should be used in the fit}
\item{rho}{Confidence values for residual distribution}
}
\value{
\item{limit}{The residual-values corresponding to the confidence values}
\item{R2}{R-squared value for the fit}
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
confidence limits for the fit residuals.
}
\references{
M.P.J. van der Loo, Distribution based outlier detection for univariate
data. Discussion paper 10003, Statistics Netherlands, The Hague (2010).
Available from www.markvanderloo.eu or www.cbs.nl.
}

\author{Mark van der Loo, see www.markvanderloo.eu}
\examples{
y <- sort(exp(rnorm(100)));
p <- seq(1,100)/1000;
L <- qqExponentialLimit(y,p,seq(10,90),0.05);
}

