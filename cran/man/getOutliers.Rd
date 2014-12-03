\name{getOutliers}
\alias{getOutliers}
\alias{getOutliersI}
\alias{getOutliersII}
\title{Detect outliers}
\description{
getOutliers is a wrapper function for getOutliersI and getOutliersII.
}
\usage{
getOutliers(y, method="I",  ...)
getOutliersI(y, rho=c(1,1), FLim=c(0.1,0.9), distribution="normal")
getOutliersII(y, alpha=c(0.05, 0.05), FLim=c(0.1, 0.9), 
   distribution="normal", returnResiduals=TRUE)
}
\arguments{
\item{y}{Vector of one-dimensional nonnegative data}
\item{method}{"I" or "II"}
\item{...}{Optional arguments to be passed to getOutliersI or getOutliersII}
\item{distribution}{Model distribution used to estimate the limit. Choose from
"lognormal", "exponential", "pareto", "weibull" or "normal" (default).}
\item{FLim}{c(Fmin,Fmax) quantile limits indicating which data should be used
to fit the model distribution. Must obey 0 < Fmin < Fmax < 1.}
\item{rho}{(Method I) A value \eqn{y_i} is an outlier if it is below (above)  the limit where less then
rho[2] (rho[1]) observations are expected. Must be >0.}
\item{alpha}{(Method II) A value \eqn{y_i} is an outlier if it has a residual below (above) the
alpha[1] (alpha[2]) confidence limit for the residues. Must be between 0 and 1.}
\item{returnResiduals}{(Method II) Whether or not to return a vector of residuals from the fit}
}

\value{
\item{nOut}{Number of left and right outliers.}
\item{iLeft}{Index vector indicating left outliers in y}
\item{iRight}{Index vector indicating right outiers in y}
\item{limit}{For \bold{Method I:} y-values below (above) limit[1] (limit[2]) are outliers. 
For \bold{Method II:}
elements with residuals below (above) limit[1] (limit[2]) are outliers if all smaller (larger) elements
are outliers as well.}
\item{method}{The used method: "method I" or "method II"}
\item{distribution}{The used model distribution}
\item{Fmin}{FLim[1]}
\item{Fmax}{FLim[2]}
\item{yMin}{Smallest y-value used in fit}
\item{yMax}{Largest y-value used in fit}
\item{Nfit}{Number of values used in the fit}
\item{rho}{\bold{Method I}, the input rho-values for left and right outliers}
\item{alphaConf}{\bold{Method II}, the input confidence levels for left and right outliers}
\item{R2}{R-squared value for the fit. Note that this is the \emph{ordinary least squares} value, defined by 
\eqn{R^2=1-SS_{err}/SS_{y}}. Where \eqn{SS_{err}} is the squared sum of residuals. For the lognormal, Pareto and Weibull models,
the \eqn{y}-variable is transformed before fitting. Since predicted values are transformed back before calculating \eqn{SS_{err}},
this \eqn{R^2} can be negative.  }
\item{lambda}{(exponential distribution) Estimated location (and spread) parameter for \eqn{f(y)=\lambda\exp(-\lambda y)}}
\item{mu}{(lognormal distribution) Estimated \eqn{ E(\ln(y))} for lognormal distribution}
\item{sigma}{(lognormal distribution) Estimated \eqn{Var(ln(y))} for lognormal distribution}
\item{ym}{(pareto distribution) Estimated location parameter (mode) for pareto distribution} 
\item{alpha}{(pareto distribution) Estimated spread parameter for pareto distribution}
\item{k}{(weibull distribution) estimated shape parameter \eqn{k} for weibull distribution}
\item{lambda}{(weibull distribution) estimated scale parameter \eqn{\lambda} for weibull distribution}
\item{mu}{(normal distribution) Estimated \eqn{ E(y)} for normal distribution}
\item{sigma}{(normal distribution) Estimated \eqn{Var(y)} for normal distribution}
   }

\details{
Both methods use the subset of \eqn{y}-values between the Fmin and Fmax quantiles
to fit a model cumulative density distribution. \bold{Method I} detects outliers by checking
which are below (above) the limit where according to the model distribution less then
rho[1] (rho[2]) observations are expected (given length(y) observations). \bold{Method II}
detects outliers by finding the observations (not used in the fit) who's fit residuals are
below (above) the estimated confidence limit alpha[1] (alpha[2]) while all lower (higher)
observations are outliers too. 
}

\references{
M.P.J. van der Loo, Distribution based outlier detection for univariate
data. Discussion paper 10003, Statistics Netherlands, The Hague.
Available from www.markvanderloo.eu or www.cbs.nl.

The file <your R directory>/R-<version>/library/extremevalues/extremevalues.pdf
contains a worked example. It can also be downloaded from my website.
}
\author{Mark van der Loo, see www.markvanderloo.eu}
\examples{
y <- rlnorm(100)
y <- c(0.1*min(y),y,10*max(y))
K <- getOutliers(y,method="I",distribution="lognormal")
L <- getOutliers(y,method="II",distribution="lognormal")
par(mfrow=c(1,2))
outlierPlot(y,K,mode="qq")
outlierPlot(y,L,mode="residual")
}

