\name{outlierPlot}
\alias{outlierPlot}
\alias{qqFitPlot}
\alias{plotMethodII}
\title{Plot results of outlierdetection}
\description{
This is a wrapper for two plot functions which can be used to analyse the
results of outlier detection with the extremevalues package.
}
\usage{
outlierPlot(y, L, mode="qq", ...)
qqFitPlot(y, L, title=NA, xlab=NA, ylab=NA, fat=FALSE)
plotMethodII(y, L, title=NA, xlab=NA, ylab=NA, fat=FALSE)
}
\arguments{
\item{y}{A vector of values}
\item{L}{The result of L <- getOutliers(y,...)}
\item{mode}{Plot type. "qq" for Quantile-quantile plot with indicated outliers, "residual"
for plot of fit residuals with indicated outliers (Method II only)}
\item{...}{Optional arguments, to be transferred to qqFitPlot or plotMethodII (see below)}
\item{title}{A custom title (must be a string)}
\item{xlab}{A custom label for the x-axis (must be a string)}
\item{ylab}{A custim label for the y-axis (must be a string)}
\item{fat}{If TRUE, axis, fonts, labels, points and lines are thicker for export and publication}
}

\details{
Outliers are marked with a color or special symbol.  If \bold{mode="qq":}
observed agains predicted y-values are plotted. Points between vertical lines
were used in the fit.  If L\$method="Method I", horizontal lines indicate the
limits below (above) which observations are outliers.  \bold{mode="residuals"}
only works when L\$Method="Method II". It generates a residual plot where
points between two vertical lines were used in the fit. Horizontal lines
indicate the computed confidence limits. The outermost points in the gray areas
are outliers.
}

\author{Mark van der Loo, www.markvanderloo.eu} 
\references{
The file <your R directory>/R-<version>/library/extremevalues/extremevalues.pdf
contains a worked example. It can also be downloaded from my website.
}

\examples{
y <- rlnorm(100)
y <- c(0.1*min(y),y,10*max(y))
K <- getOutliers(y,method="I",distribution="lognormal")
L <- getOutliers(y,method="II",distribution="lognormal")
par(mfrow=c(1,2))
outlierPlot(y,K,mode="qq")
outlierPlot(y,L,mode="residual")
}






