\name{extremevalues}
\title{An R package for outlier detection}
\alias{extremevalues-package}
\alias{extremevalues}
\docType{package}
\description{
This package offers outlier detection and plot functions for
univariate data.

The package is the implementation of the outlier detection methods introduced
in the reference below. Briefly, the methods work as follows. Using a subset of
the data, the parameters for a model distribution are estimated using
regression of the sorted data on their QQ-plot positions.

A value in the data is an outlier when it is unlikely to be drawn from the
estimated distribution. There are two methods to determine the "unlikelyness".
The first, called "Method I", determines the value above which less than
\eqn{\rho} observations are expected, given the total number of observations in
the data. Here \eqn{\rho} is a parameter which should have a value of 1 or
less. The second notion of unlikelyness uses the fit residuals. Extremely
large or small values are outliers when their residuals are above
or below a confidence limit \eqn{\alpha}, to be determined by the user.
}

\seealso{\code{\link{getOutliers}}, \code{\link{outlierPlot}} }

\references{
M.P.J. van der Loo, Distribution based outlier detection for univariate
data. Discussion paper 10003, Statistics Netherlands, The Hague (2010).
Available from www.markvanderloo.eu or www.cbs.nl.
}
