\name{evGui}
\alias{evGui}
\title{GUI to explore options and results of the "extremevalues" package }
\description{
 Opens a Graphical User Interface and plots results. Options
of the extremevalue package functions can be set and results
are updated instantly. Includes a code generator button.
}
\usage{
evGui(y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A vector of type \code{numeric} }
}
\note{
The GUI is programmed in a very  quick and pretty dirty way, but
it works fine. It will be replaced by a gtk-version in the future.
}
\references{www.markvanderloo.eu }
\author{Mark van der Loo }
\seealso{\code{\link{getOutliers}} }
\examples{
\dontrun{
    y <- rnorm(100)
    evGui(y)
    }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ outliers }
