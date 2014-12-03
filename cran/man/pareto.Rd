
\name{pareto}
\alias{dpareto}
\alias{qpareto}
\alias{rpareto}
\title{Pareto distribution}
\description{
Pareto density distribution, quantile function and random generator.
}
\usage{
dpareto(x, xm=1, alpha=1)
qpareto(p, xm=1, alpha=1)
rpareto(n, xm=1, alpha=1)
}

\arguments{
\item{xm}{location parameter (mode of distribution)}
\item{alpha}{spread parameter}
\item{x}{Vector of realizations}
\item{p}{Vector of probabilities}
\item{n}{number of samples to draw}
}

\value{
\item{dpareto}{Probability density}
\item{qpareto}{Quantile at probability p (inverse cdf)}
\item{rpareto}{Random value}
}
\author{Mark van der Loo  www.markvanderloo.eu}
\examples{
q <- qpareto(0.5);
}
