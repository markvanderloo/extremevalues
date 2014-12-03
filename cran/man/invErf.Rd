\name{invErf}

\alias{invErf}

\title{Inverse error function}
\description{
Inverse error function
}
\usage{
invErf(x)
}

\arguments{
\item{x}{(Vector of) real value(s) in the range (-1,1)}
}

\value{
(vector of) value(s) of the inverse error function
}

\author{Mark van der Loo, www.markvanderloo.eu}
\examples{
x <-seq(-0.99,0.99,0.01);
plot(x,invErf(x),'l');
}


