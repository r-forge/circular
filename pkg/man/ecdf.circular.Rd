\name{ecdf.circular}
\alias{ecdf.circular}
\title{
  Empirical Cumulative Distribution Function for Circular Data
}
\description{
  Compute an empirical cumulative distribution function for a circular data set 
}
\usage{
\method{ecdf}{circular}(x, from = NULL, ...)
}
  
\arguments{
  \item{x}{an object of class \code{circular}.}
  \item{from}{not yet used.}
  \item{\dots}{not yet used.}
}
\details{
We follows the definition of cumulative distribution function given in Mardia and Jupp (2000). In this version F(0) is set to 0. In future version F(from) will be set to 0. 
}
\value{
  a function of class \code{ecdf.circular} to be use to evaluate the empirical cumulative distribution function of a circular data set.
}
\references{
K.V. Mardia and P.E. Jupp (2000). Directional Statistics. Wiley.
}
\author{
Claudio Agostinelli
}

\examples{
x <- rvonmises(10, circular(0), 5)
plot(ecdf(x))
plot(ecdf(x), xlim=c(-20,20))    
}

\keyword{dplot}
\keyword{hplot}
