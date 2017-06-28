\name{approxfun.circular}
\alias{approxfun.circular}

\title{
 Interpolation periodic Functions defined on the circle 
}
  
\description{
 Return a list of points which linearly interpolate given data points,
  or a function performing the linear (or constant) interpolation.
  The interpolation is performed on modulo 2*pi on the x.
}
  
\usage{
approxfun.circular(x, y = NULL, method = "linear", f = 0, ties = mean)
}
  
\arguments{
  \item{x}{an object of class \code{\link{circular}}.}
  \item{y}{a numeric vector}
  \item{method}{specifies the interpolation method to be used.  Choices
    are \code{"linear"} or \code{"constant"}.}
 \item{f}{for \code{method = "constant"} a number between 0 and 1
    inclusive, indicating a compromise between left- and 
    right-continuous step functions. If \code{y0} and \code{y1} are
    the values to the left and right of the point then the value is
    \code{y0} if \code{f == 0}, \code{y1} if \code{f == 1}, and
    \code{ y0*(1-f)+y1*f} for intermediate values. In this way the result is
    right-continuous for \code{f == 0} and left-continuous for \code{f
    == 1}, even for non-finite \code{y} values.}
  \item{ties}{Handling of tied \code{x} values.  Either a function
    with a single vector argument returning a single number result or
    the string \code{"ordered"}.}
}

\value{
 The function \code{approxfun.circular} returns a function performing (linear or
  constant) interpolation of the given data points. For a given set of
  \code{x} values, this function will return the corresponding
  interpolated values. 
}
  
\author{
  Claudio Agostinelli  
}

\seealso{
\code{\link[stats]{approxfun}}.
}
  
\examples{
x <- rvonmises(100, circular(0), 5)
y <- sin(x)
approxfun(x, y)
  
}
\keyword{arith}
\keyword{dplot}
