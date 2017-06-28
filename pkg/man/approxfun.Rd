\name{approxfun}
\title{Interpolation Functions}
\alias{approxfun}
\alias{approxfun.default}

\description{
The \code{approfun} function from the \pkg{stats} package is replace by a new
\code{method} in order to report the approxfun of circular data
appropriatly. \code{approxfun.default} is an alias of the original function
\code{approxfun} see \code{\link[stats]{approxfun}}. The behavior would be the
same for objects which are not from \code{\link{class}} \code{\link{circular}}
(in this case see the function \code{\link{approxfun.circular}} for more details).
}

\usage{
approxfun(x, \dots)
\method{approxfun}{default}(x, \dots)
}

\arguments{
  \item{x}{numeric vector.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\seealso{
\code{\link[stats]{approxfun}}.
}

\keyword{dplot}
\keyword{arith}  
