\name{ecdf}
\title{Empirical Cumulative Distribution Function}
\alias{ecdf}
\alias{ecdf.default}

\description{
The \code{ecdf} function from the \pkg{stats} package is replace by a new
\code{method} in order to report the ecdf of circular data
appropriatly. \code{ecdf.default} is an alias of the original function
\code{ecdf} see \code{\link[stats]{ecdf}}. The behavior would be the same
for objects which are not from \code{\link{class}} \code{\link{circular}}
(in this case see the function \code{\link{ecdf.circular}} for more details).
}

\usage{
ecdf(x, \dots)
\method{ecdf}{default}(x, \dots)
}

\arguments{
  \item{x}{numeric vector.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\seealso{
\code{\link[stats]{ecdf}}.
}

\keyword{dplot}
\keyword{hplot}  
