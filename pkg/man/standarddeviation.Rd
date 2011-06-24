\name{sd}
\title{Standard Deviation}
\alias{sd}
\alias{sd.default}
\alias{sd.data.frame}

\description{
The \code{sd} function from the \pkg{base} is replace by a new
\code{method} in order to report the standard deviation of circular data
appropriatly. \code{sd.default} is an alias of the original function
\code{sd} see \code{\link[stats]{sd}}. The behavior would be the same
for objects which are not from \code{\link{class}}
\code{\link{data.frame}} and \code{\link{circular}} (in the last case
the standard deviation is define as in Mardia (1972) \deqn{\sqrt{-2\ln r}}{sqrt(-2 * ln(r))} where \code{r} is the mean resultant length of
the data, see \code{\link{sd.circular}} for more details). The method for
\code{data.frame} will apply the \code{sd} function to each columns.
}

\usage{
sd(x, \dots)
\method{sd}{default}(x, na.rm = FALSE, \dots)
\method{sd}{data.frame}(x, \dots)
}

\arguments{
  \item{x}{a numeric vector, matrix or data frame.}
  \item{y}{\code{NULL} (default) or a vector, matrix or data frame with
    compatible dimensions to \code{x}.  The default is equivalent to
    \code{y = x} (but more efficient).}
  \item{na.rm}{logical. Should missing values be removed?}
  \item{use}{an optional character string giving a
    method for computing covariances in the presence
    of missing values.  This must be (an abbreviation of) one of the strings
    \code{"all.obs"}, \code{"complete.obs"} or \code{"pairwise.complete.obs"}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\seealso{
\code{\link[stats]{sd}}, \code{\link{sd.circular}}, \code{\link{var.circular}} and \code{\link{summary.circular}}.
}

\keyword{univar}
