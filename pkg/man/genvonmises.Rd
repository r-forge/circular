\name{GenVonMises}
\alias{dgenvonmises}
\alias{genvonmises}
\title{Generalized Von Mises Density Function}
\description{
  Density for the Generalized von Mises circular distribution. 
}

\usage{dgenvonmises(x, mu1, mu2, kappa1, kappa2)}

\arguments{
  \item{x}{a vector. The object is coerced to class \code{\link{circular}}.}
  \item{mu1}{principal direction of the distribution. The object is coerced to class \code{\link{circular}}.}
  \item{mu2}{secondary direction parameter. The object is coerced to class \code{\link{circular}}.}
  \item{kappa1}{non-negative numeric parameter of the distribution.}
  \item{kappa2}{non-negative numeric parameter of the distribution.}
}

\details{}

\value{The density}

\references{Gatto , R. & Jammalamadaka , S.R. (2007). The generalized von Mises distribution. Statistical Methodology 4, 341–353.}

\author{Federico Rotolo}

\keyword{distribution}
\keyword{circle}
\keyword{circular}
\keyword{vonMises}
