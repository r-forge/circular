\name{KatoJones}
\alias{katojones}
\alias{dkatojones}

\title{Kato and Jones Density Function}
\description{Density for the Kato and Jones distribution.}

\usage{dkatojones(x, mu, nu, r, kappa)
}

\arguments{
  \item{x}{the angular value the density must be computed in.}
  \item{mu}{the M\"obius 'mu' parameter. The object is coerced to class \code{\link{circular}}.}
  \item{nu}{the M\"obius 'nu' parameter. The object is coerced to class \code{\link{circular}}.}
  \item{r}{the M\"obius 'r' parameter. It must be in [0,1).}
  \item{kappa}{the positive vonMises parameter.}
}

\value{The density.}

\references{Kato , S. and Jones, M.C. (2010). A family of distributions on the circle with links to, and applications arising from,
   M\"obius transformation. J. Am. Statist. Assoc. 105, 249–262.}
\author{Federico Rotolo}

\keyword{distribution}
\keyword{circle}
\keyword{circular}
