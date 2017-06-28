\name{plot.ecdf.circular}
\alias{plot.ecdf.circular}
\title{
Plot an empirical cumulative distribution function for a circular data set.  
}
\description{
Plot an empirical cumulative distribution function for a circular data set.    
}
\usage{
\method{plot}{ecdf.circular}(x, xlim = c(0, 2 * pi), ylim = NULL,
  xlab = "x", ylab = "Fn(x)", main = NULL, add = FALSE,
  verticals = FALSE, col.01line = "gray70", do.points = (n < 1000),
  pch = 19, col = par("col"), col.points = col, cex.points = par("cex"),
  col.hor = col, col.vert = col, lty = par("lty"), lwd = par("lwd"), ...)
}
\arguments{
  \item{x}{an object of class \code{\link{ecdf.circular}} or a numeric vector of class \code{\link{circular}}.}
  \item{xlim, ylim}{limits for the plot region: see \code{\link{plot.window}}. Both have sensible defaults if omitted.}
  \item{xlab, ylab}{labels for x and y axis.}
  \item{main}{main title.}
  \item{add}{logical; if \code{TRUE} only \emph{add} to an existing plot.}
  \item{verticals}{logical; if \code{TRUE}, draw vertical lines at steps.}
  \item{col.01line}{numeric or character specifying the color of the
    horizontal lines at y = 0 and 1, see \code{\link{colors}}.}
  \item{do.points}{logical; if \code{TRUE}, also draw points at the
    (\code{xlim} restricted) knot locations. Default is true, for
    sample size \eqn{< 1000}.}
  \item{pch}{character or integer code; point character if \code{do.points}.}
  \item{col}{default color of all points and lines.}
  \item{col.points}{character or integer code; color of points if
    \code{do.points}.}
  \item{cex.points}{numeric; character expansion factor if \code{do.points}.}
  \item{col.hor}{color of horizontal lines.}
  \item{col.vert}{color of vertical lines.}
  \item{lty, lwd}{line type and thickness for all lines.}
  \item{\dots}{further arguments of \code{\link{plot}(.)}, or if\code{(add)}
    \code{\link{segments}(.)}.}
}

\value{
 A list with two components
  \item{t}{abscissa (x) values, including the two outermost ones.}
  \item{y}{y values \sQuote{in between} the \code{t[]}.}
}
\references{
K.V. Mardia and P.E. Jupp (2000). Directional Statistics. Wiley.
}
\author{
Claudio Agostinelli
}

\seealso{
\code{\link{ecdf.circular}}.
}
\examples{
x <- rvonmises(10, circular(0), 5)
plot(ecdf(x))
plot(ecdf(x), xlim=c(-20,20))    
}
  
\keyword{dplot}
\keyword{hplot}
