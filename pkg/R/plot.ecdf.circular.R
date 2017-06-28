#############################################################
#                                                           #
#   plot.ecdf.circular function                             #
#   Author: Claudio Agostinelli                             #
#   Email: claudio.agostinelli@unitn.it                     #
#   Date: June, 27, 2017                                    #
#   Copyright (C) 2017 Claudio Agostinelli                  #
#                                                           #
#   Version 0.1                                             #
#############################################################
  
plot.ecdf.circular <- function(x, xlim=c(0,2*pi), ylim=NULL, xlab="x", ylab="Fn(x)", main=NULL, add=FALSE, verticals=FALSE, col.01line="gray70", do.points=(n < 1000), pch=19, col=par("col"), col.points=col, cex.points=par("cex"), col.hor=col, col.vert=col, lty=par("lty"), lwd=par("lwd"), ...) {
  if (all(class(x)!="ecdf.circular")) {
    if (is.numeric(x)) {
      sarg <- substitute(x)
      x <- ecdf.circular(as.circular(x))
      attr(x, "call") <- call("ecdf.circular", sarg)
    } else stop("'plot.ecdf.circular' called with wrong type of argument 'x'")
  }
  if (is.null(main))
    main <- {
      cl <- attr(x, "call")
      deparse(if (!is.null(cl)) 
      cl
      else sys.call())
    }
  xval <- knF <- knots(x)
  npi1 <- xlim[1L]%/%(2*pi)
  npi2 <- xlim[2L]%/%(2*pi)
  ti <- sort(c(sapply((npi1:npi2)*2*pi, function(z) xval+z)))
  ti <- ti[xlim[1L] <= ti & ti <= xlim[2L]]  
  ti.l <- ti[-length(ti)]
  ti.r <- ti[-1L]
  y <- x(circular(0.5 * (ti.l + ti.r)))
  n <- length(y)
  Fn.kn <- x(circular(ti))
  dev.hold()
  on.exit(dev.flush())
  if (add) 
    segments(ti.l, y, ti.r, y, col = col.hor, lty = lty, lwd = lwd, ...)
  else {
    if (is.null(ylim)) 
      ylim <- range(c(y, Fn.kn))
    plot(NA, NA, type="n", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, main=main, ...)
    segments(ti.l, y, ti.r, y, col = col.hor, lty = lty, lwd = lwd)
  }
  if (do.points) 
    points(ti, Fn.kn, pch = pch, col = col.points, cex = cex.points)
  if (verticals) 
    segments(xval, y[-n], xval, y[-1L], col = col.vert, lty = lty, lwd = lwd)
  abline(h = c(0, 1), col = col.01line, lty = 2)
  invisible(list(t = ti, y = y))
}
