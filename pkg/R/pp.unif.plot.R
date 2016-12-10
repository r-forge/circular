#############################################################
#                                                           #
#   pp.unif.plot function                                   #
#   Author: Claudio Agostinelli                             #
#   Email: claudio.agostinelli@unitn.it                     #
#   Date: December, 09, 2016                                #
#   Copyright (C) 2016 Claudio Agostinelli                  #
#                                                           #
#   Version 0.1                                             #
#############################################################

pp.unif.plot <- function(x, ref.line = TRUE, frac=NULL,  xlab = "Uniform Distribution", ylab = "Empirical Distribution", control.circular=list(), col=NULL, col.inf=NULL, col.sup=NULL, ...) {
    
    # Handling missing values
    x <- na.omit(x)
    if (length(x)==0) {
        warning("No observations (at least after removing missing values)")
        return(NULL)
    }
    if (is.circular(x)) {
       datacircularp <- circularp(x)
    } else {
       datacircularp <- list(type="angles", units="radians", template="none", modulo="asis", zero=0, rotation="counter")
    }

    dc <- control.circular
    if (is.null(dc$type))
       dc$type <- datacircularp$type
    if (is.null(dc$units))
       dc$units <- datacircularp$units
    if (is.null(dc$template))
       dc$template <- datacircularp$template
    if (is.null(dc$modulo))
       dc$modulo <- datacircularp$modulo
    if (is.null(dc$zero))
       dc$zero <- datacircularp$zero
    if (is.null(dc$rotation))
       dc$rotation <- datacircularp$rotation

    x <- conversion.circular(x, units="radians", zero=0, rotation="counter", modulo="2pi")
    attr(x, "class") <- attr(x, "circularp") <- NULL    
    y <- sort(x %% (2 * pi))/(2*pi)
    n <- length(y)
    z <- (1:n)/(n + 1)
    if (is.null(col))
      col <- rep(1, n)
    else
      col <- rep(col, length.out=n)  
    if (!is.null(frac)) {
      f <- round(frac*n)
      if (f) {
        zm <- -1 + ((n-f+1):n)/(n+1)
        zp <- 1 + (1:f)/(n+1)
        ym <- -1+y[(n-f+1):n]
        yp <- 1+y[1:f]
        y <- c(ym,y,yp)
        z <- c(zm,z,zp)
        if (is.null(col.inf))
          col.inf <- rep(2, f)
        else
          col.inf <- rep(col.inf, length.out=f)
        if (is.null(col.sup))
          col.sup <- rep(2, f)
        else
          col.sup <- rep(col.sup, length.out=f)
        col <- c(col.inf, col, col.sup)
      }
    }
        
    plot.default(z, y, xlab=xlab, ylab=ylab, col=col, ...)
    if (ref.line) {
      abline(0, 1)
      if (!is.null(frac)) {
        abline(h=c(0,1), lty=3)
        abline(v=c(0,1), lty=3)          
      } 
    }
}
