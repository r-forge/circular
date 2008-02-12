#############################################################
#                                                           #
#   arrows.circular function                                #
#   Author: Claudio Agostinelli                             #
#   E-mail: claudio@unive.it                                #
#   Date: February, 12, 2008                                #
#   Version: 0.1                                            #
#                                                           #
#   Copyright (C) 2008 Claudio Agostinelli                  #
#                                                           #
#############################################################

arrows.circular <- function(x, y=NULL, x0=0, y0=0, na.rm=FALSE, shrink=1, plot.info=NULL, zero=NULL, rotation=NULL, ...) {
  if (na.rm)
    x <- x[!is.na(x)]
  if (length(x)==0) {
    warning("No observations (at least after removing missing values)")
    return(NULL)
  }
  xcircularp <- attr(as.circular(x), "circularp")
  if (is.null(plot.info)) {
    if (is.null(zero))
      zero <- xcircularp$zero
    if (is.null(rotation))
      rotation <- xcircularp$rotation
  } else {
    zero <- plot.info$zero
    rotation <- plot.info$rotation
  }
  x <- conversion.circular(x, units="radians", zero=0, rotation="counter")
  attr(x, "class") <- attr(x, "circularp") <-  NULL
  if (rotation=="clock")
    x <- -x
  x <- x+zero
  x <- x%%(2*pi)
  if (is.null(y))
    y <- rep(1, length(x))
  y <- y*shrink
  if (length(x0)!=x)
    x0 <- rep(x0, length(x))
  if (length(y0)!=x)
    y0 <- rep(y0, length(x)) 
  x1 <- x0 + y*cos(x)
  y1 <- y0 + y*sin(x)
  arrows(x0, y0, x1, y1, ...)  
}
