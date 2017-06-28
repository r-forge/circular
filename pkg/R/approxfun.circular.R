approxfun <- function(x, ...) UseMethod("approxfun")
approxfun.default <- function(x, ...) stats::approxfun(x=x, ...)

#############################################################
#                                                           #
#   approxfun.circular function                             #
#   Author: Claudio Agostinelli                             #
#   Email: claudio.agostinelli@unitn.it                     #
#   Date: June, 27, 2017                                    #
#   Copyright (C) 2017 Claudio Agostinelli                  #
#                                                           #
#   Version 0.1                                             #
#############################################################

approxfun.circular <- function (x, y = NULL, method = "linear", f = 0, ties = mean) {
  method <- pmatch(method, c("linear", "constant"))
  if (is.na(method))
    stop("invalid interpolation method")
  x <- conversion.circular(x, units="radians", modulo="2pi")
  attr(x, "circularp") <- attr(x, "class") <- NULL
  x <- c(x-2*pi,x,x+2*pi)
  y <- rep(y, 3)
  x <- stats:::regularize.values(x, y, ties)
  y <- x$y
  x <- x$x
  n <- as.integer(length(x))
  if (is.na(n)) 
    stop("invalid length(x)")
  if (n <= 1) {
    if (method == 1) 
      stop("need at least two non-NA values to interpolate")
    if (n == 0) 
      stop("zero non-NA points")
  }
  force(f)
  rm(ties, n)
  x <- as.double(x)
  y <- as.double(y)
###  .Call(C_ApproxTest, x, y, method, f)
  function(v) {
    if (!is.circular(v))
      v <- circular(v)
    .approxfun.circular(x, y, v, method, f)
  }
}

.approxfun.circular <- function(x, y, v, method, f) {
  v <- conversion.circular(v, units="radians", modulo="2pi")
  attr(v, "circularp") <- attr(v, "class") <- NULL
  stats:::.approxfun(x, y, v, method, NA, NA, f)
}
