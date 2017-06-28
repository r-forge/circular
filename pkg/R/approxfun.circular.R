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

approxfun.circular <- function (x, y = NULL, method = "linear", yleft, yright,
  rule = 1, f = 0, ties = mean) {
  method <- pmatch(method, c("linear", "constant"))
  if (is.na(method))
    stop("invalid interpolation method")
  stopifnot(is.numeric(rule), (lenR <- length(rule)) >= 1L, 
    lenR <= 2L)
  if (lenR == 1)
    rule <- rule[c(1, 1)]
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
  if (missing(yleft)) 
    yleft <- if (rule[1L] == 1) 
      NA
  else y[1L]
  if (missing(yright)) 
    yright <- if (rule[2L] == 1) 
      NA
  else y[length(y)]
  force(f)
  stopifnot(length(yleft)==1L, length(yright)==1L, length(f)==1L)
  rm(rule, ties, lenR, n)
  x <- as.double(x)
  y <- as.double(y)
###  .Call(C_ApproxTest, x, y, method, f)
  function(v) .approxfun.circular(x, y, v, method, yleft, yright, f)
}

.approxfun.circular <- function(x, y, v, method, yleft, yright, f) {
  v <- conversion.circular(v, units="radians", modulo="2pi")
  attr(v, "circularp") <- attr(v, "class") <- NULL
  stats:::.approxfun(x, y, v, method, yleft, yright, f)
}

  