ecdf <- function(x, ...) UseMethod("ecdf")
ecdf.default <- function(x, ...) stats::ecdf(x=x)

#############################################################
#                                                           #
#   ecdf.circular function                                  #
#   Author: Claudio Agostinelli                             #
#   Email: claudio.agostinelli@unitn.it                     #
#   Date: June, 27, 2017                                    #
#   Copyright (C) 2017 Claudio Agostinelli                  #
#                                                           #
#   Version 0.1                                             #
#############################################################

ecdf.circular <- function(x, from=NULL, ...) {
  # Handling missing values
  x <- na.omit(x)
  if (length(x)==0) {
    warning("No observations (at least after removing missing values)")
    return(NULL)
  }    
  x <- conversion.circular(x, units="radians", modulo="2pi")
  attr(x, "circularp") <- attr(x, "class") <- NULL
  mu <- MeanCircularRad(x)
  if (is.null(from)) {
    from <- mu - pi
  } else {
    .NotYetUsed() 
    from <- conversion.circular(from, units="radians", zero=0, rotation="counter", modulo="2pi")    
  }  
  attr(from, "class") <- attr(from, "circularp") <- NULL
  x <- sort(x)
  n <- length(x)
  if (n < 1) 
    stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  rval <- ecdffun.circular(vals, cumsum(tabulate(match(x, vals)))/n, 
    method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
  class(rval) <- c("ecdf.circular", "ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}

ecdffun.circular <- function (x, y = NULL, method = "linear", yleft, yright,
  rule = 1, f = 0, ties = mean) {
  method <- pmatch(method, c("linear", "constant"))
  if (is.na(method))
    stop("invalid interpolation method")
  stopifnot(is.numeric(rule), (lenR <- length(rule)) >= 1L, 
    lenR <= 2L)
  if (lenR == 1)
    rule <- rule[c(1, 1)]
  x <- c(x-2*pi,x,x+2*pi)
  y <- c(y-1, y, y+1)
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
##  .Call(C_ApproxTest, x, y, method, f)
  function(v) .ecdffun.circular(x, y, v, method, yleft, yright, f)
}

.ecdffun.circular <- function(x, y, v, method, yleft, yright, f) {
  v <- conversion.circular(v, units="radians")
  attr(v, "circularp") <- attr(v, "class") <- NULL
  stats:::.approxfun(x, y, v%%(2*pi), method, yleft, yright, f)+v%/%(2*pi)
}

    