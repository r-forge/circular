#############################################################
#
#   bw.cv.mse.circular and bw.cv.ml.circular functions
#   Author: Claudio Agostinelli and Eduardo García Portugués
#   Email: claudio@unive.it
#   date: May, 17, 2011
#   Copyright (C) 2011 Claudio Agostinelli and Eduardo García Portugués
#
#   Version 0.1
#
#############################################################

###References: Hall, Watson and Cabrera, 1984
# Cross validation by MSE
bw.cv.mse.circular <- function(x, hmax=NULL, lower=NULL, upper=NULL, tol = 1e-4, kernel = c("vonmises", "wrappednormal"), K = NULL, min.k = 10) {
  kernel <- match.arg(kernel)
  if (kernel=="vonmises") {
    if (is.null(hmax))
      hmax <- 100
    if (is.null(lower))
      lower = 0.01 * hmax
    if (is.null(upper))
      upper = hmax
  } else {
    if (is.null(hmax))
      hmax <- 1.144 * sqrt(-2*log(RhoCircularRad(x))) * n^(-1/5) 
    if (is.null(lower))
      lower = 0.01 * hmax
    if (is.null(upper))
      upper = hmax
  }
  if ((n <- length(x)) < 2L) 
    stop("need at least 2 data points")  
  x <- conversion.circular(x, units="radians", zero=0, rotation="counter", modulo="2pi")
  attr(x, "class") <- attr(x, "circularp") <- NULL
  if (!is.numeric(x)) 
    stop("invalid 'x'")
  mse.internal <- function(bw, data) {
    ##bw: bw
    ##data: x
    tone <- integrate(function(z) DensityCircularRad(x=data, z=z, bw=bw, kernel=kernel, K=K, min.k=min.k)^2, lower=0, upper=2*pi, abs.tol=1e-4)$value
    rr <- sapply(1:length(data), function(i) DensityCircularRad(x=data[-i], z=data[i], bw=bw, kernel=kernel, K=K, min.k=min.k))
    ttwo <- 2*sum(rr)/length(data)
    result <- tone-ttwo
    return(result)
  }

  bw <- optimize(function(bw) mse.internal(bw, x), lower=lower, upper=upper, tol=tol, maximum = FALSE)$minimum
  return(bw)
}

## Cross validation by ML
bw.cv.ml.circular <- function(x, hmax=NULL, lower=NULL, upper=NULL, tol = 1e-4, kernel = c("vonmises", "wrappednormal"), K = NULL, min.k = 10) {
  kernel <- match.arg(kernel)
  if (kernel=="vonmises") {
    if (is.null(hmax))
      hmax <- 100
    if (is.null(lower))
      lower = 0.01 * hmax
    if (is.null(upper))
      upper = hmax
  } else {
    if (is.null(hmax))
      hmax <- 1.144 * sqrt(-2*log(RhoCircularRad(x))) * n^(-1/5)
    if (is.null(lower))
      lower = 0.01 * hmax
    if (is.null(upper))
      upper = hmax
  }
  if ((n <- length(x)) < 2L) 
    stop("need at least 2 data points")  
  x <- conversion.circular(x, units="radians", zero=0, rotation="counter", modulo="2pi")
  attr(x, "class") <- attr(x, "circularp") <- NULL
  if (!is.numeric(x)) 
    stop("invalid 'x'")
  ml.internal <- function(bw, data) {
    ##bw: bw
    ##data: x
    ss <- sapply(1:length(data), function(i) log(DensityCircularRad(x=data[-i], z=data[i], bw=bw, kernel=kernel, K=K, min.k=min.k)))
    result <- sum(ss)/length(data)
    return(result)
  }

  bw <- optimize(function(bw) ml.internal(bw, x), lower=lower, upper=upper, tol=tol, maximum = TRUE)$maximum
  return(bw)
}
