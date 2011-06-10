
sd <- function(x, ...) UseMethod("sd")

sd.default <- function(x, na.rm = FALSE, ...) stats::sd(x=x, na.rm=na.rm)

sd.data.frame <- function(x, ...) {
    sapply(x, sd, ...)
}

##############################################################
#                                                            #
#   sd.circular function                                     #
#   Author: Claudio Agostinelli and Jean-Olivier Irisson     #
#   Email: claudio@unive.it                                  #
#   Date: June, 10, 2011                                     #
#   Copyright (C) 2011 Claudio Agostinelli                   #
#                                                            #
# As defined in                                              #
#   Mardia, KV. Statistics of directional data. 1972, p 24,74#
# Formula actually taken from                                #
#   Zar, JH. Biostatistical analysis. 2010, sec 26.5, p 617  #
#                                                            #
#   Version 0.2                                              #
##############################################################

sd.circular <- function (x, na.rm=FALSE, control.circular=list(), ...)  {
  if (is.matrix(x)) {
    apply(x, 2, sd.circular, na.rm=na.rm)
  } else {
    if (na.rm) 
      x <- x[!is.na(x)]
    if (length(x) == 0) {
      warning("No observations (at least after removing missing values)")
      return(circular(NA))
    }
    if (!is.circular(x))
      x <- circular(x)
    datacircularp <- circularp(x)  
    x <- conversion.circular(x, units="radians", zero=0, rotation="counter")
    attr(x, "class") <- attr(x, "circularp") <-  NULL
    s <- SdCircularRad(x=x)
    dc <- control.circular
    if (is.null(dc$type))
      dc$type <- datacircularp$type
    if (is.null(dc$units))
      dc$units <- datacircularp$units
    s <- conversion.circular(circular(s), units=dc$units, type=dc$type, template="none", modulo="2pi", zero=0, rotation="counter")
    return(s)
  }
}

SdCircularRad <- function(x) {
#  if (any(is.na(x)))
#    return(NA)
#  n <- length(x)
#  c <- sum(cos(x))
#  s <- sum(sin(x))
#  r <- sqrt(c^2 + s^2)
#  rbar <- r/n
  rbar <- RhoCircularRad(x)
  circsd <- sqrt(-2*log(rbar))
  return(circsd)
}
