
var <- function(x, ...) UseMethod("var")

var.default <- function(x, y = NULL, na.rm = FALSE, use, ...) stats::var(x=x, y=y, na.rm=na.rm, use=use)

#var.matrix <- function(x, ...) {
#    apply(x, 2, var, ...)
#}

var.data.frame <- function(x, ...) {
    sapply(x, var, ...)
}

#############################################################
#                                                           #
#       Original Splus: Ulric Lund                          #
#       E-mail: ulund@calpoly.edu                           #
#                                                           #
#############################################################

#############################################################
#                                                           #
#   var.circular function                                   #
#   Author: Claudio Agostinelli                             #
#   Email: claudio@unive.it                                 #
#   Date: May, 17, 2011                                     #
#   Copyright (C) 2011 Claudio Agostinelli                  #
#                                                           #
#   Version 0.4                                             #
#############################################################

var.circular <- function (x, na.rm=FALSE, ...)  {
  if (is.matrix(x)) {
    apply(x, 2, var.circular, na.rm=na.rm)
  } else {
    if (na.rm) 
      x <- x[!is.na(x)]
    x <- conversion.circular(x, units="radians", zero=0, rotation="counter")
    attr(x, "class") <- attr(x, "circularp") <-  NULL
    VarCircularRad(x=x)
  }
}

VarCircularRad <- function(x) {
   if (any(is.na(x)))
      return(NA)
   n <- length(x)
   c <- sum(cos(x))
   s <- sum(sin(x))
   r <- sqrt(c^2 + s^2)
   rbar <- r/n
   circvar <- 1-rbar
   return(circvar)
}
