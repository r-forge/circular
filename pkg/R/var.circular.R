
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
#   Date: May, 04, 2011                                     #
#   Copyright (C) 2011 Claudio Agostinelli                  #
#                                                           #
#   Version 0.3                                             #
#############################################################

var.circular <- function (x, na.rm=FALSE, only.var=TRUE, type=c('circular', 'circular2', 'angular'), ...)  {
  type <- match.arg(type)  
  if (is.matrix(x)) {
    apply(x, 2, var.circular, na.rm=na.rm, only.var=only.var, type=type)
  } else {
    if (na.rm) 
      x <- x[!is.na(x)]
    x <- conversion.circular(x, units="radians", zero=0, rotation="counter")
    attr(x, "class") <- attr(x, "circularp") <-  NULL
    VarCircularRad(x=x, only.var=only.var, type=type)
  }
}

VarCircularRad <- function(x, only.var=TRUE, type='circular') {
   if (any(is.na(x)))
      return(NA)
   n <- length(x)
   c <- sum(cos(x))
   s <- sum(sin(x))
   r <- sqrt(c^2 + s^2)
   rbar <- r/n
   circvar <- switch(type,
                      circular = -2*log(rbar),
                      circular2 = 1-rbar,
                      angular = 2*(1-rbar)
                    )
   if (only.var) {
       return(circvar)
   } else {
       res <- list(n=n, R=r, rho=rbar, var=circvar, type=type)
       class(res) <- 'var.circular'
       return(res)
   }
}

#############################################################
#                                                           #
#   print.var.circular function                             #
#   Author: Claudio Agostinelli                             #
#   E-mail: claudio@unive.it                                #
#   Date: May, 04, 2011                                     #
#   Version: 0.1                                            #
#                                                           #
#   Copyright (C) 2011 Claudio Agostinelli                  #
#                                                           #
#############################################################

print.var.circular <- function(x, digits=4, ...) {
  cat("\n", "      Circular Variance", "\n", "\n")
  cat("Type       :", x$type, "\n")
  cat("Variance   :", round(x$var, digits=digits), "\n")
  cat("Mean Resultant Lenght:", round(x$rho, digits=digits), "\n")  
  cat("Resultant Lenght:", round(x$R, digits=digits), "\n")  
  cat("Sample size:", x$n, "\n")
  invisible(x)
}
