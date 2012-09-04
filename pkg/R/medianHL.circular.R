medianHL.circular <- function(x, method=c("HL1","HL2","HL3"), prop=NULL, na.rm=FALSE) {
   method <- match.arg(method)
   if (!is.null(prop))
     if (prop <= 0 | prop >=1)
       stop("'prop' is outside (0,1)")
   if (na.rm)
      x <- x[!is.na(x)]
   if (length(x)==0) {
      warning("No observations (at least after removing missing values)")
      return(NULL)
   }
   if (is.circular(x)) {
      dc <- circularp(x)
   } else {
      dc <- list(type="angles", units="radians", template="none", modulo="asis", zero=0, rotation="counter")
   }
   x <- conversion.circular(x, units="radians", zero=0, rotation="counter")
   attr(x, "class") <- attr(x, "circularp") <-  NULL
   circmedian <- MedianHLCircularRad(x, method, prop)
   circmedian <- conversion.circular(circular(drop(circmedian)), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)
   attr(circmedian, "medians") <- conversion.circular(circular(drop(MinusPiPlusPiRad(attr(circmedian, "medians")))), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)  
   attr(attr(circmedian, "medians"), "class") <- attr(attr(circmedian, "medians"), "circularp") <-  NULL
   return(circmedian)
}

MedianHLCircularRad <- function(x, method, prop) {
  x <- x%%(2*pi)
  x <- MinusPiPlusPiRad(x)
  n <- length(x)
  mediancirc = NA
  if (is.null(prop)) {
    if (method=='HL3') {
      datatemp <- expand.grid(x,x)
      datatemp <- apply(X=datatemp,FUN=MeanCircularRad,MARGIN=1)
      mediancirc <- MedianCircularRad(datatemp[!is.na(datatemp)])
    } else {
      whichMethod <- ifelse(method=='HL1',1,0)      
      datatemp <- rep(NA, length.out=(n*(n+1-whichMethod*2)/2))
 ## HL1 with n*(n-1)/2 and HL2 with n*(n+1)/2 obs.
      k <- 0
      for (i in 1:(n-whichMethod)) {
        for (j in (i+whichMethod):n) {
          k <- k + 1
          datatemp[k] <- MeanCircularRad(c(x[i],x[j]))
        }
      }
      datatemp <- datatemp[!is.na(datatemp)]
      mediancirc <- ifelse(length(datatemp)==0,NA,MedianCircularRad(datatemp))
    }
  } else {
    ## method subsampling
    nrep <- switch(method,
              HL3=n^2,
              HL1=n*(n-1)/2,
              HL2=n*(n+1)/2
            )
    nrep <- max(1,round(nrep*prop))
    datatemp <- rep(NA, length.out=nrep)
    for (i in 1:nrep) {
      pos <- sample(1:n, size=2, replace=ifelse(method==1, FALSE, TRUE))
      datatemp[i] <- MeanCircularRad(c(x[pos[1]],x[pos[2]]))
    }
    datatemp <- datatemp[!is.na(datatemp)]
    mediancirc <- ifelse(length(datatemp)==0,NA,MedianCircularRad(datatemp))
  }
  return(mediancirc)
}

