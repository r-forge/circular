#############################################################
#                                                           #
#   dkatojones function                                     #
#   Author: Federico Rotolo                                 #
#   Email: federico.rotolo@stat.unipd.it                    #
#   Date: October, 05, 2010                                 #
#   Copyright (C) 2010 Federico Rotolo                      #
#                                                           #
#   Version                                                 #
#############################################################


dkatojones <-function(x, mu, nu, r, kappa){
  if (length(mu)!=1|| length(nu)!=1|| length(r)!=1|| length(kappa)!=1)
    stop("all the parameters must have length 1")

  if((r<0)||(r>=1)){stop("'r' must be in [0,1)")}
  if(kappa<0){stop("'kappa' must be not negative")}

  x <- conversion.circular(x, units="radians", zero=0, rotation="counter")
  mu <- conversion.circular(mu, units="radians", zero=0, rotation="counter")
  nu <- conversion.circular(nu, units="radians", zero=0, rotation="counter")

  mu <- as.vector(mu)
  nu <- as.vector(nu)
  kappa <- as.vector(kappa)
  r <- as.vector(r)
  attr(x, "class") <- attr(x, "circularp") <-  NULL
  attr(mu, "class") <- attr(mu, "circularp") <-  NULL    
  attr(nu, "class") <- attr(nu, "circularp") <-  NULL    
  
  DkatojonesRad(x, mu, nu, r, kappa)
}


DkatojonesRad <-function(x, mu, nu, r, kappa){
	gamma<-mu+nu
	den <- 2*pi*besselI(kappa,0) * (1+r^2-2*r*cos(x-gamma))
	xi<-(r^4+2*r^2*cos(2*nu)+1)^.5
	eta<-mu+Arg(r^2*cos(2*nu)+r^2*sin(2*nu)*1i+1)
	num<-(1-r^2)*exp((kappa*(xi*cos(x-eta)-2*r*cos(nu)))/(1+r^2-2*r*cos(x-gamma)))
	return(num/den)
}


