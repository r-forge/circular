#############################################################
#                                                           
#   MinusPiPlusPiRad function                                  
#   Author: Claudio Agostinelli                             
#   E-mail: claudio@unive.it                                
#   Date: October, 14, 2007                                  
#   Version: 0.1                                          
#                                                           
#   Copyright (C) 2007 Claudio Agostinelli                  
#                                                           
#############################################################

MinusPiPlusPiRad  <- function(x)
{
	x <- .C("MinusPiPlusPiRad",x=as.double(x),n=as.integer(length(x)))$x
 	return(x) 
} 

