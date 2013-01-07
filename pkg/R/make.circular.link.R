#####################################################################
#                                                                   #
#   trigonometric.polynomials function                              #
#   Author: Claudio Agostinelli and Alessandro Gagliardi            #
#   Email: claudio@unive.it                                         #
#   Date: January, 04, 2013                                         #
#   Copyright (C) 2013 Claudio Agostinelli and Alessandro Gagliardi #
#                                                                   #
#   Version 0.1                                                     #
#####################################################################

make.circular.link <- function (link)
{
    switch(link,
           "atan" = {
               linkfun <- function(eta) 2*atan(eta) 
               linkder <- function(eta) 2/(eta^2 + 1) 
               valideta <- function(eta) TRUE
           },
           "exp" = {
               linkfun <- function(eta) exp(eta)
               linkder <- function(eta) exp(eta)
               valideta <- function(eta) TRUE
           },
           "probit" = { ## see Mardia and Jupp (2000) pag. 258
               linkfun <- function(eta) 2*pi*(pnorm(eta) - 0.5)
               linkder <- function(eta) 2*pi*dnorm(eta)
               valideta <- function(eta) TRUE
           },
           "identity" = {
               linkfun <- function(eta) eta
               linkder <- function(eta) 1
               valideta <- function(eta) TRUE
           },           
           ## else :
           stop(gettextf("%s link not recognised", sQuote(link)),
                domain = NA)
           )# end switch(.)
    structure(list(linkfun = linkfun, linkder = linkder,
              valideta = valideta, name = link), class="link-cm")
}
