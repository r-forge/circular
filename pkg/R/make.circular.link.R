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
