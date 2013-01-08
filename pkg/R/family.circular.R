vonMises <- function (link = "tan") {
  linktemp <- substitute(link)
  if (!is.character(linktemp)) linktemp <- deparse(linktemp)
  okLinks <- c("tan", "log", "probit", "identity")
  if (linktemp %in% okLinks)
    stats <- make.circular.link(linktemp)
  else if (is.character(link)) {
    stats <- make.circular.link(link)
    linktemp <- link
  } else {
  ## what else shall we allow?  At least objects of class link-glm.
    if (inherits(link, "link-glm")) {
      stats <- link
      if (!is.null(stats$name))
        linktemp <- stats$name
    } else {
      stop(gettextf('link "%s" not available for gaussian family; available links are %s',
	linktemp, paste(sQuote(okLinks), collapse =", ")),
	domain = NA)
    }
  }
  structure(list(family = "vonMises",
    link = linktemp,
    linkfun = stats$linkfun,
    linkinv = stats$linkinv,
    variance = function(mu) rep.int(1, length(mu)),
    dev.resids = function(y, mu, wt) NA,
#####    dev.resids = function(y, mu, wt) wt * ((y - mu)^2),
    aic = function(y, n, mu, wt, dev) NA,                
    ## aic = function(y, n, mu, wt, dev) {
    ##   nobs <- length(y)
    ##   nobs*(log(dev/nobs*2*pi)+1)+2 - sum(log(wt))
    ## },
    mu.eta = stats$mu.eta,
    initialize = expression({
      n <- rep.int(1, nobs)
      if(is.null(etastart) && is.null(start) &&
        is.null(mustart) && (family$link == "log" && any(y <= 0)))
        stop("cannot find valid starting values: please specify some")
        mustart <- y - MeanCircularRad(y)}),
    validmu = function(mu) TRUE,
    valideta = stats$valideta
  ),
  class = "family")
}
