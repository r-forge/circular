cm.control <- function(epsilon = 1e-8, maxit = 25, trace = FALSE) {
    if(!is.numeric(epsilon) || epsilon <= 0)
	stop("value of 'epsilon' must be > 0")
    if(!is.numeric(maxit) || maxit <= 0)
	stop("maximum number of iterations must be > 0")
    list(epsilon = epsilon, maxit = maxit, trace = trace)
}

cm.fit <- function(x, y, weights = rep(1, nobs), start = NULL, etastart = NULL, mustart = NULL, offset = rep(0, nobs), family = vonMises(), center=TRUE, control = list(), control.circular = list(), intercept = TRUE) {

  if (!is.logical(intercept))
    stop("'intercept' must be logical")
  control <- do.call("cm.control", control)
### Dependent variables
  if (is.circular(y)) {
    datacircularp <- circularp(y)
  } else {
    datacircularp <- list(type = "angles", units = "radians", template = "none",
                          modulo = "asis", zero = 0, rotation = "counter")
  }
  dc <- control.circular
  if (is.null(dc$type)) 
    dc$type <- datacircularp$type
  if (is.null(dc$units)) 
    dc$units <- datacircularp$units
  if (is.null(dc$template)) 
    dc$template <- datacircularp$template
  if (is.null(dc$modulo)) 
    dc$modulo <- datacircularp$modulo
  if (is.null(dc$zero)) 
    dc$zero <- datacircularp$zero
  if (is.null(dc$rotation)) 
    dc$rotation <- datacircularp$rotation
  
  y <- conversion.circular(y, units = "radians", zero = 0, rotation = "counter", modulo = "2pi")
  attr(y, "circularp") <- attr(y, "class") <- NULL
## Explanatory variables  
  x <- as.matrix(x)
  ## Il controllo sui casi completi viene spostato nella funzione superiore!
  ## ok <- complete.cases(x, y)
  ## if (NCOL(x) == 1) {
  ##   x <- cbind(x[ok])
  ## } else {
  ##   x <- x[ok,]
  ## }   
  ## y <- y[ok]
  ## if ((n <- length(y)) == 0) {
  ##   warning("No observations (at least after removing missing values)")
  ##   return(NULL)
  ## }  
  x <- scale(x,center=center,scale=FALSE)  
  xnames <- dimnames(x)[[2L]]
  ynames <- names(y)
  conv <- FALSE
  nobs <- NROW(y)
  nvars <- ncol(x)
  EMPTY <- nvars == 0
## define weights and offset if needed
  if (is.null(weights))
    weights <- rep.int(1, nobs)
  if (is.null(offset))
    offset <- rep.int(0, nobs)
## get family functions:
  variance <- family$variance
  linkinv  <- family$linkinv
  if (!is.function(variance) || !is.function(linkinv) )
    stop("'family' argument seems not to be a valid family object", call. = FALSE)
  dev.resids <- family$dev.resids
  aic <- family$aic
  mu.eta <- family$mu.eta
  unless.null <- function(x, if.null) if(is.null(x)) if.null else x
  valideta <- unless.null(family$valideta, function(eta) TRUE)
  validmu  <- unless.null(family$validmu,  function(mu) TRUE)
  if (is.null(mustart)) {
## calculates mustart
    eval(family$initialize)
  } else {
    mukeep <- mustart
    eval(family$initialize)
    mustart <- mukeep
  }

  if (EMPTY) {
    eta <- rep.int(0, nobs) + offset
    if (!valideta(eta))
      stop("invalid linear predictor values in empty model", call. = FALSE)
    mulinear <- linkinv(eta)
    ## calculate initial deviance and coefficient
    if (!validmu(mulinear))
      stop("invalid fitted means in empty model", call. = FALSE)
    S <- sum(weights * sin(y-mulinear)) / sum(weights)
    C <- sum(weights * cos(y-mulinear)) / sum(weights)
    R <- sqrt((S^2+C^2))
    khat <- A1inv(R)
    if (intercept)
      muhat <- atan2(S/R,C/R)
    else
      muhat <- 0
    dev <- sum(dev.resids(y, muhat, mulinear, weights))
    result <- list(coefficients = numeric(),
                   mu = muhat,
                   kappa = khat,
                   fitted.values = rep(muhat, nobs),
                   residuals = y - muhat - mulinear,
                   iter = 0L,
                   y = y,
                   converged = TRUE)
  } else {
    eta <- if (!is.null(etastart)) etastart
              else if (!is.null(start))
                if (length(start) != nvars)
                  stop(gettextf("length of 'start' should equal %d and correspond to initial coefs for %s", nvars, paste(deparse(xnames), collapse=", ")),
                         domain = NA)
                else {
                    offset + drop(x %*% start)
                }
            else family$linkfun(mustart)
    mulinear <- linkinv(eta)
    if (!(validmu(mulinear) && valideta(eta)))
      stop("cannot find valid starting values: please specify some", call. = FALSE)
    result <- CmLocationCircularRad(x=x,y=y,weights=weights,offset=offset,beta=start,eta,mulinear,family=family,epsilon=control$epsilon,maxit=control$maxit,trace=control$trace,intercept=intercept)
  }
    
  class(result) <- "cm"
  result$fitted.values <- conversion.circular(circular(result$fitted.values), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)
  result$mu <- conversion.circular(circular(result$mu), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)
  result$residuals <- conversion.circular(circular(result$residuals), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)
  result$y <- conversion.circular(circular(result$y), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)
  attr(result$mu,"class") <- attr(result$residuals,"class") <- "circular"  
  return(result)
}

CmLocationCircularRad <- function(x, y, weights, offset, beta, eta, mulinear, family, epsilon, maxit, trace, intercept=TRUE) {
      
  logLikelihood <- function(khat,muhat,mulinear,weights,y)
    -sum(weights)*log(besselI(khat,nu=0)) + khat * sum(weights * cos(y-muhat-mulinear))
   
  linkinv <- family$linkinv
  mu.eta <- family$mu.eta
  if (is.null(beta))
    beta <- rep(0, NCOL(x))
  lastbeta <- beta + 1 + epsilon
  nobs <- length(y)
  conv <- FALSE
  for (iter in 1L:maxit) {   
#### Get muhat and khat
    eta <- drop(x%*%beta + offset)
    mulinear <- linkinv(eta)
    if (intercept) {
      S <- sum(weights * sin(y - mulinear))/sum(weights)
      C <- sum(weights * cos(y - mulinear))/sum(weights)
      R <- sqrt((S^2+C^2))
      muhat <- atan2(S/R,C/R)
    } else
      muhat <- 0
    khat <- A1inv(R)
#### Get g values
    g <- mu.eta(eta)
#### Get u values
    uvector <- sin(y-muhat-mulinear)
#### Update Step    
    lastbeta <- beta
    ustar <- uvector/(A1(khat)*g)
    fit <- lm.wfit(y=ustar,x=x,w=weights*(g^2))
    beta <- fit$coefficients + beta
    if (trace) {
      cat("At iteration ",iter," :","\n")
      cat("log likelihood ",logLikelihood(khat=khat,muhat=muhat,mulinear=mulinear,weights=weights,y=y),"\n")
      cat("coefficients =", beta, "mu =", muhat, " kappa =", khat,"\n")
      cat("Value of equation=",t(x)%*%diag(weights)%*%diag(g)%*%uvector,"\n\n")
    }

    if (max(abs(beta-lastbeta)) < epsilon) {
      conv <- TRUE
      break
    } else {
      conv <- FALSE
    }
  }

  ### Per ora usiamo la somma dei pesi normalizzati!
  ### df.residual <- nobs - fit.rank - as.numeric(intercept)
  df.residual <- sum(weights)/max(weights) - fit.rank - as.numeric(intercept)
  
  if (!conv)
    warning("cm.fit: algorithm did not converge", call. = FALSE)
  fitted.values <- muhat + linkinv(drop(x%*%beta+offset))
  residuals <- y - fitted.values
  result <- list(coefficients=drop(beta), residuals=residuals, fitted.values=fitted.values, mu=muhat, kappa=khat, effects = fit$effects, rank = fit$rank, qr = structure(fit[c("qr", "rank", "qraux", "pivot", "tol")], class = "qr"), family = family, linear.predictors = eta, deviance = NA, aic = NA, null.deviance = NA, iter = iter, weights = weights*g^2, prior.weights = weights, df.residual = df.residual, df.null = NA, y = y, converged = conv)
  return(result)
}
