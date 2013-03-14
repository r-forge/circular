
clm.control <- function(epsilon = 1e-8, maxit = 25, trace = FALSE)
{
    if(!is.numeric(epsilon) || epsilon <= 0)
	stop("value of 'epsilon' must be > 0")
    if(!is.numeric(maxit) || maxit <= 0)
	stop("maximum number of iterations must be > 0")
    list(epsilon = epsilon, maxit = maxit, trace = trace)
}

clm <- function(formula, family = vonMises, data, weights,
		subset, na.action, start = NULL,
		etastart, mustart, offset,
		control = list(...), control.circular = list(),
                model = TRUE, method = "clm.fit",
                x = FALSE, y = TRUE,
                contrasts = NULL, ...) {
    call <- match.call()
    ## family
    if(is.character(family))
        family <- get(family, mode = "function", envir = parent.frame())
    if(is.function(family)){ family <- family()}
    if(is.null(family$family)) {
	print(family)
	stop("'family' not recognized")
    }

    ## extract x, y, etc from the model formula and frame
    if(missing(data)) data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
                 "etastart", "mustart", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    if(identical(method, "model.frame")) return(mf)

    if (!is.character(method) && !is.function(method))
        stop("invalid 'method' argument")
    ## for back-compatibility in return result
    if (identical(method, "clm.fit"))
        control <- do.call("clm.control", control)

    mt0 <- mt <- attr(mf, "terms") # allow model.frame to have updated it
	 attr(mt0,"intercept") <- 0
    Y <- model.response(mf, "any") # e.g. factors are allowed
    ## avoid problems with 1D arrays, but keep names
    if(length(dim(Y)) == 1L) {
        nm <- rownames(Y)
        dim(Y) <- NULL
        if(!is.null(nm)) names(Y) <- nm
    }
    ## null model support
    X <- if (is.empty.model(mt) | is.empty.model(mt0))
		matrix(,NROW(Y), 0L)
	 else
		 model.matrix(mt0, mf, contrasts)

    if(any(apply(X,2,function(x) all(x==x[1L]))) )
		 attr(mt, "intercept") <- 0
    ## avoid any problems with 1D or nx1 arrays by as.vector.
    weights <- as.vector(model.weights(mf))
    if(!is.null(weights) && !is.numeric(weights))
        stop("'weights' must be a numeric vector")
    ## check weights and offset
    if( !is.null(weights) && any(weights < 0) )
	stop("negative weights not allowed")

    offset <- as.vector(model.offset(mf))
    if(!is.null(offset)) {
        if(length(offset) != NROW(Y))
            stop(gettextf("number of offsets is %d should equal %d (number of observations)", length(offset), NROW(Y)), domain = NA)
    }
    ## these allow starting values to be expressed in terms of other vars.
    mustart <- model.extract(mf, "mustart")
    etastart <- model.extract(mf, "etastart")

    ## We want to set the name on this call and the one below for the
    ## sake of messages from the fitter function
    fit <- eval(call(if(is.function(method)) "method" else method,
                     x = X, y = Y, weights = weights, start = start,
                     etastart = etastart, mustart = mustart,
                     offset = offset, family = family, control = control,
                     intercept = attr(mt, "intercept") > 0L))

    ## This calculated the null deviance from the intercept-only model
    ## if there is one, otherwise from the offset-only model.
    ## We need to recalculate by a proper fit if there is intercept and
    ## offset.
    ##
    ## The clm.fit calculation could be wrong if the link depends on the
    ## observations, so we allow the null deviance to be forced to be
    ## re-calculated by setting an offset (provided there is an intercept).
    ## Prior to 2.4.0 this was only done for non-zero offsets.
    if(length(offset) && attr(mt, "intercept") > 0L) {
        fit2 <-
            eval(call(if(is.function(method)) "method" else method,
                      x = matrix(,NROW(Y), 0L), y = Y,
                      weights = weights, offset = offset, family = family,
                      control = control, intercept = TRUE))
        ## That fit might not have converged ....
        if(!fit2$converged)
            warning("fitting to calculate the null deviance did not converge -- increase 'maxit'?")
        fit$null.deviance <- fit2$deviance
    }
    if(model) fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    if(x) fit$x <- X
    if(!y) fit$y <- NULL
    fit <- c(fit, list(call = call, formula = formula,
		       terms = mt, data = data,
		       offset = offset, control = control, method = method,
		       contrasts = attr(X, "contrasts"),
                       xlevels = .getXlevels(mt, mf)))
    class(fit) <- c(fit$class, c("clm","glm", "lm"))
    fit
}

clm.fit <- function(x, y, weights = rep(1, nobs), start = NULL, etastart = NULL, mustart = NULL, offset = rep(0, nobs), family = vonMises(), center=TRUE, control = list(), control.circular = list(), intercept = TRUE) {

  if (!is.logical(intercept))
    stop("'intercept' must be logical")
  control <- do.call("clm.control", control)
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
    dev <- sum(family$dev.resids(y, muhat, mulinear, khat, weights))
	 S0 <- sum(weights * sin(y))/sum(weights)
    C0 <- sum(weights * cos(y))/sum(weights)
    R0 <- sqrt((S0^2+C0^2))
    k0 <- A1inv(R0)
	muhat0 <- 0
    if (intercept)
      muhat0 <- atan2(S0/R0,C0/R0)
	 null.dev <- sum(family$dev.resids(y, muhat0, 0, k0, weights))
	df.residual <- nobs - as.numeric(intercept)
    result <- list(coefficients = numeric(),
                   mu = muhat,
                   kappa = khat,
                   fitted.values = rep(muhat, nobs),
                   residuals = y - muhat - mulinear,
                   iter = 0L,
						 family = family,
						 aic = family$aic(dev=dev,rank=df.residual),
						 deviance = dev,
						 null.deviance = null.dev,
                   y = y,
						 R = R,
						 rank = 0,
						 df.residual = df.residual,
						 df.null = nobs - as.numeric(intercept),
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
    result <- ClmLocationCircularRad(x=x,y=y,weights=weights,offset=offset,beta=start,eta,mulinear,family=family,epsilon=control$epsilon,maxit=control$maxit,trace=control$trace,intercept=intercept)
  }
    
  class(result) <- "clm"
  result$fitted.values <- conversion.circular(circular(result$fitted.values), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)
  result$mu <- conversion.circular(circular(result$mu), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)
  result$residuals <- conversion.circular(circular(result$residuals), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)
  result$y <- conversion.circular(circular(result$y), dc$units, dc$type, dc$template, dc$modulo, dc$zero, dc$rotation)
  attr(result$mu,"class") <- attr(result$residuals,"class") <- "circular"  
  return(result)
}

ClmLocationCircularRad <- function(x, y, weights, offset, beta, eta, mulinear, family, epsilon, maxit, trace, intercept=TRUE) {
      
logLikelihood <- function(khat,muhat,mulinear,weights,y)
{
    if (khat < 100000)
      llik <- -sum(weights)*(log(2*pi)+log(besselI(khat,nu=0,expon.scaled=TRUE))+khat) + sum(weights * khat * cos(y-muhat-mulinear))
    else
      llik <- ifelse((y-muhat-mulinear)==0, Inf, -Inf)
    return(llik)
  }

  linkinv <- family$linkinv
  mu.eta <- family$mu.eta
  if (is.null(beta))
  {
  		res <- mle.vonmises(circular(y))
  		if(!is.finite(res$kappa))
  			res$kappa <- 1e5-1L
  		betainit1 <- c(rep(0,NCOL(x)), res$mu, res$kappa)
		opt1 <- optim(par=betainit1, fn=function(x,y,z,wt)
		{
			nc <- NCOL(z)
			mulinear <- linkinv(drop(z%*%(x[1L:nc])))
			muhat <- x[nc + 1]
			khat <- x[nc + 2]
			llik <- -logLikelihood(khat = khat, muhat = muhat, mulinear=mulinear, weights=wt, y=y)
			return(llik)
		}
		, y=y, z=x, wt=weights,
						lower=c(rep(-Inf,NCOL(x)), -pi, 0.1), upper=c(rep(Inf,NCOL(x)), pi, Inf), method="L-BFGS-B")

		betainit2 <- c(rep(0,NCOL(x)),(res$mu+pi)%%(2*pi), res$kappa)
		opt2 <- optim(par=betainit2, fn=function(x,y,z,res,wt)
		{
			nc <- NCOL(z)
			mulinear <- linkinv(drop(z%*%(x[1L:nc])))
			muhat <- x[nc + 1]
			khat <- x[nc + 2]
			llik <- -logLikelihood(khat = khat, muhat = muhat, mulinear=mulinear, weights=wt, y=y)
			return(llik)
		}
		, y=y, z=x, res=res, wt=weights,
						lower=c(rep(-Inf,NCOL(x)), -pi, 0.1), upper=c(rep(Inf,NCOL(x)), pi, Inf), method="L-BFGS-B")
		
		if(opt1$value < opt2$value)
			beta <- drop(opt1$par[1L:NCOL(x)])
		else
			beta <- drop(opt2$par[1L:NCOL(x)])
  }
  lastbeta <- beta + 1 + epsilon
  nobs <- length(y)
  conv <- FALSE
  for (iter in 1L:maxit) {   
#### Get muhat and khat
    eta <- drop(x%*%beta + offset)
    mulinear <- linkinv(eta)
    S <- sum(weights * sin(y - mulinear))/sum(weights)
    C <- sum(weights * cos(y - mulinear))/sum(weights)
    R <- sqrt((S^2+C^2))
    if (intercept) {
      muhat <- atan2(S/R,C/R)
    } else{
      muhat <- 0}

    khat <- A1inv(R)

#### Get g values
    g <- mu.eta(eta)
#### Get u values
    uvector <- sin(y-muhat-mulinear)
#### Update Step    
    lastbeta <- beta
    ustar <- uvector/(R*g)  ### R=A1(khat)
    fit <- lm.wfit(y=ustar,x=x,w=weights*(g^2))
	 if(khat>=1e5)
	 {
		khat<- 1e5-1
		conv <- TRUE
		break
	 }    
    beta <- fit$coefficients + beta
	 if(any(is.na(beta)))
	 {
			conv <- FALSE
			break
	 }
	 xgu <- t(x)%*%diag(weights)%*%diag(g)%*%uvector
	 if(max(abs(xgu)) < 1e-7 & max(abs(beta-lastbeta)) > epsilon)
	 {
	 		beta <- (beta + lastbeta)/2
	 }
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

  df.residual <- nobs - fit$rank - as.numeric(intercept)
  df.null <- df.residual + fit$rank
	aic <- NA
  if (!conv)
    warning("clm.fit: algorithm did not converge", call. = FALSE)
  if (khat==1e5-1)
    warning("clm.fit: Max 'kappa' value was reached ", call. = FALSE)

  fitted.values <- muhat + linkinv(drop(x%*%beta+offset))
  residuals <- y - fitted.values

	deviance <- sum(family$dev.resids(y=y, mu=muhat, mulinear=mulinear, kappa=khat, wt=weights))
	aic <- family$aic(dev=deviance,rank=df.residual)
	if(is.infinite(aic))
		aic <- -Inf

  S0 <- sum(weights * sin(y))/sum(weights)
  C0 <- sum(weights * cos(y))/sum(weights)
  R0 <- sqrt((S0^2+C0^2))
  k0 <- A1inv(R0)
  mu0 <- 0
  if (intercept) {
      mu0 <- atan2(S0/R0,C0/R0)
	}
  null.deviance <- sum(family$dev.resids(y=y, mu=mu0, mulinear=0, kappa=k0, wt=weights))
  result <- list(coefficients=drop(beta), residuals=residuals, fitted.values=fitted.values, mu=muhat, kappa=khat, R=R, effects = fit$effects, rank = fit$rank, qr = fit$qr, family = family, linear.predictors = eta, deviance = deviance, aic = aic, null.deviance = null.deviance, iter = iter, weights = weights*g^2, prior.weights = weights, df.residual = df.residual, df.null = df.null, y = y, converged = conv)
  return(result)
}


print.clm <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cat("\nCall:  ",
	paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
    if(length(coef(x))) {
        cat("Coefficients")
        if(is.character(co <- x$contrasts))
            cat("  [contrasts: ",
                apply(cbind(names(co),co), 1L, paste, collapse = "="), "]")
        cat(":\n")
        print.default(format(x$coefficients, digits = digits),
                      print.gap = 2, quote = FALSE)
    } else cat("No coefficients\n\n")
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ",
        x$df.residual, "Residual\n")
    if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
    cat("Null Deviance:	   ",	format(signif(x$null.deviance, digits)),
	"\nResidual Deviance:", format(signif(x$deviance, digits)),
	"\tAIC:", format(signif(x$aic, digits)), "\n")
    invisible(x)
}

summary.clm <- function(object, correlation = FALSE, symbolic.cor = FALSE, ...)
{
    df.r <- object$df.residual
    dispersion <- 1/(object$kappa*object$R)
    ## calculate scaled and unscaled covariance matrix
	 intercept <- attr(object$terms, "intercept")

    p <- object$rank
    aliased <- is.na(coef(object)) # used in print method
	 if(length(aliased) == 0L && intercept)
		aliased <- FALSE
    if (p > 0) {
        p1 <- 1L:p
			qr.clm <- function(x, ...) {
					if(is.null(x$qr))
					  stop("clm object does not have a proper 'qr' component.\nRank zero or should not have used clm(.., qr=FALSE).")
					x$qr
			}
	     Qr <- qr.clm(object)
        ## WATCHIT! doesn't this rely on pivoting not permuting 1L:p? -- that's quaranteed
        coef.p <- object$coefficients[Qr$pivot[p1]]
        covmat.unscaled <- chol2inv(Qr$qr[p1,p1,drop=FALSE])
        dimnames(covmat.unscaled) <- list(names(coef.p),names(coef.p))
        covmat <- dispersion*covmat.unscaled
        var.cf <- diag(covmat)
        ## calculate coef table

        s.err <- sqrt(var.cf)
        tvalue <- coef.p/s.err
		  if(intercept){
		    s.mu.err <- sqrt(dispersion/(object$df.residual+2))
			 if(attr(object$mu, "circularp")$units=="degrees")
				s.mu.err <- s.mu.err * 180/pi
        mu.tvalue <- object$mu/s.mu.err
 		  }
        dn <- c("Estimate", "Std. Error")
	     rn <- names(coef.p)
        if(df.r > 0) {
            pvalue <- 2*pt(-abs(tvalue), df.r)
            coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
				if(intercept){
					mu.pvalue <- 2 * pt(-abs(mu.tvalue), df.r)				
					mu.table <- c(as.numeric(object$mu), s.mu.err, mu.tvalue, mu.pvalue)
					coef.table <- rbind(coef.table, mu.table)

					rn <- c(rn,"Intercept")
				}		
            dimnames(coef.table) <- list(rn,
                                         c(dn, "t value","Pr(>|t|)"))
				
        } else { # df.r == 0
				if(intercept){
					coef.p <- c(coef.p,object$mu)
					rn <- c(rn, "Intercept")				
				}
            coef.table <- cbind(coef.p, NaN, NaN, NaN)
            dimnames(coef.table) <- list(rn,
                                         c(dn, "t value","Pr(>|t|)"))
        }
        df.f <- NCOL(Qr$qr)
    } else {

        coef.table <- matrix(,as.numeric(intercept), 4L)	

		  rn <- NULL
        covmat.unscaled <- covmat <- matrix(, 0L, 0L)
		  if(intercept)
		  {
			
		    s.mu.err <- sqrt(dispersion/(object$df.residual+2))
			 if(attr(object$mu, "circularp")$units=="degrees")
				s.mu.err <- s.mu.err * 180/pi
			  mu.tvalue <- object$mu/s.mu.err
	 		  mu.pvalue <- 2 * pt(-abs(mu.tvalue), df.r)
			  coef.table[1,] <- c(as.numeric(object$mu),s.mu.err,mu.tvalue,mu.pvalue)
				rn <- "Intercept";
		  }
        dimnames(coef.table) <-
            list(rn, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))

        df.f <- 0
    }
    ## return answer

    ## these need not all exist, e.g. na.action.
    keep <- match(c("call","terms","family","deviance", "aic",
		      "contrasts", "df.residual","null.deviance","df.null",
                      "iter", "na.action"), names(object), 0L)
    
    ans <- c(object[keep],
	     list(deviance.resid = residuals(object["residuals"], type = "deviance"),
		  coefficients = coef.table,
                  aliased = aliased,
		  dispersion = dispersion,
			kappa=object$kappa,
		  df = c(object$rank, df.r, df.f),
		  cov.unscaled = covmat.unscaled,
		  cov.scaled = covmat))

    if(correlation && p > 0) {
	dd <- sqrt(diag(covmat.unscaled))
	ans$correlation <-
	    covmat.unscaled/outer(dd,dd)
	ans$symbolic.cor <- symbolic.cor
    }
    class(ans) <- "summary.clm"
    return(ans)
}

print.summary.clm <-
    function (x, digits = max(3L, getOption("digits") - 3L),
	      symbolic.cor = x$symbolic.cor,
	      signif.stars = getOption("show.signif.stars"), ...)
{
    cat("\nCall:\n",
	paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
    cat("Residuals: \n")
    if(x$df.residual > 5) {
      summre <- as.numeric(minusPiPlusPi(quantile(x$deviance.resid,na.rm = TRUE)))
      x$deviance.resid <- setNames(summre, c("Min", "1Q", "Median", "3Q", "Max"))
    }
    xx <- zapsmall(x$deviance.resid, digits + 1L)
    print.default(xx, digits = digits, na.print = "", print.gap = 2L)

    if(length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    } else {
        ## partial matching problem here.
        df <- if ("df" %in% names(x)) x[["df"]] else NULL
        if ((!is.null(df) && (nsingular <- df[3L] - df[1L])))
            cat("\nCoefficients: (", nsingular,
                " not defined because of singularities)\n", sep = "")		else
        cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if(!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4L,
                            dimnames=list(cn, colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
                     na.print = "NA", ...)
    }
    ##
    cat("\nConcentration parameter for ", x$family$family,
	" family is ", format(x$kappa), "\n\n",
	apply(cbind(paste(format(c("Null","Residual"), justify="right"),
                          "deviance:"),
		    format(unlist(x[c("null.deviance","deviance")]),
			   digits = max(5L, digits + 1L)), " on",
		    format(unlist(x[c("df.null","df.residual")])),
		    " degrees of freedom\n"),
	      1L, paste, collapse = " "), sep = "")
    if(nzchar(mess <- naprint(x$na.action))) cat("  (", mess, ")\n", sep = "")
    cat("AIC: ", format(x$aic, digits = max(4L, digits + 1L)),"\n\n",
	"Number of Fisher Scoring iterations: ", x$iter,
	"\n", sep = "")

    correl <- x$correlation
    if(!is.null(correl)) {

	p <- NCOL(correl)
	if(p > 1) {
	    cat("\nCorrelation of Coefficients:\n")
	    if(is.logical(symbolic.cor) && symbolic.cor) {# NULL < 1.7.0 objects
		print(symnum(correl, abbr.colnames = NULL))
	    } else {
		correl <- format(round(correl, 2L), nsmall = 2L,
                                 digits = digits)
		correl[!lower.tri(correl)] <- ""
		print(correl[-1, -p, drop=FALSE], quote = FALSE)
	    }
	}
    }
    cat("\n")
    invisible(x)
}

model.frame.clm <- function (formula, ...) {
    dots <- list(...)
    nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0L)]
    if (length(nargs) || is.null(formula$model)) {
	fcall <- formula$call
	fcall$method <- "model.frame"
	fcall[[1L]] <- as.name("clm")
        fcall[names(nargs)] <- nargs
#	env <- environment(fcall$formula)  # always NULL
        env <- environment(formula$terms)
	if (is.null(env)) env <- parent.frame()
	eval(fcall, env)
    }
    else formula$model
}

weights.clm <- function(object, type = c("prior", "working"), ...)
{
    type <- match.arg(type)
    res <- if(type == "prior") object$prior.weights else object$weights
    if(is.null(object$na.action)) res
    else naresid(object$na.action, res)
}

formula.clm <- function(x, ...)
{
    form <- x$formula
    if( !is.null(form) ) {
        form <- formula(x$terms) # has . expanded
        environment(form) <- environment(x$formula)
        form
    } else formula(x$terms)
}

