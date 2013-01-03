
#' Estimate parameters by direct optimisation
#'
#' @section Details:
#' Distribution parameters are estimated by minimizing the sum of squared differences between
#' observed quantiles (the values of \eqn{\boldsymbol{x}}) and predicted quantiles (values of \code{qfun} 
#' at observed qq-plot positions) as a function of the distribution's parameters. Internally, this function 
#' uses \code{R}'s \code{optim} function.
#'
#' @param x numeric vector
#' @param qfun quantile function of the distribution for which parameters must be estimated. It must accept a vector of
#'    values between 0 and 1, and one or more named parameters as input. An example is \code{R}'s built-in \code{qnorm} function.
#' @param Flim Determines which observations to use. By default all observations are used (between the 10th and 90th percentile).
#' @param ... initial values of the parameters
#' @param optimpar A list of named parameters for R's \code{optim} function.
#'
#' @value The value is equal to the output of \code{optim}
#'
#' @export
#' @example ../examples/outliers.R
#' 
parameters <- function(x, qfun, Flim=c(0.1,0.9), ..., optimpar=list()){
   stopifnot(
      is.numeric(x),
      all_finite(x),
      Flim[1] < Flim[2]
   )
   N <- length(x)
   Fhat <- plotpositions(x)
   i <- Fhat >= Flim[1] & Fhat <= Flim[2]
   if ( sum(i) < length(list(...)) ){
      stop('Not enough observations to determine parameters (should you increase Flim?)')
   }

   x <- x[i]
   Fhat <- list(Fhat[i])
   minfun <- function(u){
      v <- x - do.call(qfun, c(Fhat, as.list(u)) )
      mean(v*v)
   }
   L <- do.call(optim, c(list(par=unlist(list(...)), fn=minfun), optimpar))

   L <- c(list(qfun=qfun, N=N, Flim=Flim, range=range(x), optimpar=optimpar), L)
   structure(L, class='parameters')
}

#' Obtain residuals from parameter fit
#'
#' @param object an object of class parameters, as returned by the \code{\link{parameters}} function.
#' @param x vector of observations
#' @param all Indicate wheter to return all residuals (including extrapolations) or just the residuals of observations used in the fit.
#' @param ... parameters to be passed to or from other methods (none at this time)
#' @export
residuals.parameters <- function(object, x, all=TRUE, ...){
   Fhat <- plotpositions(x)
   r <- x - do.call(object$qfun, c(list(Fhat), as.list(object$par)))

   if ( all){ 
      r
   } else {
      r[Fhat>=object$Flim[1] & Fhat <= object$Flim[2]]
   }
}



#' Compute outlier limits based on fitted parameters
#' 
#' @param parameters An object of class \code{parameters}, as returned by \code{\link{parameters}}
#' @param x The original vector with observations (only necessary when \code{method='residuals'})
#' @param ... Arguments to be passed to methods for limit calculation. Specifically
#'  \tabular{llll}{
#'  Method  \tab parameter  \tab default \tab description \cr
#'  direct  \tab \code{rho} \tab c(0.5,0.5) \tab  limits beyond which less than \code{rho} observations are expected\cr
#'  residual \tab \code{alpha} \tab c(0.05,0.05) \tab interval on fit residuals
#' }
#' @export
limits <- function(parameters, x=NULL, ...){
   UseMethod('limits')
}

# poor-man's multiple dispatch.
limits.parameters <- function(parameters, x, method=c('direct','residuals'), ...){
   method <- match.arg(method)
   if (method=='residuals' && is.null(x)){ 
      stop('For residual method the vector of observations must be included')
   }
   switch(method,
      direct      = direct_limits(parameters, ...),
      residuals   = residual_limits(parameters, x, ...),
      stop('unrecognized method')
   )  
}

direct_limits <- function(v, rho=c(0.5,0.5)){
   p <- rho/v$N
   
   c(
      Lmin = do.call(v$qfun,c(list(p[1]),as.list(v$par))), 
      Lplus= do.call(v$qfun,c(list(1-p[2]),as.list(v$par)))
   )
}

residual_limits <- function(v, x, alpha=c(0.05,0.05)){
   sigma <- sd(residuals(v, x, all=FALSE))
   sq2 <- sqrt(2)
   sq2*sigma*c(Lmin=invErf(2*alpha[1]-1), Lplus=invErf(1-2*alpha[2]) )
}

#' Determine which values are outliers
#'
#' @param parameters An object of class \code{parameters} as returned by 
#'    the \code{\link{parameters}} function.
#' @param x Vector of observations
#' @param method which method to use to determine the \code{\link{limits}}
#' @param ... Arguments passed to \code{limits}
#' @export
is.outlier <- function(parameters, x, method=c('direct','residuals'), ...){
   storage.mode(x) <- 'double'
   method <- match.arg(method)
   i <- order(x)
   j <- order(i)
   if ( method == 'direct' ){
      L <- limits(parameters, method='direct', ...)
      .Call('isoutlier_direct', x[i], L, parameters$range)[j]
   } else if (method == 'residuals' ){
      L <- limits(parameters, x, method='residuals', ...)
      res <- residuals(parameters,x)
      .Call('isoutlier_residuals', x[i], parameters$range, res[i], L)[j]
   } else {
      stop("Unkown method")
   }
}



#' Plot data and indicate outliers.
#'
#' @param x An object of class \code{parameters} as returned by the
#'  \code{\link{parameters}} function.
#' @param y A data vector
#' @param method The method for limit determination
#' @param ...  Graphical arguments to be passed to 'plot'
#' 
#' @export
plot.parameters <- function(x, y, method=c('direct','residuals'), ...){
   method <- match.arg(method)
   switch(method,
      direct      = plot_direct(x,y,...),
      residuals   = plot_residuals(x,y,...),
      stop('Unknown method')
   )
}

plot_direct <- function(x,y,rho=c(0.5,0.5),...){
   Fhat <- plotpositions(y)
   yhat <- do.call(x$qfun, c(list(Fhat), as.list(x$par)))
   L <- limits(x, method='direct',rho=rho)
   plotrange <- range(c(y,yhat))
   fitrange <- range(yhat[Fhat >= x$Flim[1] & Fhat <= x$Flim[2]])
   plot(yhat,y,xlim=plotrange, ylim=plotrange,...)
   abline(a=0,b=1)

   color = 'gray'
   # bottom coordinate
   b <- par("usr")[1]
   polygon(
      x=c(b, fitrange[1], fitrange[1],L[1]),
      y=c(b, b          , L[1]       ,L[1]),
      col=color
   )
   # top coordinate
   r <- par("usr")[2]
   polygon(
      x = c(fitrange[2],L[2],r,fitrange[2]),
      y = c(L[2]       ,L[2],r,r          ),
      col=color
   )
   i <- is.outlier(x, y, method='direct', rho=rho)
   points(yhat[i],y[i],pch=8)
   abline(h=L[1],lty=2)
   abline(h=L[2],lty=2)
   abline(v=fitrange[1],lty=2)
   abline(v=fitrange[2],lty=2)
}

plot_residuals <- function(x,y, alpha=c(0.05,0.05),...){
   L <- limits(x, y, method='residuals',alpha=alpha)
   
   r <- residuals(x,y,all=TRUE)
   plot(y,r,...)

   color = 'gray'
   # bottom coordinate
   b <- par("usr")[c(1,3)]
   polygon(
      x = c(b[1], x$range[1], x$range[1], b[1]), 
      y = c(b[2], b[2]    , L[1]      , L[1]  ),
      col = color
   )
   # top coordinate
   u <- par("usr")[c(2,4)]
   polygon(
      x   = c(x$range[2], u[1],u[1],x$range[2]),
      y   = c(L[2]      , L[2],u[2],u[2]),
      col = color
   )
   i <- is.outlier(x, y, method='residuals',alpha=alpha)
   points(y[i],r[i],pch=8)
   abline(h=0)
   abline(h=L[1],lty=2)
   abline(h=L[2],lty=2)
   abline(v=x$range[1],lty=2)
   abline(v=x$range[2],lty=2)  
}



