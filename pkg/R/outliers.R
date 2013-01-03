
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
#' @param Flim Determines which observations to use. By default all observations are used (between the 0th and 100th percentile).
#' @param ... initial values of the parameters
#' @param optimpar A list of named parameters for R's \code{optim} function.
#'
#' @value The value is equal to the output of \code{optim}
#'
#' @export
#' @example ../examples/outliers.R
#' 
parameters <- function(x, qfun, Flim=c(0,1), ..., optimpar=list()){
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

   L <- c(list(qfun=qfun, N=N, Flim=Flim, optimpar=optimpar), L)
   structure(L, class='parameters')
}

#' Obtain residuals from parameter fit
#'
#' @param object an object of class parameters, as returned by the \code{\link{parameters}} function.
#' @param x vector of observations
#' @param all Indicate wheter to return all residuals (including extrapolations) or just the residuals of observations used in the fit.
#'@export
residuals.parameters <- function(object, x, all=TRUE, ...){
   Fhat <- plotpositions(x)
   r <- do.call(object$qfun, c(list(Fhat), as.list(object$par)))

   if ( all){ 
      r
   } else {
      r[Fhat>=object$Flim[1] & Fhat <= object$Flim[2]]
   }
}



#' Compute outlier limits based on fitted parameters
#' 
#' @param parameters An object of class 'parameters', as returned by \code{\link{parameters}}
#' @param method Choose a method for determining the limit
#' @param ... Arguments to be passed to methods for limit calculation. Specifically
#'  \tabular{llll}{
#'  Method  \tab parameter  \tab default \tab description \cr
#'  direct  \tab \code{rho} \tab c(0.5,0.5) \tab  limits beyond which less than \code{rho} observations are expected\cr
#'  residual \tab \code{alpha} \tab c(0.05,0.05) \tab interval on fit residuals
#' }
#' @export
limits <- function(parameters, ...){
   UseMethod('limits')
}

# poor-man's multiple dispatch.
limits.parameters <- function(parameters, method=c('direct','residuals'), ...){
   method <- match.arg(method)
   switch(method,
      direct      = direct_limits(parameters, ...),
      residuals   = residual_limits(parameters, ...),
      stop('unrecognized method')
   )  
}

direct_limits <- function(x, rho=c(0.5,0.5)){
   p <- rho/x$N
   
   c(
      Lmin = do.call(x$qfun,c(list(p[1]),as.list(x$par))), 
      Lplus= do.call(x$qfun,c(list(1-p[2]),as.list(x$par)))
   )
}

residual_limits <- function(x, alpha=c(0.05,0.05)){
   sigma <- sd(residuals(x,all=FALSE))
   sq2 <- sqrt(2)
   sq2*sigma*c(Lmin=invErf(2*alpha[1]-1), Lplus=invErf(1-2*alpha[2]) )
}

#' Determine which values are outliers
#'
#' @param parameters An object of class \code{parameters} as returned by 
#'    the \code{\link{parameters}} function.
#' @param x Vector of observations
#' @export
is.outlier <- function(parameters, x, ...){
   L <- limits(parameters,...)
   x < L[1] | x > L[2]
}



#' Plot data and indicate outliers.
#'
#' @param x An object of class \code{outliers} as returned by the
#'  \code{\link{outliers}} function.
#' @param y A data vector
#' @param ... 
#' @export
plot.parameters <- function(x, y, method=c('direct','residuals'), ...){
   method <- match.arg(method)
   switch(method,
      direct = plot_direct(x,y,...),
      stop('Unknown method')
   )
}

plot_direct <- function(x,y,...){
   Fhat <- plotpositions(y)
   yhat <- do.call(x$qfun, c(list(Fhat),as.list(x$par)))
   L <- limits(x, y, method='direct')
   plotrange <- range(c(y,yhat))
   fitrange <- range(yhat[Fhat >= x$Flim[1] & Fhat <= x$Flim[2]])
   plot(yhat,y,xlim=plotrange, ylim=plotrange)
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
   points(yhat,y,xlim=plotrange, ylim=plotrange)
   abline(a=L[1],b=0,lty=2)
   abline(a=L[2],b=0,lty=2)
   abline(v=fitrange[1],lty=2)
   abline(v=fitrange[2],lty=2)

}





