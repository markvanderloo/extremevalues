
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
#' @param Flim Determines which observations to use. By default
#' @param ... initial values of the parameters
#' @param optimpar A list of named parameters for R's \code{optim} function.
#'
#' @value The value is equal to the output of \code{optim}
#'
#' @export
#' @example ../examples/outliers.R
#' 
parameters <- function(x,  qfun, Flim=c(0,1), ..., optimpar=list()){
   stopifnot(
      is.numeric(x),
      all_finite(x),
      Flim[1] < Flim[2]
   )

   Fhat <- plotpositions(x)
   i <- Fhat >= Flim[1] & Fhat <= Flim[2]
   if ( sum(i) < length(list(...)) ){
      stop('Not enough observations to determine parameters (should you increase Flim?)')
   }
   x <- x[i]
   Fhat <- list(Fhat[i])

   minfun <- function(u){
      v <- x - do.call(qfun, c(Fhat, as.list(u)) )
      sum(v*v)
   }

   do.call(optim, c(list(par=unlist(list(...)), fn=minfun), optimpar))
}

limits <- function(qfun, N, rho=c(0.5,0.5), ... ){
   x <- rho/N
   c(Lplus=qfun(1-x[2], ...), Lmin=qfun(x[1], ...))
}








