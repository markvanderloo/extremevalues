
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
#' @param ... initial values of the parameters
#' @export
#' @example ../examples/outliers.R
parameters <- function(x, qfun, ...){
   stopifnot(
      is.numeric(x),
      all_finite(x)
   )

   Fhat <- list(plotpositions(x))

   minfun <- function(u){
      v <- x - do.call(qfun, c(Fhat, as.list(u)) )
      sum(v*v)
   }

   optim(par=unlist(list(...)), fn=minfun )
}


