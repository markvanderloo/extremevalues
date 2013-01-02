#' Inverse error function
#'
#' @section Details:
#' This is the inverse of the error function as defined by Abramowitz and Stegun (1965) Eq. 7.1.1 
#'
#' @param x numeric vector with values between \eqn{-1} and \eqn{1}
#' 
#' @references
#' M. Abramowitz and I. A. Stegun (1964, Editors) Handbook of Mathematical Functions.
#' Dover Publications Inc. New York.
invErf <- function(x){
   if ( sum(x >= 1) > 0  | sum(x <= -1) > 0 )
      stop("Argument must be between -1 and 1")
   qnorm((1+x)/2)/sqrt(2)
}

