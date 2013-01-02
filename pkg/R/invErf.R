#' Inverse error function
#' 
#' @param x numeric vector with values between \eqn{-1} and \eqn{1}
#' 
invErf <- function(x){
   if ( sum(x >= 1) > 0  | sum(x <= -1) > 0 )
      stop("Argument must be between -1 and 1")
   qnorm((1+x)/2)/sqrt(2)
}

