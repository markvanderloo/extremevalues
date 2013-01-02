
all_finite <- function(x){
   stopifnot(is.numeric(x))
   storage.mode(x) <- "double"
   .Call("all_finite_double",x)
}

#' Compute QQ-plotpositions
#'
#' @section Details:
#' The QQ-plot indices are computed as as follows. For each \eqn{x_i} the rank \eqn{j_i} is determined.
#' The plot index for \eqn{x_i} is then given by \eqn{i/(N+1)}. When \eqn{\boldsymbol{x}} has duplicate
#' entries, plot indices for the duplicates are computed for each duplicate and replaced with the mean (by default).
#' Passing \code{duplicates='asis'}, the original rank is used.
#'
#'
#' @param x numeric vector. An error is thrown when \code{x} contains nonnumerical or nonfinite values.
#' @param duplicates A character constant determining how duplicate entries are indexed (see details). 
#' @return plot positions \eqn{0< p < 1} for the values in \code{x}.
#' @export
#' @example ../examples/plotpositions.R
#' 
plotpositions <- function(x,duplicates=c('mean','asis')){
   stopifnot(
      is.numeric(x),
      all_finite(x)
   )
   duplicates <- match.arg(duplicates)
   N <- length(x)
   i <- order(x)
   if (identical(duplicates,'asis')){
      pp <- (1:N)/(N+1)
      return(pp[i])
   } else {
      storage.mode(x) <- "double"
      pp <- .Call("R_plotpositions",x[i])
   }
   pp[order(i)]
}





