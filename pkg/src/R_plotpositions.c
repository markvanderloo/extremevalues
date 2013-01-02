
#include <R.h>
#include <Rdefines.h>
#include "plotpositions.h"

SEXP R_plotpositions(SEXP x){
   PROTECT(x);

   SEXP I;
   PROTECT(I = allocVector(REALSXP, length(x)));

   plotpositions(REAL(x), length(x), REAL(I)); 
   
   UNPROTECT(2);

   return I; 
}




