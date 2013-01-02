
#include <R.h>
#include <Rdefines.h>

SEXP all_finite_double(SEXP x){
   PROTECT(x);
   double *xx = REAL(x);

   SEXP y;
   PROTECT(y = allocVector(LGLSXP,1));
 
   int i, b = 1;
   for (i=0; i<length(x); i++) {
      b = R_finite(xx[i]);
      if (!b) break;
   }

   
   LOGICAL(y)[0] = b;
   UNPROTECT(2);

   return y; 
}


