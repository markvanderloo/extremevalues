
#include <R.h>
#include <Rdefines.h>
#include "plotpositions.h"

// interface to R's is.finite variant (C99) that takes care of NA representation.
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

/* An element of x is an outlier when it is 
 * 1. out of the fitrange
 * 2. larger than a predifined limit
 * 3. there are no observarions extending further outward that are not outliers.
 * NOTE: x must be sorted in ascending order.
 */
SEXP isoutlier_direct(SEXP x, SEXP limits, SEXP fitrange ){
   PROTECT(x);
   double *xx = REAL(x), *lm = REAL(limits), *fr = REAL(fitrange);
   int n = length(x), i;
   SEXP y;
   PROTECT(y=allocVector(LGLSXP,n));
   int *yy = LOGICAL(y);
   for ( i=0; i<n; yy[i++] = 0 );
   // left outliers
   i = 0;
   while ( xx[i] < lm[0] && xx[i] < fr[0] && i < n ){
      yy[i] = 1;
      i++;
   }
   // right outliers
   i = n-1;
   while( xx[i] > lm[1] && xx[i] > fr[1] && i > -1 ){
      yy[i] = 1;
      i--;
   }

   UNPROTECT(2);
   return y;
}


SEXP R_plotpositions(SEXP x){
   PROTECT(x);

   SEXP I;
   PROTECT(I = allocVector(REALSXP, length(x)));

   plotpositions(REAL(x), length(x), REAL(I)); 
   
   UNPROTECT(2);

   return I; 
}


