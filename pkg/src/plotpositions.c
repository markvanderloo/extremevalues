
// Sum of sequence i, i+1,...,j.
// - "double" arithmetic is used to avoid integer overflow.
double seq_sum(int i, int j){
   int k = ( (j - i) + 1 );
   int m = k / 2;
   double u, di = (double) i, dj = (double) j;
   if ( k % 2 == 0 ){
      u = m * (dj + di);
   } else {
      u = m * (dj + di ) + dj - m;
   }
   return u;
}


// QQ-plot positions, replacing double entries with the mean position.
// - Positions determined  as described in Makkonen (2008).
// - It is assumed that x is sortend in ascending order!
void plotpositions(double *x, int n, double *I){
   int i = 0, j, k = 1;
   double m, pp;
   int N = n + 1;
   while( i < n ){
      // find number (m)  and index (i,i+1,...,j) of duplicates
      j = i+1;
      while ( x[j] == x[i] ){ 
         j++;
      };
      m = (double) j-i;
      // compute mean QQ-plot position and assign
      pp = seq_sum(i+1,j)/(m*N);
      for ( k = i; k < j; k++ ){ 
         I[k] = pp;
      }
      // shift index
      i = j;
   }

}

/* example: uncomment to run.
#include <stdio.h>
void main(){
   double x[4] = {1.0,3.0,3.0,4.0};
   double I[4];

   plotpositions(x, 4, I);
   for (int i=0; i<4; i++){
      printf("%4.2f, %4.2f", x[i], I[i]);
      printf("\n");
   }

}
*/
