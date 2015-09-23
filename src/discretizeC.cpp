#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector discretizeC(NumericVector x, int numBins) {
  double low=min(x);
  double high=max(x);
  double diff=high-low;
  double step=diff/numBins;
  NumericVector bins(numBins);
for (int j=0;j<x.length();j++) {
  for (int i=0;i<numBins;i++) {
    if (i==0) {
    if (x[j]>=(i*step+low) & x[i]<=((i+1)*step+low)) {
      bins[i]+=1;
    }} else {
      if (x[j]>(i*step+low) & x[i]<=((i+1)*step+low)) {
        bins[i]+=1;
      }
    }
  }
}

return bins;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
discretize(c(42,2,20,3,4,5,6,7,4),3)
*/
