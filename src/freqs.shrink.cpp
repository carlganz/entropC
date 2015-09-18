#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector freqsshrinkC(NumericVector y_,double lambda) {
  Rcpp::NumericVector y = Rcpp::clone(y_);
  int m=y.length();
  double n=0;
  for (int i=0;i<m;i++) {
    n+=y(i);
  }
  y=y/n;

NumericVector add(m,lambda/m);
   y=y*(1-lambda);

  y+=add;
  return y;
}
