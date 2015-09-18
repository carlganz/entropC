#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector freqsempiricalC(NumericVector x_) {
  Rcpp::NumericVector x = Rcpp::clone(x_);
  int n=x.length();
  double sum=0;
  for (int i=0;i<n;i++) {
    sum+=x[i];
  }
  x=x/sum;
  return x;
}
