#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector freqsdirichletC(NumericVector x,double a) {
  int n=x.length();
  NumericVector xa(n);
  double na=0;
  for (int i=0;i<n;i++) {
    xa[i]=x[i]+a;
    na+=x[i]+a;
  }
  xa=xa/na;
  return xa;

}
