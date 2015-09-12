#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double KLpluginC(NumericVector x,NumericVector y, String unit="log") {
  int n=x.length();
  double sum1=0;
  double sum2=0;
  for (int i=0;i<n;i++) {
    sum1+=x[i];
    sum2+=y[i];
  }

  x=x/sum1;
  y=y/sum2;
  NumericVector LR(n);
  for (int i=0;i<n;i++) {
    if (x[i]>0) {
      LR[i]=log(x[i]/y[i]);
    } else {LR[i]=0;}
  }
  double KL=0;
  for (int i=0;i<n;i++) {
    KL+=x[i]*LR[i];
  }
  if (unit=="log2") {
    KL=KL/log(2);
  }
  if (unit=="log10") {
    KL=KL/log(10);
  }
  return KL;
}
