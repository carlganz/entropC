#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double chi2pluginC(NumericVector x,NumericVector y, String unit="log") {
  int n=x.length();
  double sum=0;
  for (int i=0;i<n;i++) {
    sum+=x[i];
  }
  x=x/sum;
  double sumy=0;
  for (int i=0;i<n;i++) {
    sumy+=y[i];
  }
  y=y/sumy;
  double chi=0;
  for (int i=0;i<n;i++) {
    chi+=(pow(x[i]-y[i],2)/y[i]);
  }
  if (unit == "log2") {
    chi = chi/log(2);}
  if (unit == "log10") {
    chi = chi/log(10);}
  return chi;
}
