#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double entropypluginC(NumericVector x,String unit="log") {
  int n=x.length();
  double total=0;

  for (int i=0;i<n;i++) {
    total+=x(i);
  }
  x=x/total;
  NumericVector logs(n);
  for (int i=0;i<n;i++) {
    if (x(i)>0) {
    logs(i)=log(x(i));
    } else {logs(i)=0;}}
  double entropy=0;
  for (int i=0;i<n;i++) {
    entropy-=x(i)*logs(i);
  }
  if (unit=="log2") {
    entropy=entropy/log(2);
  }
  if (unit=="log10") {
    entropy=entropy/log(10);
  }
  return(entropy);
}
