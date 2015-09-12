#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double entropyMMC(NumericVector x,String unit="log") {
  int l=x.length();
  double n=0;
  double m=0;
  for (int i=0;i<l;i++) {
    n+=x(i);
    if (x(i)>0) {
      m+=1;
    }
  }

  Function entropyplugin("entropypluginC");

  double entropy=as<double>(entropyplugin(x));
  entropy=entropy+(m-1)/(2*n);
  if (unit=="log2") {
    entropy=entropy/log(2);
  }
  if (unit=="log10") {
    entropy=entropy/log(10);
  }
  return entropy;
}
