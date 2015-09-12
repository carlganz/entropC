#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double entropyempiricalC(NumericVector x,String unit="log") {

  Function entropyplugin("entropypluginC");
  Function freqsempirical("freqsempiricalC");
  x=freqsempirical(x);
  double entropy=as<double>(entropyplugin(x));


  return entropy;
}
