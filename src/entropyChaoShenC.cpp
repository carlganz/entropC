#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double entropyChaoShenC(NumericVector x,String unit="log") {
  int l=x.length();

  int count=0;
  for (int i=0;i<l;i++) {
    if (x(i)>0) {
      count+=1;
    }
  }

  NumericVector yx(count);
  count=0;
  for (int i=0;i<l;i++) {
    if (x(i)>0) {
      yx(count)=x(i);
      count+=1;
    }
  }

  double n=0;
  for (int i=0;i<count;i++) {
    n+=yx(i);
  }
  NumericVector p=yx/n;
  int f1=0;
  for (int i=0;i<count;i++) {
    if (yx(i)==1) {
      f1+=1;
    }
  }
  if (f1==n) {
    f1=n-1;
  }

  double C=1-(f1/n);
  NumericVector pa=C*p;
  NumericVector la=(1-pow(1-pa,n));

  double entropy=0;
  for (int i=0;i<count;i++) {
    entropy-=(pa(i)*log(pa(i))/la(i));
  }
  if (unit=="log2") {
    entropy=entropy/log(2);
  }
  if (unit=="log10") {
    entropy=entropy/log(10);
  }
  return entropy;
}
