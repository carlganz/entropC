#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
List chi2indepC(NumericMatrix x) {
  int n=x.nrow();
  int m=x.ncol();
  double sum=0;
  for (int i=0;i<n;i++) {
    for (int j=0;j<m;j++) {
      sum+=x(i,j);
    }
  }
  for (int i=0;i<n;i++) {
    for (int j=0;j<m;j++) {
      x(i,j)=x(i,j)/sum;
    }
  }

  NumericVector freqsx(n);
  NumericVector freqsy(m);
  NumericMatrix freqsnull(n,m);

  for (int i=0;i<n;i++) {
    freqsx[i]=0;
    for (int j=0;j<m;j++) {
      freqsx[i]+=x(i,j);
    }
  }
  for (int i=0;i<m;i++) {
    freqsy[i]=0;
    for (int j=0;j<n;j++) {
      freqsy[i]+=x(j,i);
    }
  }

  for (int i=0;i<n;i++) {
    for (int j=0;j<m;j++) {
      freqsnull(i,j)=freqsx[i]*freqsy[j];
    }
  }

  NumericVector y1(n*m);
  NumericVector y2(n*m);
  int count=0;
  for (int i=0;i<n;i++) {
    for (int j=0;j<m;j++) {
      y1[count]=x(i,j);
      y2[count]=freqsnull(i,j);
      count+=1;
    }
  }
  List chi2;
  chi2["y1"]=y1;
  chi2["y2"]=y2;

return chi2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
chi2indepC(matrix(c(1,2,3,4),nrow=4,ncol=4))
*/
