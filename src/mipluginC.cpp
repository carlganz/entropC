#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List mipluginC(NumericMatrix x,String unit="log") {

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
Function KLpluginC("KLpluginC");
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

List MI=KLpluginC(y1,y2,unit);

return MI;
}

