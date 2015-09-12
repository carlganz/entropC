#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double entropypluginC(NumericVector x,String unit="log") {
  int n=x.length();
  int total=0;

  for (int i=0;i<n;i++) {
    total+=x(i);
  }

  x=x/total;
  NumericVector logs(n);
  for (int i=0;i<n;i++) {
    logs(i)=log(x(i));
  }
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

// [[Rcpp::export]]
double entropyempiricalC(NumericVector x,String unit="log") {

  Function entropyplugin("entropypluginC");
  Function freqsempirical("freqsempiricalC");
  x=freqsempirical(x);
  double entropy=as<double>(entropyplugin(x));


  return entropy;
}

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
// [[Rcpp::export]]
NumericVector freqsempiricalC(NumericVector x) {
  int n=x.length();
  double sum=0;
  for (int i=0;i<n;i++) {
    sum+=x[i];
  }
x=x/sum;
  return x;
}

// [[Rcpp::export]]
double chipluginC(NumericVector x,NumericVector y, String unit="log") {
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

// [[Rcpp::export]]
double KLdirichletC(NumericVector x,NumericVector y, double a1, double a2, String unit="log") {

  Function chi2pluginC("KLpluginC");
  Function freqsdirichletC("freqsdirichletC");
  NumericVector f1=freqsdirichletC(x,a1);
  NumericVector f2=freqsdirichletC(y,a2);
  double KL=KLpluginC(f1,f2,unit);

  return KL;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
#freq table
test<-c(1,2,3,1)
#my function
entropypluginC(test)
#compare to entropy package
library(entropy)
entropy(test)
#are the answers the same
entropypluginC(test)==entropy(test)
#create larger dataset
test<-table(mtcars$hp)
#see how long each function takes
timeold<-system.time(entropy(test))
timeCG<-system.time(entropypluginC(test))
#is my function faster
timeold
timeCG

entropy(test)
entropypluginC(test)

entropyMMC(test)
entropy(test,method = "MM")

timeold<-system.time(entropy(test,method="MM"))
timeCG<-system.time(entropyMMC(test))
#is my function faster
timeold
timeCG

entropyChaoShenC(test)
entropy.ChaoShen(test)

freqsdirichletC(c(1,2,3,4),1)
freqs.Dirichlet(c(1,2,3,4),1)

freqsempiricalC(c(1,2,3,4,5,6,7,8,1,2,3,4,5,3,2,1,4))
freqs.empirical(c(1,2,3,4,5,6,7,8,1,2,3,4,5,3,2,1,4))

system.time(freqsempiricalC(c(1,2,3,4,5,6,7,8,1,2,3,4,5,3,2,1,4)))
system.time(freqs.empirical(c(1,2,3,4,5,6,7,8,1,2,3,4,5,3,2,1,4)))

chi2.Dirichlet(c(.33,.33,.34),c(.40,.50,.10),1,1)
chidirichletC(c(.33,.33,.34),c(.40,.50,.10),1,1)

KLpluginC(c(1/5,1/5,3/5),c(1/10,4/10,1/2))
KL.plugin(c(1/5,1/5,3/5),c(1/10,4/10,1/2))

KL.Dirichlet(y1, y2, a1=1/6, a2=1/6)
KLdirichletC(y1, y2, a1=1/6, a2=1/6)
*/
