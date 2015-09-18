chi2indep.shrinkC<-function(y2d,lambda.freqs,unit="log") {
  if(missing(lambda.freqs)){
  lambda.freqs = getlambdashrinkC(y2d)
  }
  f2d<-freqs.shrinkC(y2d,lambda.freqs,unit)
  chi2<-chi2indep.pluginC(f2d,unit)
  attr(chi2,"lambda.freqs")=lambda.freqs
  return(chi2)
}

