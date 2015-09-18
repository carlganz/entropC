entropy.shrinkC<-function(y, lambda.freqs,unit="log",verbose=T) {
  if (missing(lambda.freqs)) {
    lambda.freqs = getlambdashrinkC(y)
  }
  f=freqs.shrinkC(y,lambda.freqs = lambda.freqs,verbose=verbose)
  h=entropypluginC(f,unit=unit)
  attr(h,"lambda.freqs")=lambda.freqs
  return(h)
}
