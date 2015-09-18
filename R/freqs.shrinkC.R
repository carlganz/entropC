freqs.shrinkC<-function(y,lambda.freqs,verbose=TRUE) {
  if (missing(lambda.freqs)) {
      lambda.freqs = getlambdashrinkC(y)
  }
    if (verbose==TRUE) {
      cat(paste("Specified shrinkage intensity lambda.freq (frequencies):",
                round(lambda.freqs, 4)), "\n")
    }
  ismatrix<-attributes(y)$dim
  out<-freqsshrinkC(y,lambda.freqs)
  attr(out,"lambda.freq")=lambda.freqs
  attr(out,"dim")=ismatrix
  return(out)
}
