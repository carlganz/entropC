freqs.empiricalC<-function(x) {
  ismatrix<-attributes(x)$dim
  x<-freqsempiricalC(x)
  if(!is.null(ismatrix)) {
    attr(x,"dim")=ismatrix
  }
  return(x)
}
