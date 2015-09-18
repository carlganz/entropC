freqs.DirichletC<-function(x,a) {
  ismatrix<-attributes(x)$dim
  x<-freqsdirichletC(x,a)
  if(!is.null(ismatrix)) {
    attr(x,"dim")=ismatrix
  }
  return(x)
}
