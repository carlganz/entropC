
chi.dirichlet<-function(x,y,a1,a2,unit="log") {
  x<-freqsdirichletC(x,a1)
  y<-freqsdirichletC(y,a2)
  chi<-chipluginC(x,y,unit)
  return(chi)
}
