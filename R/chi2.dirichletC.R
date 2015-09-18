
chi2.dirichletC<-function(x,y,a1,a2,unit="log") {
  x<-freqsdirichletC(x,a1)
  y<-freqsdirichletC(y,a2)
  chi<-chi2pluginC(x,y,unit)
  return(chi)
}
