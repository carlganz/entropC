
KL.dirichlet<-function(x,y,a1,a2,unit="log") {
  x<-freqsdirichletC(x,a1)
  y<-freqsdirichletC(y,a2,unit="log")
  KL<-KLpluginC(x,y,unit)
  return(KL)
}
