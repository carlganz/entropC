
KL.DirichletC<-function(x,y,a1,a2,unit="log") {
  x<-freqsdirichletC(x,a1)
  y<-freqsdirichletC(y,a2)
  KL<-KLpluginC(x,y,unit)
  return(KL)
}
