chi2indep.DirichletC<-function(y2d,a,unit="log") {
  f2d<-freqs.DirichletC(y2d,a)
  chi2<-chi2indep.pluginC(f2d,unit)
  return(chi2)
}
