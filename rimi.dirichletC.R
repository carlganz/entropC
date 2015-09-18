
mi.DirichletC<-function(y2d,a,unit="log") {
  f2d=freqs.dirichletC(y2d,a)
  mi=mi.plugin(f2d,unit)
  return(mi)
}
