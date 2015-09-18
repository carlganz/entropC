entropy.DirichletC<-function(y,a,unit="log") {
  return(entropypluginC(freqs.dirichletC(y,a),unit))
}
