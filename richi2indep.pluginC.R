chi2indep.pluginC<-function(freqs2d,unit="log") {
  x<-chi2indepC(freqs2d)
  chi2<-chi2pluginC(x$y1,x$y2,unit)
  return(chi2)
}
