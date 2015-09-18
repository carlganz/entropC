
chi2.empiricalC<-function(y1,y2,unit="log") {
  return(chi2pluginC(freqsempiricalC(y1),freqsempiricalC(y2),unit))
}
