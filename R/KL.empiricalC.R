
KL.empiricalC<-function(y1,y2,unit="log") {
  return(KLpluginC(freqsempiricalC(y1),freqsempiricalC(y2),unit))
}
