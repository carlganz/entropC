
mi.empiricalC<-function(y2d,unit="log") {
  return(mipluginC(freqsempiricalC(y2d),unit)[[1]])
}
