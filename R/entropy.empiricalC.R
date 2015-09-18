
entropy.empiricalC<-function(x,unit="log") {
  x<-freqsempiricalC(x)
  entropy<-entropypluginC(x)
  return(entropy)
}
