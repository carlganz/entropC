mi.shrinkC<-function (y2d, lambda.freqs, unit = "log",
                      verbose = TRUE)
{
  if (missing(lambda.freqs)) {
    lambda.freqs = getlambdashrinkC(y)
  }
  f2d = freqs.shrinkC(y2d, lambda.freqs = lambda.freqs, verbose = verbose)
  mi = mi.pluginC(f2d, unit = unit)
  attr(mi, "lambda.freqs") = lambda.freqs
  return(mi)
}
