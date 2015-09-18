chi2.shrinkC<-function (y1, y2, lambda.freqs1, lambda.freqs2, unit = "log", verbose = TRUE)
{
  if (missing(lambda.freqs1)) {
    lambda.freqs1 = getlambdashrinkC(y1)
  }
  if (missing(lambda.freqs2)) {
    lambda.freqs2 = getlambdashrinkC(y2)
  }
  f1 = freqs.shrinkC(y1, lambda.freqs = lambda.freqs1, verbose = verbose)
  f2 = freqs.shrinkC(y2, lambda.freqs = lambda.freqs2, verbose = verbose)
  chi2 = chi2pluginC(f1, f2, unit = unit)
  attr(chi2, "lambda.freqs1") = lambda.freqs1
  attr(chi2, "lambda.freqs2") = lambda.freqs2
  return(chi2)
}
