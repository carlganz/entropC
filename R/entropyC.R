entropyC<-function (y, lambda.freqs, method = c("ML", "MM", "Jeffreys",
                                      "Laplace", "SG", "minimax", "CS", "NSB", "shrink"), unit ="log", verbose = TRUE, ...)
{
  method = match.arg(method)
  if (method == "ML")
    H = entropy.empiricalC(y, unit = unit)
  if (method == "MM")
    H = entropyMMC(y, unit = unit)
  if (method == "NSB")
    H = entropy.NSB(y, unit = unit, ...)
  if (method == "CS")
    H = entropyChaoShenC(y, unit = unit)
  if (method == "Jeffreys")
    H = entropy.DirichletC(y, a = 1/2, unit = unit)
  if (method == "Laplace")
    H = entropy.DirichletC(y, a = 1, unit = unit)
  if (method == "SG")
    H = entropy.DirichletC(y, a = 1/length(y), unit = unit)
  if (method == "minimax")
    H = entropy.DirichletC(y, a = sqrt(sum(y))/length(y),
                          unit = unit)
  if (method == "shrink")
    H = entropy.shrinkC(y, lambda.freqs = lambda.freqs, unit = unit,
                       verbose = verbose)
  return(H)
}
