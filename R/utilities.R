rsq <- function(observed, predicted, ymean){
  SS.total    <- sum((observed - ymean) ^ 2)
  SS.residual <- sum((observed - predicted) ^ 2)
  #SS.regression <- sum((predicted - ymean) ^ 2)
  return(1 - SS.residual / SS.total)
}
