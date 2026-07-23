impute_data <- function(df, dict){
  library(missRanger)
  set.seed(67107)
  df <- missRanger(df, num.trees = 100)
return(df)
}
