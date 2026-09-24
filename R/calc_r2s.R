calc_r2s <- function(merged_summarydata, growth_model, df_features){
  library(data.table)
  loadings <- growth_model$A$values[1:10, 11:13]
  f <- merged_summarydata$forest_light
  forest <- readRDS(f)
  out <- array(NA, dim = c(nrow(df_features), length(forest), 3))
  for(i in 1:nrow(df_features)){
    for(j in 1:length(forest)){
      out[i, j, 1:3] <- semtree:::traverse_stripped(row = df_features[i, ], tree = forest[[j]])
    }
  }
  coef_pp <- apply(out, c(1, 3), median)
  coef_avg <- matrix(tail(growth_model$M$values[1,], 3), nrow = nrow(loadings), ncol = 3, byrow = TRUE)
  pred_avg <- rowSums(loadings * coef_avg)
  pred_avg <- matrix(pred_avg, nrow = nrow(df_features), ncol = length(pred_avg), byrow = TRUE)

  pred_pp <- t(apply(coef_pp, 1, function(x){
    rowSums(loadings * matrix(x, nrow = nrow(loadings), ncol = 3, byrow = TRUE))
  }))

  obs <- df_features[, 1:10]
  names(obs) <- names(pred_avg)
  obs <- as.matrix(obs)

  ss_mean <- colSums((obs-matrix(colMeans(obs), nrow = nrow(obs), ncol = ncol(obs), byrow = TRUE))^2)

  ss_baseline <- colSums((obs-pred_avg)^2)

  ss_pp <- colSums((obs - pred_pp)^2)
  r2_pp_mean <- 1-(ss_pp / ss_mean)
  r2_pp_baseline <- 1-(ss_pp / ss_baseline)
  r2s_baseline_mean <- 1-(ss_baseline / ss_mean)

  return(
    list(
    indiv_coefs = out,
    r2_pp_mean = r2_pp_mean,
    r2_pp_baseline = r2_pp_baseline,
    r2s_baseline_mean = r2s_baseline_mean
  ))
}
