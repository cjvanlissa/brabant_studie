data_preprocessing <- function(df, dict, tab_descriptives){

  sum_vars <- tab_descriptives$variable[tab_descriptives$type == "rowSums"]
  df_sum <- lapply(sum_vars, function(n){
    df_tmp <- df[, dict$items[[match(n, dict$name)]]]
    df_tmp[] <- lapply(df_tmp, as.integer)
    df_tmp[] <- df_tmp - 1L
    scores <- rowSums(df_tmp, na.rm = TRUE)
    scores[which(scores > 0)] <- 1
    ordered(scores)
  })
  names(df_sum) <- sum_vars
  df_sum <- as.data.frame(df_sum)

  mean_vars <- tab_descriptives$variable[tab_descriptives$type == "rowMeans"]
  df_mean <- lapply(mean_vars, function(n){
    df_mean <- df[, dict$items[[match(n, dict$name)]]]
    ord_vars <- sapply(df_mean, inherits, what = "ordered")
    if(any(ord_vars)){
      df_mean[ord_vars] <- lapply(df_mean[ord_vars], as.integer)
    }
    rowMeans(df_mean, na.rm = TRUE)
  })
  names(df_mean) <- mean_vars
  df_mean <- as.data.frame(df_mean)
  indiv_vars <- unlist(dict$items[sapply(dict$items, length) == 1])
  df_indiv <- df[, indiv_vars]
  names(df_indiv) <- dict$name[sapply(dict$items, length) == 1]
  df <- data.frame(df_indiv, df_mean, df_sum)

  # Preprocess numeric variables, scale and center
  nums <- names(df)[sapply(df, inherits, what = c("numeric", "integer"))]
  nums <- setdiff(nums, grep("^DAS_", names(df), value = TRUE)) # Exclude DV
  scld <- scale(df[nums], center = TRUE, scale = TRUE)
  means <- attr(scld, "scaled:center")
  sds <- attr(scld, "scaled:scale")
  df[nums] <- scld
  saveRDS(list(means = means, sds = sds), "scale_means_sds.RData")

  # Preprocess ordinal variables; keep linear and quadratic effect
  # na_option <- getOption("na.action")
  # options(na.action='na.pass')
  # ord <- names(df)[sapply(df, inherits, what = c("ordered"))]
  # df_tmp <- model.matrix(~., df[, ord, drop = F])
  # df_tmp <- df_tmp[, grep(".[LQ]$", colnames(df_tmp))]
  # df[ord] <- NULL
  # df <- data.frame(df, df_tmp)
  # options(na.action=na_option)
  #
  # # Preprocess factors ; turn into dummies
  # facs <- names(df)[sapply(df, inherits, what = "factor")]
  # df_tmp <- df[, facs]
  # numcats <- sapply(df_tmp, function(x)length(table(x)))
  # df_tmp = tidySEM::mx_dummies(df_tmp)
  # df_tmp[] <- lapply(df_tmp, as.integer)
  # df[facs] <- NULL
  # df <- data.frame(df, df_tmp)


  # Preprocess dates; days since first date
  df$Dateofbirth_baby_8wPP[which(df$Dateofbirth_baby_8wPP == as.Date("2002-10-01"))] <- as.Date("2020-10-01")
  dats <- names(df)[sapply(df, inherits, what = "Date")]
  df_tmp <- df[, dats, drop = FALSE]
  df_tmp[] <- lapply(df_tmp, function(x){
    as.integer(difftime(x, min(x, na.rm = TRUE), units = "days"))
  })
  df[dats] <- NULL
  df <- data.frame(df, df_tmp)



  return(df)
}

