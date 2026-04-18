get_data <- function(){
  out <- worcs::load_data(to_envir = FALSE)$df
  class(out) <- "data.frame"
  return(out)
}

select_features <- function(dat, features){
  out <- lapply(features, function(f){
    dat$train <- dat$train[, c("moral_concern", f)]
    dat$test <- dat$test[, c("moral_concern", f)]
    dat$train_means <- dat$train_means[names(dat$train_means) %in% f]
    dat$train_sds <- dat$train_sds[names(dat$train_sds) %in% f]
    dat
  })
  names(out) <- names(features)
  return(out)
}

get_features <- function(dat, flnm){
  selected_variables <- readxl::read_xlsx(flnm)
  selected_variables <- selected_variables[rowSums(selected_variables[, c("target attribute predictor", "judge attribute predictor", "demographic predictor")], na.rm = TRUE) > 0, ]
  out <- lapply(selected_variables[, c("target attribute predictor", "judge attribute predictor", "demographic predictor")], function(i){ selected_variables$variable_name[which(i > 0)] })
  names(out) <- gsub(" .*$", "", names(out))
  out <- lapply(out, function(vs){ vs[which(vs %in% names(dat$train))]})
  out <- c(list(all = unique(unlist(out))), out)
  return(out)
}
