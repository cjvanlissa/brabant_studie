keep_analysis_vars <- function(df, dict){
keep_these <- grep("^DAS_", names(df), value = TRUE)
dict <- dict[!grepl("^DAS_", dict$name),]
dict <- dict[dict$time %in% c("12", "20", "28"), ]
tmz <- levels(dict$time)
for(v in unique(dict$variable)){
  tmp <- dict[dict$variable == v, , drop = FALSE]
  if(nrow(tmp) > 0){
    keep_these <- c(keep_these, tmp$name[1])
  }
}

return(
  df[, keep_these]
)
}
