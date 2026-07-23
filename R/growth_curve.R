do_growthcurves <- function(df_features){
  library(OpenMx)
  library(tidySEM)


df <- df_features[, grep("^DAS_", names(df_features))]
t_int <- c("DAS_12" = 12, "DAS_20" = 20, "DAS_28" = 28, "DAS_8wPP" = 40+8, "DAS_6mPP" = 40 + 26, "DAS_1yPP" = 40+52,
           "DAS_1y6mPP" = 40+52+26, "DAS_2yPP" = 40+(2*52), "DAS_2y6mPP" = 40+(2*52)+26, "DAS_3yPP" = 40+(3*52), "DAS_3y6mPP" = 40+(3*52)+26,
           "DAS_4yPP" = 40+(4*52))
# Center at partus
t_int <- t_int - 40
# Scale by 100 to have models converge
t_int <- t_int/100

t_int <- t_int[1:10]
df <- df[, names(t_int)]

names(df) <- paste0("DAS", 1:ncol(df))

m_int_only = paste0("i =~ ", paste0("1*", names(df), collapse = " + "))

slope = c(no_slope = "",
          slope = paste0("s =~ ", paste0(t_int, "*", names(df), collapse = " + ")),
          two_slopes = paste0(paste0("s1 =~ ", paste0(t_int * (t_int <= 0), "*", names(df), collapse = " + ")), "\n", paste0("s2 =~ ", paste0(t_int * (t_int > 0), "*", names(df), collapse = " + ")))
)

step = c(no_step = "", step = paste0("step =~ ", paste0(as.integer(t_int > 0), "*", names(df), collapse = " + "))
)

models <- expand.grid(m_int_only, slope, step)
models <- apply(models, 1, c, simplify = FALSE)
names(models) <- apply(expand.grid("int", names(slope), names(step)), 1, paste0, collapse = "_")


models <- lapply(models, c, paste0(names(df), " ~~ NA*", names(df)))
models <- lapply(models, c, paste0(names(df), " ~ 0*1"))

models <- lapply(models, function(x){
  vs <- lavaan:::lavNames(x, type = "lv")
  if(length(vs) > 1){
    cs <- tidySEM:::syntax_cor_lavaan(vs, generic_label = T)
    return(c(x, cs ))
    #gsub("c\\d{1,}", "0", cs)))
  } else {
    return(x)
  }
})

models <- lapply(models, function(x){
  vs <- lavaan:::lavNames(x, type = "lv")
  mns <- paste0(vs, "~ m_", vs, "*1")
  c(x, mns)
})


models <- lapply(models, as_ram)

res <- lapply(models, function(x){try(run_mx(x, data = df))})
names(res) <- names(models)
res[which(sapply(res, inherits, what = "try-error"))] <- NULL

fts <- lapply(res, table_fit)
tab_fits <- do.call(rbind, fts)
res_sat <- omxSaturatedModel(res[[1]], run=TRUE)
fts <- do.call(rbind, lapply(res, function(m){
  tmp = summary(m, refModels=res_sat)
  out <- as.numeric(unclass(tmp)[c("RMSEA", "CFI", "TLI")])
  names(out) <- c("RMSEA", "CFI", "TLI")
  out
}))
tab_fits <- cbind(tab_fits, fts)
write.csv(tab_fits, "tab_fit_lgcmt.csv", row.names = FALSE)

return(res$int_slope_step)
}
