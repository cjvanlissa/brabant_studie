prepare_riclpms <- function(df_anal, iv_dv){
  library(OpenMx)
  yvars <- iv_dv$yvars
  predvar <- iv_dv$predvar
  out <- vector("list", length(yvars))
  names(out) <- yvars
  # Define RI-CLPM model ----------------------------------------------------
  make_riclpm <- function(df){
    
    xobs <- grep("^x", names(df), value = TRUE)
    yobs <- grep("^y", names(df), value = TRUE)
    allx <- paste0("c", grep("^x", names(df), value = TRUE))
    ally <- paste0("c", grep("^y", names(df), value = TRUE))
    
    return(mxModel("riclpm",
                   type="RAM",
                   manifestVars=c(xobs, yobs),
                   latentVars = c("RIx", "RIy", allx, ally),
                   mxData(observed=df, type="raw"),
                   mxPath(from="RIx", to=xobs, arrows=1, free=FALSE, values=1),
                   mxPath(from="RIy", to=yobs, arrows=1, free=FALSE, values=1),
                   
                   mxPath(from=allx, to=xobs, arrows=1, free=FALSE, values=1),
                   mxPath(from=ally, to=yobs, arrows=1, free=FALSE, values=1),
                   
                   # RI cors
                   mxPath(from="RIy", to="RIx", arrows=2, free=TRUE, values=0, lbound = .0001),
                   mxPath(from=c("RIy", "RIx"), arrows=2, free=TRUE, values=1.5, lbound = .0001),
                   
                   mxPath(from=allx[-length(allx)], to=allx[-1], arrows=1, free=TRUE, values=c(.1), labels="x_x"),
                   mxPath(from=ally[-length(ally)], to=ally[-1], arrows=1, free=TRUE, values=c(.1), labels="y_y"),
                   mxPath(from=allx[-c(length(ally):max(c(length(ally), length(allx))))],
                          to=ally[-1],
                          arrows=1, free=TRUE, values=c(.1), labels="yonx"),
                   mxPath(from=ally[-c(length(allx):max(c(length(allx), length(ally))))],
                          to=allx[-1],
                          arrows=1, free=TRUE, values=c(.1), labels="xony"),
                   # Vars
                   mxPath(from=allx[1], arrows=2, free=TRUE, values=c(.1), labels = "vx1", lbound = .0001),
                   mxPath(from=ally[1], arrows=2, free=TRUE, values=c(.1), labels = "vy1", lbound = .0001),
                   
                   mxPath(from=allx[-1], arrows=2, free=TRUE, values=c(.1), labels = "vx", lbound = .0001),
                   mxPath(from=ally[-1], arrows=2, free=TRUE, values=c(.1), labels = "vy", lbound = .0001),
                   #mxPath(from=c(xobs, yobs), arrows=2, free=TRUE, values=c(.1)),
                   mxPath(from=c(xobs, yobs), arrows=2, free=FALSE, values=0),
                   # Covars
                   mxPath(from=allx[1], to = ally[1], arrows=2, free=TRUE, values=0, labels = "rxy1"),
                   mxPath(from=allx[2:min(c(length(allx), length(ally)))],
                          to = ally[2:min(c(length(allx), length(ally)))],
                          arrows=2, free=TRUE, values=0, labels = "rxy"),
                   
                   mxPath(from = 'one', to = c(allx, ally))
    ))
  }
  
  remove_predictors <- c("war_(.)_m" = "warmth_m", "war_(.)_f"= "warmth_f", "con_(.)_m" = "consistent_m", "con_(.)_f" = "consistent_f", "ang_(.)_m" = "anger_m", "ang_(.)_f" = "anger_f", "mon_(.)_m" = "monitoring_m", "mon_(.)_f" = "monitoring_f", "ind_(.)_m" = "inductive_reasoning_m", "ind_(.)_f" = "inductive_reasoning_f")
  
  # Run for each yvar ----------------------------------------------
  
  for(yvar in yvars){
    # dropthese <- yvars[!yvars == yvar]
    # dropthese <- unlist(lapply(dropthese, grep, x = names(df_anal), value = TRUE))
    #df <- data.frame(df_anal[, -match(dropthese, names(df_anal))])
    df <- as.data.frame(df_anal)
    names(df) <- gsub("emo_(.)_c", "y\\1", names(df))
    names(df) <- gsub(yvar, "x\\1", names(df))
    df[[remove_predictors[yvar]]] <- NULL
    # Remove other parenting variables
    df[grep("^\\w{3}_\\d_[mf]$", names(df))] <- NULL
    out[[yvar]]$df <- df
    model <- make_riclpm(df[, grep("^[xy]\\d", names(df))])
    model <- mxAutoStart(model)
    fit <- try(mxRun(model))
    # yvars9 results in errors often so use extratries, and skip entirely if that doesn't help
    if (inherits(fit, "try-error")) {
      fit <- try(mxTryHard(model, extraTries = 100) )
    }
    if (inherits(fit, "try-error")) {
      next
    }
    out[[yvar]]$model <- fit
  }
  fits <- do.call(rbind, lapply(out, function(x){
    res <- x$model
    refmodels <- mxRefModels(res, run = TRUE)
    rmsea <- omxRMSEA(res, refModels = refmodels)["est.rmsea"]
    sums <- summary(res, refModels = mxRefModels(res, run = TRUE))
    tab_res <- table_results(res, columns = NULL)
    data.frame(min2ll = sums$Minus2LogLikelihood, chi2 = sums$Chi, df = sums$ChiDoF, RMSEA = sums$RMSEA, CFI = sums$CFI, TLI = sums$TLI, tab_res[which(tab_res$openmx_label == "yonx")[1], c("est", "confint")])
  }))
  fits <- cbind(model = gsub("_(.)", "", rownames(fits), fixed = T), fits)
  rownames(fits) <- NULL
  write.csv(fits, "riclpm_fits.csv", row.names = FALSE)
}