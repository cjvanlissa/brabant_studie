do_detect_shape <- function(df) {
  library(segmented)
  library(chngpt)
  names(df) <- c("x", "y")
  if(!inherits(df[["x"]], what = c("ordered", "numeric", "integer"))){
    return("other")
  }
  if (diff(range(df$y)) < 0.0001) return("none")
  if(nrow(df) == 1) return("none")
  if(nrow(df) == 2) return(c("positive", "negative")[(df$y[1] < df$y[2])+1L])
  res <- list(
    no        = tryCatch(lm(y ~ 1, df), error = function(e) NA),
    linear    = tryCatch(lm(y ~ x, df), error = function(e) NA),
    quadratic = tryCatch(lm(y ~ x + I(x^2), df), error = function(e) NA)
  )

  # Logistic (S-shaped continuous): y = d + L / (1 + exp(-k * (x - x_0)))
  res[["logistic"]] <- tryCatch({
    minpack.lm::nlsLM(y ~ a/(1 + exp(-b * (x-c))), data = df, start=list(a=min(df$y),b=1,c=median(df$x)))
  }, error = function(e) NA)

  res[["step"]] <- tryCatch({
    chngpt::chngptm(
      formula.1 = y ~ 1,
      formula.2 = ~ x,
      family = "gaussian",
      type = "step",
      data = df
    )
  }, error = function(e) NA)

  # BIC with parsimony threshold (Kass & Raftery 1995)
  bics <- sapply(res, function(x) tryCatch(BIC(x), error = function(e) NA))
  if(!is.na(bics[2])){
    names(bics)[2] <- c("positive", "negative")[(res$linear$coefficients[2] < 0)+1L]
  }
  if(!is.na(bics[3])){
    names(bics)[3] <- c("convex", "concave")[(res$quadratic$coefficients[3] < 0)+1L]
  }

  if(!is.na(bics[4])){
    preds <- predict(res$logistic)
    names(bics)[4] <- c("sigmoid", "rev_sigmoid")[(preds[1]>tail(preds, 1))+1L]
  }

  if(!is.na(bics[5])){
    preds <- predict(res$logistic)
    names(bics)[5] <- c("step_up", "step_down")[(preds[1]>tail(preds, 1))+1L]
  }
  return(names(which.min(bics)))
}
