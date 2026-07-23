# Make scales list
brabant_psychometrics <- function(df, dict){
  set.seed(82229)
  library(data.table)
  library(tidySEM)
  library(worcs)
  library(lavaan)
  # For the dependent variable
  df_tmp <- df[, grep("^DAS_\\d", names(df), value = TRUE)]
  df_tmp[] <- lapply(df_tmp, as.integer)
  df_tmp$id <- 1:nrow(df_tmp)
  df_tmp <- data.table(df_tmp)
  df_tmp <- melt(df_tmp, measure.vars = names(df_tmp)[1:(ncol(df_tmp)-1L)], id.vars = "id")
  df_tmp[, var := substr(df_tmp$variable, 1, 6)]
  df_tmp[, wave := gsub("(^DAS_\\d{1,})_(.*)$", "\\2", df_tmp$variable)]
  df_tmp[, variable := NULL]
  df_long <- data.table::dcast(df_tmp, id+wave~var)

  df_wide <- df[, grep("^DAS_\\d", names(df), value = TRUE)]
  df_wide[] <- lapply(df_wide, as.integer)

  df_wide <- VIM::kNN(df_wide)
  df_wide <- df_wide[, !grepl("_imp", names(df_wide), fixed = TRUE)]

  # Make syntax for configural invariance model below
  itms <- names(df_long)[-c(1:2)]
  wavs <- unique(df_long$wave)
  sntx <- do.call(c, lapply(wavs, function(w){
    paste0("DAS_", w, " =~ ", paste0(itms, "_", w, collapse = " + "))
  }))
  res_config <- lavaan::cfa(
    model = sntx,
    data = df_wide,
    ordered = names(df_wide)
  )
  sntx <- do.call(c, lapply(wavs, function(w){
    paste0("DAS_", w, " =~ ", paste0("l", itms, "*", itms, "_", w, collapse = " + "))
  }))
  res_metric <- lavaan::cfa(
    model = sntx,
    data = df_wide,
    ordered = names(df_wide)
  )
  sntx <- do.call(c, lapply(wavs, function(w){
    c(
      paste0("DAS_", w, " =~ ", paste0("l", itms, "*", itms, "_", w, collapse = " + ")),
      paste0(itms, "_", w, "~ m", itms, "*", "1")
    )
  }))
  res_strict <- lavaan::cfa(
    model = sntx,
    data = df_wide,
    ordered = names(df_wide)
  )
  tab_cfa_dv <- table_fit(list(config = res_config, metric = res_metric, strict = res_strict), digits = 2)[,c("Name", "Parameters", "chisq", "df", "cfi", "tli", "rmsea", "srmr")]
  write.csv(tab_cfa_dv, "tab_cfa_dv.csv", row.names = FALSE)


  desc <- worcs::descriptives(df)
  is_ordered <- desc$name[desc$unique < 10]
  df[is_ordered] <- lapply(df[is_ordered], ordered)

  scales_list <- dict$items
  names(scales_list) <- dict$name

  single_item <- sapply(dict$items, length) == 1L
  desc_single <- worcs::descriptives(df[, single_item])
  write.csv(desc_single, "desc_single.csv", row.names = FALSE)

  scales_list <- scales_list[-which(sapply(scales_list, length) < 2L)]


  # Make data long for multilevel CFA
  psychmet <- lapply(names(scales_list), function(scal){
    #scal = names(scales_list)[1]
    indicators <- scales_list[[scal]]

    df_tmp <- df[, indicators]
    df_num <- df_tmp
    df_num[] <- lapply(df_num, as.numeric)

    # Any ordered
    scl_type <- "rowMeans"
    is_ordr <- sapply(df_tmp, inherits, what = "ordered")
    if(all(sapply(df_tmp, function(x)length(table(x))) < 3)){
      df_tmp[] <- lapply(df_tmp, as.numeric)
      df_tmp[] <- df_tmp - 1L
      scores <- rowSums(df_tmp)
      scores[which(scores > 0)] <- 1
      scl_type <- "rowSums"
    }

    res_fa <- try(stats::prcomp(cor(df_num, use = "pairwise.complete.obs")), silent = TRUE)

    res_par <- try(psych::fa.parallel(cor(df_num, use = "pairwise.complete.obs"), n.obs = nrow(df_num)), silent = TRUE)


    fits <- try({
      # CFA
      syntx <- paste0(scal, "=~", paste0(indicators,
                                         collapse = " + "
      ))
      if(length(indicators) == 2){
        syntx <- paste0(scal, "=~", paste0("a*", indicators,
                                           collapse = " + "
        ))
      }

      res <- lavaan::cfa(
        model = syntx,
        data = df_tmp,
        ordered = if(any(is_ordr)){names(df_tmp)[is_ordr]} else {NULL},
        std.lv = TRUE,
        auto.fix.first = FALSE
      )


      tb <- tidySEM::table_fit(res)[, c("Parameters", "chisq", "df", "cfi", "tli", "rmsea", "srmr")]
      tb$comp_rel <- semTools::compRelSEM(res, ord.scale = any(is_ordr))
      tb
    }, silent = TRUE)
    if(inherits(fits, "try-error")){
      fits <- data.frame(Parameters = NA, chisq = NA, df = NA,
                         cfi = NA, tli = NA, rmsea = NA,
                         srmr = NA, comp_rel = NA)
    }
    tab <- data.frame(variable = scal,
                      items = length(indicators),
                      type = scl_type,
                      fits)


    tab$kaiser <- ifelse(inherits(res_fa, "try-error"), NA, sum(res_fa$sdev^2 > 1))
    tab$par_factors <- ifelse(inherits(res_par, "try-error"), NA, res_par$nfact)
    tab$par_components <- ifelse(inherits(res_par, "try-error"), NA, res_par$ncomp)
    return(tab)
  })

  tab_psychometrics <- do.call(rbind, psychmet)

  return(tab_psychometrics)
}
