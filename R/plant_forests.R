plant_forests <- function(df_features, growth_model){
  library(semtree)
  library(future)

  # Set controls ------------------------------------------------------------

  controls <- semforest.control()
  controls$num.trees <- 30 # number of trees to grow
  controls$sampling <- "bootstrap"
  controls$mtry <- ceiling(sqrt(sum(!grepl("^DAS_", names(df_features)))))
  controls$semtree.control
  controls$semtree.control$alpha <- 0.05
  controls$semtree.control$min.bucket <- (table_fit(growth_model)$Parameters*10L)
  controls$semtree.control$method <- "score"
  controls$semtree.control$exclude.heywood <- TRUE

  # Set constraints ---------------------------------------------------------
  cnst <- semtree.constraints(focus.parameters = "m_step")

  names(df_features)[grep("^DAS_", names(df_features))] <- paste0("DAS", seq_along(grep("^DAS_", names(df_features))))
  df_features[c("DAS11", "DAS12")] <- NULL


  # Prepare chunks
  set.seed(8178)

  RNGkind("L'Ecuyer-CMRG")
  seeds <- vector("list", 20)
  seeds[[1]] <- .Random.seed
  for(i in 2:length(seeds)){
    seeds[[i]] <- parallel::nextRNGStream(seeds[[i-1]])
  }

  # This is where parallel computing starts
  library(doSNOW)
  cl <- makeCluster(30)
  registerDoSNOW(cl)
  plan(multisession)
  filenames <- vector("character", length(seeds))
  cat("Start: ", as.character(Sys.time()), file = "forest_log.log", append = FALSE)

  for(chunk in seq_along(seeds)){
    # Set random seed
    .Random.seed <- seeds[[chunk]]
    i = 1
    while(i < 5){
      res_rf <- semforest(model = growth_model,
                          data = df_features,
                          control = controls,
                          constraints = cnst)
      i = i + 1
      if(!inherits(res_rf, "try-error")) break
    }
    if(!inherits(res_rf, "try-error")){
      fnam <- paste0(chunk, ".RData")
      saveRDS(res_rf, paste0("forest_", fnam))
      vim <- semtree::varimp(res_rf, method = "permutationFocus")
      saveRDS(vim, paste0("vim_", fnam))
      filenames[chunk] <- fnam
    } else {
      filenames[chunk] <- NA
    }
    rm(res_rf, vim)
    gc()
  }
  # Stop parallel computing and prepare output file
  parallel::stopCluster(cl)
  rm(cl)
  attr(filenames, "start_date") <- as.character(Sys.time())
  return(filenames)
}


.merge_varimp <- function (varimp_list){
  numtrees <- sapply(varimp_list, function(x){length(x$ll.baselines)})
  numfeatures <- sapply(varimp_list, function(x){dim(x$importance)[2]})
  varnames <- lapply(varimp_list, `[[`, "var.names")
  varnames_unique <- unique(unlist(varnames))
  out <- list(
    ll.baselines = vector("numeric", sum(numtrees)),
    importance = matrix(nrow = sum(numtrees), ncol = length(varnames_unique)),
    elapsed = varimp_list[[1]]$elapsed,
    var.names = varnames_unique
  )
  colnames(out$importance) <- varnames_unique
  for (i in 2:length(varimp_list)) {
    out$elapsed <- out$elapsed + varimp_list[[i]]$elapsed
  }
  index_trees <- c(0, numtrees, 0)
  for (i in 1:length(varimp_list)) {
    indcs <- (sum(index_trees[1:i])+1):sum(index_trees[1:i+1])
    out$ll.baselines[indcs] <- varimp_list[[i]]$ll.baselines
    newvalues <- matrix(NA,
                        nrow = nrow(varimp_list[[i]]$importance),
                        ncol = length(varnames_unique))
    colnames(newvalues) <- varnames_unique
    for(v in colnames(varimp_list[[i]]$importance)){
      newvalues[, v] <- varimp_list[[i]]$importance[, v]
    }
    #newvalues[, match(colnames(varimp_list[[i]]$importance), varnames_unique)] <- varimp_list[[i]]$importance
    out$importance[indcs, ] <- newvalues
  }
  class(out) <- class(varimp_list[[1]])
  return(out)
}

merge_forests_internal <- function(flz){
  num.forests <- length(flz)
  forest <- flz[[1]]
  numtrees <- sapply(flz, function(x){length(x$forest)})
  forest$forest <- vector("list", sum(numtrees))
  forest$forest.data <- vector("list", sum(numtrees))
  forest$seeds <- rep(NA, sum(numtrees))

  index_trees <- c(0, numtrees, 0)
  for (i in 1:length(flz)) {
    indcs <- (sum(index_trees[1:i])+1):sum(index_trees[1:i+1])
    forest$forest[indcs] <- flz[[i]]$forest
    forest$forest.data[indcs] <- flz[[i]]$forest.data
    forest$elapsed <- forest$elapsed + flz[[i]]$elapsed
  }
  forest$control$num.trees <- sum(numtrees)
  forest$merged <- TRUE
  return(forest)
}

merge_forests <- function(fn_forests){
  # Merge
  f <- list.files(pattern = "forest_\\d{1,}")
  dts <- file.info(f)[["ctime"]]
  f <- f[order(dts, decreasing = TRUE)][1:20]
  fnam <- ".RData"
  library(doSNOW)


    # Merge Forests
    flz <- lapply(f, function(i){ readRDS(i[1]) })
    res_rf <- merge_forests_internal(flz)
    rm(flz)
    nullforests <- sapply(res_rf$forest, is.null)
    if(any(nullforests)) res_rf$forest <- res_rf$forest[!nullforests]
    saveRDS(res_rf, paste0("forest_merged", fnam))

    # Strip forest
    library(future)
    cl <- makeCluster(30)
    registerDoSNOW(cl)
    plan(multisession)
    res_light <- semtree::strip(res_rf, parameters = "m_step")
    saveRDS(res_light, paste0("forest_light", fnam))
    rm(res_rf)
    rm(res_light)
    gc()


  # VIM
  f <- list.files(pattern = "^vim_\\d{1,}")
  dts <- sapply(f, function(x){file.info(x)[["ctime"]]})
  f <- f[order(dts, decreasing = TRUE)][1:20]

    # VIM

    flz <- lapply(f, readRDS)

    vim_merged <- .merge_varimp(flz)
    rm(flz)
    saveRDS(vim_merged, paste0("vim_merged", fnam))



  # Stop parallel computing and prepare output file
  parallel::stopCluster(cl)
  rm(cl)
  return(list(
    forest = paste0("forest_merged", fnam),
    forest_light = paste0("forest_light", fnam),
    vim = paste0("vim_merged", fnam),
    time = as.character(Sys.time())
  ))
}

aggregate_vim <- function(fn_merged){
  f <- fn_merged$vim

  vimps <- readRDS(f)
  out <- semtree:::aggregateVarimp(vimps, aggregate = "median", scale = "absolute", TRUE)
  out <- out[order(out, decreasing = TRUE)]
  out[is.na(out)] <- 0
  vimps <- as.data.frame(out)
  vimps$Variable <- rownames(vimps)
  vimps
}

plot_vimps <- function(pdps){
  library(svglite)
  library(ggplot2)
    pdps <- pdps[pdps$out > 0,]
    #pdps$Variable <- gsub("_\\d{1,}$", "", pdps$Variable)
    pdps$label <- factor(pdps$shapes,
                         levels = c("positive", "negative", "convex", "concave", "step_up", "step_down", "rev_sigmoid", "other"),
                         labels = c("p", "n", "cv", "cn", "su", "sd", "rs", "o"))
    pdps$Variable <- factor(pdps$Variable, levels = rev(pdps$Variable))
    p = ggplot(pdps, aes(x = out, y = Variable, label = label)) +
      geom_segment(aes(xend = 0, yend = Variable), linetype = 2) +
      geom_text(vjust = .2, hjust = 0) +
      xlab("Variable Importance (Permutation importance)") +
      theme_bw() + theme(panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(), axis.title.y = element_blank())
    saveRDS(p, "vim.RData")
    ggplot2::ggsave("vim.svg", p, device = "svg", width = 6, height = 8, units = "in")

  return(p)
}

plot_vimps_paper <- function(vimps, shapes){
  library(ggplot2)
  library(ggpubr)
  vimplot_list <- lapply(c("war_m", "war_f", "ang_m", "ang_f", "ind_m", "ind_f"), function(nam){
    x <- vimps[, c(nam, "Variable")]
    shaps <- shapes[which(shapes$Variable == nam), ]
    shaps <- shaps[match(x$Variable, shaps$Moderator), ]
    x$Shape <- ordered(shaps$Shape, levels = c("Positive", "Negative", "None", "Other"))
    x$Importance <- x[[nam]]
    x$Moderator <- ordered(x$Variable, levels = x$Variable[order(x$Importance, decreasing = FALSE)])
    x$Variable <- substr(nam, 1, 3)
    x$Facet <- c(war_m = "Warmth (m)", war_f = "Warmth (f)", ang_m = "Anger (m)", ang_f = "Anger (f)", ind_m = "Ind. Reas. (m)", ind_f = "Ind. Reas. (f)")[nam]
    x$Parent <- substr(nam, 5, 5)
    x <- x[order(x$Importance, decreasing = TRUE), ][c(1:10), c("Moderator", "Importance", "Shape", "Facet")]
    #x$Moderator <- droplevels(x$Moderator)

    ggplot(x, aes(y = Moderator, x = Importance, shape = Shape)) +
      geom_segment(aes(x = 0, xend = Importance, y = Moderator, yend = Moderator), colour = "grey50", linetype = 2) +
      geom_point(size = 2) + xlab("Importance") +
      # scale_x_continuous(expand = c(0,.5))+
      #scale_y_discrete(breaks = c(1:10), labels = x$Moderator[x$y])+
      scale_shape_manual(values = c("Negative" = 6, "Positive" = 2, None = 1, Other = 0))+
      theme_bw() + theme(legend.position = "none", panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(), axis.title.y = element_blank())+
      facet_wrap(~Facet)


  })
  vimplot_list[[1]] <- vimplot_list[[1]] + theme(legend.position = c(.8, .25))
  p <- ggarrange(plotlist = vimplot_list,
                 labels = letters[seq_along(vimplot_list)],
                 ncol = 2, nrow = 3)

  ggsave(paste0("vim_plot_paper.svg"), p, device = "svg", width = 8.27, height = 11.69, units = "in")
  return("vim_plot_paper.svg")
}

# plot_pdps_paper <- function(pdps, vimps){
#   library(ggplot2)
#   library(ggpubr)
#
#   for(pred in names(vimps)[-ncol(vimps)]){
#     var_order <- head(vimps$Variable[order(vimps[[pred]], decreasing = TRUE)], 10)
#     pdps <- paste0("pdp_", pred, "_", var_order, ".svg")
#     pdps$var
#
#   }
#   vimplot_list <- lapply(c("war_m", "war_f", "ang_m", "ang_f", "ind_m", "ind_f"), function(nam){
#     x <- vimps[, c(nam, "Variable")]
#     shaps <- shapes[which(shapes$Variable == nam), ]
#     shaps <- shaps[match(x$Variable, shaps$Moderator), ]
#     x$Shape <- ordered(shaps$Shape, levels = c("Positive", "Negative", "None", "Other"))
#     x$Importance <- x[[nam]]
#     x$Moderator <- ordered(x$Variable, levels = x$Variable[order(x$Importance, decreasing = FALSE)])
#     x$Variable <- substr(nam, 1, 3)
#     x$Facet <- c(war_m = "Warmth (m)", war_f = "Warmth (f)", ang_m = "Anger (m)", ang_f = "Anger (f)", ind_m = "Ind. Reas. (m)", ind_f = "Ind. Reas. (f)")[nam]
#     x$Parent <- substr(nam, 5, 5)
#     x <- x[order(x$Importance, decreasing = TRUE), ][c(1:10), c("Moderator", "Importance", "Shape", "Facet")]
#     #x$Moderator <- droplevels(x$Moderator)
#
#     ggplot(x, aes(y = Moderator, x = Importance, shape = Shape)) +
#       geom_segment(aes(x = 0, xend = Importance, y = Moderator, yend = Moderator), colour = "grey50", linetype = 2) +
#       geom_point(size = 2) + xlab("Importance") +
#       # scale_x_continuous(expand = c(0,.5))+
#       #scale_y_discrete(breaks = c(1:10), labels = x$Moderator[x$y])+
#       scale_shape_manual(values = c("Negative" = 6, "Positive" = 2, None = 1, Other = 0))+
#       theme_bw() + theme(legend.position = "none", panel.grid.major.x = element_blank(),
#                          panel.grid.minor.x = element_blank(), axis.title.y = element_blank())+
#       facet_wrap(~Facet)
#
#
#   })
#   vimplot_list[[1]] <- vimplot_list[[1]] + theme(legend.position = c(.8, .25))
#   p <- ggarrange(plotlist = vimplot_list,
#                  labels = letters[seq_along(vimplot_list)],
#                  ncol = 2, nrow = 3)
#
#   ggsave(paste0("vim_plot_paper.svg"), p, device = "svg", width = 8.27, height = 11.69, units = "in")
#   return("vim_plot_paper.svg")
# }

# plot_vimp_agg <- function(vimps){
#   vimps_mat <- as.matrix(vimps[, -ncol(vimps)])
#   vimps_rank <- apply(vimps_mat, 2, function(x){(length(x)+1L)-rank(x)})
#   vimps_rescaled <- apply(vimps_mat, 2, function(x){ x[x < 0] <- 0; x / max(x)})
#
#   df_plot <- data.frame(
#     Importance = as.vector((vimps_rescaled)),
#     Moderator = ordered(vimps$Variable, levels = rownames(vimps_mat)[order(rank(rowMeans(vimps_rank)), decreasing = TRUE)]),
#     Predictor = rep(substr(colnames(vimps_mat), 1,3), each = nrow(vimps_mat)),
#     Parent = rep(substr(colnames(vimps_mat), 5,5), each = nrow(vimps_mat))
#   )
#
#   library(ggplot2)
#   ggplot(df_plot)+
#     geom_point(aes(x = Importance, y = Moderator, shape = Parent, colr = Predictor))
#     #geom_segment(aes(x = 0, xend = Importance, y = Moderator, yend = Moderator))
#   for(var in thevars){
#     vi <- list(variable.importance = vimps[[var]])
#     names(vi$variable.importance) <- vimps$Variable
#     class(vi) <- "ranger"
#     p <- metaforest::VarImpPlot(vi, n.var = length(vi$variable.importance))
#     saveRDS(p, paste0("vim_", var, ".RData"))
#     ggplot2::ggsave(paste0("vim_", var, ".svg"), p, device = "svg", width = 6, height = 8, units = "in")
#   }
#   return(paste0("vim_", thevars, ".svg"))
# }

#   vim <- readRDS("Outputs/plantingforest/vim_full_forest_test.RData")
#

create_pdp <- function(merged_summarydata, df_features, vimps){
  library(semtree)
  library(future)

  vimps <- vimps[vimps$out > 0, ]
  # Prepare chunks
  max_cores <- 30


  set.seed(23828)
  RNGkind("L'Ecuyer-CMRG")
  seeds <- vector("list", nrow(vimps))
  seeds[[1]] <- .Random.seed
  for(i in 2:length(seeds)){
    seeds[[i]] <- parallel::nextRNGStream(seeds[[i-1]])
  }
  vimps$seed <- seeds
  vimps$filename <- NA
  # This is where parallel computing starts
  library(doSNOW)
  cl <- makeCluster(30)
  registerDoSNOW(cl)
  plan(multisession)
  forest <- readRDS(merged_summarydata$forest_light)

  shapes <- foreach(thisrep = 1:nrow(vimps), .packages = c("semtree"), .export = "do_detect_shape", .combine = "c") %dopar% {
    #attach(summarydata[thisrep, ]) # Select thisrep-th non-run chunk
    # Set random seed
    .Random.seed <- vimps$seed[[thisrep]]
      pdp <- try(semtree::partialDependence(
        x = forest,
        data = df_features,
        reference.var = vimps$Variable[thisrep],
        support = 20
      ))
      fnam <- paste0("pdp_", vimps$Variable[thisrep], ".RData")
      if(inherits(pdp, "try-error")){
        list(NULL)
      }
      saveRDS(pdp, fnam)
      shap <- do_detect_shape(pdp$samples)
      shap
    }
    rm(forest)
    gc()


  # Stop parallel computing and prepare output file
  parallel::stopCluster(cl)
  rm(cl)
  vimps$filename <- paste0("pdp_", vimps$Variable, ".RData")
  vimps$shapes <- shapes
  attr(vimps, "date") <- as.character(Sys.time())
  return(vimps)
}

plot_pdps <- function(pdps){
  #xvars <- unique(substr(pdps$vnam, 14, 18))
  ranges <- c(NA, NA)
  #names(ranges) <- xvars
  for(i in 1:nrow(pdps)){
    with(as.list(pdps[i, ]), {
      pdp <- readRDS(filename)
      if(!isFALSE(ranges[1] > min(pdp$samples$m_step))) ranges[1] <<- min(pdp$samples$m_step)
      if(!isFALSE(ranges[2] < max(pdp$samples$m_step))) ranges[2] <<- max(pdp$samples$m_step)
    })
  }

  library(ggplot2)
  for(i in 1:nrow(pdps)){
    with(as.list(pdps[i, ]), {
      pdp <- readRDS(filename)
      p <- semtree:::plot.partialDependence(pdp, parameter = "m_step") +
        theme(plot.title = element_blank())+
        theme(legend.position = "none")
      if(any(names(p@layers) == "geom_bar")){
        if(levels(p@data[[1]])[1] == "Yes"){
          levels(p@data$uncluttered) <- rev(levels(p@data$uncluttered))
        }
        p <- p + geom_point() + geom_line(alpha = .2, linetype = 2, group = 1)
        p@layers[["geom_bar"]] <- NULL
      }

      #p + scale_x_discrete(breaks = c("No", "Yes"))
      p <- p + scale_y_continuous(limits = ranges)
      ggplot2::ggsave(gsub(".RData", ".svg", filename, fixed = TRUE), p, device = "svg")
    })
  }

  pdps$filenames <- paste0("pdp_", gsub("^.{0,}forest_light_(.+?)_\\d.*$", "\\1", pdps$vnam), "_", pdps$var, ".svg")
  return(pdps)
}



# Determine sign ----------------------------------------------------------

shape_fun <- function(x){
  #x = pdp$samples[["m_step"]]
  if(length(x) > 3){
    x_smoothed <- smooth(x, kind = "3")
  } else {
    x_smoothed <- x
  }
  if(any(is.na(x_smoothed))) x_smoothed <- x
  # plot(1:length(x), smooth(x, kind = "3"), type = "b")
  sns <- ordered(sign(diff(as.numeric(x_smoothed))), levels = c("0", "1", "-1"))
  sns_tab <- table(sns)
  if(all(sns == 0)) return("None")
  if(sns_tab["1"] == 0) return("Negative")
  if(sns_tab["-1"] == 0) return("Positive")
  sns_nozero <- sns[-which(sns == 0)]
  sns_rle <- rle(as.integer(as.character(sns_nozero)))$values
  if(length(sns_rle) == 2){
    if(all(sns_rle == c(1, -1))) return("Concave")
    if(all(sns_rle == c(-1, 1))) return("Convex")
  }
  return("Other")
}

get_shapes <- function(pdps){
  pdps <- list.files(pattern = "^pdp_.+RData$")

  # Sets up clusters from number of cores
  cl <- snow::makeCluster((parallel::detectCores()-2L))

  # Create parallel backend for foreach
  doSNOW::registerDoSNOW(cl)
  library(foreach)
  pred_scales <- foreach(fnam = pdps, .export = "shape_fun") %dopar% {
    pdp <- readRDS(fnam)
    shap <- shape_fun(pdp$samples[["m_step"]])
    data.frame(Variable = substr(fnam, 5, 9),
               Moderator = gsub(".RData", "", substring(fnam, first = 11), fixed = TRUE),
               Shape = shap)
  }
  shapes <- do.call(rbind, pred_scales)
  return(shapes)
}

# Variable importance -----------------------------------------------------

# vimps <- list.files(pattern = "^vim.+RData")
# vars <- substr(vimps, 5, 9)
# vimps <- lapply(vimps, readRDS)
# names(vimps) <- vars
# rankings <- lapply(vimps, function(x){rev(as.character(x$data$Variable))})

get_merged_vimp <- function(vimps, shapes){
  vimps[] <- lapply(vimps, function(x){x[is.na(x)] <- min(x, na.rm = TRUE); x})
  rankings <- lapply(vimps[, -ncol(vimps)], function(x){ rownames(vimps)[order(x, decreasing = TRUE)] })

  rankings_merged <- TopKLists::Borda(rankings)
  top_moderators <- rankings_merged$TopK$median
  vars <- colnames(vimps)[-ncol(vimps)]
  manual_shapes <- read.table("manual_shapes.txt", sep = "\n")[[1]]
  manual_shapes <- split(manual_shapes, factor(rep(vars, each = 10)))
  # Reverse code effect of anger
  manual_shapes$ang_f <- as.character(factor(manual_shapes$ang_f, levels = c( "p", "n", "o", "no"), labels = c("n", "p", "o", "no")))
  manual_shapes$ang_m <- as.character(factor(manual_shapes$ang_m, levels = c( "p", "n", "o", "no"), labels = c("n", "p", "o", "no")))
  values <- do.call(rbind, lapply(vars, function(nam){
    #x <- vimps[[nam]]$data
    # x$Moderator <- x$Variable
    x <- vimps[, c(nam, "Variable")]
    shaps <- shapes[which(shapes$Variable == nam), ]
    shaps <- shaps[match(x$Variable, shaps$Moderator), ]
    x$Shape <- shaps$Shape
    x$Importance <- (nrow(x)+1L)-rank(x[[nam]]) #1:nrow(x) #x$importance/max(x$importance)
    x$Moderator <- x$Variable
    x$Variable <- substr(nam, 1, 3)
    x$Parent <- substr(nam, 5, 5)
    names(x)[1] <- "vimp"
    x <- x[order(x$Importance), ]
    x$Shape_manual <- NA
    x$Shape_manual[1:10] <- manual_shapes[[nam]]
    x[, c("Moderator", "Importance", "vimp", "Variable", "Parent", "Shape", "Shape_manual")]
  }))

  write.csv(values, "vim_combined_shapes.csv", row.names = FALSE)

  # Plot combined rankings --------------------------------------------------

  library(ggplot2)
  values$Moderator <- ordered(values$Moderator, levels = rev(rankings_merged$TopK$mean))
  values$Parent <- ordered(values$Parent, levels = c("m", "f"), labels = c("Mother", "Father"))
  relab <- c("ang" = "Anger", "con" = "Consistent", "ind" = "Induct.Reas.", "mon" = "Monitoring", "war" = "Warmth")
  values$Variable <- factor(relab[values$Variable])
  set.seed(1)
  p <- ggplot(data = values, aes(x = Importance, y = Moderator, color = Variable, shape = Shape)) +
    geom_point(position = position_jitter(w = 0.2, h = 0)) +
    theme_bw() +
    scale_x_reverse() +
    facet_wrap(~Parent)
  # New
  # values$Moderator <- ordered(values$Moderator, levels = rev(rankings_merged$TopK$mean))
  # values$Parent <- ordered(values$Parent, levels = c("m", "f"), labels = c("Mother", "Father"))
  # relab <- c("ang" = "Anger", "con" = "Consistent", "ind" = "Induct.Reas.", "mon" = "Monitoring", "war" = "Warmth")
  # values$Variable <- factor(relab[values$Variable])
  # set.seed(1)
  df_plot <- values[values$Moderator %in% top_moderators[1:15], ]
  df_plot$Moderator <- ordered(df_plot$Moderator, levels = rev(top_moderators))
  df_plot$text <- tolower(paste0(substr(df_plot$Parent, 1, 1), substr(df_plot$Variable, 1, 1)))
  p <- ggplot(data = df_plot, aes(x = Importance, y = Moderator, shape = text)) +
    geom_point() +
    theme_bw()
  ggsave("vim_combined_plot.svg", p, device = "svg", width = 12, height = 13, units = "in")



  # Combine vim plots -------------------------------------------------------

  df_plot <- values[values$Importance %in% c(1:10), ]
  df_plot$Importance <- 11 - df_plot$Importance
  df_plot$Variable <- ordered(df_plot$Variable, levels = c("Warmth", "Anger", "Induct.Reas."), labels = c("Warmth", "Anger", "Ind. Reas."))
  p <- ggplot(df_plot, aes(x = vimp, y = Importance)) +
    geom_segment(x = 0, aes(xend = vimp), linetype = 2, color = "grey70") +
    geom_text(aes(label = Shape_manual)) +
    # geom_point(aes(shape = Shape_manual)) +
    # geom_point(data = df_plot[df_plot$Shape_manual == "cv", ], shape ="\u25D2") +
    # scale_shape_manual(values = c("p" = 24, "n" = 25, "no" = 0, "cv" = "\u25D2", "cn" = "\u25D3"))+
    geom_text(aes(label = Moderator, x = -.5, y = Importance), hjust = 1) +
    scale_x_continuous(limits = c(-25, 34))+
    geom_vline(xintercept = 0)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())+
    facet_grid(Variable ~Parent)
  ggsave(paste0("vim_mf_pageplot.svg"), p, device = "svg", width = 8.27, height = 7, units = "in")

  #  vimps <- list.files(pattern = "^vim.+RData")
  #  vars <- substr(vimps, 5, 9)
  #  vimps <- lapply(vimps, readRDS)
  #  names(vimps) <- vars
  #  library(ggpubr)
  #
  #  var_labs = c("Anger", "Consistent", "Induct.Reas.", "Monitoring", "Warmth")
  #  names(var_labs) <- c("ang", "con", "ind", "mon", "war")
  #  var_labs <- var_labs[var_labs %in% levels(values$Variable)]
  #  par_labs <- c("Mother", "Father")
  #  names(par_labs) <- c("m", "f")
  #  df_plot <- lapply(paste0(rep(names(var_labs), each = 2), "_", rep(names(par_labs), length(names(var_labs)))), function(nam){
  #    # Keep only top 30 predictors
  #    vimps[[nam]]$data <- tail(vimps[[nam]]$data, 30)
  #    vimps[[nam]] + xlab(paste0(var_labs[substr(nam, 1, 3)], " (", substr(nam, 5, 5), ")"))
  #  })
  #  p <- ggarrange(plotlist = df_plot,
  #                 labels = letters[seq_along(df_plot)],
  #                 ncol = length(par_labs), nrow = length(var_labs))
  #
  # ggsave(paste0("vim_mf_pageplot.svg"), p, device = "svg", width = 8.27, height = 11.69, units = "in")
  #
  return(
    list(
      vim_table = "vim_combined_shapes.csv",
      vim_pic = "vim_combined_plot.svg",
      vim_by_parent = "vim_mf_pageplot.svg"
    )
  )
}



