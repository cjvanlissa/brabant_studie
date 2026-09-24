plot_pdp_curves <- function(pdps, df_features, growth_model){
  library(ggplot2)
  #xvars <- unique(substr(pdps$vnam, 14, 18))
  ranges <- c(NA, NA)
  t_int <- c(DAS_12 = -0.28, DAS_20 = -0.2, DAS_28 = -0.12, DAS_8wPP = 0.08,
    DAS_6mPP = 0.26, DAS_1yPP = 0.52, DAS_1y6mPP = 0.78, DAS_2yPP = 1.04,
    DAS_2y6mPP = 1.3, DAS_3yPP = 1.56)
  names(t_int) <- gsub("^.+_", "", names(t_int))
  names(t_int) <- gsub("PP", "", names(t_int))
  names(t_int)[1:3] <- paste0("-", names(t_int)[1:3])
  loadings <- growth_model$A$values[1:10, -c(1:10)]
  #names(ranges) <- xvars
  plot_list <- lapply(1:nrow(pdps), function(i){

      pdp <- readRDS(pdps$filename[i])
      v <- df_features[[names(pdp$samples)[1]]]
      df_plot <- pdp$samples
      if(inherits(v, c("numeric", "integer"))){
        med <- median(v)
        sdv <- sd(v)
        selecthese <- sapply(c(med-sdv, med, med+sdv), function(x){which.min((pdp$samples[[1]] -x)^2)})
        df_plot <- df_plot[selecthese, ]
        df_plot[[1]] <- ordered(c("Low", "Median", "High"), levels = c("Low", "Median", "High"))
      }
      out <- do.call(rbind, lapply(1:nrow(df_plot), function(i){
        estimates <- matrix(as.numeric(df_plot[i, -1]), nrow = nrow(loadings),
                            ncol = 3, byrow = TRUE)
        data.frame(Predictor = names(df_plot)[1], df_plot[i, ], Time = t_int, Curve_Value = rowSums(loadings * estimates))
      }))
      names(out)[2] <- "Predictor_Value"
      get_shape <- FALSE
      if(inherits(v, c("numeric", "integer"))){
        get_shape <- TRUE
      } else {
        if(inherits(v, "factor")){
          if(all(levels(out$Predictor_Value) %in% c(0,1,"Yes", "No"))){
            get_shape <- TRUE
          }
        }
      }
      if(get_shape){
        low <- out[out$Predictor_Value == out$Predictor_Value[1], ]
        low <- sum(rowSums(embed(low$Curve_Value, 2)) * diff(low$Time)) / 2
        hi <- out[out$Predictor_Value == tail(out$Predictor_Value, 1), ]
        hi <- sum(rowSums(embed(hi$Curve_Value, 2)) * diff(hi$Time)) / 2
        out$shape <- c("Positive", "Negative")[(hi<low)+1L]
      } else {
        out$shape = "Other"
      }
      out
    })
  ranges <- sapply(plot_list, function(x){c(min(x$Curve_Value), max(x$Curve_Value))})
  ranges <- c(min(ranges[1, ]), max(ranges[2, ]))
  colz <- 5; rowz <- 10
  plots <- lapply(1:length(plot_list), function(i){
    df_plot <- plot_list[[i]]
    p <- ggplot(df_plot, aes(x =Time, y = Curve_Value, linetype = Predictor_Value)) +
      geom_path() +
      scale_x_continuous(breaks = t_int, labels = names(t_int)) +
      theme_bw()+
      theme(axis.title = element_blank(), legend.position = c(.85,.85))
    # if(i > 1 & all(levels(df_plot$Predictor_Value) == c("Low", "Median", "High"))){
    #   p <- p + theme(legend.position = "none")
    # }
    # if(i < (length(plot_list)-colz)+1L){
    #   p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    # }
    # if(!(i %% colz == 1)){
    #   p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    # }
    p <- p + facet_wrap(~ Predictor)
    ggsave(paste0("pdp_plot_", df_plot$Predictor[1], ".svg"), p, device = "svg")
    p
  })
  # p <- ggpubr::ggarrange(plotlist = plots, ncol = colz, nrow = rowz)
  # ggsave("pdp_plot.pdf", p, device = "pdf", width = 210, height = 297, units = "mm", dpi = 600)
  pdps$shape <- sapply(plot_list, function(x){x$shape[1]})
  return(pdps)
}

#   library(ggplot2)
#   for(i in 1:nrow(pdps)){
#     with(as.list(pdps[i, ]), {
#       pdp <- readRDS(filename)
#       p <- semtree:::plot.partialDependence(pdp, parameter = "m_step") +
#         theme(plot.title = element_blank())+
#         theme(legend.position = "none")
#       if(any(names(p@layers) == "geom_bar")){
#         if(levels(p@data[[1]])[1] == "Yes"){
#           levels(p@data$uncluttered) <- rev(levels(p@data$uncluttered))
#         }
#         p <- p + geom_point() + geom_line(alpha = .2, linetype = 2, group = 1)
#         p@layers[["geom_bar"]] <- NULL
#       }
#
#       #p + scale_x_discrete(breaks = c("No", "Yes"))
#       p <- p + scale_y_continuous(limits = ranges)
#       ggplot2::ggsave(gsub(".RData", ".svg", filename, fixed = TRUE), p, device = "svg")
#     })
#   }
#
#   pdps$filenames <- paste0("pdp_", gsub("^.{0,}forest_light_(.+?)_\\d.*$", "\\1", pdps$vnam), "_", pdps$var, ".svg")
#   return(pdps)
# }
#
