cluster_cases <- function (riclpms){
  library(data.table)
  f <- list.files(pattern = "^forest_light")
  forest <- readRDS(f)
  out <- matrix(NA, nrow(df_features), length(forest))
  for(i in 1:nrow(df_features)){
    for(j in 1:length(forest)){
      out[i, j] <- semtree:::traverse_stripped(row = df_features[i, ], tree = forest[[j]])
    }
  }
  saveRDS(out, "pred_by_tree.RData")


  out <- apply(out, 1, median, na.rm = TRUE)

  out <- data.frame(out)
  names(out) <- "m_step"
  df_plot <- data.frame(Value = out)
  p <- ggplot(df_plot, aes(x = m_step)) +
    geom_vline(xintercept = 0) +
    geom_boxplot(staplewidth = 0) +
    theme_bw() +
    xlab("Parenting effect") +
    theme(axis.title.y = element_blank())
  ggsave("effects_boxplots.svg", p, device = "svg", width = 5, height = 3, units = "in")
  res_umap <- umap::umap(out)
  df_plot <- data.frame(res_umap$layout)
  res_tsne <- Rtsne::Rtsne(as.matrix(out), dims = 3)
  df_plot <- data.frame(res_tsne$Y)
  ggplot(df_plot, aes(x = X2, y = X3)) +
    geom_point()+
    scale_x_continuous(limits = c(-10,10)) +
    scale_y_continuous(limits = c(-10,10))

}
