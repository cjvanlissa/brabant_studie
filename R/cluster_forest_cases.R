# cluster_cases <- function (riclpms){
#   f <- list.files(pattern = "^forest_light")
#   f <- f[order(as.Date(file.info(f)$mtime), decreasing = TRUE)][1:6]
#   names(f) <- substr(f, 14, 18)
#   names(riclpms) <- paste0(substr(names(riclpms), 1, 3), "_", substr(names(riclpms), 9, 9))
#   out <- lapply(names(f), function(pred){
#     forest <- readRDS(f[pred])
#     data <- setDT(riclpms[[pred]]$df)
#     parnams <- attr(forest, "parameters")
#     out <- data[, as.list(apply(do.call(cbind, lapply(forest,
#                                                         function(t) {
#                                                           semtree:::traverse_stripped(row = .SD, tree = t)[2]
#                                                         })), 1, FUN = c)), by = 1:nrow(data)][, -1]
#     saveRDS(out, paste0("pred_by_tree_", pred, ".RData"))
#
#   })
#   out <- lapply(names(f), function(pred){
#     apply(readRDS(paste0("pred_by_tree_", pred, ".RData")), 1, median, na.rm = TRUE)
#   })
#   out <- data.frame(out)
#   names(out) <- names(f)
#   out$ang_f <- out$ang_f
#   out$ang_m <- out$ang_m
#   cor_behs <- cor(out)
#   write.csv(cor_behs, "cor_parenting_effects.csv", row.names = FALSE)
#   df_plot <- data.frame(
#     Variable = rep(c(war_m = "Warmth (m)", war_f = "Warmth (f)", ang_m = "Anger (m)", ang_f = "Anger (f)", ind_m = "Ind. Reas. (m)", ind_f = "Ind. Reas. (f)")[names(out)], each = nrow(out)),
#     Value = unlist(out))
#   df_plot$Variable <- ordered(df_plot$Variable, levels = rev(c("Warmth (m)", "Warmth (f)", "Anger (m)", "Anger (f)", "Ind. Reas. (m)", "Ind. Reas. (f)")))
#   df_plot$Parent <- factor(df_plot$Variable, levels = c("Warmth (m)", "Warmth (f)", "Anger (m)", "Anger (f)", "Ind. Reas. (m)", "Ind. Reas. (f)"), labels = rep(c("m", "f"), 3))
#   # ggplot(df_plot, aes(x = Value, y = Variable)) +
#   #   geom_vline(xintercept = 0) +
#   #   ggridges::geom_density_ridges()+
#   #   geom_boxplot()+
#   #   theme_bw() +
#   #   xlab("Parenting effect") +
#   #   theme(axis.title.y = element_blank())
#   p <- ggplot(df_plot, aes(x = Value, y = Variable)) +
#     geom_vline(xintercept = 0) +
#     geom_boxplot(staplewidth = 0) +
#     theme_bw() +
#     xlab("Parenting effect") +
#     theme(axis.title.y = element_blank())
#   ggsave("parenting_effects_boxplots.svg", p, device = "svg", width = 5, height = 3, units = "in")
#   res_umap <- umap::umap(out)
#   df_plot <- data.frame(res_umap$layout)
#   res_tsne <- Rtsne::Rtsne(as.matrix(out), dims = 3)
#   df_plot <- data.frame(res_tsne$Y)
#   ggplot(df_plot, aes(x = X2, y = X3)) +
#     geom_point()+
#     scale_x_continuous(limits = c(-10,10)) +
#     scale_y_continuous(limits = c(-10,10))
#
# }
