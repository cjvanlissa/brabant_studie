# desc <- read.csv("codebook_df_anal.csv")
# desc <- desc[!grepl("_\\d_[mfc]$", desc$name), ]
# desc <- desc[!grepl("^id(\\.\\d)?$", desc$name), ]
#
# interp <- read.csv("variable_interpretation.csv", stringsAsFactors = FALSE)
# interp$name <- tolower(gsub(" .*$", "", interp$Variable))
# desc <- merge(desc, interp, by = "name", all.x = TRUE)
#
# desc$report_values <- unlist(lapply(strsplit(desc$Values, ";"), function(x){
#   out <- gsub("^\\d{1,} ", "", trimws(c(head(x, 1), tail(x, 1))))
#   if(all(is.na(out))) return(NA)
#   if(all(c("Yes", "No") %in% out)) return("No/Yes")
#   if(all(out == "Number")) return("Number")
#   paste0('from "', out[1], '" to "', out[2], '"')
#   }))
#
#
# vimps <- read.csv("vim_combined_shapes.csv", stringsAsFactors = FALSE)
# vimps <- vimps[vimps$Importance %in% c(1:10), ]
#
# if(file.exists("draft_results.txt")) file.remove("draft_results.txt")
# file.create("draft_results.txt")
# for(pred in unique(vimps$Variable)){
#   for(parnt in unique(vimps$Parent)){
#
#     tmp <- vimps[vimps$Variable == pred & vimps$Parent == parnt, ]
#     tmp <- tmp[order(tmp$Importance),]
#     # var_order <- head(tmp$Moderator[order(tmp$Importance, decreasing = TRUE)], 10)
#     for(mod in tmp$Moderator){
#       #mod = "involvement_f"
#       pdp <- readRDS(paste0("pdp_", pred, "_", parnt, "_", mod, ".RData"))
#       signflip <- length(unique(sign(pdp$samples$yonx))) > 1
#
#       cat(c("\nModerator ",
#                  c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")[which(tmp$Moderator == mod)],
#                  " was ",
#                  mod,
#                  " (",
#                  desc$Question[which(desc$name == mod)],
#                  ", ",
#                  desc$report_values[which(desc$name == mod)],
#                  ").\n",
#                  "The marginal relationship of ",
#                  mod,
#                  " with the parenting effect of ",
#                  c(m = "mothers'", f = "fathers'")[parnt],
#             " ",
#                  c(ang = "anger", war = "warmth", ind = "inductive reasoning")[pred],
#                  " was ",
#                  c(p = "positive", n = "negative", "o" = "unclear (other)", cv = "convex", cn = "concave", "no" = "flat")[tmp$Shape_manual[which(mod == tmp$Moderator)]],
#             ifelse(signflip, " and the effect flipped sign", ""),
#                  ".\n\n"), sep = "", file = "draft_results.txt", append = TRUE)
#     }
#
#   }
# }
