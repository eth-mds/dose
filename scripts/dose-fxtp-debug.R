nrow(test_24)

for (cp in names(score)) {
  
  if (cp != "metabolic") {
    sfcp <- grep(paste0("^sofa.", cp), names(test_24), value = TRUE)
    sfcp <- test_24[[sfcp]]
    sfcp[is.na(sfcp)] <- 0
    cat("SOFA", cp, "AUC", 
        PRROC::roc.curve(scores.class0 = sfcp,
                         weights.class0 = test_24$death)$auc, ";")
  }
  cat("DOSE", cp, "AUC", 
      PRROC::roc.curve(scores.class0 = rowSums(test_24[, score[[cp]], with = FALSE]),
                       weights.class0 = test_24$death)$auc, "\n")
}

old_sf <- 0
old_ds <- 0
for (cp in names(score)) {
  
  if (cp != "metabolic") {
    sfcp <- grep(paste0("^sofa.", cp), names(test_24), value = TRUE)
    sfcp <- test_24[[sfcp]]
    sfcp[is.na(sfcp)] <- 0
    old_sf <- old_sf + sfcp
    cat("Add SOFA", cp, "AUC", 
        PRROC::roc.curve(scores.class0 = old_sf,
                         weights.class0 = test_24$death)$auc, ";")
  }
  old_ds <- old_ds + rowSums(test_24[, score[[cp]], with = FALSE])
  cat("Add DOSE", cp, "AUC", 
      PRROC::roc.curve(scores.class0 = old_ds,
                       weights.class0 = test_24$death)$auc, "\n")
}

mat_sf <- NULL
mat_ds <- NULL
for (cp in names(score)) {
  
  if (cp != "metabolic") {
    sfcp <- grep(paste0("^sofa.", cp), names(test_24), value = TRUE)
    sfcp <- test_24[[sfcp]]
    sfcp[is.na(sfcp)] <- 0
    mat_sf <- cbind(mat_sf, sfcp)
  }
  mat_ds <- cbind(mat_ds, rowSums(test_24[, score[[cp]], with = FALSE]))
}
colnames(mat_sf) <- names(score)[-7]
colnames(mat_ds) <- names(score)

cowplot::plot_grid(
  ggplot(melt(cor(mat_sf)), aes(x = Var1, y = Var2, fill = value)) + geom_tile() +
    scale_fill_viridis_c(limits = c(0, 1)),
  ggplot(melt(cor(mat_ds)), aes(x = Var1, y = Var2, fill = value)) + geom_tile() +
    scale_fill_viridis_c(limits = c(0, 1)), ncol = 2L
)
