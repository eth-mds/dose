auc_optimizer <- function(train_data, cfg, ...) {

  systems <- c("cardio", "liver", "cns", "coag", "renal", "resp", "metabolic")
  best <- lapply(systems, function(x) list(auc = 0.5))
  names(best) <- systems

  for (sys in systems) {

    components <- NULL
    for (i in 1:length(cfg))
      if(cfg[[i]][["category"]] == sys)
        components <- append(components, names(cfg)[i])

      for (cp in components) {
        
        cp.idx <- grep(paste0("^", cp, "\\."), names(train_data[[1]]))
        mat1 <- as.matrix(train_data[[1]][, cp.idx, with=FALSE])
        mat2 <- as.matrix(train_data[[2]][, cp.idx, with=FALSE])
        cpt <- replicate(500, sample(seq_len(ncol(mat1)), 4, replace = FALSE))

        for (k in 1:ncol(cpt)) {
          
          auc <- 0
          for (j in seq_along(train_data)) {
            auc <- auc + PRROC::roc.curve(
              weights.class0 = train_data[[1]][["death"]],
              scores.class0 = rowSums(mat1[, cpt[, k]]))$auc
          }
          auc <- auc / length(train_data)
          
          # auc1 <- PRROC::roc.curve(weights.class0 = train_data[[1]][["death"]],
          #   scores.class0 = rowSums(mat1[, cpt[, k]]))$auc
          # 
          # auc2 <- PRROC::roc.curve(weights.class0 = train_data[[2]][["death"]],
          #   scores.class0 = rowSums(mat2[, cpt[, k]]))$auc
          # 
          # auc <- (auc1 + auc2) / 2

          if (auc > best[[sys]][["auc"]]) {
            
            cat("Component", cp, "of system", sys, "has AUC", round(auc, 3),"\n")

            best[[sys]][["auc"]] <- auc
            best[[sys]][["feature"]] <- cp
            best[[sys]][["cols"]] <- colnames(mat1)[cpt[, k]]

          }

        }

      }

  }

  best

}
