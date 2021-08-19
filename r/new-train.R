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

        print(cp)
        cp.idx <- grep(paste0("^", cp), names(train_data))
        mat <- as.matrix(train_data[, cp.idx, with=FALSE])
        cpt <- replicate(500, sample(1:ncol(mat), 4, F))

        for (k in 1:ncol(cpt)) {

          auc <- PRROC::roc.curve(weights.class0 = train_data[["death"]],
            scores.class0 = rowSums(mat[, cpt[, k]]))$auc

          if (auc > best[[sys]][["auc"]]) {

            best[[sys]][["auc"]] <- auc
            best[[sys]][["feature"]] <- cp
            best[[sys]][["cols"]] <- colnames(mat)[cpt[, k]] # careful here

            eval <- evalmod(
              scores = list(rowSums(mat[, cpt[, k]])),
              labels = list(train_data[["death"]]),
              dsids = 1, modnames = paste(c("DOSE"), sys)
            )

          }

        }

      }

  }

  best

}
