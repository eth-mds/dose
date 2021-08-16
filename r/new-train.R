auc_optimizer <- function(train_data, cfg, ...) {

  systems <- c("cardio", "liver", "cns", "coag", "renal", "resp", "immunological",
               "metabolic")
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

            if (length(grep(paste0("^sofa.", sys), names(train_data))) == 0) {
              sofa.cp <- rep(0, nrow(train_data))
              sofa.name <- "Prev. no"
            } else {
              sofa.cp <- train_data[[grep(paste0("^sofa.", sys), names(train_data))]]
              sofa.name <- "SOFA"
            }

            eval <- evalmod(
              scores = list(rowSums(mat[, cpt[, k]]), replace_na(sofa.cp, 0)),
              labels = list(train_data[["death"]], train_data[["death"]]),
              dsids = 1:2, modnames = paste(c("DOSE", sofa.name), sys)
            )

            best[[sys]][["plot"]]<- autoplot(eval, "ROC") + geom_line(size = 2) +
              theme(legend.position = "bottom",
                legend.text = element_text(size=15),
                plot.title = element_blank())

          }

        }

      }

  }

  best

}
