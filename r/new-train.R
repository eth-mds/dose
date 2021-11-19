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
        mat <- lapply(train_data, 
                      function(dat) as.matrix(dat[, cp.idx, with=FALSE]))
        cpt <- replicate(500, sample(seq_len(ncol(mat[[1]])), 4, replace = FALSE))

        for (k in 1:ncol(cpt)) {
          
          auc <- 0
          for (j in seq_along(train_data)) {
            auc <- auc + PRROC::roc.curve(
              weights.class0 = train_data[[j]][["death"]],
              scores.class0 = rowSums(mat[[j]][, cpt[, k]]))$auc
          }
          auc <- auc / length(train_data)

          if (auc > best[[sys]][["auc"]]) {
            
            cat("Component", cp, "of system", sys, "has AUC", round(auc, 3),"\n")

            best[[sys]][["auc"]] <- auc
            best[[sys]][["feature"]] <- cp
            best[[sys]][["cols"]] <- colnames(mat[[1]])[cpt[, k]]

          }

        }

      }

  }

  best

}

running_decorr <- function(train_data, test_data, cfg, score, lambda = 1, 
                           output = "vector", max_epoch = 50) {
  
  sys_components <- list()
  for (i in seq_along(cfg)) {
    
    catg <- cfg[[i]][["category"]]
    sys_components[[catg]] <- c(sys_components[[catg]], names(cfg)[i])
    
  }
  
  auc_test <- PRROC::roc.curve(
    weights.class0 = test_data[["death"]],
    scores.class0 = rowSums(test_data[, unlist(score), with = FALSE]))$auc
  
  res <- NULL # c(run_auc, auc_test)
  for (epoch in seq_len(max_epoch)) {
    
    change <- F
    
    for (sys in names(score)) {
      
      smsys <- rowSums(train_data[, unlist(score[-which(names(score) == sys)]),
                                  with=F])
      run_auc <- PRROC::roc.curve(
        weights.class0 = train_data[["death"]],
        scores.class0 = rowSums(train_data[, unlist(score), with = FALSE]))$auc
      
      marg_auc <- PRROC::roc.curve(
        weights.class0 = train_data[["death"]],
        scores.class0 = rowSums(train_data[, score[[sys]], with = FALSE]))$auc
      
      run_obj <- lambda * run_auc + (1 - lambda) * marg_auc
      
      for (cp in sys_components[[sys]]) {
        
        print(cp)
        cp.idx <- grep(paste0("^", cp, "[.]"), names(train_data))
        mat <- as.matrix(train_data[, cp.idx, with=FALSE])
        cpt <- replicate(500, sample(1:ncol(mat), 4, F))
        
        for (k in 1:ncol(cpt)) {
          
          auc <- PRROC::roc.curve(
            weights.class0 = train_data[["death"]],
            scores.class0 = smsys + rowSums(mat[, cpt[, k]]))$auc
          
          auc_cmp <- PRROC::roc.curve(
            weights.class0 = train_data[["death"]],
            scores.class0 = rowSums(mat[, cpt[, k]]))$auc
          
          obj <- lambda * auc + (1 - lambda) * auc_cmp
          
          if (obj > run_obj) {
            
            run_obj <- obj
            score[[sys]] <- colnames(mat)[cpt[, k]]
            change <- T
            
          }
          
        }
        
        auc_train <- PRROC::roc.curve(
          weights.class0 = train_data[["death"]],
          scores.class0 = rowSums(train_data[, unlist(score), with=FALSE]))$auc
        auc_test <- PRROC::roc.curve(
          weights.class0 = test_data[["death"]],
          scores.class0 = rowSums(test_data[, unlist(score), with=FALSE]))$auc
        
        
        res <- rbind(res, c(auc_train, auc_test))
        
      }
      
    }
    
    
    if (!change) break
    
  }
  
  if (output == "plot") {
    
    res <- cbind(res, seq_len(nrow(res)))
    res <- as.data.frame(res)
    names(res) <- c("Train", "Test", "iter")
    return(
      ggplot(reshape2::melt(res, id.vars = "iter", variable.name = "split",
                            value.name = "auc"), 
             aes(x = iter, y = auc, color = split)) +
        geom_line() + theme_bw() +
        ggtitle("By component overfitting") +
        xlab("Iteration") + ylab("AUROC")
    )
    
  } else {
    
    run_auc <- PRROC::roc.curve(
      weights.class0 = train_data[["death"]],
      scores.class0 = rowSums(train_data[, unlist(score), with = FALSE])
    )$auc
    
    cmp_auc <- lapply(
      score, function(x) {
        PRROC::roc.curve(
          weights.class0 = train_data[["death"]],
          scores.class0 = rowSums(train_data[, x, with = FALSE])
        )$auc
      }
    )
    
    return(c(run_auc, as.vector(cmp_auc)))
    
  }
  
}
