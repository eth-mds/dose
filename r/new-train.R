auc_optimizer <- function(train_data, cfg, hard_thresh = 0, ...) {

  systems <- c("cardio", "liver", "cns", "coag", "renal", "resp", "metabolic")
  best <- lapply(systems, function(x) list(auc = 0.5))
  arch <- lapply(systems, function(x) lapply(train_data, function(y) 0.5))
  names(best) <- names(arch) <- systems

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
            cmp_auc <- PRROC::roc.curve(
              weights.class0 = train_data[[j]][["death"]],
              scores.class0 = rowSums(mat[[j]][, cpt[, k]]))$auc
            auc <- auc + cmp_auc
            if (cmp_auc > arch[[sys]][[j]]) arch[[sys]][[j]] <- cmp_auc
            if (cmp_auc < hard_thresh) auc <- -Inf 
          }
          auc <- auc / length(train_data)

          if (auc > best[[sys]][["auc"]]) {
            
            #cat("Component", cp, "of system", sys, "has AUC", round(auc, 3),"\n")

            best[[sys]][["auc"]] <- auc
            best[[sys]][["feature"]] <- cp
            best[[sys]][["cols"]] <- colnames(mat[[1]])[cpt[, k]]

          }

        }

      }

  }
  
  incl <- vapply(best, function(x) x$auc == 0.5, logical(1L))
  if (any(incl)) {
    cat("Could not find components for:", systems[incl], "\n")
    for (sys in systems[incl]) {
      cat("Inconclusive", sys, "with best AUCs", unlist(arch[[sys]]), "\n")
    }
  }
  best

}

running_decorr <- function(train_data, cfg, score, test_data = NULL, lambda = 1, 
                           hard_thresh = 0, output = "score", max_epoch = 50) {
  
  sys_components <- list()
  for (i in seq_along(cfg)) {
    
    catg <- cfg[[i]][["category"]]
    sys_components[[catg]] <- c(sys_components[[catg]], names(cfg)[i])
    
  }
  
  if (output == "plot") {
    auc_test <- PRROC::roc.curve(
      weights.class0 = test_data[["death"]],
      scores.class0 = rowSums(test_data[, unlist(score), with = FALSE]))$auc
  }
  
  res <- NULL # c(run_auc, auc_test)
  for (epoch in seq_len(max_epoch)) {
    
    change <- F
    
    for (sys in names(score)) {
      
      run_obj <- 0
      
      for (j in seq_along(train_data)) {
        auc <- PRROC::roc.curve(
          weights.class0 = train_data[[j]][["death"]],
          scores.class0 = rowSums(train_data[[j]][, unlist(score), with=F]))$auc
        
        auc_cmp <- PRROC::roc.curve(
          weights.class0 = train_data[[j]][["death"]],
          scores.class0 = rowSums(train_data[[j]][, score[[sys]], with = FALSE]))$auc
        
        obj <- lambda * auc + (1 - lambda) * auc_cmp
        
        run_obj <- run_obj + obj
        
      }
      
      for (cp in sys_components[[sys]]) {
        
        print(cp)
        cp.idx <- grep(paste0("^", cp, "[.]"), names(train_data[[1]]))
        mat <- lapply(train_data, 
                      function(dat) as.matrix(dat[, cp.idx, with=FALSE]))
        cpt <- replicate(500, sample(1:ncol(mat[[1]]), 4, FALSE))
        
        for (k in 1:ncol(cpt)) {
          
          obj_sum <- 0
          
          for (j in seq_along(train_data)) {
            
            smsys <- rowSums(
              train_data[[j]][, unlist(score[-which(names(score) == sys)]),
                              with=FALSE])
            
            auc <- PRROC::roc.curve(
              weights.class0 = train_data[[j]][["death"]],
              scores.class0 = smsys + rowSums(mat[[j]][, cpt[, k]]))$auc
            
            auc_cmp <- PRROC::roc.curve(
              weights.class0 = train_data[[j]][["death"]],
              scores.class0 = rowSums(mat[[j]][, cpt[, k]]))$auc
            
            if (auc_cmp < hard_thresh) {
              obj_sum <- -Inf
            }
            
            obj <- lambda * auc + (1 - lambda) * auc_cmp
            
            obj_sum <- obj_sum + obj
            
          }
          
          if (is.na(obj_sum) | is.na(run_obj) | is.na(obj_sum > run_obj)) browser()
          if (obj_sum > run_obj) {
            
            run_obj <- obj_sum
            score[[sys]] <- colnames(mat[[1]])[cpt[, k]]
            change <- T
            
          }
          
        }
        
        if (output == "plot") {
          
          auc_train <- PRROC::roc.curve(
            weights.class0 = train_data[[1]][["death"]],
            scores.class0 = rowSums(train_data[[1]][, unlist(score), with=FALSE]))$auc
          auc_test <- PRROC::roc.curve(
            weights.class0 = test_data[["death"]],
            scores.class0 = rowSums(test_data[, unlist(score), with=FALSE]))$auc
          
          
          res <- rbind(res, c(auc_train, auc_test))
          
        }
        
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
    
  } else if (output == "aucs") {
    
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
    
  } else {
    
    return(score)
    
  }
  
}
