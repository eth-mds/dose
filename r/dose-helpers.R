train_dose <- function(X, y, method = "glm", max_points = 24L, coef_frac = 0.1, max_feats = 10L, nfolds = 5L) {

  coefs <- function(fit) coef(fit$glmnet.fit)[-1L, ]

  gt_thresh <- function(col, frac) abs(col) >= (max(abs(col)) * frac)

  which_feats <- function(col, frac, feat) unique(feat[gt_thresh(col, frac)])

  as_score <- function(x) round(x / min(abs(x)[abs(x) > 0]))

  plot_glmnet <- function(fit, counts) {

    old_par <- par(mfrow = c(2, 2))
    on.exit(par(old_par))

    plot(counts, fit$cvm, pch = 19, xlab = "complexity",
         ylab = "Cross-validated AUC", main = "Complexity-AUC path")
    plot(counts, pch = 19, type = "b", main = "Features used")
    plot(fit$glmnet.fit$df, pch = 19, type = "b", main = "Degrees of freedom")
    plot(fit$cvm, pch = 19, type = "b", main = "Cross-validated AUC")
  }

  conc <- gsub("\\..*","", colnames(X))

  if (method == "glm") {

    fit <- glmnet::cv.glmnet(X, y, family = "binomial", type.measure = "auc")
    fit_count <- lengths(apply(coefs(fit), 2L, which_feats, coef_frac, conc))

    plot_glmnet(fit, fit_count)

    hit <- readline("choose model: ")
    hit <- as.integer(hit)

    beta <- coefs(fit)[, hit]
    beta[!gt_thresh(beta, coef_frac)] <- 0

    return(as_score(beta))

  } else if (method == "robustFS") {

    beta <- rep(0, ncol(X))

    for (m in 1:max_points) {

      feats <- which_feats(beta, coef_frac, conc)

      allowed <- 1:ncol(X)
      if(length(feats) >= max_feats) allowed <- which(conc %in% feats)

      score <- as.vector(X %*% beta)

      folds <- lapply(1:nfolds, function(i) sample(1:nrow(X), round(0.75*nrow(X))))

      best_auc <- sapply(1:ncol(X), function(i) {

        if (!(i %in% allowed)) return(-1)

        curr_score <- score + X[, i]
        curr_auc <- min(
          sapply(1:nfolds, function(j)
            PRROC::roc.curve(scores.class0 = curr_score[folds[[j]]], weights.class0 = y[folds[[j]]])$auc)
        )
        return(curr_auc)
      })

      # update the coefficient
      beta[which.max(best_auc)] <- beta[which.max(best_auc)]+1

    }

    names(beta) <- colnames(X)
    return(beta)

  } else {

    stop("Method should be either `glm` or `robustFS`")

  }

}

dose_vs_sofa <- function(dose, sofa, outcome, splits = 10L, frac = 0.75) {

  point_eval <- function(dose, sofa) {

    ids <- lapply(1:splits, function(i) sample(1:length(outcome), round(frac*length(outcome))))

    sofa.vals <- sapply(1:splits, function(i) {

      PRROC::roc.curve(scores.class0 = sofa[ids[[i]]], weights.class0 = outcome[ids[[i]]])$auc

    })

    dose.vals <- sapply(1:splits, function(i) {

      PRROC::roc.curve(scores.class0 = dose[ids[[i]]], weights.class0 = outcome[ids[[i]]])$auc

    })

    ret <- data.frame(
      rbind(c(mean(sofa.vals), mean(sofa.vals) + sd(sofa.vals), mean(sofa.vals) - sd(sofa.vals)),
          c(mean(dose.vals), mean(dose.vals) + sd(dose.vals), mean(dose.vals) - sd(dose.vals)))
    )

    ret <- cbind(ret, c("SOFA", "DOSE"))
    names(ret) <- c("auc", "auc_max", "auc_min", "score")

    return(ret)

  }

  res <- Map(point_eval, dose, sofa)

  Reduce(rbind, res)

}
