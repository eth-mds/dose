#' * auc_optimizer finds the best marginal score *
auc_optimizer <- function(train_data, cfg, hard_thresh = 0, ntry = 1000,
                          forbiden = NULL, ...) {

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

        if (is.element(cp, forbiden)) next

        # pick the column names corresponding to feature cp
        cp.idx <- grep(paste0("^", cp, "\\."), names(train_data[[1]]))
        # subset the data matrix for the feature
        mat <- lapply(train_data,
                      function(dat) as.matrix(dat[, cp.idx, with=FALSE]))
        # try out 500 combinations of 4 thresholds
        cpt <- replicate(ntry, sample(seq_len(ncol(mat[[1]])), 4,
                                     replace = FALSE))

        # iterate over the 500 attempts
        for (k in 1:ncol(cpt)) {

          auc <- 0
          for (j in seq_along(train_data)) {
            cmp_auc <- PRROC::roc.curve(
              weights.class0 = train_data[[j]][["death"]],
              scores.class0 = rowSums(mat[[j]][, cpt[, k]]))$auc
            auc <- auc + cmp_auc

            if (cmp_auc > arch[[sys]][[j]]) arch[[sys]][[j]] <- cmp_auc
            # if worse than hard threshold, ignore
            if (cmp_auc < hard_thresh) auc <- -Inf
          }
          auc <- auc / length(train_data)

          if (auc > best[[sys]][["auc"]]) {

            #cat("Component", cp, "of system", sys, "has AUC",
            #    round(auc, 3),"\n")

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

running_decorr <- function(train_data, cfg, score, test_data = NULL,
                           lambda = 1, thresh = .75, output = "score",
                           n_try = 1000, max_epoch = 50,
                           dev_country = FALSE, forbiden = NULL,
                           accept = \(marg, jnt) marg > 0 ||
                             jnt * length(score) > abs(marg),
                           n_cores = get_cores()) {

  auc_calc <- function(dat, x_cols, y_col = "death", y = dat) {
    if (is.null(dat)) return(NA_real_)
    PRROC::roc.curve(weights.class0 = dat[[y_col]],
                     scores.class0 = rowSums(y[, x_cols, with = FALSE]))$auc
  }

  aucs_calc <- function(dat, ...) {
    if (inherits(dat, "data.frame")) auc_calc(dat, ...)
    else mean(vapply(dat, auc_calc, numeric(1L), ...))
  }

  auc_thresh <- function(..., thresh = 0.75) {
    res <- auc_calc(...)
    0.5 + thresh * (res - 0.5)
  }

  aucs_thresh <- function(x_cols, dat, ...) lapply(dat, auc_thresh, x_cols, ...)

  res_calc <- function(train_data, test_data, score, ...) {
    list(
      c(
        train = aucs_calc(train_data, unlist(score)),
        test = if (length(test_data)) aucs_calc(test_data, unlist(score)),
        vapply(score, function(x) aucs_calc(train_data, x), numeric(1L)),
        list(...)
      )
    )
  }

  one_try <- function(k, each, train_data, j, score, sys, cp_samp, thresh,
                      prop_keep, lambda) {

    if (!k %% each) message(".", appendLF = FALSE)

    obj_sum <- 0

    for (j in seq_along(train_data)) {

      rest <- unlist(score[-which(names(score) == sys)])

      auc_all <- auc_calc(train_data[[j]], c(rest, cp_samp[, k]))
      auc_cmp <- auc_calc(train_data[[j]], cp_samp[, k])

      if (is.list(thresh) && auc_cmp < prop_keep[[sys]][[j]]) {
        obj_sum <- -Inf
        break
      }

      obj <- lambda * auc_all + (1 - lambda) * auc_cmp
      obj_sum <- obj_sum + obj
    }

    list(update = cp_samp[, k], sum = obj_sum)
  }

  if (dev_country) {

    sys_components <- list(metabolic = "bicar", resp = "spfi")
    max_epoch <- 2L
  } else {

    sys_components <- split(
      names(cfg),
      vapply(cfg, `[[`, character(1L), "category")
    )
  }

  prop_keep <- lapply(config("best-marg"), aucs_thresh, train_data,
                      thresh = thresh)

  res <- res_calc(train_data, test_data, score, epoch = 0, component = "init",
                  feature = "init")

  for (epoch in seq_len(max_epoch)) {

    message("epoch ", epoch)

    change <- FALSE

    # iterate over components
    for (sys in names(score)) {

      message("  - component `", sys, "`")

      run_obj <- 0
      # iterate over training datasets
      if (dev_country) {

        run_obj <- length(train_data) * 0.5
      } else {

        for (j in seq_along(train_data)) {

          # compute AUC overall and marginal
          auc_all <- auc_calc(train_data[[j]], unlist(score))
          auc_cmp <- auc_calc(train_data[[j]], score[[sys]])

          # take a convex combination of overall and marginal AUC
          obj <- lambda * auc_all + (1 - lambda) * auc_cmp
          run_obj <- run_obj + obj
        }
      }

      # run over all possible features within an organ system
      for (cp in sys_components[[sys]]) {

        if (is.element(cp, forbiden)) next

        message("    ", format(cp, width = max(nchar(unlist(sys_components)))),
                " ", appendLF = FALSE)
        # take a convex combination of overall and marginal AUC
        best_sum <- run_obj

        cp_opts <- grep(paste0("^", cp, "\\."), names(train_data[[1]]),
                        value = TRUE)
        cp_samp <- replicate(n_try, sample(cp_opts, 4, FALSE))

        each <- floor(n_try / 50)

        par_grd <- mclapply(
          seq_len(n_try),
          function(k) {
            if (!k %% each) message(".", appendLF = FALSE)

            obj_sum <- 0

            for (j in seq_along(train_data)) {

              # get the score representation with organ system removed
              rest <- unlist(score[-which(names(score) == sys)])

              # add the candidate feature for the organ system back in
              auc_all <- auc_calc(train_data[[j]], c(rest, cp_samp[, k]))
              # compute the marginal AUC of the candidate feature
              auc_cmp <- auc_calc(train_data[[j]], cp_samp[, k])

              if (is.list(thresh) && auc_cmp < prop_keep[[sys]][[j]]) {
                obj_sum <- -Inf
                break
              }

              # compute weighted objective
              obj <- lambda * auc_all + (1 - lambda) * auc_cmp
              obj_sum <- obj_sum + obj
            }
            obj_sum
          }, mc.cores = n_cores()
        )

        # run over all the investigated updates
        for (k in seq_len(n_try)) {

          obj_sum <- par_grd[[k]]

          # if improvement spotted, update the score
          if (obj_sum > best_sum) {

            marg <- aucs_calc(train_data, cp_samp[, k]) -
              aucs_calc(train_data, score[[sys]])
            jnt <- (obj_sum - run_obj) / length(train_data)

            if (accept(marg, jnt)) {
              best_update <- cp_samp[, k]
              best_sum <- obj_sum
            } else {

              m_marg <- max(abs(marg))
              # message("Score improvement ", round(jnt, 4), "but JoinToMarg ratio ",
              #         round(m_marg/jnt), " rejected")
            }

          }
        }

        message("")

        if (best_sum > run_obj) {

          # compute the improvement of (new - old)
          marg <- aucs_calc(train_data, best_update) -
            aucs_calc(train_data, score[[sys]])
          jnt <- (best_sum - run_obj) / length(train_data)

          # check if the improvement for the epoch is accepted
          if (accept(marg, jnt)) {

            run_obj <- best_sum
            score[[sys]] <- best_update
            change <- TRUE

          } else {

            m_marg <- max(abs(marg))
            message("Score improvement ", round(jnt, 4), "but JoinToMarg ratio ",
                    round(m_marg/jnt), " rejected")

          }
        }

        res <- append(res,
          res_calc(train_data, test_data, score, epoch = epoch,
                   component = sys, feature = cp)
        )
      }
    }

    if (!change) break
  }

  res <- Map(\(i, typ) vapply(res, `[[`, typ, i), names(res[[1L]]), res[[1L]])
  res <- as.data.frame(res)

  structure(score, aucs = res)
}
