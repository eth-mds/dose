dose_otp <- function(test, times, dose, sofa, cohort, data_src) {

  within <- dose_eval(dose, test)
  scor0  <- sofa_eval(data_src, res = sofa, times, cohort = cohort)
  scor0 <- merge(within, scor0)

  aucs <- auc(eval_score(scor0))
  aucs <- aucs[, list(mean = mean(aucs), sd = sd(aucs)),
    by = c("modnames", "curvetypes")]
  aucs <- aucs[, c("lwr", "upr") := list(mean - sd, mean + sd)]
  aucs <- aucs[, c("type", "time") := list(sub(" .+", "", modnames),
    as.integer(sub(".+, ", "", sub(" hours]$", "", modnames)))
  )]

  cbind(aucs, Dataset = srcwrap(data_src))

}

otp_fig <- function(df) {

  df$curvetypes <- factor(df$curvetypes, levels = c("ROC", "PRC"))
  df$type <- ifelse(df$type == "DOSE", "SOFA 2.0", "SOFA")
  df$type <- factor(df$type, levels = c("SOFA 2.0", "SOFA"))

  ggplot(df, aes(x = time, y = mean, color = type, fill = type)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, linetype = 0) +
    facet_grid(rows = vars(curvetypes), cols = vars(Dataset),
               scales = "free") +
    theme_bw() +
    xlab("Hours in ICU") + ylab("Mean AUC") +
    theme(legend.position = "bottom") +
    scale_colour_brewer(name = "Score", type = "qual", palette = 6,
                        direction = 1) +
    scale_fill_discrete(name = "Score")
}

dose_fxtp <- function(test_24, score, data_src, nboot = 500) {

  test_24[, dose := rowSums(test_24[, unlist(score), with = FALSE])]
  fx_plot <- list()
  fx_legend <- list()
  fx_roc <- list()
  fx_prc <- list()

  if (nboot > 0) {

    scores <- list()
    labels <- list()
    methods <- c("dose", "sofa")

    for(bt in 1:nboot) {

      ids <- sample(1:nrow(test_24), nrow(test_24), replace = TRUE)
      scores <- c(scores,
                  lapply(methods, function(x) as.numeric(test_24[[x]][ids])))
      labels <- c(labels,
                  lapply(1:length(methods), function(x) test_24[["death"]][ids]))

    }

    dsids <- rep(1:nboot, each = length(methods))
    modnames <- rep(methods, nboot)

    eval <- evalmod(scores = scores, labels = labels, dsids = dsids,
                    modnames = modnames)

    aurocs <- auc(eval)$aucs[c(TRUE, FALSE)]
    auprcs <- auc(eval)$aucs[c(FALSE, TRUE)]

    baseline <- which(methods == "dose")

    aurocs_full <- tapply(aurocs, factor(modnames, levels = unique(modnames)),
                          function(x) x, simplify = FALSE)
    names(aurocs_full) <- methods

    # more powerful, paired hypothesis testing
    pair.pvals <- rep(0, length(aurocs_full))
    if (length(baseline) == 1L) {

      base_aucs <- aurocs_full[[baseline]]

      for (i in 1:length(aurocs_full)) {

        cmp_aucs <- aurocs_full[[i]]
        pair.pvals[i] <- pnorm(mean(base_aucs - cmp_aucs) /
                                 sd(base_aucs - cmp_aucs),
                               lower.tail = FALSE)

      }

    }

    cat("for", srcwrap(data_src), "patients", round(mean(base_aucs), 3),
        "vs.", round(mean(cmp_aucs[methods == "sofa"]), 3), ";")
    cat("p-value", pair.pvals[methods == "sofa"], ";")
  }

  for (cp in names(score)) {

    if (length(grep(paste0("^sofa.", cp), names(test_24))) == 0) {
      sofa.cp <- rep(0, nrow(test_24))
      sofa.name <- "Prev. no"
    } else {
      sofa.cp <- test_24[[grep(paste0("^sofa.", cp), names(test_24))]]
      sofa.name <- "SOFA"
    }

    if (nboot > 0) {
      scores <- list()
      labels <- list()
      methods <- c("DOSE", "SOFA")
      nboot <- 500
      dose.cp <- rowSums(test_24[, score[[cp]], with = FALSE])

      for(bt in 1:nboot) {

        ids <- sample(1:nrow(test_24), nrow(test_24), replace = TRUE)
        scores <- c(scores, list(dose.cp[ids], sofa.cp[ids]))
        labels <- c(labels,
                    lapply(1:length(methods), function(x) test_24[["death"]][ids]))

      }

      dsids <- rep(1:nboot, each = length(methods))
      modnames <- rep(methods, nboot)

    }

    eval <- evalmod( # again need to beware here
      scores = scores,
      labels = labels,
      dsids = dsids,
      #modnames = paste(c("DOSE", sofa.name), cp),
      modnames = modnames,
      cb_alpha = 0.05
    )

    fx_roc[[cp]] <- list(
      dose = auc(eval)$aucs[c(TRUE, FALSE, FALSE, FALSE)],
      sofa = auc(eval)$aucs[c(FALSE, FALSE, TRUE, FALSE)]
    )

    fx_prc[[cp]] <- list(
      dose = auc(eval)$aucs[c(FALSE, TRUE, FALSE, FALSE)],
      sofa = auc(eval)$aucs[c(FALSE, FALSE, FALSE, TRUE)]
    )

    tmp <- ggplot2::fortify(eval)
    tmp <- cbind(tmp, source = rep(srcwrap(data_src), nrow(tmp)),
                 component = rep(scwrap(cp), nrow(tmp)))

    fx_plot[[cp]] <- tmp
  }

  list(fx_plot = fx_plot, fx_roc = fx_roc, fx_prc = fx_prc,
       fx_legend = fx_legend)
}
