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

  ggplot(aucs, aes(x = time, y = mean, color = type, fill = type)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, linetype = 0) +
    facet_grid(rows = vars(curvetypes), scales = "free_y") +
    theme_bw() +
    xlab("Hours in ICU") + ylab("Mean AUC") +
    theme(legend.position = "bottom") +
    scale_colour_brewer(type = "qual", palette = 6, direction = 1)

}

dose_fxtp <- function(test_24, score, data_src) {

  test_24
  fx_plot <- list()
  fx_roc <- list()
  fx_prc <- list()

  for (cp in names(score)) {

    if (length(grep(paste0("^sofa.", cp), names(test_24))) == 0) {
      sofa.cp <- rep(0, nrow(test_24))
      sofa.name <- "Prev. no"
    } else {
      sofa.cp <- test_24[[grep(paste0("^sofa.", cp), names(test_24))]]
      sofa.name <- "SOFA"
    }

    eval <- evalmod( # again need to beware here
      scores = list(rowSums(test_24[, score[[cp]], with=F]),
                    replace_na(sofa.cp, 0)),
      labels = list(test_24[["death"]], test_24[["death"]]),
      dsids = 1:2, modnames = paste(c("DOSE", sofa.name), cp), cb_alpha = 0.05
    )

    fx_roc[[cp]] <- auc(eval)$aucs[c(1, 3)]

    fx_prc[[cp]] <- auc(eval)$aucs[c(2, 4)]

    fx_plot[[cp]] <- autoplot(eval, "ROC") + geom_line(size = 1) +
      ggtitle(paste("ROC curve on", srcwrap(data_src) ,"at 24 hours")) +
      theme(legend.position = "bottom",
        legend.text = element_text(size=10))

  }

  list(fx_plot = fx_plot, fx_roc = fx_roc, fx_prc = fx_prc)

}
