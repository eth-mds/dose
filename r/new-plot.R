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

dose_fxtp <- function(test_24, score, data_src, boot = TRUE) {
  
  test_24[, dose := rowSums(test_24[, unlist(score), with=F])]
  fx_plot <- list()
  fx_roc <- list()
  fx_prc <- list()
  
  if (boot) {
    
    scores <- list()
    labels <- list()
    methods <- c("dose", "sofa")
    nboot <- 500
    
    for(bt in 1:nboot) {
      
      ids <- sample(1:nrow(test_24), nrow(test_24), replace = T)
      scores <- c(scores,
                  lapply(methods, function(x) as.numeric(test_24[[x]][ids])))
      labels <- c(labels,
                  lapply(1:length(methods), function(x) test_24[["death"]][ids]))
      
    }
    
    dsids <- rep(1:nboot, each = length(methods))
    modnames <- rep(methods, nboot)
    
    eval <- evalmod(scores = scores, labels = labels, dsids = dsids,
                    modnames = modnames)
    
    aurocs <- auc(eval)$aucs[c(T, F)]
    auprcs <- auc(eval)$aucs[c(F, T)]
    
    baseline <- which(methods == "dose")
    
    aurocs_full <- tapply(aurocs, factor(modnames, levels = unique(modnames)),
                          function(x) x, simplify = F)
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
    
    cat("AUROC for", srcwrap(data_src), "patients", round(mean(base_aucs), 3),
        "vs.", round(mean(cmp_aucs[methods == "sofa"]), 3), "\n")
    cat("p-value", pair.pvals[methods == "sofa"], "\n")
    
  }
  
  

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
      dsids = 1:2, 
      #modnames = paste(c("DOSE", sofa.name), cp),
      modnames = c("DOSE", "SOFA"),
      cb_alpha = 0.05
    )

    fx_roc[[cp]] <- auc(eval)$aucs[c(1, 3)]

    fx_prc[[cp]] <- auc(eval)$aucs[c(2, 4)]

    fx_plot[[cp]] <- autoplot(eval, "ROC") + geom_line(size = 1) +
      ggtitle(NULL) +
      #ggtitle(paste("ROC curve on", srcwrap(data_src) ,"at 24 hours")) +
      theme(legend.position = "none",
        legend.text = element_text(size=10),
        plot.margin = unit(c(0, -5, 0, -5), "cm")) + xlab(NULL) + ylab(NULL)
    
    if (data_src != "aumc" | cp != "metabolic") {
      
      fx_plot[[cp]] <- fx_plot[[cp]] + xlab(NULL)
      
    }
    
    if (data_src != "mimic" | cp != "renal") {
      
      fx_plot[[cp]] <- fx_plot[[cp]] + ylab(NULL)
      
    }
    
    if (cp != "metabolic") {
      
      fx_plot[[cp]] <- fx_plot[[cp]] + 
        theme(axis.text.x = element_text(color = "white"))
    }
    
    if (data_src != "mimic") {
      
      fx_plot[[cp]] <- fx_plot[[cp]] + 
        theme(axis.text.y = element_text(color = "white"))
      
    }
    

  }
  
  #fx_plot <- cowplot::plot_grid(plotlist = fx_plot, ncol = 1L)

  list(fx_plot = fx_plot, fx_roc = fx_roc, fx_prc = fx_prc)

}
