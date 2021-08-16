library(ricu)
library(ggplot2)
library(assertthat)
library(precrec)
library(matrixStats)
library(magrittr)
library(cowplot)
library(officer)

root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
Sys.setenv(RICU_CONFIG_PATH = file.path(root, "config", "custom-dict"))

cfg <- get_config("features", config_dir())

src <- c("mimic", "aumc", "hirid")
set.seed(2021)
train_cohort <- sample(config("cohort")[[src[1]]], 
                       size = round(0.75 * length(config("cohort")[[src[1]]])))

times <- hours(seq.int(6, 24, 2))
at24 <- which(times == hours(24L))
train <- load_data(src[1], cfg, times - 24L, times, cohort = train_cohort)

sofa <- lapply(
  src, function(data_src) {
    res <- load_concepts("sofa", data_src, 
                         explicit_wins = times, 
                         keep_components = T, 
                         verbose = F)
    
    replace_na(res, 0L)
  }
)
names(sofa) <- src

# train if score not available (need to nuke score)
if (!file.exists(file.path(proj_root(), "config", "score.json"))) {
  
  train_time <- hours(16L)
  train_slice <- which(times == train_time)
  
  sofa_slice <- sofa[[src[1]]]
  sofa_slice <- sofa_slice[get(index_var(sofa_slice)) == train_time]
  
  best <- auc_optimizer(merge(train[[train_slice]], sofa_slice, all.x = T), cfg)

  score <- lapply(best, `[[`, "cols")
  score["cns"] <- NULL
  
  config("score", score)
  
} else score <- config("score")

# construct the vectorized score
{
  train_t <- train[[1L]]
  train_t <- train_t[, setdiff(names(train_t), c(id_vars(train_t), "death")), 
                     with = F]
  dose <- rep(0, ncol(train_t))
  names(dose) <- names(train_t)
  for (at in c("concept", "threshold", "right")) {
    
    attr(dose, at) <- as.vector(sapply(train_t, attr, at))
    
  }
  
  dose[names(dose) %in% unlist(score)] <- 1L
  
  kableExtra::save_kable(score2table(dose), 
                         file = file.path(root, "tables", "Table2.html"))
  acs <- dose != 0
  dose_sparse <- dose[acs]
  cfg_sparse <- cfg[names(cfg) %in% attr(dose, "concept")[acs]]
}

# evaluate within
{
  test_cohort1 <- setdiff(config("cohort")[[src[1]]], train_cohort)
  test <- load_data(src[1], cfg_sparse, times - 24L, times, cohort = test_cohort1)
  
  int1 <- dose_otp(test, times, dose_sparse, sofa[[src[1]]], test_cohort1, 
                   src[1])
}

# evaluate outside 1st
{
  test2 <- load_data(src[2], cfg_sparse, times - 24L, times, 
                     cohort = config("cohort")[[src[2]]])
  ext1 <- dose_otp(test2, times, dose_sparse, sofa[[src[2]]], 
                   config("cohort")[[src[2]]], src[2])
}

# evaluate outside 2nd
{
  test3 <- load_data(src[3], cfg_sparse, times - 24L, times,
                     cohort = config("cohort")[[src[3]]])
  ext2 <- dose_otp(test3, times, dose_sparse, sofa[[src[3]]], 
                   config("cohort")[[src[3]]], src[3])
}

fig1 <- otp_fig(rbind(int1, ext1, ext2))
ggsave(file.path(root, "figures", "Figure1.tiff"), fig1,
       width = 12, height = 7)

all_test <- list(test, test2, test3)
names(all_test) <- src

fxt_test <- lapply(
  src, function(data_src) {
    test <- all_test[[data_src]]
    test <- test[[at24]]
    
    merge(test, sofa[[data_src]], all.x = T)
  }
)

fxt_plots <- Map(dose_fxtp, fxt_test, list(score, score, score), src)

efig1 <- plot_grid(
  plotlist = Reduce(c, lapply(fxt_plots, function(x) x[["fx_plot"]])),
  ncol = 3L, byrow = FALSE
)

row_titl <- plot_grid(
  plotlist = lapply(
    names(score), 
    function(cp) ggdraw() + draw_label(scwrap(cp), size = 12, angle = 270,
                                       hjust = 0)),
  ncol = 1L
)

efig1 <- plot_grid(efig1, row_titl, rel_widths = c(3, 0.1))

titl <- plot_grid(
  plotlist = lapply(
    c(src, "     "), function(dsrc) ggdraw() + draw_label(srcwrap(dsrc), 
                                                          size = 14, vjust = 0.2)
  ), ncol = 3L
)

efig1 <- plot_grid(titl, efig1, rel_heights = c(0.15 / 7, 1), ncol = 1L)

efig1 <- plot_grid(efig1, 
                   ggdraw() + draw_label("1 - Specificity", size = 10),
                   get_legend(fig1), ncol = 1L, rel_heights = c(1, 0.05, 0.05))
efig1 <- plot_grid(ggdraw() + draw_label("Sensitivity", size = 10, angle = 90,
                                         hjust = 1),
                   efig1, rel_widths = c(0.01, 1))

ggsave(file.path(root, "figures", "eFigure1.tiff"), plot = efig1, 
       width = 8.25, height = 11.75)

aucs <- Map(
  function(x, y) {
    df <- cbind(
      Reduce(rbind, x[["fx_roc"]]),
      Reduce(rbind, x[["fx_prc"]])
    )
    colnames(df) <- c("DOSE AUROC", "SOFA AUROC", "DOSE AUPRC", "SOFA AUPRC")
    rownames(df) <- names(x[["fx_roc"]])
    
    df <- round(df, digits = 2L)
    rbind(srcwrap(y), c("AUC", "AUC", "AUPRC", "AUPRC"), c("DOSE", "SOFA"), df)
  }, fxt_plots, src
)
aucs <- data.frame(Reduce(cbind, aucs))
df_to_word(cbind(rownames(aucs), aucs), 
           file.path(root, "tables", "eTable2.docx"),
           header = FALSE)