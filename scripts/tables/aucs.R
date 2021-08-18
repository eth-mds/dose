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

fxt_test <- lapply(
  src, function(data_src) {
    test <- load_data(data_src, cfg, hours(0L), hours(24L), 
                      patient_ids = config("cohort")[[data_src]][["test"]])
    sofa <- get_sofa(data_src, hours(24L))
    merge(test, all.x = T)
  }
)

fxt_plots <- Map(dose_fxtp, fxt_test, list(score, score, score), src)

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