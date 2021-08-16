library(ricu)
library(ggplot2)
library(assertthat)
library(precrec)
library(magrittr)
library(officer)

root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
Sys.setenv(RICU_CONFIG_PATH = file.path(root, "config", "custom-dict"))

src <- c("mimic", "aumc", "hirid")
cfg <- get_config("features", config_dir())

var_tbl <- function(src, cfg) {
  
  res <- load_data(src, cfg, hours(0L), hours(24L), enc = FALSE)
  n_coh <- length(unique(id_col(res)))
  
  res[, c(id_vars(res)) := NULL]
  
  attr(res, "aucs") <- vapply(
    names(cfg),
    function(cnc) {
      round(
        PRROC::roc.curve(
          scores.class0 = res[[cnc]] * 
            (-1)^(cfg[[cnc]][["direction"]] == "decreasing"), 
          weights.class0 = res$death)$auc,
        digits = 3L
      )
    }, numeric(1L)
  )
  
  vars <- paste0(
    sapply(cfg, `[[`, "full_name"), " (", 
    sapply(cfg, function(x) x[["unit"]][1L]), ")"
  )
  df <- data.frame(
    Variable = vars,
    AUROC = attr(res, "aucs"),
    counts = paste0(attr(res, "counts")[-c(1, 2)], " (", 
                        round(attr(res, "counts")[-c(1, 2)] / n_coh, 2L) , ")"),
    med_iqr = attr(res, "med_iqr"),
    Category = sapply(cfg, function(x) scwrap(x[["category"]]))
  )
  names(df) <- c("Variable (unit)", "AUROC", "n (n/pp)", "Median [IQR]", 
                 "Category")
  if (grepl("mimic", src)) {
    ord <- order(df$AUROC, decreasing = TRUE)
    config("feature-order", list(ord))
  }
  ord <- config("feature-order")[[1]]
  df <- df[ord, ]
  
}

for (dsrc in src) {
  
  df_to_word(
    var_tbl(dsrc, cfg),
    file.path(root, "tables", paste0("eTable1", "_", dsrc, ".docx"))
  )
  
}
