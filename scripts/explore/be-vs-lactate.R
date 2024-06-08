
root <- rprojroot::find_root(".gitignore")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

compute_auc <- function(out, pred) {

  PRROC::roc.curve(scores.class0 = pred, weights.class0 = as.integer(out))$auc
}

be_vs_lact <- function(src) {

  pids <- si_cohort(src)
  tbl <- load_concepts(c("lact", "be", "inr_pt"), src, patient_ids = pids,
                       verbose = FALSE)
  tbl <- tbl[get(index_var(tbl)) >= hours(0L) & get(index_var(tbl)) < hours(24L)]

  # take the worst value
  tbl <- tbl[, list(lact = max(lact), be = min(be), inr_pt = max(inr_pt)),
             by = c(id_vars(tbl))]

  tbl[is.na(lact), lact := 1]
  tbl[is.na(be), be := 0]
  tbl[is.na(inr_pt), inr_pt := 1]

  # cor
  cat("Lactate / Base Excess correlation:", cor(tbl$lact, -tbl$be), "\n")

  # compute the outcome
  out <- load_concepts("death", src, verbose = FALSE)

  tbl <- merge(tbl, out, all.x = TRUE)
  tbl[is.na(death), death := FALSE]

  # auc of the two things
  cat("AUC of Lactate", compute_auc(tbl$death, tbl$lact), "\n")
  cat("AUC of Base Excess", compute_auc(tbl$death, -tbl$be), "\n")
  cat("AUC of INR(PT)", compute_auc(tbl$death, tbl$inr_pt), "\n")
}

be_vs_lact("aumc")
be_vs_lact("sic")
be_vs_lact("miiv")


