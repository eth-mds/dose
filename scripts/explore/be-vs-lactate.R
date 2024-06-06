
compute_auc <- function(out, pred) {

  if(!all(out %in% c(0, 1))) return(0)
  # if(any(pred < 0 | pred > 1)) return(0)

  # Combine and sort by predicted probabilities in descending order
  data <- data.frame(out, pred)
  data <- data[order(data$pred, decreasing = TRUE),]

  # Calculate TPR and FPR
  n_pos <- sum(data$out == 1)
  n_neg <- sum(data$out == 0)
  cum_pos_rate <- cumsum(data$out == 1) / n_pos
  cum_neg_rate <- (1:nrow(data) - cumsum(data$out == 1)) / n_neg

  # Calculate AUC using trapezoidal rule
  auc <- sum((cum_neg_rate[-1] - cum_neg_rate[-nrow(data)]) *
               (cum_pos_rate[-1] + cum_pos_rate[-nrow(data)]) / 2)

  return(auc)
}

src <- "miiv"

be_vs_lact <- function(src) {

  pids <- si_cohort(src)
  tbl <- load_concepts(c("lact", "be"), src, patient_ids = pids)
  tbl <- tbl[get(index_var(tbl)) >= hours(0L) & get(index_var(tbl)) < hours(24L)]

  # take the worst value
  tbl <- tbl[, list(lact = max(lact), be = min(be)), by = c(id_vars(tbl))]

  tbl[is.na(lact), lact := 1]
  tbl[is.na(be), be := 0]

  # cor
  cat("Lactate / Base Excess correlation:", cor(tbl$lact, -tbl$be), "\n")

  # compute the outcome
  out <- load_concepts("death", src)

  tbl <- merge(tbl, out, all.x = TRUE)
  tbl[is.na(death), death := FALSE]

  # auc of the two things
  cat("AUC of Lactate", compute_auc(tbl$death, tbl$lact), "\n")
  cat("AUC of Base Excess", compute_auc(tbl$death, -tbl$be), "\n")
}

be_vs_lact("aumc")
be_vs_lact("sic")


