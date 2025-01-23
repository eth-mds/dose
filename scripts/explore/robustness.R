
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

cardio_data <- function(src) {
  
  cfg <- list()
  cfg[["norepi_equiv"]] <- list(
    direction = "increasing", lower = 0, upper = 0.5, step = 0.01,
    category = "cardio", full_name = "Norepinephrine Equivalents", 
    unit = "mcg/kg/min"
  )
  
  dt1 <- load_data(src, cfg, lwr = hours(0L), upr = hours(24L),
                   enc = FALSE, impute_vals = TRUE, dat_nm = "neq_dat")
  dt1[, death := NULL]
  dt2 <- load_data(src, get_config("features", config_dir()), 
                   lwr = hours(0L), upr = hours(24L),
                   enc = FALSE, impute_vals = TRUE)
  
  dt <- merge(dt2, dt1, all.x = TRUE)
  dt[is.na(norepi_equiv), norepi_equiv := 0]
  dt[, c(id_vars(dt), "map_beta200", "norepi_equiv", "death"), with=FALSE]
}

four_thresh <- function(dt, tst, feat = c("map_beta200", "norepi_equiv")) {
  
  cfg <- get_config("features", config_dir())
  cfg[["norepi_equiv"]] <- list(
    direction = "increasing", lower = 0, upper = 0.5, step = 0.01,
    category = "cardio", full_name = "Norepinephrine Equivalents", 
    unit = "mcg/kg/min"
  )
  
  thrs <- seq(cfg[[feat]]$lower, cfg[[feat]]$upper, by = cfg[[feat]]$step)
  if (feat == "map_beta200") {
    
    thrs <- -thrs
    dt[, c(feat) := -get(feat)]
    tst[, c(feat) := -get(feat)]
  }
  cpt <- replicate(500, sample(thrs, 4))
  
  best_auc <- 0
  for (i in 1:ncol(cpt)) {
    
    score <- 0  
    for (t in 1:4) score <- score + (dt[[feat]] > cpt[t, i])
    score <- score / 4
    
    curr_auc <- compute_auc(dt$death, score)
    if (curr_auc > best_auc) {
      
      dt[, score := score]
      best_auc <- curr_auc
      tst_score <- 0
      for (t in 1:4) tst_score <- tst_score + (tst[[feat]] > cpt[t, i])
      tst[, score := tst_score / 4]
    }
  }
  
  # get bootstrap estimates of AUC 
  aucs <- compute_auc(tst$death, tst$score, boot = TRUE)
  res <- data.frame(auc = mean(aucs), auc_sd = sd(aucs))
  
  # get bootstrap estimates of the Brier Score
  probs <- dt[, list(pred_p = mean(death)), by = "score"]
  
  bss <- c()
  for (bt in 1:100) {
    
    tst_bt <- tst[sample(nrow(tst), replace = TRUE)]
    probs_bt <- merge(
      probs,
      merge(
        tst_bt[, list(obs_p = mean(death)), by = "score"],
        tst[, list(size = .N), by = "score"], by = "score"
      ), by = "score"
    )
    
    bss <- c(bss, sum((probs_bt$pred_p - probs_bt$obs_p)^2 * probs_bt$size) / 
                    sum(probs_bt$size))
  }
  
  res <- cbind(res, data.frame(bs = mean(bss), bs_sd = sd(bss)))
  res$feat <- feat
  res
}

train <- rbind(cardio_data("aumc"), cardio_data("sic"))
test <- cardio_data("miiv")

res <- NULL
for (feat in c("map_beta200", "norepi_equiv")) {
  
  res <- rbind(res, four_thresh(copy(test), copy(train), feat))
}

ggplot(
  res, aes(x = bs, y = auc, color = feat)
) +
  geom_errorbar(aes(ymin = auc - auc_sd, ymax = auc + auc_sd)) +
  geom_errorbarh(aes(xmin = bs - bs_sd, xmax = bs + bs_sd)) +
  theme_bw()
