library(ricu)
library(ggplot2)

r_dir <- file.path(rprojroot::find_root(".git/index"), "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))


# load data across all three datasets
src <- c("mimic", "eicu")

win_lwr_train <- hours(-Inf)
win_upr_train <- hours(8L)

win_lwr_test <- hours(-Inf)
win_upr_test <- hours(8L:9L)

cfg <- get_config("concepts", config_dir())

dat_all <- lapply(src, function(x) {

  load_dictionary(x, names(cfg), aggregate = aggreg_fun(cfg),
                             id_type = "icustay", patient_ids = si_cohort(x))

})
names(dat_all) <- src

# construct train data
train_src <- "mimic"
dat <- preproc(dat_all[[train_src]], cfg, win_lwr_train, win_upr_train)
X_train <- indicator_encoding(dat, cfg)
outcome_train <- load_dictionary(train_src, "death", id_type = "icustay",
                              patient_ids = si_cohort(train_src))

outcome_train <- merge(dat, outcome_train, by = id(dat), all.x = T)[["death"]]
outcome_train[is.na(outcome_train)] <- FALSE

train_mat <- as.matrix(X_train)
colnames(train_mat) <- names(X_train)

# train to get the beta vector
dose_beta <- train_dose(train_mat, outcome_train, method = "robustFS") # or method = "robustFS"

dose_beta[dose_beta != 0] # inspect the score

# construct test data
test_src <- "eicu"
dat <- lapply(win_upr_test, function(t) preproc(dat_all[[test_src]], cfg, win_lwr_test, win_upr = hours(t)))

X_test <- lapply(dat, function(x) indicator_encoding(x, cfg))

outcome_test <- load_dictionary(test_src, "death", id_type = "icustay",
                              patient_ids = si_cohort(test_src))

outcome_test <- merge(dat[[1]], outcome_test, by = id(dat[[1]]), all.x = T)[["death"]]
outcome_test[is.na(outcome_test)] <- FALSE

sofa_info <- sofa(test_src, id_type = "icustay", patient_ids = dat[[1]][, id(dat[[1]]), with = FALSE],
                  explicit_wins = win_upr_test)

sofa_test <- lapply(1:length(win_upr_test), function(t) {
  
  sofa_curr <- sofa_info[get(index(sofa_info)) == win_upr_test[t], c(id(sofa_info), "sofa_score"), with = FALSE]
  sofa_curr <- merge(dat[[t]], sofa_curr, by = id(dat[[t]]), all.x = T)
  sofa_curr[is.na(sofa_score), "sofa_score"] <- 0
  
  return(sofa_curr[["sofa_score"]])
  
})

# compute the performance on test data
dose_test <- lapply(X_test, function(X) as.vector(as.matrix(X) %*% dose_beta))
dvs <- dose_vs_sofa(dose = dose_test, sofa = sofa_test, outcome = outcome_test)

# plot the results
if (length(win_upr_test) == 1) {

  ggplot(dvs, aes(x = score, y = auc, color = score)) +
    geom_point(size = 6) +
    geom_errorbar(mapping= aes(ymin=auc_min, ymax=auc_max), width=.25, size = 1) +
    theme_bw(25) + ylab("AUC") + xlab("Score") +
    theme(legend.position = c(0.8, 0.6), legend.background = element_rect(color = "black", size = 0.5)) +
    scale_colour_brewer(type = "qual", palette = 6, direction = 1)

} else {

  dvs <- cbind(dvs, time = rep(win_upr_test, each = 2))

  ggplot(dvs, aes(x = time, y = auc, color = score, fill = score)) +
    geom_line() + geom_ribbon(aes(ymin = auc_min, ymax = auc_max), alpha = 0.3, linetype = 0) +
    xlab("Hours in ICU") + ylab("AUC") + theme_bw(25) +
      theme(legend.position="bottom") + scale_colour_brewer(type = "qual", palette = 6, direction = 1)

}
