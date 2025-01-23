
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
cfg <- get_config("features", config_dir())

src <- c("miiv", "aumc")

train_time <- hours(24L)
train <- list()
for (i in seq_along(train_time)) {
  for (j in seq_along(src)) {
    train[[length(src) * (i-1) + j]] <-
      load_data(src[j], cfg, train_time[i] - 24L, train_time[i],
                cohort = config("cohort")[[src[j]]][["train"]])
  }
}

# train marginally optimal score
set.seed(2024)
best <- auc_optimizer(train, cfg)
score <- lapply(best, `[[`, "cols")
config("best-marg", score)

# train overall optimal score
decorr_score <- running_decorr(train, cfg, score)
config("dose", decorr_score)

# replace (lactate, pafi) with (base excess, spfi)
decorr_score_II <- running_decorr(train, cfg, decorr_score,
                                  dev_country = TRUE)
config("dose-II", decorr_score_II)
