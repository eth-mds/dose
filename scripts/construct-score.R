library(ricu)
library(ggplot2)
library(assertthat)
library(precrec)
library(matrixStats)
library(magrittr)
library(cowplot)
library(officer)
library(parallel)

root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
Sys.setenv(RICU_CONFIG_PATH = file.path(root, "config", "custom-dict"))

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

# train
set.seed(2022)
best <- auc_optimizer(train, cfg)
score <- lapply(best, `[[`, "cols")
config("best-marg", score)

decorr_score <- running_decorr(train, cfg, config("best-marg"))
config("dose", decorr_score)