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

src <- c("miiv", "aumc")

# train_time <- hours(24L)
# train <- list(
#   load_data(src[1], cfg, train_time - 24L, train_time,
#                      cohort = config("cohort")[[src[1]]][["train"]]),
#   load_data(src[2], cfg, train_time - 24L, train_time,
#             cohort = config("cohort")[[src[2]]][["train"]])
# )

train_time <- hours(seq.int(24, 120, 24))
train <- list()
for (i in seq_along(train_time)) {
  for (j in seq_along(src)) {
    train[[length(src) * (i-1) + j]] <-
      load_data(src[j], cfg, train_time[i] - 24L, train_time[i],
                cohort = config("cohort")[[src[j]]][["train"]])
  }
}

# train
best <- auc_optimizer(train, cfg)

score <- lapply(best, `[[`, "cols")
config("score", score)
