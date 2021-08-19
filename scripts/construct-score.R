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

src <- c("miiv", "aumc", "hirid")

train_time <- hours(24L)
train <- list(
  load_data(src[1], cfg, train_time - 24L, train_time,
                     cohort = config("cohort")[[src[1]]][["train"]]),
  load_data(src[2], cfg, train_time - 24L, train_time,
            cohort = config("cohort")[[src[2]]][["train"]])
)

# train
best <- auc_optimizer(train, cfg)

score <- lapply(best, `[[`, "cols")
config("score", score)
