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
set.seed(2021)
train_cohort <- sample(config("cohort")[[src[1]]],
                       size = round(0.75 * length(config("cohort")[[src[1]]])))

times <- hours(seq.int(6, 24, 2))
at24 <- which(times == hours(24L))
train <- load_data(src[1], cfg, times - 24L, times, cohort = train_cohort)

sofa <- lapply(
  src, function(data_src) {
    res <- get_sofa(data_src, wins = times)
    replace_na(res, 0L)
  }
)
names(sofa) <- src

# train
train_time <- hours(16L)
train_slice <- which(times == train_time)

sofa_slice <- sofa[[src[1]]]
sofa_slice <- sofa_slice[get(index_var(sofa_slice)) == train_time]

best <- auc_optimizer(merge(train[[train_slice]], sofa_slice, all.x = T), cfg)

score <- lapply(best, `[[`, "cols")
score["cns"] <- NULL

config("score", score)
