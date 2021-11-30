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

### vectorized score
score <- config("dose")
score[["metabolic"]] <- NULL
dose <- vec_score(score)

### determine times
times <- hours(seq.int(6, 24, 2))

evl <- list()
for (i in seq_along(src)) {

  test <- load_data(src[i], cfg, times - 24L, times,
                    cohort = config("cohort")[[src[i]]][["test"]])
  sf <- get_sofa(src[i], times)
  
  evl[[i]] <- dose_otp(test, times, dose, sf,
                       config("cohort")[[src[i]]][["test"]], src[i])
}

fig1 <- otp_fig(Reduce(rbind, evl))
ggsave(file.path(root, "figures", "eFigure3.tiff"), fig1,
       width = 12, height = 7, type = "cairo", compression = "lzw")
