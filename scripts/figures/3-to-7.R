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
dose <- vec_score()

### determine times
times <- hours(seq.int(72, 168, 4))

evl <- list()
for (i in seq_along(src)) {

  test <- load_data(src[i], cfg, times - 24L, times,
                    cohort = config("cohort")[[src[i]]][["test"]])
  sf <- get_sofa(src[i], times)

  evl[[i]] <- dose_otp(test, times, dose, sf,
                       config("cohort")[[src[i]]][["test"]], src[i])
}

fig1 <- otp_fig(Reduce(rbind, evl))
ggsave(file.path(root, "figures", "Figure2.tiff"), fig1,
       width = 18, height = 8, type = "cairo", compression = "lzw")
