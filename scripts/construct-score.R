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

train_time <- hours(24L) # hours(seq.int(24, 120, 24))
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
decorr_score <- running_decorr(train, cfg, score)
config("score", decorr_score)

# to explore the effects of decorrelation:
# lambda_seq <- seq(0, 1, length.out = 50)
# res <- mclapply(
#   lambda_seq, function(lam) {
#     running_decorr(train[[1]], test, cfg, score, lambda = lam, max_epoch = 10)
#   }
# )
# res <- as.data.frame(Reduce(rbind, res))
# names(res) <- c("all", names(score))
# res <- cbind(res, lambda_seq)
# for (var in names(res)) {
#   res[[var]] <- unlist(res[[var]])
# }
# 
# ggplot(reshape2::melt(res, id.vars = "lambda_seq", variable.name = "system", 
#                       value.name = "auc"), 
#        aes(x = lambda_seq, y = auc, color = system)) +
#   geom_line() + theme_bw() +
#   theme(
#     legend.position = "bottom",
#     legend.box.background = element_rect()
#   )