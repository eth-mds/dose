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
score <- config("score")

src <- c("miiv", "aumc", "hirid")

fxt_test <- lapply(
  src, function(data_src) {
    test <- load_data(data_src, cfg, hours(0L), hours(24L),
                      cohort = config("cohort")[[data_src]][["test"]])
    sf <- get_sofa(data_src, hours(24L))
    merge(test, sf, all.x = TRUE)
  }
)

fxt_plots <- Map(dose_fxtp, fxt_test, list(score), src)
fxt_plots <- do.call(rbind, do.call(c, lapply(fxt_plots, `[[`, "fx_plot")))

efig1 <- ggplot(subset(fxt_plots, curvetype == "ROC"), aes(x = x, y = y)) +
  geom_line(aes(color = modname)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray60") +
  facet_grid(rows = vars(component), cols = vars(source)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

ggsave(file.path(root, "figures", "eFigure1.tiff"), plot = efig1,
       width = 8.25, height = 11.75, type = "cairo", compression = "lzw",
       bg = "white")
