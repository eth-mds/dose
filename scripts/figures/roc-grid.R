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
fx_tim <- hours(120L)

fxt_test <- lapply(
  src, function(data_src) {
    test <- load_data(data_src, cfg, fx_tim - hours(24L), fx_tim,
                      cohort = config("cohort")[[data_src]][["test"]])
    sf <- get_sofa(data_src, fx_tim)
    merge(test, sf, all.x = TRUE)
  }
)

cat("AUROC")
fxt_plots <- Map(dose_fxtp, fxt_test, list(score), src, nboot = 1)
fxt_plots <- do.call(rbind, do.call(c, lapply(fxt_plots, `[[`, "fx_plot")))
cat("all p < 0.05")

rm_rws <- which(fxt_plots$modname == "SOFA" & fxt_plots$component == "Metabolic")
fxt_plots <- fxt_plots[-rm_rws, ]

efig1 <- ggplot(subset(fxt_plots, curvetype == "ROC"), aes(x = x, y = y)) +
  geom_line(aes(color = modname)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray60") +
  facet_grid(rows = vars(component), cols = vars(source)) +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

ggsave(file.path(root, "figures", "eFigure4.tiff"), plot = efig1,
       width = 8.25, height = 11.75, type = "cairo", compression = "lzw",
       bg = "white")
