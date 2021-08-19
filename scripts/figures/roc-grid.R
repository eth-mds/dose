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
    merge(test, sf, all.x = T)
  }
)

fxt_plots <- Map(dose_fxtp, fxt_test, list(score, score, score), src)

efig1 <- plot_grid(
  plotlist = Reduce(c, lapply(fxt_plots, function(x) x[["fx_plot"]])),
  ncol = 3L, byrow = FALSE
)

row_titl <- plot_grid(
  plotlist = lapply(
    names(score),
    function(cp) ggdraw() + draw_label(scwrap(cp), size = 12, angle = 270,
                                       hjust = 0)),
  ncol = 1L
)

efig1 <- plot_grid(efig1, row_titl, rel_widths = c(3, 0.1))

titl <- plot_grid(
  plotlist = lapply(
    c(src, "     "), function(dsrc) ggdraw() + draw_label(srcwrap(dsrc),
                                                          size = 14, vjust = 0.2)
  ), ncol = 3L
)

efig1 <- plot_grid(titl, efig1, rel_heights = c(0.15 / 7, 1), ncol = 1L)

efig1 <- plot_grid(efig1,
                   ggdraw() + draw_label("1 - Specificity", size = 10),
                   fxt_plots[[1]][["fx_legend"]][["cardio"]], ncol = 1L, 
                   rel_heights = c(1, 0.025, 0.025))
efig1 <- plot_grid(ggdraw() + draw_label("Sensitivity", size = 10, angle = 90,
                                         hjust = 1),
                   efig1, rel_widths = c(0.01, 1))

ggsave(file.path(root, "figures", "eFigure1.tiff"), plot = efig1,
       width = 8.25, height = 11.75, type = "cairo", compression = "lzw",
       bg = "white")
