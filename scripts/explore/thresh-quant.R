library(ricu)
library(ggplot2)
library(assertthat)

root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
Sys.setenv(RICU_CONFIG_PATH = file.path(root, "config", "custom-dict"))

src <- "mimic"
patient_ids <- config("cohort")[[src]]

dat <- lapply(
  setdiff(names(config("features")), "etco2"),
  function(cnc) {
    cnc_meta <- config("features")[[cnc]]
    cnc_dir <- (-1)^(cnc_meta$direction == "decreasing")
    grid <- seq(cnc_meta$lower, cnc_meta$upper, cnc_meta$step) * cnc_dir
    
    samp <- load_concepts(cnc, src, patient_ids = patient_ids)[[cnc]]
    ticks <- ecdf(samp * cnc_dir)(grid)
    
    data.frame(ticks = ticks, concept = cnc_meta$full_name)
  }
)

df <- Reduce(rbind, dat)

ggplot(df, aes(x = ticks, y = concept)) +
  geom_line() + geom_point() + xlim(c(0, 1)) + theme_bw() +
  xlab("Quantile") + ylab("Concept") + ggtitle(srcwrap(src))
