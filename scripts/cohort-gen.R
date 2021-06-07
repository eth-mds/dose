library(ricu)
library(ggplot2)
library(assertthat)

r_dir <- file.path(rprojroot::find_root(".git/index"), "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

si_cohort("mimic_demo")

src <- c("mimic_demo", "eicu_demo", "mimic", "hirid", "aumc")

cohort <- lapply(src, si_cohort)
names(cohort) <- src

config("cohort", cohort)
