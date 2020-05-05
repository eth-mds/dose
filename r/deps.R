
packages <- c("ricu", "rprojroot", "assertthat", "precrec", "ggplot2",
              "kableExtra", "cowplot", "doMC", "glmnet")

installed <- packages %in% installed.packages()

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(assertthat)
library(ggplot2)
library(ricu)
