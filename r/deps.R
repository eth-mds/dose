
packages <- c("ricu", "rprojroot", "assertthat")

installed <- packages %in% installed.packages()

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(assertthat)
library(ricu)
