# 
# proj_root <- function() rprojroot::find_root(".gitmodules")
# 
# packages <- c("ricu", "rprojroot", "assertthat", "precrec", "ggplot2",
#               "kableExtra", "cowplot", "doMC", "glmnet")
# 
# installed <- packages %in% installed.packages()
# 
# if (any(!installed)) {
#   install.packages(packages[!installed])
# }
# 
# library(assertthat)
# library(ggplot2)
# 
# Sys.setenv(RICU_SRC_LOAD = "mimic,mimic_demo,eicu,eicu_demo,hirid,aumc",
#            RICU_CONFIG_PATH = file.path(proj_root(), "aumc", "config"))
# 
# library(ricu)
# 
# invisible(lapply(
#   grep(list.files(file.path(proj_root(), "aumc", "r"), full.names = TRUE),
#        pattern = "aaa.R$", value = TRUE, invert = TRUE),
#   source)
# )
