library(assertthat)
library(ricu)
library(matrixStats)
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

dose <- vec_score()
kableExtra::save_kable(score2table(dose),
                       file = file.path(root, "tables", "Table2.html"))
