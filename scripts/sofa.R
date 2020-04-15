
r_dir <- file.path(rprojroot::find_root(".git/index"), "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

library(precrec)

src <- "mimic_demo"
sofa_time <- hours(8L)

sofa_score <- sofa(src, id_type = "icustay", patient_ids = si_cohort(src),
                   single_win = sofa_time)

mort_outco <- load_dictionary(src, "death", id_type = "icustay",
                              patient_ids = si_cohort(src))

dat <- merge(sofa_score, mort_outco)

sofa_cols <- setdiff(data_cols(dat), "death")
eval_sofa <- evalmod(scores = c(dat[, sofa_cols, with = FALSE]),
                     labels = dat[["death"]], modnames = sofa_cols)

print(eval_sofa)
ggplot2::autoplot(eval_sofa)
