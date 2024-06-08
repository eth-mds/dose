
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

dose <- vec_score(config("dose"))
save_as_docx(score2table(dose), path = file.path(root, "tables", "Table2.docx"))

dose_lmic <- vec_score(config("dose-II"))
save_as_docx(score2table(dose_lmic),
             path = file.path(root, "tables", "Table2-LMIC.docx"))
