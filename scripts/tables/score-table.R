root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

score <- config("score")
cfg <- get_config("features", config_dir())

# construct the vectorized score
train_t <- load_data("miiv", cfg, hours(0L), hours(24L), 
                     cohort = config("cohort")[["miiv"]][["all"]])
train_t <- train_t[, setdiff(names(train_t), c(id_vars(train_t), "death")), 
                   with = F]
dose <- rep(0, ncol(train_t))
names(dose) <- names(train_t)
for (at in c("concept", "threshold", "right")) {
  
  attr(dose, at) <- as.vector(sapply(train_t, attr, at))
  
}

dose[names(dose) %in% unlist(score)] <- 1L

kableExtra::save_kable(score2table(dose), 
                       file = file.path(root, "tables", "Table2.html"))