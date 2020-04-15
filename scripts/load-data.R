
r_dir <- file.path(rprojroot::find_root(".git/index"), "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

src <- "mimic_demo"
win_lwr <- hours(-Inf)
win_upr <- hours(8L)

cfg <- get_config("concepts", config_dir())

sus_inf <- si(src, id_type = "icustay")
pat_ids <- unique(sus_inf[[id(sus_inf)]])

dat_all <- load_dictionary(src, names(cfg), aggregate = aggreg_fun(cfg),
                           id_type = "icustay", patient_ids = pat_ids)

dat <- preproc(dat_all, cfg, win_lwr, win_upr)
dat <- indicator_encoding(dat, cfg)
