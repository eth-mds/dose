
r_dir <- file.path(rprojroot::find_root(".git/index"), "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

cfg <- list(
  alanine_aminotransferase = list(
    direction = "increasing", lower = 36, upper = 250, step = 5
  ),
  alkaline_phosphatase = list(
    direction = "increasing", lower = 104, upper = 500, step = 20
  ),
  asparate_aminotransferase = list(
    direction = "increasing", lower = 47, upper = 250, step = 5
  ),
  basophils = list(
    direction = "decreasing", lower = 0, upper = 0.2, step = 0.01
  ),
  bicarbonate = list(
    direction = "decreasing", lower = 7, upper = 24, step = 1
  ),
  bilirubin_total = list(
    direction = "increasing", lower = 1, upper = 30, step = 1
  ),
  calcium = list(
    direction = "decreasing", lower = 4.9, upper = 10.3, step = 0.2
  ),
  calcium_ionized = list(
    direction = "decreasing", lower = 0.6, upper = 1.15, step = 0.1
  ),
  calculated_total_co2 = list(
    direction = "decreasing", lower = 7, upper = 25, step = 1
  ),
  chloride = list(
    direction = "decreasing", lower = 80, upper = 104, step = 2
  ),
  creatinine = list(
    direction = "increasing", lower = 1.1, upper = 14, step = 1
  ),
  diastolic_bp = list(
    direction = "decreasing", lower = 20, upper = 80, step = 5
  ),
  eosinophils = list(
    direction = "decreasing", lower = 0, upper = 1, step = 0.05
  ),
  fi_o2 = list(
    direction = "increasing", lower = 35, upper = 100, step = 5
  ),
  heart_rate = list(
    direction = "increasing", lower = 50, upper = 120, step = 5
  ),
  hematocrit = list(
    direction = "decreasing", lower = 15, upper = 30, step = 1
  ),
  hemoglobin = list(
    direction = "decreasing", lower = 7, upper = 10, step = 0.1
  ),
  inr_pt = list(
    direction = "increasing", lower = 1.1, upper = 4, step = 0.3
  ),
  lactate = list(
    direction = "increasing", lower = 1, upper = 20, step = 1
  ),
  lymphocytes = list(
    direction = "decreasing", lower = 0, upper = 12, step = 0.5
  ),
  magnesium = list(
    direction = "increasing", lower = 2, upper = 4.3, step = 0.1
  ),
  mch = list(
    direction = "increasing", lower = 30, upper = 40, step = 0.1
  ),
  mchc = list(
    direction = "decreasing", lower = 27.6, upper = 33.6, step = 0.3
  ),
  mcv = list(
    direction = "increasing", lower = 90, upper = 125, step = 1
  ),
  mean_bp = list(
    direction = "decreasing", lower = 40, upper = 75, step = 5
  ),
  neutrophils = list(
    direction = "increasing", lower = 0, upper = 80.6, step = 5
  ),
  o2_saturation = list(
    direction = "decreasing", lower = 80, upper = 100, step = 1
  ),
  pa_co2 = list(
    direction = "decreasing", lower = 16, upper = 40, step = 2
  ),
  pa_o2 = list(
    direction = "decreasing", lower = 55, upper = 105, step = 2
  ),
  ph = list(
    direction = "decreasing", lower = 6.9, upper = 7.4, step = 0.2
  ),
  phosphate = list(
    direction = "increasing", lower = 3.4, upper = 12.4, step = 0.5
  ),
  platelet_count = list(
    direction = "decreasing", lower = 10, upper = 150, step = 5
  ),
  potassium = list(
    direction = "increasing", lower = 4, upper = 8.4, step = 0.2
  ),
  prothrombine_time = list(
    direction = "increasing", lower = 14, upper = 40, step = 0.5
  ),
  ptt = list(
    direction = "increasing", lower = 40, upper = 95, step = 5
  ),
  rdw = list(
    direction = "increasing", lower = 15, upper = 28, step = 1
  ),
  red_blood_cells = list(
    direction = "decreasing", lower = 1.58, upper = 3.33, step = 0.1
  ),
  respiratory_rate = list(
    direction = "increasing", lower = 14, upper = 30, step = 2
  ),
  sodium = list(
    direction = "decreasing", lower = 115, upper = 140, step = 2
  ),
  systolic_bp = list(
    direction = "decreasing", lower = 70, upper = 120, step = 5
  ),
  urea_nitrogen = list(
    direction = "increasing", lower = 15, upper = 50, step = 5
  ),
  white_blood_cells = list(
    direction = "increasing", lower = 10, upper = 35, step = 1
  )
)

src <- "mimic_demo"
max_step <- 30

dat <- load_dictionary(src, c(names(cfg), "death"), id_type = "icustay",
                       patient_ids = si_cohort(src))

win <- stay_windows(src, id_type = "icustay", win_type = "icustay",
                    in_time = "intime", out_time = "outtime")
win <- win[, c("intime") := get("outtime") - hours(24L)]

dat <- dat[, c("tmptime") := get(index(dat))]
joi <- c(paste(id(dat), "==", id(win)), "tmptime >= intime",
         "tmptime <= outtime")
dat <- dat[win, on = joi, nomatch = NULL]

dir <- dat[, lapply(.SD, mean, na.rm = TRUE), by = "death",
           .SDcols = names(cfg)]
dir <- t(dir)[-1L, ]
dir <- ifelse(dir[, 1L] < dir[, 2L], "decreasing", "increasing")

failed <- names(
  dir[vapply(cfg, `[[`, character(1L), "direction")[names(dir)] != dir]
)

if (length(failed)) {
  message("for the following features, the direction does not agree with ",
          "data:\n  * ", paste0("`", failed, "`", collapse = "\n  * "))
}

count_steps <- function(x) (x[["upper"]] - x[["lower"]]) / x[["step"]]

n_steps <- vapply(cfg, count_steps, numeric(1L))

failed <- n_steps[n_steps > max_step]

if (length(failed)) {
  message("for the following features, more than ", max_step, " steps are ",
          "configured:\n  * ", paste0("`", names(failed), "` (",
          ceiling(failed), ")", collapse = "\n  * "))
}

full_dict <- read_dictionary("concept-dict")

for (src in c("mimic", "eicu", "hirid")) {

  failed <- setdiff(names(cfg), names(full_dict[, source = src]))

  if (length(failed)) {
    message("the following features are missing from `", src, "`:\n  * ",
            paste0("`", failed, "`", collapse = "\n  * "))
  }
}

set_config(cfg[order(names(cfg))], "concepts", config_dir())
