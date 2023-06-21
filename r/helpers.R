
srcwrap <- function(src) {
  if (length(src) > 1) return(sapply(src, srcwrap))

  if(src == "mimic") {
    return("MIMIC-III")
  } else if (src == "miiv") {
    return("MIMIC-IV")
  } else if (src == "eicu") {
    return("eICU")
  } else if (src == "hirid") {
    return("HiRID")
  } else if (src == "mimic_demo") {
    return("MIMIC-III (demo)")
  } else if (src == "eicu_demo") {
    return("eICU (demo)")
  } else if (src == "aumc") {
    return("AUMC")
  } else if (src == "sic") {
    return("SIC")
  } else {
    return(src)
  }
}

clean_unit_str <- function(x) {
  x <- gsub("/l", "/L", x)
  x <- gsub("/dl", "/dL", x)
  x <- gsub("mmhg", "mmHg", x)
  x <- gsub("meq", "mEq", x)
  x <- gsub("\\(iu", "\\(IU", x)
  x
}

nnquant <- function(x, y, upto = hours(24L)) {

  x <- x[get(index_var(x)) <= upto]
  val_col <- setdiff(names(x), meta_vars(x))
  x <- x[[val_col]]

  med <- spec_dec(median(x, na.rm = T), 2)
  IQR <- quantile(x, c(0.25, 0.75), na.rm = T)
  n <- length(x)

  res <- paste0(med, " (", spec_dec(IQR[1], 2), "-", spec_dec(IQR[2], 2), ")")

  c(
    "Number,\n Number per patient stay,\n Median (IQR)",
    paste(n, spec_dec(sum(!is.na(x))/length(y), 2), res, sep = ",\n")
  )

}

med_iqr <- function(x, y) {
  val_col <- setdiff(names(x), meta_vars(x))
  if(is_ts_tbl(x)) x <- x[get(index_var(x)) == 24L]
  quants <- quantile(x[[val_col]], probs = c(0.25, 0.5, 0.75), na.rm = T)
  if (all(x[[val_col]] == as.integer(x[[val_col]]))) {
    res <- paste0(
      round(quants[2]), " (",
      round(quants[1]), "-",
      round(quants[3]), ")"
    )
  } else {
    res <- paste0(
      spec_dec(quants[2], 2), " (",
      spec_dec(quants[1], 2), "-",
      spec_dec(quants[3], 2), ")"
    )
  }

  list(val_col, "Median (IQR)", res)
}

multi_med_iqr <- function(x, y) {
  x <- replace_na(x, 0L)
  val_cols <- setdiff(names(x), meta_vars(x))
  res <- lapply(
    val_cols, function(vcol) med_iqr(x[, c(meta_vars(x), vcol), with = FALSE], y)
  )
  lapply(1:3, function(i) {
    Reduce(c, lapply(res, `[[`, i))
  })
}

tab_design <- function(x, y) {
  val_col <- setdiff(names(x), meta_vars(x))
  res <- table(x[[val_col]])
  res <- spec_dec(100 * res / sum(res), 0)

  list(names(res), "%", as.integer(res))
}

percent_fun <- function(x, y) {
  val_col <- setdiff(names(x), meta_vars(x))
  list(val_col, "%", spec_dec( 100 * sum(x[[val_col]]) / length(y), 1))
}

concept_translator <- list(
  age = "Age (years)",
  med = "- Medical",
  other = "- Other",
  surg = "- Surgical",
  death = "Mortality",
  `Cohort size` = "Cohort size",
  los_icu = "ICU LOS",
  los_hosp = "Hospital LOS (days)",
  Male = "Gender (Male)",
  Female = "Gender (Female)",
  sofa = "- Total",
  sofa_resp_comp = "- Respiratory",
  sofa_coag_comp = "- Coagulation",
  sofa_cns_comp = "- CNS",
  sofa_liver_comp = "- Hepatic",
  sofa_cardio_comp = "- Cardiovascular",
  sofa_renal_comp = "- Renal"
)

vec_score <- function(score = config("score")) {

  cfg <- config("features")
  train_t <- load_data("mimic_demo", cfg, hours(0L), hours(24L),
                       cohort = config("cohort")[["mimic_demo"]][["test"]])
  train_t <- train_t[, setdiff(names(train_t), c(id_vars(train_t), "death")),
                     with = F]
  dose <- rep(0, ncol(train_t))
  names(dose) <- names(train_t)
  for (at in c("concept", "threshold", "right")) {

    attr(dose, at) <- as.vector(sapply(train_t, attr, at))

  }

  dose[names(dose) %in% unlist(score)] <- 1L
  dose
}

n_cores <- function() {
  as.integer(
    Sys.getenv("LSB_DJOB_NUMPROC", unset = parallel::detectCores() / 2L)
  )
}
