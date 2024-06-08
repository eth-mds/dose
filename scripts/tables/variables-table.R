
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

src <- c("miiv", "aumc", "sic")
cfg <- get_config("features", config_dir())

var_tbl <- function(src, cfg) {

  res <- load_data(src, cfg, hours(0L), hours(24L),
                   cohort = config("cohort")[[src]][["all"]], enc = FALSE)
  n_coh <- length(unique(id_col(res)))

  res[, c(id_vars(res)) := NULL]

  attr(res, "aucs") <- vapply(
    names(cfg),
    function(cnc) {
      round(
        PRROC::roc.curve(
          scores.class0 = res[[cnc]] *
            (-1)^(cfg[[cnc]][["direction"]] == "decreasing"),
          weights.class0 = res$death)$auc,
        digits = 3L
      )
    }, numeric(1L)
  )

  vars <- paste0(
    sapply(cfg, `[[`, "full_name"), " (",
    sapply(cfg, function(x) x[["unit"]][1L]), ")"
  )
  df <- data.frame(
    Variable = vars,
    AUROC = attr(res, "aucs"),
    counts = paste0(attr(res, "counts")[-c(1, 2)], " (",
                        round(attr(res, "counts")[-c(1, 2)] / n_coh) , ")"),
    med_iqr = attr(res, "med_iqr"),
    Category = sapply(cfg, function(x) scwrap(x[["category"]]))
  )
  names(df) <- c("Variable (unit)", "AUROC", "n (n/pp)", "Median [IQR]",
                 "Category")
  if (grepl("sic", src)) {
    df$AUROC <- NULL
  }
  df
}

tbl <- lapply(src, var_tbl, cfg = cfg)
res <- Reduce(function(x, y) merge(x, y, by = c("Variable (unit)", "Category")),
              tbl)

col_ord <- c(
  "Variable (unit)", grep("AUROC", names(res), value = TRUE),
  grep("^n ", names(res), value = TRUE),
  grep("Median", names(res), value = TRUE), "Category"
)

res <- res[order(res$AUROC.x + res$AUROC.y, decreasing = TRUE), col_ord]
hdr <- as.list(gsub("\\.[xy]", "", names(res)))
names(hdr) <- names(res)

res <- rbind(srcwrap(c("", src[-3], src, src, "")), res)

df_to_word(
  res,
  path = file.path(root, "tables", "eTable1.docx"),
  caption =
  paste0(
    "eTable 1. Comprehensive list of features that were assessed for predictive",
    " power in predicting mortality within the suspected infection cohorts. ",
    "Area under receiver operator characteristic (AUC) for the MIMIC-IV and AUMC",
    " cohorts (development cohorts) is reported. Number of measurements of each",
    " feature within the first 24 hours, median and IQR values are presented for",
    " each database. The organ failure category of each biomarker is also included."
  ),
  landscape = TRUE,
  header = hdr,
  fix_width = 11,
  footnotes = c(
    "APTT activate plasma thromboplastin time",
    "BP blood pressure",
    "CNS Central Nervous System",
    "GCS Glasgow Coma Scale", "FiO2 fraction of inspired oxygen",
    "INR international normalized ratio of prothrombine time",
    "MCH mean corpuscular hemoglobin",
    "MCHC mean corpuscular hemoglobin concentration",
    "MCV mean corpuscular volume",
    "NEQ norepinephrine equivalents (see eAppendix: Vasopressor Adjusted MAP)",
    "PaCO2 partial arterial CO2 pressure",
    "PaO2 partial arterial O2 pressure",
    "RBC red blood cells", "RDW red cell distribution width",
    "WBC white blood cells."
  )
)
