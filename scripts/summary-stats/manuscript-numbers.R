library(ricu)
library(ggplot2)
library(precrec)
library(assertthat)
library(data.table)

r_dir <- file.path(rprojroot::find_root(".git/index"), "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

cohort <- list(
  mimic = si_cohort("mimic"),
  aumc = si_cohort("aumc")
)

n_measures <- function(cnc, src, patient_ids, upto = hours(24L)) {
  
  cat(srcwrap(src), "\n")
  cat("Cohort size:", length(patient_ids), "\n")
  
  for(cncpt in cnc) {
    
    tbl <- load_concepts(cncpt, src, patient_ids = patient_ids,
      verbose = F)
    tbl <- tbl[get(index_var(tbl)) <= upto]
    
    cat("Number of", 
      get_config("concept-dict")[[cncpt]][["description"]],
      "measurements upto 24 hours into ICU:",
      nrow(tbl), "\n")
    
  }
  
}

concept_list <- c("pafi", "ast", "lact", 
  "map", "norepi_equiv", "lymph", "ptt", "inr_pt",
  "bun")

n_measures(concept_list, "mimic", cohort[["mimic"]])
n_measures(concept_list, "aumc", cohort[["aumc"]])
