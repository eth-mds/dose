library(ricu)
library(stringr)
library(magrittr)
library(officer)

root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

cfg <- get_config("concepts", config_dir())

src <- c("mimic", "aumc", "hirid")
cohorts <- lapply(src, si_cohort)
names(cohorts) <- src

vars <- list(
  age = list(
    concept = "age",
    callback = med_iqr
  ),
  admission = list(
    concept = "adm",
    callback = tab_design
  ),
  death = list(
    concept = "death",
    callback = percent_fun
  ),
  los_icu = list(
    concept = "los_icu",
    callback = med_iqr
  ),
  los_hosp = list(
    concept = "los_hosp",
    callback = med_iqr
  ),
  gender = list(
    concept = "sex",
    callback = tab_design
  ),
  sofa = list(
    concept = "sofa",
    callback = multi_med_iqr
  )
)

pts_source_sum <- function(source, patient_ids) {
  tbl_list <- lapply(
    vars,
    function(x) {
      
      if (source == "hirid" & x[["concept"]] == "adm") {
        return(list(c("med", "surg", "other"), "%", rep(NA_integer_, 3)))
      }
      x[["callback"]](load_concepts(x[["concept"]], source, 
                                    patient_ids = patient_ids, 
                                    keep_components = T))
      
    }
  )
  
  pts_tbl <- Reduce(rbind,
    lapply(
      tbl_list,
      function(x) data.frame(Reduce(cbind, x), stringsAsFactors = FALSE)
    )
  )
  
  cohort_info <- as.data.frame(cbind("Cohort size", "n", length(patient_ids)),
                               stringsAsFactors = FALSE)
  names(cohort_info) <- names(pts_tbl)
  
  pts_tbl <- rbind(
    cohort_info,
    pts_tbl
  )
  
  names(pts_tbl) <- c("Variable", "Reported", srcwrap(source))
  pts_tbl$Variable <- vapply(as.character(pts_tbl$Variable), 
                                     function(x) concept_translator[[x]],
                                     character(1L))
  
  pts_tbl
}

res <- Reduce(
  function(x, y) merge(x, y, by = c("Variable", "Reported"), sort = FALSE),
  Map(pts_source_sum, src, cohorts)
)

df_to_word(res, file.path(root, "tables", "Table1.docx"))
