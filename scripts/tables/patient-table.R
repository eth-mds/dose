library(ricu)
library(stringr)
library(magrittr)
library(officer)

root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

cfg <- get_config("concepts", config_dir())

src <- c("mimic", "aumc") # c("mimic_demo", "eicu_demo")
cohorts <- lapply(src, si_cohort)

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
    function(x) x[["callback"]](load_concepts(x[["concept"]], source, 
                                              patient_ids = patient_ids, 
                                              keep_components = T))
  )
  
  pts_tbl <- Reduce(rbind,
    lapply(
      tbl_list,
      function(x) data.frame(Reduce(cbind, x))
    )
  )
  
  cohort_info <- as.data.frame(cbind("Cohort size", "n", length(patient_ids)))
  names(cohort_info) <- names(pts_tbl)
  
  pts_tbl <- rbind(
    cohort_info,
    pts_tbl
  )
  
  names(pts_tbl) <- c("Variable", "Reported", srcwrap(source))
  levels(pts_tbl$Variable) <- vapply(as.character(pts_tbl$Variable), 
                                     function(x) concept_translator[[x]],
                                     character(1L))
  
  pts_tbl
}

res <- Reduce(
  function(x, y) merge(x, y, by = c("Variable", "Reported")),
  Map(pts_source_sum, src, cohorts)
)

data.table::setorderv(res, "Variable")

my_doc <- read_docx()

my_doc <- my_doc %>%
  body_add_table(res, style = "table_template")

print(my_doc, target = file.path(root, "paper", "tables", "patient_table.docx"))
