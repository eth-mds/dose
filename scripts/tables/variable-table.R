library(ricu)
library(stringr)
library(magrittr)
library(officer)

root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

cfg <- get_config("concepts", config_dir())

src <- c("mimic_demo", "eicu_demo")
cohorts <- lapply(src, si_cohort)

variable_tbl <- function(src, patient_ids) {
  var_tab <- lapply(
    names(cfg),
    function(x) {
      nnquant(load_concepts(x, src, patient_ids = patient_ids), patient_ids)
    }
  )
  
  var_tab <- Reduce(rbind, var_tab)
  
  dict <- get_config("concept-dict", ricu:::default_config_path())
  
  var_names <- Reduce(c, sapply(names(cfg), function(x) {
    nm <- str_to_sentence(
      paste0(dict[[x]][["description"]], " (", dict[[x]][["unit"]][1], ")")
    )
    clean_unit_str(nm)
  }))
  
  
  res <- data.frame(var_names, var_tab)
  names(res) <- c("Variable", "Reported", srcwrap(src))
  res
}

res <- Reduce(
  function(x, y) merge(x, y, by = c("Variable", "Reported")),
  Map(variable_tbl, src, cohorts)
)

my_doc <- read_docx()

my_doc <- my_doc %>%
  body_add_table(res, style = "table_template")

print(my_doc, target = file.path(root, "paper", "tables", "variable_table.docx"))
