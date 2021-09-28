
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
Sys.setenv(RICU_CONFIG_PATH = file.path(root, "config", "custom-dict"))

library(ricu)
library(assertthat)
library(precrec)
library(ggplot2)

dat <- load_concepts("gcs_sed", "miiv")

gcs_val <- function(src, cohort = si_cohort(src), lwr = hours(0L), upr = hours(24L)) {
  
  tbl <- load_concepts(c("gcs_raw", "sed"), src, patient_ids = cohort)
  tbl <- tbl[get(index_var(tbl)) >= lwr & get(index_var(tbl)) < upr]
  tbl <- tbl[, head(.SD, which.min(gcs_raw)[1]), by = eval(id_var(tbl))]
  tbl <- tbl[, tail(.SD, 1), by = eval(id_var(tbl))]
  
  out <- load_concepts(c("death", "adm"), src, patient_ids = cohort)
  res <- merge(tbl, out, by = id_var(tbl), all.x = T)
  
  res[is.na(death), "death"] <- FALSE
  res[is.na(sed), "sed"] <- FALSE

  res
}

gcs_rocs <- function(src, gcs = gcs_val(src)) {
  grid <- expand.grid(c("med", "surg"), c(TRUE, FALSE))
  
  scores <- lapply(1:nrow(grid), function(i) -gcs[adm == grid[i, 1] & sed == grid[i, 2]][["gcs_raw"]])
  labels <- lapply(1:nrow(grid), function(i) gcs[adm == grid[i, 1] & sed == grid[i, 2]][["death"]])
  
  eval <- evalmod(
    scores = scores, 
    labels = labels,
    dsids = 1:nrow(grid), 
    modnames = paste("Admission:", grid$Var1, ",", "Sedation:", grid$Var2)
  )
  
  autoplot(eval, "ROC") + geom_line(size = 1) + ggtitle(paste0("GCS raw on ", srcwrap(src))) +
    theme(legend.position = c(0.7, 0.25), 
      legend.box.background = element_rect(color="black", size=2),
      legend.text = element_text(size = 12))
}

gcs1 <- gcs_val("mimic")
p1 <- gcs_rocs("mimic", gcs1)

gcs2 <- gcs_val("aumc")
p2 <- gcs_rocs("aumc", gcs2)

gcs_bg <- cowplot::plot_grid(p1, p2)

ggsave("plots/gcs_by_group.png", plot = gcs_bg, width = 12, height = 8)
