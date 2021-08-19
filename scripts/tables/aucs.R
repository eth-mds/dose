library(ricu)
library(ggplot2)
library(assertthat)
library(precrec)
library(matrixStats)
library(magrittr)
library(cowplot)
library(officer)
library(flextable)

root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))
Sys.setenv(RICU_CONFIG_PATH = file.path(root, "config", "custom-dict"))

cfg <- get_config("features", config_dir())
score <- config("score")
src <- c("miiv", "aumc", "hirid")


fxt_dev <- lapply(
  src, function(data_src) {
    test <- load_data(data_src, cfg, hours(0L), hours(24L),
                      cohort = config("cohort")[[data_src]][["train"]])
    sf <- get_sofa(data_src, hours(24L))
    merge(test, sf, all.x = T)
  }
)

fxt_val <- lapply(
  src, function(data_src) {
    test <- load_data(data_src, cfg, hours(0L), hours(24L),
                      cohort = config("cohort")[[data_src]][["test"]])
    sf <- get_sofa(data_src, hours(24L))
    merge(test, sf, all.x = T)
  }
)

aucs_dev <- Map(dose_fxtp, fxt_dev[-length(src)],
                list(score, score, score)[-length(src)],
                src[-length(src)])
aucs_val <- Map(dose_fxtp, fxt_val, list(score, score, score), src)
aucs_dev[[length(src)]] <- aucs_val[[length(src)]]

aucs <- Map(function(x, y) list(Development = x, Validation = y),
            aucs_dev, aucs_val)
names(aucs) <- src

ft_compute <- function(aucs, src, type = "fx_roc") {
  tilz <- list()
  for (i in seq_along(src)) {

    for (fold in c("Development", "Validation")) {
      
      if (src[i] == "hirid" & fold == "Development") next

      x <- aucs[[src[i]]][[fold]][["fx_roc"]]
      res <- Reduce(
        rbind,
        lapply(
          x,
          function(z) {
            acd <- lapply(
              z, function (y) {
                aci <- c(mean(y), mean(y) - 1.96 * sd(y),
                         mean(y) + 1.96 * sd(y))
                aci <- spec_dec(aci, 2)
                paste0(aci[1], " (", aci[2], "-", aci[3], ")")
              }
            )
            Reduce(c, acd)
          }
        )
      )

      res <- rbind(c("DOSE", "SOFA"), res)
      res <- rbind(c(fold, fold), res)
      res <- rbind(srcwrap(c(src[i], src[i])), res)
      tilz <- c(tilz, list(res))

    }

  }

  areas <- data.frame(Reduce(cbind, tilz))
  areas <- cbind(X0 = c("", "", "Component", names(x)), areas)

  ft <- flextable(areas)
  ft <- font(ft, fontname = "Calibri (Headings)", part = "all")
  ft <- fontsize(ft, size = 10, part = "all")
  ft <- delete_part(ft, part = "header")

  ft
}

doc <- read_docx() %>%
  body_add_flextable(ft_compute(aucs, src)) %>%
  body_end_section_landscape() %>%
  body_add_flextable(ft_compute(aucs, src, "fx_prc")) %>%
  body_end_section_landscape()

print(doc, file.path(root, "tables", "eTable2.docx"))
