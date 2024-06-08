
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

cfg <- get_config("features", config_dir())
score <- config("score")
src <- c("miiv", "aumc", "sic")

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

      if (src[i] == "sic" & fold == "Development") next

      x <- aucs[[src[i]]][[fold]][[type]]
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

      res <- rbind(c("SOFA 2.0", "SOFA"), res)
      res <- rbind(c(fold, fold), res)
      res <- rbind(srcwrap(c(src[i], src[i])), res)
      tilz <- c(tilz, list(res))

    }

  }

  areas <- data.frame(Reduce(cbind, tilz))
  areas <- cbind(X0 = c("", "", "Component", scwrap(names(x))), areas)

  ft <- flextable(areas)
  ft <- font(ft, fontname = "Calibri (Headings)", part = "all")
  ft <- fontsize(ft, size = 10, part = "all")
  ft <- delete_part(ft, part = "header")
  ft <- merge_h(ft, i = 1:2)

  ft <- autofit(ft)
  ft <- fit_to_width(ft, 11)

  ft <- set_caption(ft,
                    caption = ifelse(type == "fx_roc",
                                     paste0(
                                       "eTable 2A. Area under receiver operator ",
                                       "characteristic (AUROC) per component ",
                                       "and dataset, for validation and ",
                                       "development cohorts"
                                     ),
                                     paste0(
                                       "eTable 2B. Area under precision recall ",
                                       "(AUPRC) per component ",
                                       "and dataset, for validation and ",
                                       "development cohorts"
                                     )))
}

doc <- read_docx() %>%
  body_add_flextable(ft_compute(aucs, src)) %>%
  body_end_section_landscape() %>%
  body_add_flextable(ft_compute(aucs, src, "fx_prc")) %>%
  body_end_section_landscape()

print(doc, file.path(root, "tables", "eTable2.docx"))
