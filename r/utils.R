
proj_root <- function() rprojroot::find_root(".git/index")

config_dir <- function() file.path(proj_root(), "config")

aggreg_fun <- function(cfg, inc = "max", dec = "min") {

  dir <- vapply(cfg, `[[`, character(1L), "direction")

  assert_that(setequal(dir, c("increasing", "decreasing")))

  ifelse(dir == "increasing", inc, dec)
}

si_cohort <- function(source, ...) {

  if (grepl("eicu", source)) {

    susp_infec <- si(source, abx_min_count = 2L, positive_cultures = TRUE,
                     id_type = "icustay", si_mode = "or", ...)

  } else if (identical(source, "hirid")) {

    susp_infec <- si(source, abx_min_count = 2L, id_type = "icustay",
                     si_mode = "or", ...)

  } else {

    susp_infec <- si(source, id_type = "icustay", ...)
  }

  unique(susp_infec[[id(susp_infec)]])
}

strat_samp <- function(dat, by, frac = 0.75, replace = FALSE) {

  samp <- function(x, repl) sample(x, length(x) * frac, repl)

  assert_that(inherits(dat, "data.frame"), has_name(dat, by))

  rows <- split(seq_len(nrow(dat)), dat[, by, with = FALSE])

  unlist(lapply(rows, samp, replace))
}

score2table <- function(score) {

  tbl <- cbind(Feature = attr(score, "concept"),
               Points = score)

  rownames(tbl) <- paste(ifelse(attr(score, "right"), ">=", "<"),
                                attr(score, "threshold"))

  res <- kableExtra::kable(tbl[score > 0, 2L, drop = FALSE])

  ind <- rle(tbl[score > 0, 1L])
  ind <- stats::setNames(ind[["lengths"]], gsub("_", " ", ind[["values"]]))

  res <- kableExtra::pack_rows(res, index = ind)

  res
}

get_cores <- function(n_cores = NULL) {

  max_cores <- as.integer(
    Sys.getenv("LSB_DJOB_NUMPROC", unset = parallel::detectCores())
  )

  if (is.null(n_cores)) {
    n_cores <- max_cores
  }

  if (n_cores > max_cores) {
    warning("asked for ", n_cores, " cores but only ", max_cores,
            " are available")
    n_cores <- max_cores
  }

  n_cores
}

roc_pr_plot <- function(x, ..., nrow_legend = 1) {

  roc <- autoplot(x, "ROC", ...)
  prc <- autoplot(x, "PR", ...)

  legend <- cowplot::get_legend(
    roc + guides(color = guide_legend(nrow = nrow_legend)) +
          theme(legend.position = "bottom")
  )

  plot <- cowplot::plot_grid(roc + theme(legend.position = "none"),
                             prc + theme(legend.position = "none"), ncol = 2)

  cowplot::plot_grid(plot, legend, nrow = 2, rel_heights = c(1, .1))
}

auc <- function(x) data.table::setDT(precrec::auc(x))
