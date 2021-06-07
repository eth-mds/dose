
proj_root <- function() rprojroot::find_root(".git/index")

config_dir <- function() file.path(proj_root(), "config")

aggreg_fun <- function(cfg, inc = "max", dec = "min") {

  dir <- vapply(cfg, `[[`, character(1L), "direction")

  #assert_that(setequal(dir, c("increasing", "decreasing")))

  ifelse(dir == "increasing", inc, dec)
}

si_cohort <- function(source, age_threshold = 18L, ...) {

  if (grepl("eicu", source)) {

    patient_ids <- get_config("eicu-cohort", config_dir())[["eicu"]][["all"]]

    susp_infec <- load_concepts("susp_inf", source, abx_min_count = 2L,
      positive_cultures = T, si_mode = "or", patient_ids = patient_ids, ...)

  } else if (identical(source, "hirid")) {

    susp_infec <- load_concepts("susp_inf", source, abx_min_count = 2L,
                     si_mode = "or", ...)

  } else {

    susp_infec <- load_concepts("susp_inf", source, id_type = "icustay", ...)

  }

  si_lwr <- hours(48L)
  si_upr <- hours(24L)
  susp_infec <- susp_infec[is_true(get("susp_inf")), ]
  susp_infec <- susp_infec[, c("susp_inf") := NULL]
  susp_infec <- susp_infec[, c("si_lwr", "si_upr") := list(
    get(index_var(susp_infec)) - si_lwr,
    get(index_var(susp_infec)) + si_upr
  )]

  susp_infec <- susp_infec[(si_lwr <= 0L) & (si_upr >= 0L)]

  above_age <- load_concepts("age", source)[age > age_threshold]

  unique(intersect(id_col(susp_infec), id_col(above_age)))
}

strat_samp <- function(dat, by, frac = 0.75, replace = FALSE) {

  samp <- function(x, repl) sample(x, length(x) * frac, repl)

  assert_that(inherits(dat, "data.frame"), has_name(dat, by))

  rows <- split(seq_len(nrow(dat)), dat[, by, with = FALSE])

  unlist(lapply(rows, samp, replace))
}

score2table <- function(score) {
  flip_sum <- function(x) {
    x <- if (all(x[["Dir"]])) x else x[rev(seq_len(nrow(x))), ]
    x <- x[x[["Points"]] > 0, ]
    x[["Points"]] <- cumsum(x[["Points"]])
    x
  }

  dict <- get_config("concept-dict")
  full_names <- sapply(attr(score, "concept"), function(x) stringr:::str_to_title(dict[[x]][["description"]]))
  tbl <- data.frame(
    Feature = full_names, Dir = attr(score, "right"),
    Thresh = attr(score, "threshold"), Points = score, stringsAsFactors = FALSE
  )
  tbl <- do.call(rbind, lapply(split(tbl, tbl[["Feature"]]), flip_sum))
  tbl[["Thresh"]] <- paste(ifelse(tbl[["Dir"]], "&ge;", "&lt;"),
                           tbl[["Thresh"]])
  res <- kableExtra::kable(tbl[ , c("Thresh", "Points")], row.names = FALSE,
                           col.names = c("", "Points"), escape = FALSE)
  ind <- rle(tbl[ , "Feature"])
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

is_difftime <- function(x) inherits(x, "difftime")
is_interval <- function(x) {
  assert_that(is_difftime(x), length(x) > 0L) && all(x >= 0)
}

noreq <- function(source, press = "map", beta = 10, patient_ids = NULL, upr = hours(24L), lwr = hours(0L)) {

  na_z <- function(x) ifelse(is.na(x), 0, x)
  col_name <- paste0(press, beta)
  imp_val <- cfg[[press]][["upper"]]*1.01

  tbl <- load_concepts(c("norepi_rate", "epi_rate", "dopa_rate", "dobu_rate", press),
    source, patient_ids = patient_ids)
  tbl[, noreq := (na_z(norepi_rate) + na_z(epi_rate) + na_z(dopa_rate)/150 + na_z(dobu_rate > 0)*0.015)]

  tbl[, cf_bp := nafill(get(press), "locf")]

  tbl[, c(col_name) := (cf_bp - beta*noreq)]

  tbl[, c(meta_vars(tbl), col_name), with = FALSE]

  tbl <- tbl[get(index_var(tbl)) <= upr & get(index_var(tbl)) >= lwr,
      -min(get(col_name), na.rm = T), by = eval(id_var(tbl))]

  setnames(tbl, "V1", col_name)
  tbl <- replace_na(tbl, imp_val)

  tbl
}

load_cts <- function(src, cfg, lwr, upr, cohort = si_cohort(src), load_sofa = F) {

  load_win <- function(lwr, upr, cfg, dat, out) {

    dat <- preproc(dat, cfg, lwr, upr)
    #dat <- indicator_encoding(dat, cfg)

    ret <- merge(dat, out, all.x = T)
    ret[is.na(death), "death"] <- F

    ret
  }

  load_wins <- function(lwr, upr, cfg, dat, out) {
    Map(function(a, b) {
      load_win(as.difftime(a, units = units(lwr)),
        as.difftime(b, units = units(upr)), cfg, dat, out)
    }, lwr, upr)
  }

  dat <- load_concepts(names(cfg), src, aggregate = aggreg_fun(cfg), patient_ids = cohort)
  out <- load_concepts("death", src, patient_ids = cohort)
  out[, c(index_var(out)) := NULL]

  res <- load_wins(lwr, upr, cfg, dat, out)

  if (length(res) == 1) res <- res[[1L]]

  if(load_sofa) {
    res <- merge(
      res,
      replace_na(load_concepts("sofa", src, keep_components = T, patient_ids = cohort, explicit_wins = times), 0)
    )
  }


  res
}

raw_cb <- function(gcs, ...) rename_cols(gcs, "gcs_raw", "gcs")

aptt_inr_cb <- function (..., match_win = hours(6L), interval = NULL) {

    cnc <- c("ptt", "inr_pt")
    res <- ricu:::collect_dots(cnc, interval, ...)
    assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))

    on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
    on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))

    res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
      res[[2L]][res[[1L]], on = on21, roll = match_win])
    res <- unique(res)

    res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
    res <- res[, `:=`(c("aptt_inr"), get(cnc[1L])*get(cnc[2L]))]
    res <- rm_cols(res, cnc)
    res
}

map_beta_200 <- function (..., match_win = hours(2L), beta = 200, interval = NULL) {

    cnc <- c("map", "norepi_equiv")
    res <- ricu:::collect_dots(cnc, interval, ...)

    assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))

    on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
    on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
    res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
      res[[2L]][res[[1L]], on = on21, roll = match_win])
    res <- unique(res)
    res[is.na(get(cnc[2L])), cnc[2L]] <- 0 # impute a 0 value for vasos where needed

    res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
    res <- res[, `:=`(c(paste0("map_beta", beta)), get(cnc[1L]) - beta*get(cnc[2L]))]
    res <- rm_cols(res, cnc)
    res
}

map_beta_100 <- function (..., match_win = hours(2L), beta = 100, interval = NULL) {

    cnc <- c("map", "norepi_equiv")
    res <- ricu:::collect_dots(cnc, interval, ...)

    assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))

    on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
    on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
    res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
      res[[2L]][res[[1L]], on = on21, roll = match_win])
    res <- unique(res)
    res[is.na(get(cnc[2L])), cnc[2L]] <- 0 # impute a 0 value for vasos where needed

    res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
    res <- res[, `:=`(c(paste0("map_beta", beta)), get(cnc[1L]) - beta*get(cnc[2L]))]
    res <- rm_cols(res, cnc)
    res
}

map_beta_50 <- function (..., match_win = hours(2L), beta = 50, interval = NULL) {

    cnc <- c("map", "norepi_equiv")
    res <- ricu:::collect_dots(cnc, interval, ...)

    assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))

    on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
    on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
    res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
      res[[2L]][res[[1L]], on = on21, roll = match_win])
    res <- unique(res)
    res[is.na(get(cnc[2L])), cnc[2L]] <- 0 # impute a 0 value for vasos where needed

    res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
    res <- res[, `:=`(c(paste0("map_beta", beta)), get(cnc[1L]) - beta*get(cnc[2L]))]
    res <- rm_cols(res, cnc)
    res
}

neut_lymph_cb <- function (..., match_win = hours(6L), interval = NULL) {

    cnc <- c("neut", "lymph")
    res <- ricu:::collect_dots(cnc, interval, ...)
    assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))

    on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
    on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))

    res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
      res[[2L]][res[[1L]], on = on21, roll = match_win])
    res <- unique(res)

    res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
    res <- res[, `:=`(c("neut_div_lymph"), get(cnc[1L])/get(cnc[2L]))]
    res <- rm_cols(res, cnc)
    res
}
