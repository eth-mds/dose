map_beta_200 <- function (..., match_win = hours(2L), beta = 200, 
                          interval = NULL) {
  
  cnc <- c("map", "norepi_equiv")
  res <- ricu:::collect_dots(cnc, interval, ...)
  
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  res[is.na(get(cnc[2L])), cnc[2L]] <- 0 # impute a 0 value for vasos
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c(paste0("map_beta", beta)), 
                    get(cnc[1L]) - beta*get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

map_beta_100 <- function (..., match_win = hours(2L), beta = 100, 
                          interval = NULL) {
  
  cnc <- c("map", "norepi_equiv")
  res <- ricu:::collect_dots(cnc, interval, ...)
  
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  res[is.na(get(cnc[2L])), cnc[2L]] <- 0 # impute a 0 value for vasos
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c(paste0("map_beta", beta)), 
                    get(cnc[1L]) - beta*get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

map_beta_50 <- function (..., match_win = hours(2L), beta = 50, 
                         interval = NULL) {
  
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
  res <- res[, `:=`(c(paste0("map_beta", beta)), 
                    get(cnc[1L]) - beta*get(cnc[2L]))]
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

noreq <- function(source, press = "map", beta = 10, patient_ids = NULL, 
                  upr = hours(24L), lwr = hours(0L)) {
  
  na_z <- function(x) ifelse(is.na(x), 0, x)
  col_name <- paste0(press, beta)
  imp_val <- cfg[[press]][["upper"]]*1.01
  
  tbl <- load_concepts(c("norepi_rate", "epi_rate", "dopa_rate", "dobu_rate", 
                         press),
                       source, patient_ids = patient_ids)
  tbl[, noreq := (na_z(norepi_rate) + na_z(epi_rate) + na_z(dopa_rate)/150 + 
                    na_z(dobu_rate > 0)*0.015)]
  
  tbl[, cf_bp := nafill(get(press), "locf")]
  
  tbl[, c(col_name) := (cf_bp - beta*noreq)]
  
  tbl[, c(meta_vars(tbl), col_name), with = FALSE]
  
  tbl <- tbl[get(index_var(tbl)) <= upr & get(index_var(tbl)) >= lwr,
             -min(get(col_name), na.rm = T), by = eval(id_var(tbl))]
  
  setnames(tbl, "V1", col_name)
  tbl <- replace_na(tbl, imp_val)
  
  tbl
}

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

plt_inr_cb <- function (..., match_win = hours(6L), interval = NULL) {
  
  cnc <- c("plt", "inr_pt")
  res <- ricu:::collect_dots(cnc, interval, ...)
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  
  res <- res[is.na(get(cnc[2L])), c(cnc[2L]) := 1]
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c("plt_div_inr"), get(cnc[1L])/get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}


ts_to_win_12hours <- function(x, dur_var, ...) {

  x[, c(list(...)$val_var) := NULL]
  x[, c(list(...)$val_var) := TRUE]
  x[, c(dur_var) := NULL]
  x[, c(dur_var) := mins(720L)]

  as_win_tbl(x, dur_var = dur_var, by_ref = TRUE)
}

hirid_pharma_win12 <- function(x, dur_var, group_var, ...) {

  x[, c(list(...)$val_var) := NULL]
  x[, c(list(...)$val_var) := TRUE]
  x[, c(dur_var) := NULL]

  x[, c(dur_var) := max(get(index_var(x))) - min(get(index_var(x))) +
      hours(12L), by = c(group_var)]

  x <- x[, head(.SD, n = 1L), by = c(group_var)]

  x[, c(dur_var) := `units<-`(get(dur_var), "mins")]

  as_win_tbl(x, dur_var = dur_var, by_ref = TRUE)
}

sed_rass <- function(rass, ..., thresh = -2, win_dur = hours(6L)) {

  res <- rass[, c("dur_var", "sed_rass", "rass") := list(
    win_dur, rass <= thresh, NULL
  )]

  as_win_tbl(res, dur_var = "dur_var", by_ref = TRUE)
}

gcs_cb_generator <- function(name, sed_cncpt = "sed_gcs") {

  assert_that(is.string(name), is.string(sed_cncpt))

  function(..., valid_win = hours(6L),
           sed_impute = c("none", "verb", "max", "prev"),
           set_na_max = TRUE, interval = NULL) {

    zero_to_na <- function(x) replace(x, x == 0, NA_real_)

    sed_impute <- match.arg(sed_impute)

    if (is.na(sed_cncpt)) {
      assert_that(identical(sed_impute, "none"))
      cnc <- c("egcs", "vgcs", "mgcs", "tgcs")
    } else {
      cnc <- c("egcs", "vgcs", "mgcs", "tgcs", sed_cncpt)
    }

    res <- ricu:::collect_dots(cnc, interval, ...)

    assert_that(is_interval(valid_win), valid_win > ricu:::check_interval(res),
                is.flag(set_na_max))

    sed <- res[[cnc[5L]]]
    res <- ricu:::reduce(merge, res[cnc[-5L]], all = TRUE)

    expr <- substitute(list(egcs = fun(egcs), vgcs = fun(vgcs),
                            mgcs = fun(mgcs), tgcs = fun(tgcs)),
                       list(fun = ricu:::locf))

    res <- slide(res, !!expr, before = valid_win)

    if (identical(sed_impute, "none")) {

      cnc <- cnc[-5L]

    } else {

      sed <- sed[is_true(get(cnc[5L])), ]

      if (is_win_tbl(sed)) {
        sed <- expand(sed, aggregate = "any")
      }

      res <- merge(res, sed, all.x = TRUE)
    }

    if (identical(sed_impute, "max")) {

      res <- res[is_true(get(cnc[5L])), c(cnc[4]) := 15]

    } else if (identical(sed_impute, "verb")) {

      res <- res[is_true(get(cnc[5L])), c(cnc[c(2L, 4L)]) := list(5, NA_real_)]

    } else if (identical(sed_impute, "prev")) {

      idv <- id_vars(res)
      res <- res[, c(cnc[-5L]) := lapply(.SD, replace_na, 0),
                 .SDcols = cnc[-5L]]
      res <- res[is_true(get(cnc[5L])), c(cnc[-5L]) := NA_real_]
      res <- res[, c(cnc[-5L]) := lapply(.SD, replace_na, type = "locf"),
                 .SDcols = cnc[-5L], by = c(idv)]
      res <- res[, c(cnc[-5L]) := lapply(.SD, zero_to_na), .SDcols = cnc[-5L]]
    }

    if (set_na_max) {
      res <- res[, c(cnc[1L:3L]) := Map(replace_na, .SD, c(4, 5, 6)),
                 .SDcols = cnc[1L:3L]]
    }

    res <- res[is.na(get(cnc[4L])), c(cnc[4L]) := rowSums(.SD),
               .SDcols = cnc[1L:3L]]

    if (set_na_max) {
      res <- res[, c(cnc[4L]) := list(replace_na(get(cnc[4L]), 15))]
    }

    res <- rename_cols(res, name, cnc[4L], by_ref = TRUE)
    res <- rm_cols(res, cnc[-4L], by_ref = TRUE)

    res
  }
}
