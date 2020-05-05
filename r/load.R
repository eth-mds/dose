
#' Load preprocessed data
#'
#' For a given data source (alongside a config file, and a cohort) and for a
#' single time window or multiple time windows returns preprocessed data. The
#' Returned data first is preprocessed by [preproc()] and subsequently
#' indicator encoded, using [indicator_encoding()]. Finally the `death`
#' outcome is merged in.
#'
#' @param src String valued data source
#' @param cfg A named list containing a slot per data column, each with
#' entries `direction`, `upper`, `lower` and `step`
#' @param lwr,upr Lower and upper bound of data windows; the length of one has
#' to be a multiple of the other
#' @param cohort Vector of patient ids
#'
#' @return Either a single or a list of `id_tbl` objects, depending on whether
#' a single or multiple time windows were specified.
#'
load_data <- function(src, cfg, lwr, upr, cohort = si_cohort(src)) {

  load_win <- function(lwr, upr, cfg, dat, out) {

    dat <- preproc(dat, cfg, lwr, upr)
    dat <- indicator_encoding(dat, cfg)

    merge(dat, out, all = FALSE)
  }

  load_wins <- function(lwr, upr, cfg, dat, out) {
    Map(function(a, b) {
      load_win(as.difftime(a, units = units(lwr)),
               as.difftime(b, units = units(upr)), cfg, dat, out)
    }, lwr, upr)
  }

  dat <- load_dictionary(src, names(cfg), aggregate = aggreg_fun(cfg),
                         id_type = "icustay", patient_ids = cohort)
  out <- load_dictionary(src, "death", id_type = "icustay",
                         patient_ids = cohort)

  res <- load_wins(lwr, upr, cfg, dat, out)

  names(res) <- paste0(ifelse(is.finite(lwr), "[", "("), format(lwr), ", ",
                       format(upr), ifelse(is.finite(upr), "]", ")"))

  if (length(res) == 1L) {

    res <- res[[1L]]

  } else {

    ids <- Map(`[[`, res, lapply(res, id))
    ids <- Reduce(intersect, ids)
    res <- lapply(res, function(x) x[get(id(x)) %in% ids])
    ids <- Map(`[[`, res, lapply(res, id))

    assert_that(all(vapply(ids, identical, logical(1L), ids[[1L]])))
  }

  res
}

#' Data preprocessing
#'
#' Fist, rows are filtered out where the index is strictly smaller than the
#' `win_lwr` or strictly larger than the `win_upr` argument. From this reduced
#' data, feature-wise medians are calculated for imputing `NA` data later on.
#' Per id group, the worst value is calculated according to the `cfg` argument
#' which for the value `increasing` means a maximum and for `decreasing` a
#' minimum. If not a single value is available for a given feature and id
#' group, the median value for this feature calculated earlier (over all rows)
#' is imputed.
#'
#' @param dat A `ts_tbl` object holding patient data
#' @param cfg Named list (names have to correspond to feature columns in
#' `dat`), each slot containing a `direction` string, indicating the direction
#' of the given feature
#' @param win_lwr,win_upr Filter criteria for rows
#'
#' @return An `id_tbl` object
#'
preproc <- function(dat, cfg, win_lwr = hours(-Inf),
                    win_upr = hours(Inf)) {

  do_call <- function(fun, x) do.call(fun, list(x))

  repl_na <- function(x, val) replace(x, is.na(x), val)

  assert_that(is_ts_tbl(dat), is_time(win_lwr), is_time(win_upr),
              is.list(cfg), all(data_cols(dat) %in% names(cfg)))

  cfg <- cfg[data_cols(dat)]

  agg <- aggreg_fun(cfg, list(max_or_na), list(min_or_na))
  med <- lapply(dat[, names(cfg), with = FALSE], median, na.rm = TRUE)

  res <- dat[get(index(dat)) >= win_lwr & get(index(dat)) <= win_upr, ]
  res <- res[, Map(do_call, agg, .SD), .SDcols = names(agg), by = c(id(dat))]
  res <- res[, c(names(med)) := Map(repl_na, .SD, med), .SDcols = names(med)]

  as_id_tbl(res, id(dat), id_opts(dat))
}

#' Convert to indicator encoded data
#'
#' Turn an `id_tbl` object containing numeric data columns into an indicator
#' encoded `id_tbl` object consisting of logical columns according to intervals
#' as specified by the configuration list passed as `cfg`. Each slot in `cfg`
#' is expected to contain an entry for `direction`, alongside a discretization
#' specification as `upper`, `lower` and `step`.
#'
#' @param dat An `id_tbl` object containing numeric data
#' @param cfg A named list containing a slot per data column, each with
#' entries `direction`, `upper`, `lower` and `step`.
#'
#' @return An `id_tbl` object
#'
indicator_encoding <- function(dat, cfg) {

  encode <- function(x, sequ, is_inc, name, len) {

    if (is.null(x)) {
      ival <- cut(sequ, sequ, right = !is_inc)
    } else {
      ival <- cut(x, sequ, right = !is_inc)
    }

    if (is_inc) {
      col_names <- sub(",\\d+(\\.\\d+)?\\)$", ",Inf)", levels(ival))
    } else {
      col_names <- sub("^\\(\\d+(\\.\\d+)?,", "(-Inf,", levels(ival))
    }

    res <- matrix(FALSE, nrow = len, ncol = nlevels(ival),
                  dimnames = list(NULL, col_names))

    if (!is.null(x)) {

      if (is_inc) {
        seq_fun <- function(i) seq.int(1L, i)
        res[!is.na(x) & x >= cfg[["upper"]], ] <- TRUE
      } else {
        seq_fun <- function(i) seq.int(i, ncol(res))
        res[!is.na(x) & x <= cfg[["lower"]], ] <- TRUE
      }

      lvls <- as.integer(ival)

      for (i in seq_len(nlevels(ival))) {
        res[!is.na(lvls) & lvls == i, seq_fun(i)] <- TRUE
      }
    }

    res <- asplit(res, 2L)
    res <- lapply(res, `dimnames<-`, NULL)
    res <- lapply(res, `attr<-`, "concept", name)
    res <- Map(`attr<-`, res, "threshold",
               if (is_inc) sequ[-length(sequ)] else sequ[-1])
    res <- lapply(res, `attr<-`, "right", is_inc)

    res
  }

  assert_that(is_id_tbl(dat), is.list(cfg),
              all(data_cols(dat) %in% names(cfg)))

  seqs <- Map(seq,
    vapply(cfg, `[[`, numeric(1L), "lower"),
    vapply(cfg, `[[`, numeric(1L), "upper"),
    vapply(cfg, `[[`, numeric(1L), "step")
  )

  res <- Map(encode, c(dat)[names(cfg)], seqs, aggreg_fun(cfg, TRUE, FALSE),
             names(cfg), nrow(dat))
  names(res) <- names(cfg)

  res <- data.table::setDT(unlist(res, recursive = FALSE))
  res <- cbind(dat[[id(dat)]], res)
  res <- data.table::setnames(res, "V1", id(dat))

  as_id_tbl(res, id(dat), id_opts(dat))
}
