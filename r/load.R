
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
              is.list(cfg), setequal(names(cfg), data_cols(dat)))

  agg <- aggreg_fun(cfg, list(max_or_na), list(min_or_na))
  med <- lapply(dat[, names(cfg), with = FALSE], median, na.rm = TRUE)

  res <- dat[get(index(dat)) >= win_lwr & get(index(dat)) <= win_upr, ]
  res <- res[, Map(do_call, agg, .SD), .SDcols = names(agg), by = c(id(dat))]
  res <- res[, c(names(med)) := Map(repl_na, .SD, med), .SDcols = names(med)]

  as_id_tbl(res, id(dat), id_opts(dat))
}

#' Convert to indicator encoded data
#'
#' Turn an `id_tbl` containing numeric data columns object into an indicator
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

  encode <- function(x, sequ, is_inc, name) {

    ival <- cut(x, sequ, right = is_inc)
    lvls <- as.integer(ival)

    res <- matrix(FALSE, nrow = length(ival), ncol = nlevels(ival),
                  dimnames = list(NULL, levels(ival)))

    if (is_inc) {
      seq_fun <- function(i) seq.int(1L, i)
      res[!is.na(x) & x >= cfg[["upper"]], ] <- TRUE
    } else {
      seq_fun <- function(i) seq.int(i, ncol(res))
      res[!is.na(x) & x <= cfg[["lower"]], ] <- TRUE
    }

    for (i in seq_len(nlevels(ival))) {
      res[!is.na(lvls) & lvls == i, seq_fun(i)] <- TRUE
    }

    res <- asplit(res, 2L)
    res <- lapply(res, `dimnames<-`, NULL)
    res <- lapply(res, `attr<-`, "concept", name)
    res <- Map(`attr<-`, res, "range", Map(c, sequ[-length(sequ)], sequ[-1]))
    res <- lapply(res, `attr<-`, "right", is_inc)

    res
  }

  assert_that(is_ts_tbl(dat), is.list(cfg),
              setequal(names(cfg), data_cols(dat)))

  seqs <- Map(seq,
    vapply(cfg, `[[`, numeric(1L), "lower"),
    vapply(cfg, `[[`, numeric(1L), "upper"),
    vapply(cfg, `[[`, numeric(1L), "step")
  )

  res <- Map(encode, dat[, names(cfg), with = FALSE],
             seqs, aggreg_fun(cfg, TRUE, FALSE), names(cfg))

  res <- data.table::setDT(unlist(res, recursive = FALSE))
  res <- cbind(dat[[id(dat)]], res)
  res <- data.table::setnames(res, "V1", id(dat))

  as_id_tbl(res, id(dat), id_opts(dat))
}
