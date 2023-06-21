
load_difftime.sic_tbl <- function(x, rows, cols = colnames(x),
                                  id_hint = id_vars(x),
                                  time_vars = ricu::time_vars(x), ...) {

  sec_as_mins <- function(x) ricu:::min_as_mins(as.integer(x / 60))
  ricu:::warn_dots(...)
  # TODO: consider renaming fun to reflect its use for SICdb
  ricu:::load_eiau(x, {{ rows }}, cols, id_hint, time_vars, sec_as_mins)
}

id_win_helper.sic_env <- function(x) {
  
  sec_as_mins <- function(x) ricu:::min_as_mins(as.integer(x / 60))
  
  cfg <- sort(as_id_cfg(x), decreasing = TRUE)
  
  ids <- vctrs::field(cfg, "id")
  sta <- c(unique(vctrs::field(cfg, "start")), "HospAdmTime")
  end <- vctrs::field(cfg, "end")
  
  tbl <- as_src_tbl(x, unique(vctrs::field(cfg, "table")))
  
  mis <- setdiff(sta, colnames(tbl))
  
  res <- load_src(tbl, cols = c(ids, intersect(sta, colnames(tbl)), end))
  
  if (length(mis) > 0L) {
    res[, c(mis) := 0L]
  }
  
  res <- res[, c(sta, end) := lapply(.SD, sec_as_mins), .SDcols = c(sta, end)]
  # browser()
  res <- data.table::setcolorder(res, c(ids, sta, end))
  res <- rename_cols(res, c(ids, paste0(ids, "_start"),
                            paste0(ids, "_end")), by_ref = TRUE)
  
  as_id_tbl(res, ids[2L], by_ref = TRUE)
}
