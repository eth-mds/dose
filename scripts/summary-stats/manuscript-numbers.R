
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

src <- c("miiv", "aumc", "sic")

n_measures <- function(cnc, src, upto = hours(24L)) {

  cohort <- config("cohort")
  cfg <- config("features")

  nums <- ncoh <- list()
  for (dsrc in src) {
    dat_fil <- file.path(root, "dose-dat", paste0("dat_", dsrc, ".RData"))
    load(dat_fil)
    dat <- dat[id_col(dat) %in% cohort[[dsrc]][["all"]]]
    ncoh[[dsrc]] <- length(cohort[[dsrc]][["all"]])
    nums[[dsrc]] <- vapply(
      dat[get(index_var(dat)) >= upto - hours(24L) &
          get(index_var(dat)) <= upto, cnc, with=FALSE],
      function(x) sum(!is.na(x)),
      integer(1L))
  }
  cat("We evaluated", paste0(ncoh[[1]], " (", ncoh[[2]], ", ", ncoh[[3]], ")"),
      "potentially septic patients comprising ")
  cat(
    paste0(nums[[1]], " (", nums[[2]], ", ", nums[[3]], ") ",
           vapply(cnc, function(x) stringr::str_to_lower(cfg[[x]][["full_name"]]),
                  character(1L)),
           " measurements; ",
           collapse = "")
  )
  cat("\n")
}

cnc_list <- vapply(config("dose"), function(x) gsub("[.].*", "", x[1]),
                   character(1L))
n_measures(cnc_list, src)


