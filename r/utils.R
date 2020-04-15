
proj_root <- function() rprojroot::find_root(".git/index")

config_dir <- function() file.path(proj_root(), "config")

aggreg_fun <- function(cfg, inc = "max", dec = "min") {

  dir <- vapply(cfg, `[[`, character(1L), "direction")

  assert_that(setequal(dir, c("increasing", "decreasing")))

  ifelse(dir == "increasing", inc, dec)
}
