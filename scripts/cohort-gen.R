
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

src <- c("mimic_demo", "eicu_demo", "miiv", "aumc", "sic")

set.seed(2024)
cohort <- lapply(
  src,
  function(dsrc) {
    coh <- si_cohort(dsrc)
    if (is.element(dsrc, c("miiv", "aumc", "mimic_demo", "eicu_demo"))) {
      idx <- sample.int(length(coh), size = 0.6 * length(coh))
      return(list(all = coh, train = coh[idx], test = coh[-idx]))
    } else {
      return(list(all = coh, train = -1L, test = coh))
    }
  }
)
names(cohort) <- src
config("cohort", cohort)
