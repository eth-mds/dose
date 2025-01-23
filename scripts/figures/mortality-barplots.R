
root <- rprojroot::find_root(".git/index")
r_dir <- file.path(root, "r")
invisible(lapply(list.files(r_dir, full.names = TRUE), source))

src <- c("miiv", "aumc", "sic")
cfg <- config("features")
score <- config("dose")
set.seed(2024)

dat <- lapply(
  src, function(data_src) {
    tbl <- load_data(data_src, cfg, hours(0L), hours(24L),
                      cohort = config("cohort")[[data_src]][["all"]])
    for (i in seq_along(score)) {
      cp <- names(score)[i]
      tbl[[paste0("dose_", cp)]] <- rowSums(tbl[, score[[cp]], with=FALSE])
    }
    tbl[["source"]] <- data_src
    sf <- get_sofa(data_src, hours(24L))
    tbl <- merge(tbl, sf, all.x = T)

    keep <- c(
      setdiff(names(sf), c(meta_vars(sf), "sofa")),
      paste0("dose_", names(score)), "death", "source"
    )
    tbl[, keep, with = FALSE]
  }
)

res <- Reduce(rbind, dat)
res <- replace_na(res, 0)

plt <- lapply(
  setdiff(names(res), c("source", "death")),
  function(var) {
    slc <- res[, list(p = mean(death), n = .N), by = c("source", var)]
    slc <- data.table::setnames(slc, var, "value")
    id_vars <- strsplit(var, "_")[[1]]
    slc <- cbind(slc, method = id_vars[1])
    slc <- cbind(slc, component = id_vars[2])
  }
)

plt <- Reduce(rbind, plt)

# plt[, pmin := p - 1.96 * sqrt(p * (1 - p)) / sqrt(n)]
# plt[, pmax := p + 1.96 * sqrt(p * (1 - p)) / sqrt(n)]
plt[, pmin := bin_ci(p, n, lower = TRUE)]
plt[, pmax := bin_ci(p, n, lower = FALSE)]

# plt[abs(pmin - pmin_new) > 0.01]

ggplot(plt, aes(x = value, y = p, fill = str_to_upper(method))) +
  geom_col(color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = pmin, ymax = pmax, width = .2),
                position = position_dodge(0.9)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name = "Score",
                      labels = c(Sys.getenv("SCORE_NAME"), "SOFA")) +
  facet_grid(rows = vars(scwrap(component)), cols = vars(srcwrap(source)),
             scales = "free_y") +
  theme(legend.position = "bottom") +
  xlab("Score") + ylab("Mortality rate")

ggsave(file.path(root, "figures", "mortality-barplots.png"),
       width = 15, height = 21)
