
# missingness per feature


# missigness and mortality
src <- "miiv"
mis <- load_data(src, cfg, hours(0L), hours(24L),
                 cohort = config("cohort")[[src]][["all"]],
                 enc = FALSE, impute_vals = FALSE)

df <- data.frame(
  value = vapply(mis[, -c(id_var(mis), "death"), with = FALSE],
                 function(x) mean(is.na(x)), numeric(1L)),
  var = names(mis[, -c(id_var(mis), "death"), with=FALSE]),
  variable = "miss_prop"
)

mort_rates <- do.call(
  rbind,
  lapply(
    rownames(df),
    function(x) {
      res <- mis[, c("death", x), with=FALSE]
      res[, is_miss := is.na(get(x))]
      res <- res[, mean(death), by = "is_miss"]
      if (!is.element(TRUE, res$is_miss)) {

        res <- rbind(res, data.table(V1 = NA, is_miss = TRUE))
      }
      if (!is.element(FALSE, res$is_miss)) {

        res <- rbind(res, data.table(V1 = NA, is_miss = FALSE))
      }
      res <- setorderv(res, "is_miss")
      res$V1
    }
  )
)

rates <- data.frame(mort_rates, rownames(df))
names(rates) <- c("non_miss", "miss", "var")
melt(rates, id.vars = "var")

m_rates <- melt(rates, id.vars = "var")
ggplot(
  m_rates,
  aes(x = factor(var), y = factor(variable), fill = value)
) +
  geom_tile() +
  geom_text(data = df, aes(label = round(value, 2)), color = "black", size = 4) +
  ggtitle(paste(srcwrap(src), "with mortality", spec_dec(mean(mis$death), 2))) +
  scale_fill_viridis_c(limits = range(m_rates$value)) +
  theme_bw() + theme(legend.position = "bottom")

# MIMIC-IV abx route

presc <- sort(table(mimic$prescriptions$route), decreasing = TRUE)

sum(presc[grepl("IV", names(presc))])

subset(
  miiv$prescriptions,
  grep("aztreonam|bactrim|cephalexin|chloramphenicol|cipro|flagyl|metronidazole|
       nitrofurantoin|tazobactam|rifampin|sulfadiazine|timentin|trimethoprim|
       (amika|gentami|vanco)cin|(amoxi|ampi|dicloxa|naf|oxa|peni|pipera)cillin|
       (azithro|clarithro|erythro|clinda|strepto|tobra|vanco)mycin|cef(azolin|
       tazidime|adroxil|epime|otetan|otaxime|podoxime|uroxime)|(doxy|mino|tetra)
       cycline|(levofl|moxifl|ofl)oxacin|macro(bid|dantin)|(una|zo)syn",
       drug)
)

# ventilation comparison
vent_per_stay <- function(src) {

  n_coh <- nrow(load_concepts("age", src))
  vent_dur <- expand(load_concepts("vent_ind", src))

  nrow(
    vent_dur[get(index_var(vent_dur)) >= hours(0L) &
               get(index_var(vent_dur)) < hours(24L)]
  ) / n_coh
}

vent_per_stay("aumc")

vent_dur <- load_concepts("fio2", "sic")
vent_dur <- vent_dur[fio2 > 21]
nrow(
  vent_dur[get(index_var(vent_dur)) >= hours(0L) &
             get(index_var(vent_dur)) < hours(24L)]
) / nrow(load_concepts("age", "sic"))

vent_per_stay <- function(src) {

  n_coh <- nrow(load_concepts("age", src))
  vent_dur <- expand(load_concepts("vent_ind", src))

  nrow(
    vent_dur[get(index_var(vent_dur)) >= hours(0L) &
               get(index_var(vent_dur)) < hours(24L)]
  ) / n_coh
}

# sampling and antibiotics number
count_per_stay <- function(src, cncpt) {

  n_coh <- nrow(load_concepts("age", src, verbose = FALSE))
  vent_dur <- load_concepts(cncpt, src, verbose = FALSE)

  nrow(
    vent_dur[get(index_var(vent_dur)) >= hours(0L) &
               get(index_var(vent_dur)) < hours(24L)]
  ) / n_coh
}

count_per_stay("miiv", "abx")
count_per_stay("miiv", "samp")

count_per_stay("aumc", "abx")
count_per_stay("aumc", "samp")

count_per_stay("sic", "abx")
count_per_stay("sic", "samp")
