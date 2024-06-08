
si_cohort <- function(source, age_threshold = 18L, ...) {

  above_age_first <- load_concepts(c("age", "adm_episode"), source,
                                   verbose = FALSE)
  above_age_first <- above_age_first[age >= age_threshold & adm_episode == 1L]

  susp_infec <- load_concepts("susp_inf", source, id_type = "icustay",
                              verbose = FALSE, ...)

  si_lwr <- hours(48L)
  si_upr <- hours(24L)
  susp_infec <- susp_infec[is_true(get("susp_inf")), ]
  susp_infec <- susp_infec[, c("susp_inf") := NULL]
  susp_infec <- susp_infec[, c("si_lwr", "si_upr") := list(
    get(index_var(susp_infec)) - si_lwr,
    get(index_var(susp_infec)) + si_upr
  )]

  susp_infec <- susp_infec[(si_lwr <= 0L) & (si_upr >= 0L)]
  susp_coh <- id_col(susp_infec)

  unique(intersect(susp_coh, id_col(above_age_first)))
}

