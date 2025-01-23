
si_cohort <- function(source, age_threshold = 18L, mday_abx = NULL, ...) {

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

  streak <- function(x) {
    x <- sort(unique(x))  # Sort and remove duplicates
    diff_x <- diff(x)     # Compute differences between consecutive elements
    streaks <- rle(diff_x == 1)  # Find runs where difference is 1
    max_streak <- max(streaks$lengths[streaks$values], 0) + 1  # Longest streak
    return(max_streak)
  }

  if (!is.null(mday_abx)) {

    mabx <- load_concepts("abx", source, verbose = FALSE)
    mabx[, day := 1 + floor(as.numeric(get(index_var(mabx))) / 24)]
    abx_cnt <- mabx[, list(mday = streak(day)), by = c(id_vars(mabx))]
    mday_coh <- id_col(abx_cnt[mday >= mday_abx])
    susp_infec <- susp_infec[get(id_var(susp_infec)) %in% mday_coh]
  }

  susp_coh <- id_col(susp_infec)

  unique(intersect(susp_coh, id_col(above_age_first)))
}

