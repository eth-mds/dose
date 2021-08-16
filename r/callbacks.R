map_beta_200 <- function (..., match_win = hours(2L), beta = 200, 
                          interval = NULL) {
  
  cnc <- c("map", "norepi_equiv")
  res <- ricu:::collect_dots(cnc, interval, ...)
  
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  res[is.na(get(cnc[2L])), cnc[2L]] <- 0 # impute a 0 value for vasos
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c(paste0("map_beta", beta)), 
                    get(cnc[1L]) - beta*get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

map_beta_100 <- function (..., match_win = hours(2L), beta = 100, 
                          interval = NULL) {
  
  cnc <- c("map", "norepi_equiv")
  res <- ricu:::collect_dots(cnc, interval, ...)
  
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  res[is.na(get(cnc[2L])), cnc[2L]] <- 0 # impute a 0 value for vasos
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c(paste0("map_beta", beta)), 
                    get(cnc[1L]) - beta*get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

map_beta_50 <- function (..., match_win = hours(2L), beta = 50, 
                         interval = NULL) {
  
  cnc <- c("map", "norepi_equiv")
  res <- ricu:::collect_dots(cnc, interval, ...)
  
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  res[is.na(get(cnc[2L])), cnc[2L]] <- 0 # impute a 0 value for vasos where needed
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c(paste0("map_beta", beta)), 
                    get(cnc[1L]) - beta*get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

neut_lymph_cb <- function (..., match_win = hours(6L), interval = NULL) {
  
  cnc <- c("neut", "lymph")
  res <- ricu:::collect_dots(cnc, interval, ...)
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c("neut_div_lymph"), get(cnc[1L])/get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}

noreq <- function(source, press = "map", beta = 10, patient_ids = NULL, 
                  upr = hours(24L), lwr = hours(0L)) {
  
  na_z <- function(x) ifelse(is.na(x), 0, x)
  col_name <- paste0(press, beta)
  imp_val <- cfg[[press]][["upper"]]*1.01
  
  tbl <- load_concepts(c("norepi_rate", "epi_rate", "dopa_rate", "dobu_rate", 
                         press),
                       source, patient_ids = patient_ids)
  tbl[, noreq := (na_z(norepi_rate) + na_z(epi_rate) + na_z(dopa_rate)/150 + 
                    na_z(dobu_rate > 0)*0.015)]
  
  tbl[, cf_bp := nafill(get(press), "locf")]
  
  tbl[, c(col_name) := (cf_bp - beta*noreq)]
  
  tbl[, c(meta_vars(tbl), col_name), with = FALSE]
  
  tbl <- tbl[get(index_var(tbl)) <= upr & get(index_var(tbl)) >= lwr,
             -min(get(col_name), na.rm = T), by = eval(id_var(tbl))]
  
  setnames(tbl, "V1", col_name)
  tbl <- replace_na(tbl, imp_val)
  
  tbl
}

raw_cb <- function(gcs, ...) rename_cols(gcs, "gcs_raw", "gcs")

aptt_inr_cb <- function (..., match_win = hours(6L), interval = NULL) {
  
  cnc <- c("ptt", "inr_pt")
  res <- ricu:::collect_dots(cnc, interval, ...)
  assert_that(is_interval(match_win), match_win > ricu:::check_interval(res))
  
  on12 <- paste(meta_vars(res[[1L]]), "==", meta_vars(res[[2L]]))
  on21 <- paste(meta_vars(res[[2L]]), "==", meta_vars(res[[1L]]))
  
  res <- rbind(res[[1L]][res[[2L]], on = on12, roll = match_win],
               res[[2L]][res[[1L]], on = on21, roll = match_win])
  res <- unique(res)
  
  res <- res[!is.na(get(cnc[1L])) & !is.na(get(cnc[2L])), ]
  res <- res[, `:=`(c("aptt_inr"), get(cnc[1L])*get(cnc[2L]))]
  res <- rm_cols(res, cnc)
  res
}