root <- rprojroot::find_root(".git/index")
cfg_dir <- file.path(root, "config")
source(file.path(root, "r", "callbacks.R"))
source(file.path(root, "r", "utils.R"))
Sys.setenv(RICU_CONFIG_PATH = file.path(root, "config", "custom-dict"))

library(assertthat)
library(ggplot2)
get_coh <- function(src) id_col(load_concepts("age", src)[age >= 18L])

load_data <- function(src, cfg, lwr, upr, cohort = get_coh(src)) {
  
  load_win <- function(lwr, upr, cfg, dat, out) {
    
    dat <- preproc(dat, cfg, lwr, upr)
    
    ret <- merge(dat, out, all.x = T)
    ret[is.na(death), "death"] <- F
    attr(ret, "counts") <- attr(dat, "counts")
    attr(ret, "med_iqr") <- attr(dat, "med_iqr")
    
    ret
  }
  
  load_wins <- function(lwr, upr, cfg, dat, out) {
    Map(function(a, b) {
      load_win(as.difftime(a, units = units(lwr)),
               as.difftime(b, units = units(upr)), cfg, dat, out)
    }, lwr, upr)
  }
  
  dat <- load_concepts(names(cfg), src, aggregate = aggreg_fun(cfg),
                       patient_ids = cohort)
  out <- load_concepts("death", src, patient_ids = cohort)
  out[, c(index_var(out)) := NULL]
  
  res <- load_wins(lwr, upr, cfg, dat, out)
  
  names(res) <- paste0(ifelse(is.finite(lwr), "[", "("), format(lwr), ", ",
                       format(upr), ifelse(is.finite(upr), "]", ")"))
  
  if (length(res) == 1L) {
    
    res <- res[[1L]]
    
  } else {
    
    ids <- Map(`[[`, res, lapply(res, id_var))
    ids <- Reduce(intersect, ids)
    res <- lapply(res, function(x) x[get(id_vars(x)[1L]) %in% ids])
    ids <- Map(`[[`, res, lapply(res, id_var))
    
    assert_that(all(vapply(ids, identical, logical(1L), ids[[1L]])))
  }
  
  res
}

preproc <- function(dat, cfg, win_lwr = hours(-Inf),
                    win_upr = hours(Inf)) {
  
  do_call <- function(fun, x) do.call(fun, list(x))
  
  repl_na <- function(x, val) replace(x, is.na(x), val)
  
  assert_that(is_ts_tbl(dat), inherits(win_lwr, "difftime"), 
              inherits(win_upr, "difftime"),
              is.list(cfg), all(data_vars(dat) %in% names(cfg)))
  
  cfg <- cfg[data_vars(dat)]
  
  agg <- aggreg_fun(cfg, list(max_or_na), list(min_or_na))
  med <- lapply(cfg, function(x)
    ifelse(x[["direction"]] == "increasing", x[["lower"]]*0.99, x[["upper"]]*1.01)
  )
  
  res <- dat[(get(index_var(dat)) >= win_lwr) & (get(index_var(dat)) <= win_upr), ]
  
  res <- res[, Map(do_call, agg, .SD), .SDcols = names(agg), by = c(id_vars(dat))]
  
  ret <- as_id_tbl(res, id_vars(dat))
  ret
}

cfg <- get_config("features", cfg_dir)

cmp_miss <- function(src, cfg, tbl) {
 
  signed_pval <- function(out, idx1, idx2) {
    
    p <- mean(out[idx1 | idx2]) # prevalence
    stat <- (sum(out[idx1] - p) / sqrt(sum(idx1)) - 
               sum(out[idx2] - p) / sqrt(sum(idx2))) / sqrt(2 * p * (1 - p))
    
    logp <- -log(pnorm(stat, lower.tail = stat < 0), 10)
    if (!is.na(logp) & logp > 10) {
      logp <- 10
    }
    logp * sign(mean(out[idx1]) -  mean(out[idx2]))
    
  }
  
  targ_cnc <- setdiff(
    names(tbl),
    c(meta_vars(tbl), "tco2", "dbp", "fio2", "hr", "map", "o2sat", "resp",
      "sbp", "temp", "gcs", "gcs_raw", "safi", "map_beta50", "map_beta100",
      "map_beta200", "death")
  )
  res <- lapply(
    targ_cnc,
    function(cnc) {
      
      miss_idx <- is.na(tbl[[cnc]])
      cmp_idx <- !miss_idx
      
      if (cfg[[cnc]][["direction"]] == "increasing") {
        norm_idx <- tbl[[cnc]] <= cfg[[cnc]][["lower"]]
      } else norm_idx <- tbl[[cnc]] >= cfg[[cnc]][["upper"]]
      norm_idx[is.na(norm_idx)] <- FALSE
      
      if (sum(norm_idx) < 100) {
        cat(src, ",", cnc, ":")
        cat(sum(norm_idx), "patients within normal range\n")
      }
      if (sum(miss_idx) < 100) cat(sum(miss_idx), "patients with missing vals\n")
      if (sum(cmp_idx) < 100) cat(sum(cmp_idx), "patients with a value\n")
      
      # compare the p-values
      miss_cmp <- signed_pval(tbl[["death"]], miss_idx, cmp_idx)
      miss_norm <- signed_pval(tbl[["death"]], miss_idx, norm_idx)
      
      return(c(miss_cmp, miss_norm, mean(miss_idx)))
      
    }
  )
  
  res <- data.frame(Reduce(rbind, res))
  res <- cbind(res, targ_cnc, src)
  names(res) <- c("miss_cmp", "miss_norm", "miss_prop", "feature", "dataset")
  
  res
}

src <- c("mimic", "hirid", "aumc") # c("mimic_demo", "eicu_demo") #

data <- lapply(src, load_data, cfg = cfg, lwr = hours(0L), upr = hours(24L))

plt <- Reduce(rbind, Map(cmp_miss, src, list(cfg), data))

# reorder the features by mimic missingness
ord <- plt[plt$dataset == "mimic", ]
plt$feature <- factor(plt$feature, levels = ord$feature[order(ord$miss_prop)])

miss_cmp <- ggplot(plt, aes(x = feature, y = dataset, fill = miss_cmp)) +
  geom_tile() + theme_bw() + 
  scale_fill_gradient2(high = "red", low = "green", name = "Danger") +
  ggtitle("Missing vs. present measurement")

miss_norm <- ggplot(plt, aes(x = feature, y = dataset, fill = miss_norm)) +
  geom_tile() + theme_bw() + 
  scale_fill_gradient2(high = "red", low = "green", name = "Danger") +
  ggtitle("Missing vs. normal range")

cowplot::plot_grid(miss_cmp, miss_norm, ncol = 1L)
