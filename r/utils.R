
proj_root <- function() rprojroot::find_root(".git/index")

config_dir <- function() file.path(proj_root(), "config")

aggreg_fun <- function(cfg, inc = "max", dec = "min", type = "vec") {
  
  if (type == "list") {
    dir <- lapply(cfg, `[[`, "direction")
    res <- lapply(dir, function(x) if (x == "increasing") inc else dec)
    names(res) <- names(cfg)
    res[grep("^gcs", names(res))] <- list(NULL)
    return(res) 
  }
  
  dir <- vapply(cfg, `[[`, character(1L), "direction")
  res <- ifelse(dir == "increasing", inc, dec)
}

si_cohort <- function(source, age_threshold = 18L, ...) {

  if (grepl("eicu", source)) {

    patient_ids <- get_config("eicu-cohort", config_dir())[["eicu"]][["all"]]

    susp_infec <- load_concepts("susp_inf", source, abx_min_count = 2L, 
      positive_cultures = T, si_mode = "or", patient_ids = patient_ids, 
      verbose = FALSE, ...)

  } else if (identical(source, "hirid")) {

    susp_infec <- load_concepts("susp_inf", source, abx_min_count = 2L,
                     si_mode = "or", verbose = FALSE, ...)

  } else {

    susp_infec <- load_concepts("susp_inf", source, id_type = "icustay", 
                                verbose = FALSE, ...)

  }

  si_lwr <- hours(48L)
  si_upr <- hours(24L)
  susp_infec <- susp_infec[is_true(get("susp_inf")), ]
  susp_infec <- susp_infec[, c("susp_inf") := NULL]
  susp_infec <- susp_infec[, c("si_lwr", "si_upr") := list(
    get(index_var(susp_infec)) - si_lwr,
    get(index_var(susp_infec)) + si_upr
  )]

  susp_infec <- susp_infec[(si_lwr <= 0L) & (si_upr >= 0L)]

  above_age <- load_concepts("age", source, verbose = FALSE)[age >= age_threshold]

  unique(intersect(id_col(susp_infec), id_col(above_age)))
}

strat_samp <- function(dat, by, frac = 0.75, replace = FALSE) {

  samp <- function(x, repl) sample(x, length(x) * frac, repl)

  assert_that(inherits(dat, "data.frame"), has_name(dat, by))

  rows <- split(seq_len(nrow(dat)), dat[, by, with = FALSE])

  unlist(lapply(rows, samp, replace))
}

score2table <- function(score) {
  flip_sum <- function(x) {
    x <- if (all(x[["Dir"]])) x else x[rev(seq_len(nrow(x))), ]
    x <- x[x[["Points"]] > 0, ]
    x[["Points"]] <- cumsum(x[["Points"]])
    x
  }

  dict <- config("features")
  full_names <- vapply(attr(score, "concept"),
                       function(x) paste0(dict[[x]][["full_name"]], " (",
                                          dict[[x]][["unit"]][1], ")"),
                       character(1L))
  tbl <- data.frame(
    Feature = full_names, Dir = attr(score, "right"),
    Thresh = attr(score, "threshold"), Points = score, stringsAsFactors = FALSE
  )
  tbl <- do.call(rbind, lapply(split(tbl, tbl[["Feature"]]), flip_sum))
  tbl[["Thresh"]] <- paste(ifelse(tbl[["Dir"]], "≥", "<"),
                           tbl[["Thresh"]])
  
  names(tbl)[names(tbl) == "Thresh"] <- "Threshold"
  res <- autofit(flextable(tbl[, c("Feature", "Threshold", "Points")]))
  
  if (any(grepl("^bun", names(score[score > 0])))) {
    footnotes <- c(
      paste0("Blood Urea Nitrogen conversion factor from mg/dL ",
             "to mmol/L is 0.36"),
      "APPT activated partial thromboplastin time",
      "FiO2 fraction of inspired oxygen",
      "GCS Glasgow Coma Scale score",
      "INR(PT) international normalized ratio of prothrombin time",
      "MAP mean arterial pressure",
      "NEQ norepinephrine equivalents",
      "PaO2 partial arterial oxygen pressure"
    )
    res <- footnote(res, i = 1, j = rep(1, length(footnotes)), 
                    value = as_paragraph(footnotes),
                    ref_symbols = rep("", length(footnotes)),
                    part = "header", inline = TRUE)
  }
  res <- fontsize(res, size = 10)
  for (i in seq_along(unique(tbl$Feature))) {
    res <- merge_at(res, i = (i-1)*4 + 1:4, j = 1)
  }
  hline(res, i = 4 * seq_along(unique(tbl$Feature)))
}

get_cores <- function(n_cores = NULL) {

  max_cores <- as.integer(
    Sys.getenv("LSB_DJOB_NUMPROC", unset = parallel::detectCores())
  )

  if (is.null(n_cores)) {
    n_cores <- max_cores
  }

  if (n_cores > max_cores) {
    warning("asked for ", n_cores, " cores but only ", max_cores,
            " are available")
    n_cores <- max_cores
  }

  n_cores
}

roc_pr_plot <- function(x, ..., nrow_legend = 1) {

  roc <- autoplot(x, "ROC", ...)
  prc <- autoplot(x, "PR", ...)

  legend <- cowplot::get_legend(
    roc + guides(color = guide_legend(nrow = nrow_legend)) +
          theme(legend.position = "bottom")
  )

  plot <- cowplot::plot_grid(roc + theme(legend.position = "none"),
                             prc + theme(legend.position = "none"), ncol = 2)

  cowplot::plot_grid(plot, legend, nrow = 2, rel_heights = c(1, .1))
}

auc <- function(x) data.table::setDT(precrec::auc(x))

is_difftime <- function(x) inherits(x, "difftime")
is_interval <- function(x) {
  assert_that(is_difftime(x), length(x) > 0L) && all(x >= 0)
}

in_icu <- function(pids, src, tim) {
  wins <- stay_windows(src, patient_ids = pids)
  if (file.exists(file.path(root, "dose-dat", paste0("death_", src, ".RData")))){
    load(file.path(root, "dose-dat", paste0("death_", src, ".RData")))
  } else {
    tod <- load_concepts("death", src, verbose = FALSE)
    save(tod, file = file.path(root, "dose-dat", paste0("death_", src, ".RData")))
  }
  wins <- merge(wins, tod, all.x = TRUE)
  
  wins[, end := pmin(end, get(index_var(wins)), na.rm = TRUE)]
  
  id_col(wins[start <= tim & end >= tim])
}

load_cts <- function(src, cfg, lwr, upr, cohort = si_cohort(src),
                     load_sofa = F) {

  load_win <- function(lwr, upr, cfg, dat, out) {

    dat <- preproc(dat, cfg, lwr, upr)
    #dat <- indicator_encoding(dat, cfg)

    ret <- merge(dat, out, all.x = T)
    ret[is.na(death), "death"] <- F

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

  if (length(res) == 1) res <- res[[1L]]

  if(load_sofa) {
    res <- merge(
      res,
      replace_na(load_concepts("sofa", src, keep_components = T,
                               patient_ids = cohort, explicit_wins = times), 0)
    )
  }


  res
}


df_to_word <- function(df, path, caption = "", landscape = FALSE, 
                       footnotes = NULL, header = NULL, fix_width = NULL, ...) {
  
  ft <- flextable(df)
  
  if (!is.null(header)) {
    # start with no header
    ft <- delete_part(ft, part = "header")
    # add another line of row at the top position
    ft <- add_header(ft, values = header, top = TRUE)
    ft <- merge_h(ft, part = "header")
  }
  
  ft <- set_caption(ft, caption = caption)
  ft <- font(ft, fontname = "Calibri (Headings)", part = "all")
  ft <- fontsize(ft, size = 10, part = "all")
  
  ft <- autofit(ft)
  if (!is.null(fix_width)) ft <- fit_to_width(ft, fix_width)
  # for (i in seq_along(fix_width)) {
  #   ft <- width(ft, j = fix_width[[i]][1], width = fix_width[[i]][2])
  # }
  
  if (!is.null(footnotes)) {
    ft <- footnote(ft, i = 1, j = rep(1, length(footnotes)),
                   value = as_paragraph(footnotes),
                   ref_symbols = rep("", length(footnotes)),
                   part = "header", inline = TRUE)
  }
  
  my_doc <- read_docx()

  my_doc <- body_add_flextable(my_doc, value = ft)

  if (landscape) my_doc <- my_doc %>% body_end_section_landscape()

  print(my_doc, target = path)
}

scwrap <- function(sc) {
  if (length(sc) > 1L) return(vapply(sc, scwrap, character(1L)))
  switch(sc, cardio = "Cardio", renal = "Renal", liver = "Hepatic",
         metabolic = "Metabolic", resp = "Respiratory",
         cns = "CNS", coag = "Coagulation", "-")
}

spec_dec <- function(x, k) trimws(format(round(x, k), nsmall=k))
