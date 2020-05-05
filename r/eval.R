
#' Evaluate DOSE scores
#'
#' Provided one or several possible DOSE scores and one or several datasets
#' (e.g. data for different time windows) provided in indicator encoded
#' fashion, `dose_eval` computes the DOSE scores across all datasets.
#'
#' @param scores A single named numeric vector or a list of named numeric
#' vectors (the names of which are used to name the score columns)
#' @param dat A single `id_tbl` or a list of `id_tbl` datasets
#' @param label `NULL` or the name of the label column
#'
#' @return An `id_tbl` with a column per combination of score and dataset and
#' if provided a column containing the labels
#'
dose_eval <- function(scores, dat, label = "death") {

  eval_score <- function(score, name, dat, label) {

    eval_one <- function(dat, name) {

      assert_that(is_id_tbl(dat), has_name(dat, c(label, names(score))))

      score <- as.vector(
        as.matrix(dat[, names(score), with = FALSE]) %*% score
      )

      res <- dat[, c(id(dat), label), with = FALSE]
      res <- res[, c(name) := score]

      res
    }

    if (is_id_tbl(dat)) {
      eval_one(dat, name)
    } else {
      Reduce(merge_all, Map(eval_one, dat, paste(name, names(dat))))
    }
  }

  if (!is.list(scores)) {
    scores <- list(scores)
  }

  if (is.null(names(scores))) {

    if (length(scores) == 1L) {
      names(scores) <- "DOSE"
    } else {
      names(scores) <- paste0("DOSE_", seq_along(scores))
    }
  }

  Reduce(merge_all,
    Map(eval_score, scores, names(scores), MoreArgs = list(dat, label))
  )
}

#' Evaluate sofa scores
#'
#' For a given data source and a single or multiple time points, the SOFA
#' score is evaluated. As `cohort` argument, either a numeric vector of
#' patient ids or an `id_tbl` object as returned from [dose_eval()] may be
#' provided. In the latter case, this object is concatenated with the table
#' of SOFA scores and provides the label column which in the former case,
#' the `death` outcome is retrieved and added as label column.
#'
#' @param src String-valued data source name
#' @param upr Time-point(s) for which SOFA is evaluated
#' @param cohort Either a numeric vector of patient IDs or an `id_tbl`
#'
#' @return An `id_tbl` with columns per SOFA time-point, potentially merged
#' with the object passed as `cohort` argument.
#'
sofa_eval <- function(src, upr, cohort = si_cohort(src)) {

  extract_score <- function(x, name) {
    x <- x[, c(id(x), "sofa_score"), with = FALSE]
    x <- rename_cols(x, name, "sofa_score")
    x
  }

  merge_two <- function(x, y) merge(x, y, all = TRUE)

  if (is_id_tbl(cohort)) {
    out    <- cohort
    cohort <- out[[id(out)]]
  } else {
    out <- load_dictionary(src, "death", id_type = "icustay",
                           patient_ids = cohort)
  }

  res <- sofa(src, id_type = "icustay", patient_ids = cohort,
              explicit_wins = upr)

  unt <- time_unit(res)
  res <- split(res, by = index(res), keep.by = FALSE)

  unt <- as.difftime(as.numeric(names(res)), units = unt)
  res <- Map(extract_score, res,
             paste0("SOFA [", format(unt - 24L), ", ", format(unt), "]"))

  Reduce(merge_all, c(res, list(out)))
}

#' Calculate ROC/PR
#'
#' For an `id_tbl` containing several scores (such as DOSE or SOFA),
#' potentially over multiple time windows, for each column vs the column
#' designated as `label`, resampled RO and PR curves are calculated.
#'
#' @param dat `id_tbl` holding scores and label as columns
#' @param lable String-valued name of the label column
#' @param n_rep Number of resampling steps
#' @param frac Fraction of data included in each sampling step
#'
#' @return See [precrec::evalmod()]
#'
eval_score <- function(dat, label = "death", n_rep = 10L, frac = 0.75) {

  extract_sco <- function(i, x, sco) c(x[i, sco, with = FALSE])
  extract_lab <- function(i, x, lab, n) rep(list(x[[lab]][i]), n)

  assert_that(is_id_tbl(dat), has_name(dat, label))

  scores <- setdiff(names(dat), c(id(dat), label))

  folds  <- replicate(n_rep, sample(nrow(dat), frac * nrow(dat)),
                      simplify = FALSE)

  splits <- lapply(folds, extract_sco, dat, scores)
  labels <- lapply(folds, extract_lab, dat, label, length(scores))

  precrec::evalmod(scores = splits, labels = labels,
                   modnames = rep(scores, n_rep),
                   dsids = rep(seq_len(n_rep), each = length(scores)))
}

merge_all <- function(x, y) merge_cols(x, y, all = TRUE)

merge_cols <- function(x, y, ...) {

  non_id_cols <- function(z) setdiff(colnames(z), id(z))

  common <- intersect(non_id_cols(x), non_id_cols(y))
  suffix <- c(".x", ".y")

  res <- merge(x, y, ..., suffixes = suffix)

  if (length(common) > 0L) {

    for (col in (common)) {

      cols <- paste0(col, suffix)

      if (!identical(res[[cols[1L]]], res[[cols[2L]]])) browser()

      assert_that(identical(res[[cols[1L]]], res[[cols[2L]]]))

      res[, c(col, cols) := list(get(cols[1L]), NULL, NULL)]
    }
  }

  res
}

