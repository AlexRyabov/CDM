#' Compare model evaluations on the same folds
#'
#' Compares every pair of model evaluations, for example an SVM and a random
#' forest, that were computed with [evaluate_model()] on the same data and the
#' same cross-validation folds. Differences are given for the pooled
#' out-of-fold metrics and for the metrics of single folds, where the paired
#' fold differences show how consistent an advantage is.
#'
#' @param ... Two or more named `cdm_model_evaluation` objects, for example
#'   `svm = svm_evaluation, rf = rf_evaluation`.
#'
#' @return A data frame of class `cdm_model_comparison` with one row per pair
#'   of models and metric: the pooled values of both models (`value_1`,
#'   `value_2`, means over repeats), their difference (`difference`, model 1
#'   minus model 2), the mean and standard deviation of the paired fold
#'   differences, and the number of folds in which model 1 is better. For the
#'   Brier score smaller values are better; for all other metrics larger
#'   values are better.
#'
#' @export
compare_models <- function(...) {
  evaluations <- list(...)
  model_names <- names(evaluations)
  if (length(evaluations) < 2L || is.null(model_names) || any(!nzchar(model_names)) ||
      anyDuplicated(model_names)) {
    stop("compare_models() needs at least two evaluations with unique names.", call. = FALSE)
  }
  if (!all(vapply(evaluations, inherits, logical(1), "cdm_model_evaluation"))) {
    stop("All arguments must be created by evaluate_model().", call. = FALSE)
  }
  for (evaluation in evaluations) {
    stop_if_kernlab(evaluation, "evaluate_model")
  }
  reference <- evaluations[[1L]]$provenance
  for (name in model_names[-1L]) {
    provenance <- evaluations[[name]]$provenance
    if (!identical(provenance$cv_folds_hash, reference$cv_folds_hash)) {
      stop(
        "Evaluations '", model_names[[1L]], "' and '", name,
        "' were computed on different data or folds and cannot be compared.",
        call. = FALSE
      )
    }
  }

  final_metrics <- lapply(evaluations, `[[`, "metrics")

  rows <- list()
  pairs <- utils::combn(model_names, 2L, simplify = FALSE)
  for (pair in pairs) {
    for (metric in unique(final_metrics[[1L]]$metric)) {
      pooled <- vapply(pair, function(name) {
        mean(final_metrics[[name]]$value[final_metrics[[name]]$metric == metric])
      }, numeric(1))
      folds <- lapply(pair, function(name) {
        fold_metrics <- evaluations[[name]]$fold_metrics
        fold_metrics <- fold_metrics[fold_metrics$metric == metric, , drop = FALSE]
        fold_metrics[order(fold_metrics$repeat_id, fold_metrics$fold), "value"]
      })
      fold_difference <- folds[[1L]] - folds[[2L]]
      better <- if (identical(metric, "brier")) fold_difference < 0 else fold_difference > 0
      rows[[length(rows) + 1L]] <- data.frame(
        model_1 = pair[[1L]],
        model_2 = pair[[2L]],
        metric = metric,
        value_1 = pooled[[1L]],
        value_2 = pooled[[2L]],
        difference = pooled[[1L]] - pooled[[2L]],
        fold_difference_mean = mean(fold_difference, na.rm = TRUE),
        fold_difference_sd = stats::sd(fold_difference, na.rm = TRUE),
        folds_model_1_better = sum(better, na.rm = TRUE),
        n_folds = sum(!is.na(fold_difference)),
        stringsAsFactors = FALSE
      )
    }
  }

  result <- do.call(rbind, rows)
  class(result) <- c("cdm_model_comparison", "data.frame")
  result
}

#' @export
print.cdm_model_comparison <- function(x, ...) {
  cat("Paired comparison of model evaluations on the same folds",
      "(difference = model 1 - model 2; smaller Brier is better)\n")
  print(as.data.frame(unclass(x), stringsAsFactors = FALSE), row.names = FALSE, digits = 3)
  invisible(x)
}
