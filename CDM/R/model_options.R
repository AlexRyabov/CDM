#' Create options for community-type models
#'
#' Creates the method settings used by [cross_validate_model()] and [fit_model()]
#' to predict community types and their probabilities from environmental
#' predictors. Like an unfitted scikit-learn estimator, the options do not
#' contain data: the predictors are the columns of `x` given to
#' [cross_validate_model()] and [fit_model()]. All settings have defaults.
#'
#' Inside every training set the predictors pass a fixed preprocessing recipe:
#' new factor levels are allowed (`recipes::step_novel()`), predictors without
#' variance are removed, numeric predictors are standardized (SVM only), and of
#' every pair of numeric predictors with absolute correlation above
#' `correlation_threshold` one is removed. The best predictors are then
#' selected with `important::step_predictor_best()`, before dummy coding, so
#' that a categorical predictor counts as one predictor. Classes are balanced
#' afterwards with SMOTE-NC (`themis::step_smotenc()`). The number of
#' predictors and the engine parameters are tuned together.
#'
#' Classes with fewer than `min_class_size` samples in a training set are left
#' out of that training set; the model gives them probability 0. SMOTE-NC uses
#' `min(5, min_class_size - 1)` nearest neighbours.
#'
#' Tuned engine parameters: for `"svm"` (`e1071::svm()`, libsvm, radial
#' kernel `exp(-rbf_sigma * |u - v|^2)` on standardized predictors, Platt
#' probabilities) `cost` in `2^[-2, 8]` and `rbf_sigma` (libsvm `gamma`) in
#' `10^[-3, 0]`; for `"rf"` (`ranger`, 500 trees) `mtry`, as the proportion of
#' predictors in `[0.1, 1]`, and `min_n` in `[2, 40]`. `grid_size` parameter
#' combinations are chosen by a space-filling design and crossed with every
#' number of predictors from `n_predictors_min` to `n_predictors_max`.
#'
#' @param engine `"svm"` for a support vector machine or `"rf"` for a random
#'   forest.
#' @param n_predictors_min Smallest number of selected predictors to try.
#' @param n_predictors_max Largest number of selected predictors to try.
#'   `NULL` means all columns of `x`; it is set when the model is fitted.
#' @param selection_score Score used to rank predictors, passed to
#'   `important::step_predictor_best()`. `"imp_rf"` (random forest
#'   importance) works with numeric and categorical predictors.
#' @param imbalance `"smotenc"` to oversample minority classes with SMOTE-NC,
#'   or `"none"`.
#' @param metric Metric used to select the parameters: `"macro_recall"` (mean
#'   recall over classes), `"macro_f1"`, or `"roc_auc"` (Hand-Till).
#'   Parameters are selected with the one-standard-error rule, preferring fewer
#'   predictors and then simpler models.
#' @param grid_size Number of engine parameter combinations.
#' @param inner_v Number of folds of the inner (tuning) cross-validation in
#'   [cross_validate_model()].
#' @param correlation_threshold Absolute correlation above which one predictor
#'   of a pair is removed.
#' @param min_class_size Minimum number of samples of a class in a training
#'   set.
#' @param seed Integer seed for all random steps.
#'
#' @return An object of class `cdm_model_options`.
#'
#' @examples
#' svm_options <- model_options()
#' rf_options <- update(svm_options, engine = "rf")
#' small_models <- model_options(n_predictors_max = 3)
#'
#' @export
model_options <- function(engine = c("svm", "rf"),
                          n_predictors_min = 1L,
                          n_predictors_max = NULL,
                          selection_score = c("imp_rf", "imp_rf_conditional", "imp_rf_oblique",
                                              "info_gain", "gain_ratio", "sym_uncert"),
                          imbalance = c("smotenc", "none"),
                          metric = c("macro_recall", "macro_f1", "roc_auc"),
                          grid_size = 10L,
                          inner_v = 5L,
                          correlation_threshold = 0.9,
                          min_class_size = 6L,
                          seed = 1L) {
  engine <- match.arg(engine)
  selection_score <- match.arg(selection_score)
  imbalance <- match.arg(imbalance)
  metric <- match.arg(metric)

  n_predictors_min <- validate_count_argument(n_predictors_min, "n_predictors_min", minimum = 1L)
  if (!is.null(n_predictors_max)) {
    n_predictors_max <- validate_count_argument(n_predictors_max, "n_predictors_max", minimum = 1L)
    if (n_predictors_min > n_predictors_max) {
      stop("n_predictors_min must not be larger than n_predictors_max.", call. = FALSE)
    }
  }
  if (!is.numeric(correlation_threshold) || length(correlation_threshold) != 1L ||
      is.na(correlation_threshold) || correlation_threshold <= 0 || correlation_threshold > 1) {
    stop("correlation_threshold must be a number in (0, 1].", call. = FALSE)
  }

  options <- list(
    engine = engine,
    n_predictors_min = n_predictors_min,
    n_predictors_max = n_predictors_max,
    selection_score = selection_score,
    imbalance = imbalance,
    metric = metric,
    grid_size = validate_count_argument(grid_size, "grid_size", minimum = 1L),
    inner_v = validate_count_argument(inner_v, "inner_v", minimum = 2L),
    correlation_threshold = correlation_threshold,
    min_class_size = validate_count_argument(min_class_size, "min_class_size", minimum = 2L),
    seed = validate_count_argument(seed, "seed", minimum = 0L)
  )
  class(options) <- "cdm_model_options"
  options
}

#' Update model options
#'
#' Changes some fields of a `cdm_model_options` object and validates the
#' result again with [model_options()].
#'
#' @param object A `cdm_model_options` object.
#' @param ... Named arguments of [model_options()] to change.
#'
#' @return An updated `cdm_model_options` object.
#'
#' @export
update.cdm_model_options <- function(object, ...) {
  changes <- list(...)
  unknown <- setdiff(names(changes), names(formals(model_options)))
  if (is.null(names(changes)) || any(!nzchar(names(changes))) || length(unknown) > 0L) {
    stop(
      "update() accepts only named arguments of model_options(). Unknown: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  settings <- unclass(object)[intersect(names(unclass(object)), names(formals(model_options)))]
  do.call(model_options, utils::modifyList(settings, changes))
}

#' @export
print.cdm_model_options <- function(x, ...) {
  n_max <- if (is.null(x$n_predictors_max)) "all predictors" else x$n_predictors_max
  cat("<cdm_model_options> engine: ", x$engine, "\n", sep = "")
  cat("Number of predictors tried: ", x$n_predictors_min, " to ", n_max,
      "; selection score: ", x$selection_score, "\n", sep = "")
  cat("Imbalance: ", x$imbalance, "; metric: ", x$metric, "\n", sep = "")
  cat("Grid size: ", x$grid_size, "; inner folds: ", x$inner_v, "; seed: ", x$seed, "\n", sep = "")
  invisible(x)
}
