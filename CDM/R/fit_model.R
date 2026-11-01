#' Fit the final community-type model
#'
#' Tunes the number of predictors and the engine parameters on the folds of
#' `cv` and trains the model on all rows of `x` and `y`, as
#' `GridSearchCV(estimator, cv = cv).fit(X, y)` in scikit-learn. The final
#' model is refitted with the selected predictors only, so [predict()] needs
#' only those columns.
#'
#' The tuning runs in parallel when a parallel `future::plan()` is set; see
#' [cross_validate_model()].
#'
#' The tuning starts again and does not use the results of
#' [cross_validate_model()], which only estimates the prediction quality.
#' The predictors selected here can differ from those selected in the outer
#' folds of [cross_validate_model()]; compare them with `summary()` of both
#' objects.
#'
#' @inheritParams cross_validate_model
#'
#' @return An object of class `cdm_model`, a list with the prepared recipe,
#'   the fitted engine (`fit`: an `svm` object from e1071 or a `parsnip`
#'   model of ranger), the selected `parameters` and
#'   `predictors`, the classes of `y`
#'   (`classes`) and those the model predicts (`model_classes`), the type and
#'   factor levels of every selected predictor (`predictor_info`), the tuning
#'   results, the options, and provenance.
#'
#' @export
fit_model <- function(x, y, cv, options) {
  inputs <- prepare_model_inputs(x, y, cv, options)
  options <- inputs$options
  data <- inputs$data
  message("Fitting final ", options$engine, " model on ", nrow(data), " samples")

  fitted <- withr::with_seed(options$seed, {
    tuning <- tune_class_model(data, inputs$folds, options)
    selection <- train_class_model(data, tuning$parameters, options)
    final_parameters <- tuning$parameters
    final_parameters$n_predictors <- length(selection$predictors)
    model <- train_class_model(data[c(".class", selection$predictors)], final_parameters, options)
    list(tuning = tuning, model = model)
  })

  predictors <- fitted$model$predictors
  predictor_info <- lapply(data[predictors], function(column) {
    list(type = if (is.factor(column)) "factor" else "numeric", levels = if (is.factor(column)) levels(column) else NULL)
  })

  issues <- c(
    fitted$tuning$notes,
    if (length(fitted$model$excluded_classes) > 0L) paste0(
      "Class ", paste(fitted$model$excluded_classes, collapse = ", "),
      " has fewer than ", options$min_class_size, " samples and cannot be predicted."
    )
  )
  if (length(issues) > 0L) {
    warning(length(issues), " issue(s) during fitting; see $issues. First: ", issues[[1L]], call. = FALSE)
  }

  result <- list(
    recipe = fitted$model$recipe,
    fit = fitted$model$fit,
    parameters = fitted$tuning$parameters,
    predictors = predictors,
    candidate_predictors = setdiff(names(data), ".class"),
    predictor_info = predictor_info,
    classes = levels(data$.class),
    model_classes = fitted$model$classes,
    tuning = fitted$tuning$grid,
    options = options,
    issues = issues,
    provenance = list(
      data_hash = rlang::hash(data),
      cv_folds_hash = rlang::hash(inputs$folds),
      options_hash = rlang::hash(unclass(options)),
      seed = options$seed,
      versions = model_package_versions(options),
      created = Sys.time()
    )
  )
  class(result) <- "cdm_model"
  result
}

#' Predict community-type probabilities
#'
#' Predicts the community type and the probability of every community type
#' for new samples. The type is the model's own decision: one-vs-one voting of
#' the SVMs (`e1071::svm()`) or the majority over the trees (ranger). The
#' probabilities are libsvm's Platt probabilities combined over the class
#' pairs, or the tree frequencies of the random forest; see
#' [cross_validate_model()]. The most probable type can differ from the predicted
#' type.
#'
#' @param object A `cdm_model` object created by [fit_model()].
#' @param newdata A data frame or tibble with the selected predictors (see
#'   `object$predictors`), for example `cdm_data$env`.
#' @param ... Not used.
#'
#' @return A data frame with one row per row of `newdata`, in the same order:
#'   `sample_id` (from the column `sample_id` of `newdata`, or the row
#'   numbers), `.pred_class`, and one `.pred_<class>` probability column per
#'   class. Classes the model could not learn have probability 0.
#'
#' @export
predict.cdm_model <- function(object, newdata, ...) {
  stop_if_kernlab(object, "fit_model")
  if (!is.data.frame(newdata)) {
    stop("newdata must be a data frame.", call. = FALSE)
  }
  newdata <- as.data.frame(newdata, check.names = FALSE)
  rownames(newdata) <- NULL
  sample_ids <- table_sample_ids(newdata)

  predictors <- prepare_new_predictors(object, newdata)
  rownames(predictors) <- sample_ids

  prediction <- predict_classes(
    list(recipe = object$recipe, fit = object$fit, classes = object$model_classes),
    predictors,
    object$classes
  )

  result <- data.frame(sample_id = sample_ids, .pred_class = prediction$class, stringsAsFactors = FALSE)
  probability_columns <- as.data.frame(prediction$probabilities, check.names = FALSE)
  names(probability_columns) <- paste0(".pred_", object$classes)
  rownames(probability_columns) <- NULL
  cbind(result, probability_columns)
}

# Selects and checks the predictors of a cdm_model in new data: all selected
# predictors must be present, without missing or infinite values, numeric
# predictors numeric, and factor predictors without levels unseen in training.
prepare_new_predictors <- function(model, newdata) {
  missing_columns <- setdiff(model$predictors, names(newdata))
  if (length(missing_columns) > 0L) {
    stop("newdata is missing predictor columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  predictors <- convert_predictors(newdata[model$predictors])
  for (name in model$predictors) {
    info <- model$predictor_info[[name]]
    x <- predictors[[name]]
    if (identical(info$type, "numeric") && !is.numeric(x)) {
      stop("Predictor '", name, "' must be numeric.", call. = FALSE)
    }
    if (identical(info$type, "factor")) {
      new_levels <- setdiff(unique(as.character(x)), info$levels)
      if (length(new_levels) > 0L) {
        stop(
          "Predictor '", name, "' has levels not seen in the training data: ",
          paste(new_levels, collapse = ", "), ".",
          call. = FALSE
        )
      }
      predictors[[name]] <- factor(as.character(x), levels = info$levels)
    }
  }
  predictors
}

#' @export
print.cdm_model <- function(x, ...) {
  cat("<cdm_model> ", x$options$engine, " model of ", length(x$classes), " classes\n", sep = "")
  cat("Predictors (", length(x$predictors), "): ", paste(x$predictors, collapse = ", "), "\n", sep = "")
  cat("Parameters: ", paste0(names(x$parameters), " = ", signif(unlist(x$parameters), 3), collapse = ", "), "\n", sep = "")
  cat("Classes: ", paste(x$model_classes, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' Summarize a fitted community-type model
#'
#' @param object A `cdm_model` object created by [fit_model()].
#' @param evaluation Optional `cdm_model_evaluation` of the same data and
#'   folds, used to show how often the final predictors were selected in the
#'   outer folds.
#' @param ... Not used.
#'
#' @return An object of class `summary.cdm_model`.
#'
#' @export
summary.cdm_model <- function(object, evaluation = NULL, ...) {
  selected_grid <- object$tuning[object$tuning$selected, , drop = FALSE]
  predictor_table <- data.frame(predictor = object$predictors, stringsAsFactors = FALSE)
  if (!is.null(evaluation)) {
    if (!inherits(evaluation, "cdm_model_evaluation")) {
      stop("evaluation must be created by cross_validate_model().", call. = FALSE)
    }
    if (!identical(evaluation$provenance$cv_folds_hash, object$provenance$cv_folds_hash) ||
        !identical(evaluation$provenance$data_hash, object$provenance$data_hash)) {
      stop("evaluation was computed for different data or folds than the model.", call. = FALSE)
    }
    n_splits <- length(unique(evaluation$parameters$split))
    frequency <- table(factor(evaluation$selected_predictors$predictor, levels = object$candidate_predictors)) / n_splits
    predictor_table <- data.frame(
      predictor = object$candidate_predictors,
      in_final_model = object$candidate_predictors %in% object$predictors,
      outer_fold_frequency = as.numeric(frequency),
      stringsAsFactors = FALSE
    )
    predictor_table <- predictor_table[order(-predictor_table$in_final_model, -predictor_table$outer_fold_frequency), , drop = FALSE]
  }

  result <- list(
    engine = object$options$engine,
    parameters = object$parameters,
    tuning_metric = c(metric = object$options$metric, mean = selected_grid$mean, std_err = selected_grid$std_err),
    predictors = predictor_table,
    classes = object$classes,
    model_classes = object$model_classes,
    issues = object$issues
  )
  class(result) <- "summary.cdm_model"
  result
}

#' @export
print.summary.cdm_model <- function(x, ...) {
  cat("Final ", x$engine, " model\n\n", sep = "")
  cat("Selected parameters: ", paste0(names(x$parameters), " = ", signif(unlist(x$parameters), 3), collapse = ", "), "\n", sep = "")
  cat("Tuning ", x$tuning_metric[["metric"]], ": ", format(as.numeric(x$tuning_metric[["mean"]]), digits = 3),
      " (SE ", format(as.numeric(x$tuning_metric[["std_err"]]), digits = 2), ")\n", sep = "")
  cat("\nPredictors:\n")
  print(x$predictors, row.names = FALSE, digits = 3)
  missing_classes <- setdiff(x$classes, x$model_classes)
  if (length(missing_classes) > 0L) {
    cat("\nClasses not predicted: ", paste(missing_classes, collapse = ", "), "\n", sep = "")
  }
  if (length(x$issues) > 0L) {
    cat("\nIssues:\n", paste0("  ", x$issues, collapse = "\n"), "\n", sep = "")
  }
  invisible(x)
}
