#' Cross-validate a community-type model (nested cross-validation)
#'
#' Estimates how well community types are predicted from environmental
#' predictors. For every outer fold of `cv`, the training part is split again
#' with [make_cv_folds()] into `options$inner_v` inner folds with the same
#' group, strata, and balance settings. On the inner folds the number of
#' predictors and the engine parameters are tuned (see [model_options()]). The
#' model with the selected parameters is then trained on the whole outer
#' training part and predicts the held-out outer fold. Predictor selection and
#' oversampling are therefore never fitted on held-out samples.
#'
#' The predicted class is the model's own decision: one-vs-one voting of the
#' SVMs (`e1071::svm()`, libsvm) or the majority over the trees (ranger). The
#' class probabilities come from a second prediction: for the SVM, libsvm fits
#' a Platt sigmoid to every pairwise classifier on an internal 5-fold
#' cross-validation and combines the pairwise probabilities into class
#' probabilities (Wu, Lin, and Weng 2004). This is similar to `'FitPosterior'`
#' in MATLAB (a sigmoid per binary learner and pairwise coupling), but MATLAB's
#' default coupling (`'kl'`, Hastie and Tibshirani) and its sigmoid fitting
#' differ, so the probabilities are not expected to match exactly. For the
#' random forest they are the tree
#' frequencies. Recall and F1 use the predicted classes, ROC AUC and the
#' Brier score the probabilities. The most probable class can differ from the
#' predicted class. With `imbalance = "smotenc"` the probabilities refer to
#' classes balanced by oversampling, not to their observed frequencies.
#'
#' The tuning runs with `future.apply::future_lapply()`, one task per pair of
#' tuning fold and number of predictors (`inner_v` times the number of values
#' from `n_predictors_min` to `n_predictors_max`). Without a parallel plan it
#' runs sequentially. To use several processes, set a plan before the call,
#' for example
#' `future::plan(future::multisession, workers = 15)`; the most efficient
#' number of workers is the number of tasks or a divisor of it. Every task gets
#' its own random number stream (`future.seed = TRUE`), so the results do not
#' depend on the plan or on the number of workers.
#'
#' Composition problems of inner folds, classes left out of training sets, and
#' failed fits are collected in `issues` and reported in one warning.
#'
#' @param x A data frame or tibble of predictors with one row per sample, for
#'   example `cdm_data$env[predictors]`. All columns are used as predictors.
#'   They must be numeric, factor, character, or logical, without missing
#'   values.
#' @param y A vector or factor with the class of every row of `x`, for example
#'   `cdm_data$env$community_type`.
#' @param cv Outer folds: an `rset` created by [make_cv_folds()] or another
#'   `rsample` function from the same samples in the same order as `x` and
#'   `y`, usually from `cdm_data$env`. Only the numbers of rows are compared.
#'   Samples in the results are named by the `sample_id` column of the data of
#'   `cv`, or by row numbers. The inner folds use the group, strata, and
#'   balance settings stored in `cv` (by [make_cv_folds()],
#'   `rsample::group_vfold_cv()`, or `rsample::vfold_cv(strata = )`); folds
#'   without these settings get random inner folds. Folds in which a sample is
#'   held out more than once per repeat, or in which a group is split, are
#'   rejected.
#' @param options Model options created by [model_options()].
#'
#' @return An object of class `cdm_model_evaluation`, a list with:
#' \describe{
#'   \item{predictions}{Out-of-fold predictions: `sample_id`, `repeat_id`,
#'     `fold`, `truth`, `.pred_class`, and one `.pred_<class>` probability
#'     column per class.}
#'   \item{metrics}{Metrics of the pooled out-of-fold predictions per repeat.}
#'   \item{fold_metrics}{Metrics of every outer fold.}
#'   \item{class_metrics}{Recall and F1 of every class per repeat.}
#'   \item{parameters}{Selected parameters of every outer fold, with the
#'     number of selected predictors and the inner metric.}
#'   \item{selected_predictors}{Predictors selected in every outer fold.}
#'   \item{tuning}{Inner tuning results of every outer fold.}
#'   \item{classes, options, issues, provenance}{Class levels, options (with
#'     the numbers of predictors tried in `n_predictors`), collected problems,
#'     and provenance.}
#' }
#'
#' @export
cross_validate_model <- function(x, y, cv, options) {
  inputs <- prepare_model_inputs(x, y, cv, options)
  options <- inputs$options
  classes <- levels(inputs$data$.class)
  splits <- cv_splits(inputs$folds)
  outer_issues <- check_missing_classes(inputs$folds, inputs$data)
  cv_data <- inputs$cv$data
  balance <- if (is.null(inputs$cv$balance)) "observations" else inputs$cv$balance

  results <- vector("list", length(splits))
  for (i in seq_along(splits)) {
    split <- splits[[i]]
    message("Evaluating ", options$engine, " model: ", split$id, " (", i, " of ", length(splits), ")")
    results[[i]] <- withr::with_seed(options$seed + i, {
      train <- inputs$data[split$analysis, , drop = FALSE]
      # Inner folds use the group and strata columns of the outer folds.
      train_cv_data <- cv_data[match(split$analysis, cv_data$sample_id), , drop = FALSE]
      n_groups <- if (is.null(inputs$cv$group)) nrow(train) else
        length(unique(train_cv_data[[inputs$cv$group]]))
      if (options$inner_v > n_groups) {
        stop(
          "inner_v (", options$inner_v, ") is larger than the number of groups (", n_groups,
          ") in the training part of ", split$id, ".",
          call. = FALSE
        )
      }
      inner_cv <- make_cv_folds(
        train_cv_data,
        group = inputs$cv$group,
        v = options$inner_v,
        balance = balance,
        strata = inputs$cv$strata
      )
      inner_folds <- rset_folds(inner_cv)$folds
      inner_issues <- check_missing_classes(inner_folds, train)

      tuning <- tune_class_model(train, inner_folds, options)
      model <- train_class_model(train, tuning$parameters, options)
      test <- inputs$data[split$assessment, , drop = FALSE]
      prediction <- predict_classes(model, test[names(test) != ".class"], classes)

      list(
        split = split,
        prediction = prediction,
        parameters = data.frame(
          split = split$id,
          tuning$parameters,
          n_selected = length(model$predictors),
          inner_metric = tuning$grid$mean[tuning$grid$selected],
          row.names = NULL
        ),
        predictors = model$predictors,
        tuning = tuning$grid,
        issues = c(
          # Inner fold IDs are "Repeat1_FoldNN"; report them as "inner FoldNN".
          if (length(c(inner_issues, tuning$notes)) > 0L) {
            paste0(split$id, ", ", sub("^Repeat1_", "inner ", c(inner_issues, tuning$notes)))
          },
          if (length(model$excluded_classes) > 0L) paste0(
            split$id, ": class ", paste(model$excluded_classes, collapse = ", "),
            " left out of the outer training part (fewer than ", options$min_class_size, " samples)."
          )
        )
      )
    })
  }

  predictions <- do.call(rbind, lapply(results, function(result) {
    data.frame(
      sample_id = result$split$assessment,
      repeat_id = result$split$repeat_id,
      fold = result$split$fold,
      stringsAsFactors = FALSE
    )
  }))
  probabilities <- do.call(rbind, lapply(results, function(result) result$prediction$probabilities))
  rownames(probabilities) <- NULL
  predictions$truth <- factor(as.character(inputs$data[predictions$sample_id, ".class"]), levels = classes)
  predictions$.pred_class <- factor(
    unlist(lapply(results, function(result) as.character(result$prediction$class))),
    levels = classes
  )
  probability_columns <- as.data.frame(probabilities, check.names = FALSE)
  names(probability_columns) <- paste0(".pred_", classes)
  predictions <- cbind(predictions, probability_columns)

  metric_rows <- list()
  class_rows <- list()
  for (repeat_id in unique(predictions$repeat_id)) {
    rows <- predictions$repeat_id == repeat_id
    metrics <- class_metrics(predictions$truth[rows], predictions$.pred_class[rows], probabilities[rows, , drop = FALSE])
    metric_rows[[length(metric_rows) + 1L]] <- data.frame(
      repeat_id = repeat_id,
      metric = names(metrics$summary),
      value = unname(metrics$summary),
      stringsAsFactors = FALSE
    )
    class_rows[[length(class_rows) + 1L]] <- data.frame(repeat_id = repeat_id, metrics$per_class)
  }

  fold_metrics <- do.call(rbind, lapply(results, function(result) {
    rows <- predictions$repeat_id == result$split$repeat_id & predictions$fold == result$split$fold
    metrics <- class_metrics(
      predictions$truth[rows], predictions$.pred_class[rows], probabilities[rows, , drop = FALSE]
    )$summary
    data.frame(
      repeat_id = result$split$repeat_id,
      fold = result$split$fold,
      metric = names(metrics),
      value = unname(metrics),
      stringsAsFactors = FALSE
    )
  }))

  issues <- unique(c(outer_issues, unlist(lapply(results, `[[`, "issues"))))
  if (length(issues) > 0L) {
    warning(
      length(issues), " issue(s) during evaluation; see $issues. First: ", issues[[1L]],
      call. = FALSE
    )
  }

  result <- list(
    predictions = predictions,
    metrics = do.call(rbind, metric_rows),
    fold_metrics = fold_metrics,
    class_metrics = do.call(rbind, class_rows),
    parameters = do.call(rbind, lapply(results, `[[`, "parameters")),
    selected_predictors = do.call(rbind, lapply(results, function(result) {
      data.frame(split = result$split$id, predictor = result$predictors, stringsAsFactors = FALSE)
    })),
    tuning = stats::setNames(lapply(results, `[[`, "tuning"), vapply(splits, `[[`, character(1), "id")),
    fold_classes = do.call(rbind, lapply(splits, function(split) {
      parts <- list(train = split$analysis, test = split$assessment)
      do.call(rbind, lapply(names(parts), function(part) {
        counts <- table(factor(inputs$data[parts[[part]], ".class"], levels = classes))
        data.frame(split = split$id, part = part, t(as.matrix(counts)), check.names = FALSE)
      }))
    })),
    classes = classes,
    options = options,
    issues = issues,
    provenance = list(
      data_hash = rlang::hash(inputs$data),
      cv_folds_hash = rlang::hash(inputs$folds),
      options_hash = rlang::hash(unclass(options)),
      seed = options$seed,
      versions = model_package_versions(options),
      created = Sys.time()
    )
  )
  class(result) <- "cdm_model_evaluation"
  result
}

#' @export
print.cdm_model_evaluation <- function(x, ...) {
  stop_if_kernlab(x, "cross_validate_model")
  pooled <- tapply(x$metrics$value, x$metrics$metric, mean)
  cat("<cdm_model_evaluation> ", x$options$engine, " model of ", length(x$classes), " classes, ",
      length(unique(x$parameters$split)), " outer folds\n", sep = "")
  cat("Pooled out-of-fold metrics: ",
      paste0(names(pooled), " = ", format(round(pooled, 3)), collapse = ", "), "\n", sep = "")
  if (length(x$issues) > 0L) {
    cat(length(x$issues), " issue(s); see summary() or $issues.\n", sep = "")
  }
  invisible(x)
}

#' Summarize a model evaluation
#'
#' @param object A `cdm_model_evaluation` object created by [cross_validate_model()].
#' @param ... Not used.
#'
#' @return An object of class `summary.cdm_model_evaluation`, a list with
#'   pooled metrics (mean over repeats), fold metrics (mean and standard
#'   deviation over folds), recall and F1 per class, the parameters selected in
#'   every outer fold, the selection frequency of every predictor, and issues.
#'
#' @export
summary.cdm_model_evaluation <- function(object, ...) {
  stop_if_kernlab(object, "cross_validate_model")
  pooled <- stats::aggregate(value ~ metric, data = object$metrics, FUN = mean)
  fold_mean <- stats::aggregate(value ~ metric, data = object$fold_metrics, FUN = mean)
  fold_sd <- stats::aggregate(value ~ metric, data = object$fold_metrics, FUN = stats::sd)
  n_splits <- length(unique(object$parameters$split))
  frequency <- table(object$selected_predictors$predictor) / n_splits

  result <- list(
    engine = object$options$engine,
    pooled_metrics = pooled,
    fold_metrics = data.frame(metric = fold_mean$metric, mean = fold_mean$value,
                              sd = fold_sd$value[match(fold_mean$metric, fold_sd$metric)]),
    class_metrics = stats::aggregate(cbind(recall, f1) ~ class, data = object$class_metrics,
                                     FUN = mean, na.action = stats::na.pass),
    parameters = object$parameters,
    fold_classes = object$fold_classes,
    predictor_frequency = data.frame(
      predictor = names(frequency),
      frequency = as.numeric(frequency),
      row.names = NULL
    )[order(-as.numeric(frequency)), , drop = FALSE],
    issues = object$issues
  )
  class(result) <- "summary.cdm_model_evaluation"
  result
}

#' @export
print.summary.cdm_model_evaluation <- function(x, ...) {
  cat("Evaluation of the ", x$engine, " model\n\n", sep = "")
  cat("Pooled out-of-fold metrics (mean over repeats):\n")
  print(x$pooled_metrics, row.names = FALSE, digits = 3)
  cat("\nMetrics of single folds:\n")
  print(x$fold_metrics, row.names = FALSE, digits = 3)
  cat("\nRecall and F1 per class:\n")
  print(x$class_metrics, row.names = FALSE, digits = 3)
  cat("\nClasses in the training and test parts of the outer folds:\n")
  print(x$fold_classes, row.names = FALSE)
  cat("\nSelected parameters per outer fold:\n")
  print(x$parameters, row.names = FALSE, digits = 3)
  cat("\nFraction of outer folds selecting each predictor:\n")
  print(x$predictor_frequency, row.names = FALSE, digits = 3)
  if (length(x$issues) > 0L) {
    cat("\nIssues:\n", paste0("  ", x$issues, collapse = "\n"), "\n", sep = "")
  }
  invisible(x)
}

#' Plot a model evaluation
#'
#' Shows the confusion matrix of the predicted classes normalized by rows
#' (true classes) and the recall of every class (pooled, with single folds as
#' points).
#'
#' @param x A `cdm_model_evaluation` object created by [cross_validate_model()].
#' @param type `"all"` for both panels, or one of `"confusion"` and
#'   `"recall"`.
#' @param ... Not used.
#'
#' @return A `ggplot` object or, for `type = "all"`, a patchwork object.
#'
#' @export
plot.cdm_model_evaluation <- function(x, type = c("all", "confusion", "recall"), ...) {
  type <- match.arg(type)
  predictions <- x$predictions
  classes <- x$classes

  confusion <- as.data.frame(prop.table(table(truth = predictions$truth, predicted = predictions$.pred_class), 1L))
  confusion$Freq[!is.finite(confusion$Freq)] <- NA_real_
  confusion_plot <- ggplot2::ggplot(confusion, ggplot2::aes(x = .data$predicted, y = .data$truth, fill = .data$Freq)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(.data$Freq), "", sprintf("%.2f", .data$Freq))), size = 3) +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue4", limits = c(0, 1), name = "Fraction") +
    ggplot2::scale_y_discrete(limits = rev(classes)) +
    ggplot2::labs(x = "Predicted class", y = "True class", title = "Confusion matrix (rows sum to 1)") +
    ggplot2::theme_classic(base_size = 11)

  fold_recall <- do.call(rbind, lapply(split(predictions, list(predictions$repeat_id, predictions$fold), drop = TRUE), function(rows) {
    data.frame(class = classes, recall = vapply(classes, function(class) {
      truth_rows <- rows$truth == class
      if (any(truth_rows)) mean(rows$.pred_class[truth_rows] == class) else NA_real_
    }, numeric(1)))
  }))
  pooled_recall <- stats::aggregate(recall ~ class, data = x$class_metrics, FUN = mean)
  recall_plot <- ggplot2::ggplot(pooled_recall, ggplot2::aes(x = factor(.data$class, levels = classes), y = .data$recall)) +
    ggplot2::geom_col(fill = "grey75") +
    ggplot2::geom_point(data = fold_recall[!is.na(fold_recall$recall), ],
                        position = ggplot2::position_jitter(width = 0.15, height = 0, seed = 1),
                        size = 1.2, alpha = 0.6) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = "Class", y = "Recall", title = "Recall per class (bars: pooled, points: folds)") +
    ggplot2::theme_classic(base_size = 11)

  switch(
    type,
    confusion = confusion_plot,
    recall = recall_plot,
    all = patchwork::wrap_plots(confusion_plot, recall_plot, nrow = 1)
  )
}
