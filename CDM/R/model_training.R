# Internal functions shared by cross_validate_model() and fit_model(): model data,
# recipes, engines, prediction, tuning, and metrics. The classes are stored in
# the column `.class` of the internal model data, whose row names are the
# sample IDs.

# Validates x, y, cv, and options, as fit(X, y) with a cross-validation object
# in scikit-learn. The rows of x and y are the rows of the data of cv; only
# their numbers are compared. Returns the model data (.class and the
# predictors, rows named by sample ID), the checked fold table, the
# cross-validation settings (data, group, strata, balance) of cv, and the
# options with the numbers of predictors to try (`n_predictors`).
prepare_model_inputs <- function(x, y, cv, options) {
  if (!inherits(options, "cdm_model_options")) {
    stop("options must be created by model_options().", call. = FALSE)
  }
  if (!is.data.frame(x)) {
    stop("x must be a data frame or tibble with one predictor per column.", call. = FALSE)
  }
  x <- as.data.frame(x, check.names = FALSE)
  if (ncol(x) == 0L) {
    stop("x must have at least one predictor column.", call. = FALSE)
  }
  if (".class" %in% names(x) || anyDuplicated(names(x)) || any(!nzchar(names(x)))) {
    stop("The columns of x must have unique, non-empty names other than \".class\".", call. = FALSE)
  }
  if (is.data.frame(y) || is.list(y) && !is.factor(y)) {
    stop("y must be a vector or factor of classes, for example cdm_data$env$community_type.", call. = FALSE)
  }
  cv_info <- rset_folds(cv)
  if (nrow(x) != nrow(cv_info$data) || length(y) != nrow(cv_info$data)) {
    stop(
      "x, y, and the data of cv must have the same rows; their numbers are ",
      nrow(x), ", ", length(y), ", and ", nrow(cv_info$data), ". Make cv from the same samples.",
      call. = FALSE
    )
  }

  n_max <- if (is.null(options$n_predictors_max)) ncol(x) else options$n_predictors_max
  if (n_max > ncol(x)) {
    stop("n_predictors_max (", n_max, ") is larger than the number of columns of x (", ncol(x), ").", call. = FALSE)
  }
  if (options$n_predictors_min > n_max) {
    stop("n_predictors_min (", options$n_predictors_min, ") is larger than the number of predictors in x (", ncol(x), ").", call. = FALSE)
  }
  options$n_predictors <- seq(options$n_predictors_min, n_max)

  target <- y
  if (any(is.na(target) | as.character(target) == "")) {
    stop("y has missing or empty classes.", call. = FALSE)
  }
  if (!is.factor(target)) {
    # Numeric class labels such as "1", ..., "10" are ordered numerically.
    values <- unique(as.character(target))
    numeric_values <- suppressWarnings(as.numeric(values))
    target <- factor(
      as.character(target),
      levels = if (anyNA(numeric_values)) sort(values) else values[order(numeric_values)]
    )
  }
  target <- droplevels(target)

  predictors <- convert_predictors(x)
  data <- data.frame(.class = target, predictors, check.names = FALSE)
  rownames(data) <- cv_info$data$sample_id

  list(data = data, folds = cv_info$folds, cv = cv_info, options = options)
}

# Checks predictor types and missing values; character and logical
# predictors become factors.
convert_predictors <- function(predictors) {
  na_counts <- vapply(predictors, function(x) sum(is.na(x)), numeric(1))
  if (any(na_counts > 0)) {
    stop(
      "Predictors contain missing values: ",
      paste0(names(na_counts)[na_counts > 0], " (", na_counts[na_counts > 0], ")", collapse = ", "),
      ". Impute or remove them before modelling.",
      call. = FALSE
    )
  }
  inf_counts <- vapply(predictors, function(x) if (is.numeric(x)) sum(is.infinite(x)) else 0, numeric(1))
  if (any(inf_counts > 0)) {
    stop(
      "Predictors contain infinite values: ",
      paste0(names(inf_counts)[inf_counts > 0], " (", inf_counts[inf_counts > 0], ")", collapse = ", "),
      ". Replace or remove them before modelling.",
      call. = FALSE
    )
  }

  for (name in names(predictors)) {
    x <- predictors[[name]]
    if (is.character(x) || is.logical(x)) {
      predictors[[name]] <- factor(x)
    } else if (!(is.numeric(x) || is.factor(x)) || inherits(x, c("Date", "POSIXt"))) {
      stop("Predictor '", name, "' must be numeric, factor, character, or logical.", call. = FALSE)
    }
  }
  predictors
}

# Engine parameter combinations from a space-filling design.
engine_parameter_grid <- function(options) {
  parameters <- if (identical(options$engine, "svm")) {
    list(dials::cost(range = c(-2, 8)), dials::rbf_sigma(range = c(-3, 0)))
  } else {
    list(dials::mtry_prop(range = c(0.1, 1)), dials::min_n(range = c(2L, 40L)))
  }
  grid <- as.data.frame(dials::grid_space_filling(parameters, size = options$grid_size))
  if (identical(options$engine, "rf")) {
    names(grid)[names(grid) == "mtry"] <- "mtry_prop"
  }
  unique(grid)
}

# Removes classes with fewer than min_class_size samples from a training set.
drop_rare_classes <- function(data, min_class_size) {
  counts <- table(data$.class)
  excluded <- names(counts)[counts > 0L & counts < min_class_size]
  data <- data[!data$.class %in% excluded, , drop = FALSE]
  data$.class <- droplevels(data$.class)
  if (nlevels(data$.class) < 2L) {
    stop(
      "A training set has fewer than two classes with at least ", min_class_size,
      " samples. Reduce min_class_size or use fewer folds.",
      call. = FALSE
    )
  }
  list(data = data, classes = levels(data$.class), excluded = excluded)
}

# Preprocessing steps applied before predictor selection.
add_filter_steps <- function(recipe, options) {
  recipe <- recipes::step_novel(recipe, recipes::all_nominal_predictors())
  recipe <- recipes::step_zv(recipe, recipes::all_predictors())
  if (identical(options$engine, "svm")) {
    recipe <- recipes::step_normalize(recipe, recipes::all_numeric_predictors())
  }
  recipes::step_corr(
    recipe,
    recipes::all_numeric_predictors(),
    threshold = options$correlation_threshold
  )
}

# Prepares the full recipe on a training set and returns it with the
# selected predictors. The proportion passed to step_predictor_best() is
# computed from the predictors left after filtering, so that exactly
# min(n_predictors, available) predictors are kept.
prep_model_recipe <- function(data, n_predictors, options) {
  base_recipe <- recipes::recipe(.class ~ ., data = data)
  filter_recipe <- recipes::prep(add_filter_steps(base_recipe, options), training = data)
  filter_summary <- summary(filter_recipe)
  available <- filter_summary$variable[filter_summary$role == "predictor"]

  recipe <- add_filter_steps(base_recipe, options)
  recipe <- important::step_predictor_best(
    recipe,
    recipes::all_predictors(),
    score = options$selection_score,
    prop_terms = min(1, (n_predictors + 0.5) / length(available)),
    id = "predictor_best"
  )
  if (identical(options$imbalance, "smotenc")) {
    recipe <- themis::step_smotenc(
      recipe,
      ".class",
      neighbors = min(5L, min(table(data$.class)) - 1L),
      seed = sample.int(1e5, 1L)
    )
  }
  if (identical(options$engine, "svm")) {
    recipe <- recipes::step_dummy(recipe, recipes::all_nominal_predictors())
    recipe <- recipes::step_zv(recipe, recipes::all_predictors())
  }
  recipe <- recipes::prep(recipe, training = data)

  # With a single available predictor the step scores nothing and returns an
  # empty table without the `removed` column.
  selection <- recipes::tidy(recipe, id = "predictor_best")
  removed <- if ("removed" %in% names(selection)) selection$terms[selection$removed] else character()
  list(
    recipe = recipe,
    predictors = setdiff(available, removed)
  )
}

# Fits the engine to the processed training data of a prepared recipe.
# SVMs are fitted with e1071::svm() (libsvm): radial kernel, one-vs-one
# classifiers, and Platt probabilities. kernlab is not used because its
# couple() combines the pairwise probabilities wrongly for four or more
# classes (kernlab 0.9.33). Random forests are fitted with parsnip and ranger.
fit_engine <- function(recipe, parameters, options) {
  training <- recipes::bake(recipe, new_data = NULL)
  x <- training[setdiff(names(training), ".class")]

  if (identical(options$engine, "svm")) {
    return(e1071::svm(
      x = as.matrix(x),
      y = training$.class,
      kernel = "radial",
      gamma = parameters$rbf_sigma,
      cost = parameters$cost,
      probability = TRUE,
      scale = TRUE
    ))
  }

  spec <- parsnip::rand_forest(
    mtry = max(1L, round(parameters$mtry_prop * ncol(x))),
    min_n = min(parameters$min_n, nrow(x)),
    trees = 500L
  )
  spec <- parsnip::set_mode(spec, "classification")
  spec <- parsnip::set_engine(spec, "ranger")
  parsnip::fit_xy(spec, x = x, y = training$.class)
}

# Trains a model with fixed parameters: removes rare classes, prepares the
# recipe with predictor selection, and fits the engine.
train_class_model <- function(data, parameters, options) {
  prepared <- drop_rare_classes(data, options$min_class_size)
  prepped <- prep_model_recipe(prepared$data, parameters$n_predictors, options)
  list(
    recipe = prepped$recipe,
    fit = fit_engine(prepped$recipe, parameters, options),
    classes = prepared$classes,
    excluded_classes = prepared$excluded,
    predictors = prepped$predictors
  )
}

# Predicts new data with the standard predict() methods of the engines. The
# class is the model's own decision: one-vs-one voting for the SVM
# (e1071 predict() without probabilities) and the majority over the trees for
# the random forest (parsnip `type = "class"`). The probabilities come from a
# second call: libsvm's Platt probabilities combined over the class pairs
# (e1071 `probability = TRUE`) or ranger's tree frequencies (parsnip
# `type = "prob"`). They are placed by class name in one column per class in
# `classes`; classes the model was not trained on get probability 0. With
# `class = FALSE` only the probabilities are predicted and `class` is NULL.
predict_classes <- function(model, new_data, classes, class = TRUE) {
  baked <- recipes::bake(model$recipe, new_data = new_data)
  baked <- baked[setdiff(names(baked), ".class")]

  predicted_class <- NULL
  if (inherits(model$fit, "svm")) {
    # libsvm matches columns by position. The columns come from the same
    # prepared recipe as in training, so their order is the same.
    x <- as.matrix(baked)
    if (class) {
      predicted_class <- stats::predict(model$fit, x)
    }
    model_probabilities <- attr(stats::predict(model$fit, x, probability = TRUE), "probabilities")
  } else {
    if (class) {
      predicted_class <- stats::predict(model$fit, baked, type = "class")$.pred_class
    }
    model_probabilities <- as.matrix(stats::predict(model$fit, baked, type = "prob"))
    colnames(model_probabilities) <- sub("^\\.pred_", "", colnames(model_probabilities))
  }

  probabilities <- matrix(0, nrow = nrow(new_data), ncol = length(classes),
                          dimnames = list(rownames(new_data), classes))
  probabilities[, colnames(model_probabilities)] <- model_probabilities
  list(
    class = if (class) factor(as.character(predicted_class), levels = classes),
    probabilities = probabilities
  )
}

# Tunes the number of predictors and the engine parameters over the folds in
# `folds` (sample_id, repeat_id, fold). Every pair of tuning fold and number of
# predictors is an independent task: the fold is prepared once and all engine
# parameters are fitted on that preparation. The tasks run with
# future.apply::future_lapply(), so they run in parallel when the user sets a
# parallel future::plan(); `future.seed = TRUE` gives every task its own
# random number stream, so the result does not depend on the plan or the
# number of workers. Every task returns its results and its notes (failed fits
# and excluded classes); nothing is written outside the task.
# Returns the selected parameters, the grid summary, and the notes.
tune_class_model <- function(data, folds, options) {
  classes <- levels(data$.class)
  grid <- engine_parameter_grid(options)
  splits <- cv_splits(folds)
  # expand.grid() varies the first column fastest: folds outside, numbers of
  # predictors inside, as in a nested loop.
  tasks <- expand.grid(n_predictors = options$n_predictors, split = seq_along(splits))

  task_results <- future.apply::future_lapply(seq_len(nrow(tasks)), function(task) {
    split <- splits[[tasks$split[task]]]
    n_predictors <- tasks$n_predictors[task]
    notes <- character()

    prepared <- drop_rare_classes(data[split$analysis, , drop = FALSE], options$min_class_size)
    if (length(prepared$excluded) > 0L) {
      notes <- c(notes, paste0(
        split$id, ": class ", paste(prepared$excluded, collapse = ", "),
        " has fewer than ", options$min_class_size, " training samples and was left out."
      ))
    }
    assessment <- data[split$assessment, , drop = FALSE]
    prepped <- prep_model_recipe(prepared$data, n_predictors, options)

    rows <- vector("list", nrow(grid))
    for (g in seq_len(nrow(grid))) {
      prediction <- tryCatch(
        predict_classes(
          list(
            recipe = prepped$recipe,
            fit = fit_engine(prepped$recipe, grid[g, , drop = FALSE], options),
            classes = prepared$classes
          ),
          assessment[names(assessment) != ".class"],
          classes
        ),
        error = function(error) error
      )
      failed <- inherits(prediction, "error")
      if (failed) {
        notes <- c(notes, paste0(split$id, ": fit failed: ", conditionMessage(prediction)))
      }
      rows[[g]] <- data.frame(
        config = paste(n_predictors, g, sep = "_"),
        n_predictors = n_predictors,
        grid[g, , drop = FALSE],
        split = split$id,
        n_selected = length(prepped$predictors),
        value = if (failed) NA_real_ else class_metrics(
          assessment$.class, prediction$class, prediction$probabilities
        )$summary[[options$metric]],
        failed = failed,
        row.names = NULL
      )
    }
    list(fold_results = do.call(rbind, rows), notes = notes)
  }, future.seed = TRUE)

  fold_results <- do.call(rbind, lapply(task_results, `[[`, "fold_results"))
  notes <- unlist(lapply(task_results, `[[`, "notes"))
  if (is.null(notes)) {
    notes <- character()
  }
  parameter_names <- c("n_predictors", names(grid))
  grid_summary <- do.call(rbind, lapply(split(fold_results, fold_results$config), function(rows) {
    values <- rows$value[!is.na(rows$value)]
    data.frame(
      rows[1L, c("config", parameter_names)],
      n_selected = mean(rows$n_selected),
      mean = if (length(values) > 0L) mean(values) else NA_real_,
      std_err = if (length(values) > 1L) stats::sd(values) / sqrt(length(values)) else NA_real_,
      n_folds = length(values),
      failed = any(rows$failed),
      row.names = NULL
    )
  }))

  valid <- grid_summary[!grid_summary$failed & !is.na(grid_summary$mean), , drop = FALSE]
  if (nrow(valid) == 0L) {
    stop("All parameter combinations failed during tuning:\n", paste(unique(notes), collapse = "\n"), call. = FALSE)
  }

  # One-standard-error rule: among configurations within one standard error of
  # the best, take the one with the fewest predictors and the simplest model.
  best <- valid[which.max(valid$mean), ]
  bound <- best$mean - if (is.na(best$std_err)) 0 else best$std_err
  candidates <- valid[valid$mean >= bound, , drop = FALSE]
  simplicity <- if (identical(options$engine, "svm")) {
    order(candidates$n_predictors, candidates$cost, candidates$rbf_sigma)
  } else {
    order(candidates$n_predictors, -candidates$min_n, candidates$mtry_prop)
  }
  selected <- candidates[simplicity[1L], , drop = FALSE]

  grid_summary$selected <- grid_summary$config == selected$config
  list(
    parameters = as.list(selected[parameter_names]),
    grid = grid_summary[order(grid_summary$n_predictors, grid_summary$config), , drop = FALSE],
    notes = unique(notes)
  )
}

# Classification metrics from true classes, predicted classes, and a
# probability matrix whose columns are the classes. Recall and F1 use the
# predicted classes; ROC AUC (Hand-Till) and the Brier score use the
# probabilities. Classes absent from `truth` do not enter the macro averages.
# Brier score: mean over samples of the summed squared differences between
# probabilities and 0/1 class indicators.
class_metrics <- function(truth, predicted, probabilities) {
  classes <- colnames(probabilities)
  truth <- factor(as.character(truth), levels = classes)
  predicted <- factor(as.character(predicted), levels = classes)

  confusion <- table(truth, predicted)
  true_positive <- diag(confusion)
  n_true <- rowSums(confusion)
  n_predicted <- colSums(confusion)
  present <- n_true > 0
  f1 <- 2 * true_positive / (n_true + n_predicted)

  macro_recall <- withCallingHandlers(
    yardstick::sens_vec(truth, predicted, estimator = "macro"),
    yardstick_warning_sens_undefined = function(warning) invokeRestart("muffleWarning")
  )

  present_classes <- classes[present]
  roc_auc <- if (length(present_classes) > 2L) {
    yardstick::roc_auc_vec(
      factor(as.character(truth), levels = present_classes),
      probabilities[, present_classes, drop = FALSE],
      estimator = "hand_till"
    )
  } else if (length(present_classes) == 2L) {
    yardstick::roc_auc_vec(
      factor(as.character(truth), levels = present_classes),
      probabilities[, present_classes[1L]],
      event_level = "first"
    )
  } else {
    NA_real_
  }

  indicators <- outer(as.character(truth), classes, "==") * 1
  list(
    summary = c(
      macro_recall = macro_recall,
      macro_f1 = mean(f1[present]),
      roc_auc = roc_auc,
      brier = mean(rowSums((probabilities - indicators)^2))
    ),
    per_class = data.frame(
      class = classes,
      n = as.integer(n_true),
      recall = ifelse(present, true_positive / n_true, NA_real_),
      f1 = ifelse(present, f1, NA_real_),
      row.names = NULL
    ),
    confusion = confusion
  )
}

# Stops for models and evaluations whose SVM was fitted with kernlab (CDMr
# versions before the switch to e1071). They still run, because parsnip loads
# kernlab, but kernlab's couple() gives wrong class probabilities for four or
# more classes, so their probabilities, ROC AUC, and Brier score are wrong.
stop_if_kernlab <- function(object, refit_with) {
  if ("kernlab" %in% names(object$provenance$versions)) {
    stop(
      "This object was fitted with kernlab, whose class probabilities are wrong for four or ",
      "more classes (couple() in kernlab 0.9.33). Fit it again with ", refit_with, "().",
      call. = FALSE
    )
  }
}

model_package_versions <- function(options) {
  package_versions(c(
    "recipes", "parsnip", "dials", "yardstick", "important", "themis",
    if (identical(options$engine, "svm")) "e1071" else "ranger"
  ))
}
