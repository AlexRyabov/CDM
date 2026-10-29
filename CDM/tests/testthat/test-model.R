test_that("model_options() validates settings and update() changes them", {
  options <- make_fast_options()

  expect_s3_class(options, "cdm_model_options")
  expect_identical(options$n_predictors, 1:2)
  expect_identical(update(options, engine = "rf")$engine, "rf")
  expect_identical(update(options, engine = "rf")$predictors, options$predictors)

  expect_error(
    model_options(predictors = c("x1", "x2"), n_predictors = 2:3),
    "must not be larger than the number of predictors \\(2\\): 3"
  )
  expect_error(model_options(predictors = c("x1", "x1")), "unique column names")
  expect_error(model_options(predictors = "x1", n_predictors = 0), "positive integers")
  expect_error(update(options, kernel = "rbf"), "Unknown: kernel")
})

test_that("class_metrics() uses macro averages over observed classes", {
  probabilities <- rbind(
    c(0.8, 0.1, 0.1),
    c(0.6, 0.3, 0.1),
    c(0.2, 0.7, 0.1),
    c(0.1, 0.2, 0.7)
  )
  colnames(probabilities) <- c("A", "B", "C")
  truth <- c("A", "B", "B", "B")
  # The predicted classes differ from the most probable classes in row 2.
  predicted <- c("A", "B", "B", "C")

  metrics <- class_metrics(truth, predicted, probabilities)

  expect_equal(metrics$per_class$recall, c(1, 2 / 3, NA))
  expect_equal(metrics$summary[["macro_recall"]], mean(c(1, 2 / 3)))
  expect_equal(metrics$summary[["macro_f1"]], mean(c(1, 4 / 5)))
  indicators <- outer(truth, c("A", "B", "C"), "==") * 1
  expect_equal(metrics$summary[["brier"]], mean(rowSums((probabilities - indicators)^2)))
  expect_equal(metrics$per_class$n, c(1L, 3L, 0L))
})

test_that("evaluate_model() runs nested cross-validation with sample IDs", {
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2, strata = month))

  evaluation <- suppressMessages(evaluate_model(cdm_data, cv, make_fast_options()))

  expect_s3_class(evaluation, "cdm_model_evaluation")
  expect_setequal(evaluation$predictions$sample_id, cdm_data$sample_names)
  expect_identical(
    as.character(evaluation$predictions$truth),
    as.character(cdm_data$env[evaluation$predictions$sample_id, "community_type"])
  )
  probabilities <- as.matrix(evaluation$predictions[paste0(".pred_", c("A", "B", "C"))])
  expect_equal(unname(rowSums(probabilities)), rep(1, 160), tolerance = 1e-6)
  expect_named(evaluation$metrics, c("repeat_id", "metric", "value"))
  expect_identical(nrow(evaluation$parameters), 2L)
  expect_true(all(evaluation$parameters$n_selected <= 2L))
  expect_true(all(evaluation$selected_predictors$predictor %in% c("x1", "x2", "x3", "habitat")))
  # x1 determines the classes and x3 duplicates it; one of them is selected.
  expect_true(all(tapply(
    evaluation$selected_predictors$predictor %in% c("x1", "x3"),
    evaluation$selected_predictors$split,
    any
  )))
  pooled <- evaluation$metrics
  expect_gt(pooled$value[pooled$metric == "macro_recall"], 0.6)

  expect_s3_class(summary(evaluation), "summary.cdm_model_evaluation")
  expect_output(print(summary(evaluation)), "Selected parameters per outer fold")
  expect_s3_class(plot(evaluation, type = "confusion"), "ggplot")
  expect_s3_class(plot(evaluation), "patchwork")
})

test_that("evaluate_model() handles one predictor and a random forest", {
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))
  options <- model_options(
    predictors = "x1", engine = "rf", n_predictors = 1, grid_size = 1,
    inner_v = 2, imbalance = "none"
  )

  evaluation <- suppressMessages(evaluate_model(cdm_data, cv, options))

  expect_identical(unique(evaluation$selected_predictors$predictor), "x1")
  expect_gt(evaluation$metrics$value[evaluation$metrics$metric == "macro_recall"], 0.6)
})

test_that("evaluate_model() gives probability 0 to classes missing from training", {
  cdm_data <- make_model_test_data()
  cdm_data$env$community_type[cdm_data$env$site == "site1"] <- "C"
  cdm_data$env$community_type[cdm_data$env$site != "site1" & cdm_data$env$community_type == "C"] <- "B"
  cv <- suppressWarnings(withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2)))
  options <- make_fast_options()

  expect_warning(
    evaluation <- suppressMessages(evaluate_model(cdm_data, cv, options)),
    "issue\\(s\\) during evaluation"
  )
  expect_match(evaluation$issues, "^Repeat1_Fold0\\d, inner Fold0\\d: the training part has no samples of class C", all = FALSE)

  site1_rows <- evaluation$predictions$sample_id %in% rownames(cdm_data$env)[cdm_data$env$site == "site1"]
  expect_true(all(evaluation$predictions$.pred_C[site1_rows] == 0))
  expect_true(all(evaluation$predictions$.pred_class[site1_rows] != "C"))
})

test_that("evaluate_model() and fit_model() reject missing values and mismatched folds", {
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))

  with_na <- cdm_data
  with_na$env$x2[c(2, 5)] <- NA
  expect_error(evaluate_model(with_na, cv, make_fast_options()), "missing values: x2 \\(2\\)")

  with_inf <- cdm_data
  with_inf$env$x2[c(2, 5)] <- c(Inf, -Inf)
  expect_error(evaluate_model(with_inf, cv, make_fast_options()), "infinite values: x2 \\(2\\)")
  expect_error(fit_model(with_inf, cv, make_fast_options()), "infinite values: x2 \\(2\\)")

  other_data <- filter_cdm_data(cdm_data, site != "site8")
  expect_error(evaluate_model(other_data, cv, make_fast_options()), "must be the samples of cdm_data")
  expect_error(fit_model(other_data, cv, make_fast_options()), "must be the samples of cdm_data")

  unnamed_env <- cdm_data$env
  rownames(unnamed_env) <- NULL
  unnamed_cv <- withr::with_seed(1, rsample::group_vfold_cv(unnamed_env, group = site, v = 2))
  expect_error(evaluate_model(cdm_data, unnamed_cv, make_fast_options()), "must be the samples of cdm_data")

  expect_error(
    evaluate_model(cdm_data, cv, update(make_fast_options(), target = "biome")),
    "missing columns: biome"
  )
  expect_error(evaluate_model(cdm_data, cv, list()), "options must be created by model_options")
  expect_error(evaluate_model(cdm_data, list(), make_fast_options()), "rsample resampling object")
})

test_that("compare_models() requires evaluations on the same folds", {
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))
  other_cv <- withr::with_seed(2, make_cv_folds(cdm_data$env, group = site, v = 2))
  options <- make_fast_options()

  svm <- suppressMessages(evaluate_model(cdm_data, cv, options))
  rf <- suppressMessages(evaluate_model(cdm_data, cv, update(options, engine = "rf")))

  comparison <- compare_models(svm = svm, rf = rf)
  expect_s3_class(comparison, "cdm_model_comparison")
  expect_identical(comparison$metric, c("macro_recall", "macro_f1", "roc_auc", "brier"))
  expect_equal(comparison$difference, comparison$value_1 - comparison$value_2)
  expect_identical(unique(comparison$n_folds), 2L)

  if (!identical(rset_folds(cv, cdm_data$sample_names)$folds, rset_folds(other_cv, cdm_data$sample_names)$folds)) {
    other <- suppressMessages(evaluate_model(cdm_data, other_cv, options))
    expect_error(compare_models(svm = svm, other = other), "different data or folds")
  }
  expect_error(compare_models(svm), "at least two evaluations")
})

test_that("fit_model() and predict() use the selected predictors and sample IDs", {
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))

  model <- suppressMessages(fit_model(cdm_data, cv, make_fast_options()))

  expect_s3_class(model, "cdm_model")
  expect_true(length(model$predictors) <= 2L)
  expect_identical(model$classes, c("A", "B", "C"))
  expect_null(model$calibrator)

  predictions <- predict(model, cdm_data)
  expect_identical(predictions$sample_id, cdm_data$sample_names)
  # The class is the SVM's own decision: e1071 predict() without probabilities
  # (one-vs-one voting), not the most probable class.
  expect_s3_class(model$fit, "svm")
  baked <- as.matrix(recipes::bake(model$recipe, cdm_data$env[model$predictors]))
  expect_identical(as.character(predictions$.pred_class), as.character(predict(model$fit, baked)))
  expect_named(predictions, c("sample_id", ".pred_class", ".pred_A", ".pred_B", ".pred_C"))
  expect_equal(rowSums(predictions[3:5]), rep(1, 160), tolerance = 1e-6)
  expect_gt(mean(predictions$.pred_class == cdm_data$env$community_type), 0.7)

  # Data frames are matched by sample ID; only the selected predictors are needed.
  new_data <- cdm_data$env[rev(cdm_data$sample_names), c("Sample", model$predictors), drop = FALSE]
  reversed <- predict(model, new_data, sample_id = "Sample")
  expect_identical(reversed$sample_id, rev(cdm_data$sample_names))
  expect_equal(reversed[rev(seq_len(160)), 3:5], predictions[3:5], ignore_attr = TRUE)

  expect_error(predict(model, new_data[setdiff(names(new_data), model$predictors[1])], sample_id = "Sample"),
               "missing predictor columns")
  numeric_predictor <- Filter(function(name) is.numeric(new_data[[name]]), model$predictors)[1]
  with_inf <- new_data
  with_inf[[numeric_predictor]][1] <- -Inf
  expect_error(predict(model, with_inf, sample_id = "Sample"), "infinite values")
  new_data[[model$predictors[1]]][1] <- NA
  expect_error(predict(model, new_data, sample_id = "Sample"), "missing values")
  duplicated_ids <- cdm_data$env[c(1, 1), c("Sample", model$predictors), drop = FALSE]
  expect_error(predict(model, duplicated_ids, sample_id = "Sample"), "must be unique")
})

test_that("predict() rejects new factor levels", {
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))
  options <- model_options(
    predictors = c("x1", "habitat"), n_predictors = 2, grid_size = 1,
    inner_v = 2, correlation_threshold = 1
  )

  model <- suppressMessages(fit_model(cdm_data, cv, options))
  expect_setequal(model$predictors, c("x1", "habitat"))

  new_data <- cdm_data$env[1:3, c("x1", "habitat")]
  new_data$habitat[2] <- "abyss"
  expect_error(predict(model, new_data), "levels not seen in the training data: abyss")
})

test_that("summary.cdm_model() compares final and outer-fold predictors", {
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))
  options <- make_fast_options()
  evaluation <- suppressMessages(evaluate_model(cdm_data, cv, options))
  model <- suppressMessages(fit_model(cdm_data, cv, options))

  model_summary <- summary(model, evaluation = evaluation)

  expect_named(model_summary$predictors, c("predictor", "in_final_model", "outer_fold_frequency"))
  expect_setequal(model_summary$predictors$predictor, options$predictors)
  expect_output(print(model_summary), "Final svm model")

  other_cv <- withr::with_seed(5, make_cv_folds(cdm_data$env, group = site, v = 2))
  if (!identical(rset_folds(cv, cdm_data$sample_names)$folds, rset_folds(other_cv, cdm_data$sample_names)$folds)) {
    other_model <- suppressMessages(fit_model(cdm_data, other_cv, options))
    expect_error(summary(other_model, evaluation = evaluation), "different data or folds")
  }
})
