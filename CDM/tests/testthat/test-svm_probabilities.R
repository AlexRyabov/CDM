# Eight overlapping Gaussian blobs on a circle (radius 4, unit spread). With
# these data kernlab's couple() makes the most probable class agree with the
# vote in only about 75-80 % of the samples, libsvm (e1071) in about 98-99 %.
make_blob_data <- function(n_classes = 8L, per_class = 40L, radius = 4, seed = 1L) {
  withr::with_seed(seed, {
    angle <- 2 * pi * (seq_len(n_classes) - 1L) / n_classes
    cls <- rep(seq_len(n_classes), each = per_class)
    data.frame(
      .class = factor(paste0("k", cls), levels = paste0("k", seq_len(n_classes))),
      x = radius * cos(angle[cls]) + stats::rnorm(length(cls)),
      y = radius * sin(angle[cls]) + stats::rnorm(length(cls)),
      row.names = sprintf("s%03d", seq_along(cls))
    )
  })
}

train_blob_svm <- function(data) {
  options <- model_options(c("x", "y"), n_predictors = 2, imbalance = "none", correlation_threshold = 1)
  withr::with_seed(1, train_class_model(data, list(n_predictors = 2, cost = 1, rbf_sigma = 0.5), options))
}

test_that("SVM probabilities of eight classes agree with the vote", {
  data <- make_blob_data()
  model <- train_blob_svm(data)

  prediction <- predict_classes(model, data[c("x", "y")], levels(data$.class))

  expect_s3_class(model$fit, "svm")
  expect_equal(unname(rowSums(prediction$probabilities)), rep(1, nrow(data)), tolerance = 1e-6)
  most_probable <- colnames(prediction$probabilities)[max.col(prediction$probabilities, "first")]
  expect_gt(mean(most_probable == as.character(prediction$class)), 0.95)
  expect_gt(mean(prediction$class == data$.class), 0.8)
})

test_that("SVM probabilities are placed in the columns of their classes", {
  data <- make_blob_data()
  # libsvm orders its labels by their first appearance in the training data.
  data <- data[order(data$.class != "k5", seq_len(nrow(data))), ]
  model <- train_blob_svm(data)

  prediction <- predict_classes(model, data[c("x", "y")], levels(data$.class))

  x <- as.matrix(recipes::bake(model$recipe, data[c("x", "y")]))
  libsvm <- attr(stats::predict(model$fit, x, probability = TRUE), "probabilities")
  expect_identical(colnames(libsvm)[1], "k5")
  expect_identical(colnames(prediction$probabilities), levels(data$.class))
  for (class in levels(data$.class)) {
    expect_equal(unname(prediction$probabilities[, class]), unname(libsvm[, class]))
  }
})

test_that("evaluate_model() is reproducible with the same seed", {
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))

  first <- suppressMessages(evaluate_model(cdm_data, cv, make_fast_options()))
  second <- suppressMessages(evaluate_model(cdm_data, cv, make_fast_options()))

  expect_identical(first$predictions, second$predictions)
})

test_that("saved SVM and random forest models predict in a fresh R session", {
  # predict() of a saved cdm_model needs S3 methods of other packages: bake()
  # methods of the recipe steps and predict() of e1071 `svm` or parsnip
  # `model_fit` objects. CDMr's NAMESPACE must import these packages so that
  # they are loaded with CDMr. (pkgload::load_all() loads all DESCRIPTION
  # imports and would hide a missing directive in the subprocess below.)
  imported <- names(getNamespaceImports(asNamespace("CDMr")))
  for (package in c("e1071", "important", "themis", "parsnip", "ranger")) {
    expect_true(package %in% imported, info = package)
  }
  skip_on_cran()

  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))
  data_file <- tempfile(fileext = ".rds")
  saveRDS(cdm_data, data_file)
  # Load CDMr from the source (devtools::test()) or the installed package
  # (R CMD check), without attaching any other package.
  source_dir <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = FALSE)
  load_call <- if (file.exists(file.path(source_dir, "DESCRIPTION"))) {
    sprintf("pkgload::load_all('%s', quiet = TRUE, export_all = FALSE)", source_dir)
  } else {
    "loadNamespace('CDMr')"
  }

  for (engine in c("svm", "rf")) {
    model <- suppressMessages(fit_model(cdm_data, cv, make_fast_options(engine)))
    expected <- predict(model, cdm_data)
    model_file <- tempfile(fileext = ".rds")
    result_file <- tempfile(fileext = ".rds")
    saveRDS(model, model_file)

    script <- tempfile(fileext = ".R")
    writeLines(c(
      load_call,
      sprintf("model <- readRDS('%s')", normalizePath(model_file, winslash = "/")),
      sprintf("cdm_data <- readRDS('%s')", normalizePath(data_file, winslash = "/")),
      sprintf("saveRDS(predict(model, cdm_data), '%s')", normalizePath(result_file, winslash = "/", mustWork = FALSE))
    ), script)
    output <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(script)),
      stdout = TRUE, stderr = TRUE
    ))

    expect_true(file.exists(result_file), info = paste(engine, paste(output, collapse = "\n")))
    if (file.exists(result_file)) {
      expect_equal(readRDS(result_file), expected, info = engine)
    }
  }
})

test_that("objects fitted with kernlab are rejected where probabilities are used", {
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))
  model <- suppressMessages(fit_model(cdm_data, cv, make_fast_options()))
  evaluation <- suppressMessages(evaluate_model(cdm_data, cv, make_fast_options()))

  old_model <- model
  old_model$provenance$versions <- c(old_model$provenance$versions, kernlab = "0.9.33")
  old_evaluation <- evaluation
  old_evaluation$provenance$versions <- c(old_evaluation$provenance$versions, kernlab = "0.9.33")

  expect_error(predict(old_model, cdm_data), "fitted with kernlab.*fit_model", fixed = FALSE)
  expect_error(summary(old_evaluation), "fitted with kernlab.*evaluate_model")
  expect_error(print(old_evaluation), "fitted with kernlab")
  expect_error(compare_models(old = old_evaluation, new = evaluation), "fitted with kernlab")
  # Results that do not use probabilities stay available.
  expect_output(print(old_model), "<cdm_model>")
  expect_s3_class(plot(old_evaluation, type = "confusion"), "ggplot")
})
