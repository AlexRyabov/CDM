test_that("evaluate_model() gives the same results sequentially and in parallel", {
  skip_on_cran()
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))

  sequential <- suppressMessages(evaluate_model(cdm_data, cv, make_fast_options()))
  local_two_workers()
  parallel <- suppressMessages(evaluate_model(cdm_data, cv, make_fast_options()))

  expect_identical(parallel$predictions, sequential$predictions)
  expect_identical(parallel$parameters, sequential$parameters)
  expect_identical(parallel$issues, sequential$issues)
})

test_that("failed fits in worker processes are reported in the tuning notes", {
  skip_on_cran()
  # An infinite predictor value passes the recipe, but e1071::svm() stops on
  # it. The failure comes from the data, so it also happens in the workers.
  # The value is set after prepare_model_inputs(), which rejects it.
  cdm_data <- make_model_test_data()
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))
  inputs <- prepare_model_inputs(cdm_data, cv, make_fast_options())
  inputs$data$x2[5] <- Inf

  tune <- function() {
    suppressWarnings(withr::with_seed(1, tune_class_model(inputs$data, inputs$folds, make_fast_options())))
  }
  sequential <- tune()
  local_two_workers()
  parallel <- tune()

  expect_match(sequential$notes, "fit failed", all = FALSE)
  expect_identical(parallel$notes, sequential$notes)
  expect_identical(parallel$grid, sequential$grid)
})
