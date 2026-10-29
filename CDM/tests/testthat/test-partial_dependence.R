# A model on the numeric predictors x1 (determines the type) and x2 (noise).
fit_pd_test_model <- function(cdm_data) {
  cv <- withr::with_seed(1, make_cv_folds(cdm_data$env, group = site, v = 2))
  options <- model_options(
    predictors = c("x1", "x2"), n_predictors = 2, grid_size = 1,
    inner_v = 2, correlation_threshold = 1, seed = 1
  )
  suppressMessages(fit_model(cdm_data, cv, options))
}

test_that("pd2D_find() averages the predicted probabilities over the data rows", {
  cdm_data <- make_model_test_data()
  model <- fit_pd_test_model(cdm_data)
  data <- cdm_data$env[cdm_data$env$site %in% c("site1", "site2"), ]

  pd <- pd2D_find(model, data, pred1 = list(x1 = c(0.1, 0.5, 0.9)), pred2 = list(x2 = c(-1, 0, 1, 2)))

  expect_s3_class(pd, "cdm_pd2d")
  expect_identical(c(pd$pred1, pd$pred2), c("x1", "x2"))
  expect_identical(dim(pd$probabilities), c(3L, 4L, 3L))
  expect_identical(dimnames(pd$probabilities)[[3]], c("A", "B", "C"))
  expect_identical(pd$n_data, nrow(data))
  expect_identical(pd$data_ids, rownames(data))
  expect_identical(pd$model_provenance, model$provenance)
  expect_equal(apply(pd$probabilities, c(1, 2), sum), matrix(1, 3, 4), tolerance = 1e-6, ignore_attr = TRUE)

  # Grid point x1 = 0.5, x2 = 1: the mean of predict() with both values set.
  fixed <- data
  fixed$x1 <- 0.5
  fixed$x2 <- 1
  expected <- colMeans(predict(model, fixed)[c(".pred_A", ".pred_B", ".pred_C")])
  expect_equal(pd$probabilities[2, 3, ], expected, ignore_attr = TRUE, tolerance = 1e-10)
})

test_that("pd2D_find() validates its arguments", {
  cdm_data <- make_model_test_data()
  model <- fit_pd_test_model(cdm_data)
  data <- cdm_data$env

  expect_error(pd2D_find(model, data, list(x3 = 1:2), list(x2 = 1:2)), "'x3' is not a predictor")
  expect_error(pd2D_find(model, data, 1:2, list(x2 = 1:2)), "pred1 must be a named list")
  expect_error(pd2D_find(model, data, list(x1 = 1:2), list(x1 = 1:2)), "different predictors")
  expect_error(pd2D_find(model, data, list(x1 = c(1, Inf)), list(x2 = 1:2)), "finite values")
  expect_error(pd2D_find(model, data[0, ], list(x1 = 1:2), list(x2 = 1:2)), "at least one row")
  expect_error(pd2D_find(model, data[setdiff(names(data), "x2")], list(x1 = 1:2), list(x2 = 1:2)),
               "missing predictor columns: x2")
  expect_error(pd2D_find(list(), data, list(x1 = 1:2), list(x2 = 1:2)), "created by fit_model")
})

test_that("pd2D_mostprobable() finds the most probable type and its probability", {
  cdm_data <- make_model_test_data()
  model <- fit_pd_test_model(cdm_data)
  pd <- pd2D_find(model, cdm_data$env, pred1 = list(x1 = c(0.1, 0.5, 0.9)), pred2 = list(x2 = c(-1, 1)))

  most_probable <- pd2D_mostprobable(pd)

  expect_s3_class(most_probable, "cdm_pd2d_mostprobable")
  expect_identical(dim(most_probable$class), c(3L, 2L))
  for (i in 1:3) {
    for (j in 1:2) {
      expect_identical(most_probable$class[i, j], names(which.max(pd$probabilities[i, j, ])))
      expect_equal(most_probable$p_max[i, j], max(pd$probabilities[i, j, ]))
    }
  }
  # The type is determined by x1: A below 1/3, B in the middle, C above 2/3.
  expect_identical(most_probable$class[, 1], c("A", "B", "C"))
})

test_that("pd2D_plot() draws the map with contours, points, and the mask", {
  cdm_data <- make_model_test_data()
  model <- fit_pd_test_model(cdm_data)
  pd <- pd2D_find(
    model, cdm_data$env,
    pred1 = list(x1 = seq(0, 1, length.out = 15)), pred2 = list(x2 = seq(-2, 2, length.out = 12))
  )
  most_probable <- pd2D_mostprobable(pd)

  plain <- pd2D_plot(most_probable, title = "All samples")
  expect_s3_class(plain, "ggplot")
  expect_identical(plain$labels$title, "All samples")

  with_points <- pd2D_plot(most_probable, points = cdm_data$env)
  layer_types <- vapply(with_points$layers, function(layer) class(layer$geom)[1], character(1))
  expect_true(all(c("GeomRaster", "GeomPolygon", "GeomContour", "GeomPoint") %in% layer_types))
  # The mask is one polygon (the plot area) with the hull of the points as a
  # hole; without a single group ggplot2 fills the hull as a second polygon.
  mask_layer <- with_points$layers[[which(layer_types == "GeomPolygon")]]
  expect_false(is.null(mask_layer$mapping$group))
  expect_false(is.null(mask_layer$mapping$subgroup))
  without_mask <- pd2D_plot(most_probable, points = cdm_data$env, mask = FALSE)
  expect_false("GeomPolygon" %in% vapply(without_mask$layers, function(layer) class(layer$geom)[1], character(1)))
  expect_error(pd2D_plot(most_probable, points = cdm_data$env[c("x1", "x2")]), "columns")
  expect_error(pd2D_plot(pd), "pd2D_mostprobable")

  irregular <- pd2D_mostprobable(pd2D_find(
    model, cdm_data$env, pred1 = list(x1 = c(0, 0.1, 0.5, 1)), pred2 = list(x2 = c(-1, 0, 1))
  ))
  expect_error(pd2D_plot(irregular), "equally spaced grids")
})

test_that("pd2D_find() gives the same results sequentially and in parallel", {
  skip_on_cran()
  cdm_data <- make_model_test_data()
  model <- fit_pd_test_model(cdm_data)
  grids <- list(pred1 = list(x1 = c(0.2, 0.4, 0.6, 0.8)), pred2 = list(x2 = c(-1, 0, 1)))

  sequential <- pd2D_find(model, cdm_data$env, grids$pred1, grids$pred2)
  local_two_workers()
  parallel <- pd2D_find(model, cdm_data$env, grids$pred1, grids$pred2)

  expect_identical(parallel$probabilities, sequential$probabilities)
})
