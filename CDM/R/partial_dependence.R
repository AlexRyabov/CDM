#' Two-dimensional partial dependence of community-type probabilities
#'
#' Computes the partial dependence of the probability of every community type
#' on two predictors of a fitted model. For every point of the grid
#' `pred1 x pred2`, both predictors are set to the grid values in all rows of
#' `data`, the class probabilities are predicted with the model, and they are
#' averaged over the rows. The other predictors keep their observed values, so
#' the result is averaged over the samples in `data` (Friedman's partial
#' dependence). Use `data` to restrict the averaging, for example to the
#' samples of one depth layer.
#'
#' The grid points are computed with `future.apply::future_lapply()`, one task
#' per value of `pred1`; they run in parallel when a parallel
#' `future::plan()` is set. The number of predictions is
#' `length(pred1[[1]]) * length(pred2[[1]]) * nrow(data)`.
#'
#' @param model A `cdm_model` object created by [fit_model()].
#' @param data A data frame with the samples to average over; it must contain
#'   all predictors of the model (`model$predictors`).
#' @param pred1,pred2 Named lists with one element each: the name is a numeric
#'   predictor of the model, the element the grid of its values, for example
#'   `list(temp_imputed = seq(-2, 8, length.out = 100))`. [pd2D_plot()] needs
#'   increasing, equally spaced grids such as these.
#'
#' @return An object of class `cdm_pd2d`, a list with the predictor names
#'   `pred1` and `pred2`, the grids `x` (values of `pred1`) and `y` (values of
#'   `pred2`), the `classes`, the array `probabilities` with dimensions
#'   `length(x) x length(y) x length(classes)`, the number of averaged samples
#'   `n_data`, their IDs `data_ids` (row names of `data`), and the provenance
#'   of the model (`model_provenance`).
#'
#' @export
pd2D_find <- function(model, data, pred1, pred2) {
  if (!inherits(model, "cdm_model")) {
    stop("model must be created by fit_model().", call. = FALSE)
  }
  stop_if_kernlab(model, "fit_model")
  if (!is.data.frame(data) || nrow(data) == 0L) {
    stop("data must be a data frame with at least one row.", call. = FALSE)
  }
  grids <- list(pred1 = pred1, pred2 = pred2)
  for (argument in names(grids)) {
    grid <- grids[[argument]]
    if (!is.list(grid) || length(grid) != 1L || is.null(names(grid)) || !nzchar(names(grid))) {
      stop(argument, " must be a named list with one grid, for example list(temp = 1:10).", call. = FALSE)
    }
    if (!names(grid) %in% model$predictors) {
      stop(
        argument, " '", names(grid), "' is not a predictor of the model: ",
        paste(model$predictors, collapse = ", "), ".",
        call. = FALSE
      )
    }
    if (!identical(model$predictor_info[[names(grid)]]$type, "numeric") ||
        !is.numeric(grid[[1L]]) || length(grid[[1L]]) == 0L || any(!is.finite(grid[[1L]]))) {
      stop(argument, " must be a numeric predictor with a non-empty grid of finite values.", call. = FALSE)
    }
  }
  name1 <- names(pred1)
  name2 <- names(pred2)
  if (identical(name1, name2)) {
    stop("pred1 and pred2 must be different predictors.", call. = FALSE)
  }
  grid1 <- pred1[[1L]]
  grid2 <- pred2[[1L]]

  base <- prepare_new_predictors(model, data)
  rownames(base) <- NULL
  n_data <- nrow(base)
  fitted <- list(recipe = model$recipe, fit = model$fit, classes = model$model_classes)

  # One task per value of pred1: all values of pred2 for all rows of data,
  # averaged over the rows. Result: length(grid2) x number of classes.
  averages <- future.apply::future_lapply(seq_along(grid1), function(i) {
    new_data <- base[rep(seq_len(n_data), times = length(grid2)), , drop = FALSE]
    new_data[[name1]] <- grid1[i]
    new_data[[name2]] <- rep(grid2, each = n_data)
    rownames(new_data) <- NULL
    probabilities <- predict_classes(fitted, new_data, model$classes, class = FALSE)$probabilities
    rowsum(probabilities, rep(seq_along(grid2), each = n_data), reorder = TRUE) / n_data
  })

  probabilities <- array(
    NA_real_,
    dim = c(length(grid1), length(grid2), length(model$classes)),
    dimnames = list(NULL, NULL, model$classes)
  )
  for (i in seq_along(grid1)) {
    probabilities[i, , ] <- averages[[i]]
  }

  result <- list(
    pred1 = name1,
    pred2 = name2,
    x = grid1,
    y = grid2,
    classes = model$classes,
    probabilities = probabilities,
    n_data = n_data,
    data_ids = rownames(data),
    model_provenance = model$provenance
  )
  class(result) <- "cdm_pd2d"
  result
}

#' Most probable community type of a partial dependence
#'
#' Finds, for every grid point of a two-dimensional partial dependence, the
#' community type with the highest partial-dependence probability and this
#' probability.
#'
#' @param pd A `cdm_pd2d` object created by [pd2D_find()].
#'
#' @return An object of class `cdm_pd2d_mostprobable`, a list with `pred1`,
#'   `pred2`, the grids `x` and `y`, the `classes`, the matrix `class` (most
#'   probable type at every grid point, `length(x) x length(y)`), and the
#'   matrix `p_max` (its probability).
#'
#' @export
pd2D_mostprobable <- function(pd) {
  if (!inherits(pd, "cdm_pd2d")) {
    stop("pd must be created by pd2D_find().", call. = FALSE)
  }
  index <- apply(pd$probabilities, c(1L, 2L), which.max)
  result <- list(
    pred1 = pd$pred1,
    pred2 = pd$pred2,
    x = pd$x,
    y = pd$y,
    classes = pd$classes,
    class = matrix(pd$classes[index], nrow = length(pd$x)),
    p_max = apply(pd$probabilities, c(1L, 2L), max)
  )
  class(result) <- "cdm_pd2d_mostprobable"
  result
}

#' Plot the most probable community type over two predictors
#'
#' Draws a map of the most probable community type over the grid of a
#' two-dimensional partial dependence, with contour lines of its probability.
#' Observed samples can be added as points colored by their type; the area
#' outside the convex hull of these points is then shaded grey, because the
#' model has no data there.
#'
#' @param most_probable A `cdm_pd2d_mostprobable` object created by
#'   [pd2D_mostprobable()].
#' @param points Optional data frame of observed samples with the columns of
#'   both predictors and the column `class_column`.
#' @param class_column Name of the column of `points` with the community type.
#' @param mask If `TRUE` and `points` has at least three rows, the area outside
#'   the convex hull of the points is shaded.
#' @param levels Probability levels of the contour lines.
#' @param title Plot title.
#'
#' @return A `ggplot` object.
#'
#' @export
pd2D_plot <- function(most_probable,
                      points = NULL,
                      class_column = "community_type",
                      mask = TRUE,
                      levels = c(0.4, 0.6, 0.8),
                      title = NULL) {
  if (!inherits(most_probable, "cdm_pd2d_mostprobable")) {
    stop("most_probable must be created by pd2D_mostprobable().", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot partial dependence.", call. = FALSE)
  }
  mp <- most_probable
  # geom_raster() and contourLines() need increasing, equally spaced grids.
  for (grid in list(mp$x, mp$y)) {
    steps <- diff(grid)
    if (length(grid) < 2L || any(steps <= 0) || diff(range(steps)) > 1e-6 * diff(range(grid))) {
      stop(
        "pd2D_plot() needs increasing, equally spaced grids with at least two values, ",
        "for example seq(from, to, length.out = n).",
        call. = FALSE
      )
    }
  }
  classes <- mp$classes
  colors <- colors_fresh(factor(classes, levels = classes), order = "my")

  # expand.grid() varies x fastest, as the rows of the matrices.
  grid <- expand.grid(x = mp$x, y = mp$y)
  grid$class <- factor(as.vector(mp$class), levels = classes)
  grid$p_max <- as.vector(mp$p_max)

  plot <- ggplot2::ggplot(grid, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$class), show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = colors, limits = classes, drop = FALSE, name = "Community type")

  if (!is.null(points)) {
    missing_columns <- setdiff(c(mp$pred1, mp$pred2, class_column), names(points))
    if (!is.data.frame(points) || length(missing_columns) > 0L) {
      stop("points must be a data frame with the columns: ", paste(c(mp$pred1, mp$pred2, class_column), collapse = ", "), call. = FALSE)
    }
    observed <- data.frame(
      x = points[[mp$pred1]],
      y = points[[mp$pred2]],
      class = factor(as.character(points[[class_column]]), levels = classes)
    )
    if (mask && nrow(observed) >= 3L) {
      hull <- observed[grDevices::chull(observed$x, observed$y), c("x", "y")]
      outer <- data.frame(x = range(mp$x)[c(1L, 2L, 2L, 1L)], y = range(mp$y)[c(1L, 1L, 2L, 2L)])
      shade <- rbind(cbind(outer, part = "outer"), cbind(hull, part = "hull"))
      plot <- plot + ggplot2::geom_polygon(
        data = shade,
        ggplot2::aes(x = .data$x, y = .data$y, group = 1L, subgroup = .data$part),
        fill = "grey70", alpha = 0.5, colour = NA, show.legend = FALSE
      )
    }
  }

  plot <- plot + ggplot2::geom_contour(
    ggplot2::aes(z = .data$p_max),
    breaks = levels, colour = "black", linewidth = 0.4
  )
  # Label every contour line at its middle point.
  lines <- grDevices::contourLines(mp$x, mp$y, mp$p_max, levels = levels)
  if (length(lines) > 0L) {
    labels <- do.call(rbind, lapply(lines, function(line) {
      middle <- ceiling(length(line$x) / 2)
      data.frame(x = line$x[middle], y = line$y[middle], label = format(line$level))
    }))
    plot <- plot + ggplot2::geom_label(
      data = labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 2.5, linewidth = 0, label.padding = ggplot2::unit(0.1, "lines"), fill = "white"
    )
  }

  if (!is.null(points)) {
    plot <- plot + ggplot2::geom_point(
      data = observed,
      ggplot2::aes(x = .data$x, y = .data$y, fill = .data$class),
      shape = 21, colour = "black", size = 1.8, show.legend = FALSE
    )
  }

  plot +
    ggplot2::coord_cartesian(xlim = range(mp$x), ylim = range(mp$y), expand = FALSE) +
    ggplot2::labs(x = mp$pred1, y = mp$pred2, title = title) +
    ggplot2::theme_classic(base_size = 11)
}
