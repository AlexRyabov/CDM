#' Plot CDM map coordinates
#'
#' Creates a scatter plot of map coordinates colored by a map coordinate, an
#' environmental variable, or a user-supplied vector.
#'
#' Environmental variables are matched to map coordinates by sample ID.
#'
#' @param cdm_map An object created by `map_comm()`.
#' @param cdm_data An object created by `prepare_data()`. It must contain all
#'   samples of `cdm_map`.
#' @param x Name of the map coordinate used on the x-axis.
#' @param y Name of the map coordinate used on the y-axis.
#' @param color Optional color variable. Can be a column name in `cdm_map$map`,
#'   a column name in `cdm_data$env`, or a vector with one value per sample.
#'   A named vector is matched to samples by name; an unnamed vector must
#'   follow the row order of `cdm_map$map`.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_map <- function(cdm_map,
                     cdm_data,
                     x = "dm_1",
                     y = "dm_2",
                     color = NULL) {
  if (!inherits(cdm_map, "cdm_map")) {
    stop("cdm_map must be an object created by map_comm().", call. = FALSE)
  }
  if (!inherits(cdm_data, "cdm_data")) {
    stop("cdm_data must be an object created by prepare_data().", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot maps.", call. = FALSE)
  }
  if (!requireNamespace("viridisLite", quietly = TRUE)) {
    stop("Package 'viridisLite' is required to plot maps.", call. = FALSE)
  }

  if (!all(c(x, y) %in% names(cdm_map$map))) {
    stop("x and y must be column names in cdm_map$map.", call. = FALSE)
  }

  missing_samples <- setdiff(rownames(cdm_map$map), rownames(cdm_data$env))
  if (length(missing_samples) > 0L) {
    stop(
      "cdm_data$env is missing samples from cdm_map: ",
      paste(missing_samples, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(color)) {
    plot_data <- data.frame(
      x_value = cdm_map$map[[x]],
      y_value = cdm_map$map[[y]]
    )

    return(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = x_value, y = y_value)) +
        ggplot2::geom_point(size = 2.2, alpha = 0.9) +
        ggplot2::labs(x = x, y = y) +
        ggplot2::theme_classic(base_size = 12)
    )
  }

  color_data <- resolve_plot_color(color, cdm_map, cdm_data)
  plot_data <- data.frame(
    x_value = cdm_map$map[[x]],
    y_value = cdm_map$map[[y]],
    color_value = color_data$values
  )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = x_value, y = y_value, color = color_value)
  ) +
    ggplot2::geom_point(size = 2.2, alpha = 0.9) +
    ggplot2::labs(
      x = x,
      y = y,
      title = color_data$name,
      color = color_data$name
    ) +
    ggplot2::theme_classic(base_size = 12)

  if (is.numeric(plot_data$color_value)) {
    p <- p + ggplot2::scale_color_gradientn(
      colors = viridisLite::turbo(100),
      name = color_data$name
    )
  }

  p
}

# Columns of plot_data used in ggplot2::aes().
utils::globalVariables(c("x_value", "y_value", "color_value"))

resolve_plot_color <- function(color, cdm_map, cdm_data) {
  sample_ids <- rownames(cdm_map$map)

  if (is.character(color) && length(color) == 1L) {
    if (color %in% names(cdm_map$map)) {
      return(list(values = cdm_map$map[[color]], name = color))
    }
    if (color %in% names(cdm_data$env)) {
      env_rows <- match(sample_ids, rownames(cdm_data$env))
      return(list(values = cdm_data$env[[color]][env_rows], name = color))
    }
    stop("color must be a column name in cdm_map$map or cdm_data$env.", call. = FALSE)
  }

  if (length(color) != length(sample_ids)) {
    stop("color vector must have one value for each sample.", call. = FALSE)
  }
  if (!is.null(names(color))) {
    if (!setequal(names(color), sample_ids) || anyDuplicated(names(color))) {
      stop("Names of the color vector must be the sample IDs of cdm_map.", call. = FALSE)
    }
    color <- color[sample_ids]
  }

  list(values = unname(color), name = "color")
}
