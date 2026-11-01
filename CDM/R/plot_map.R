#' Plot CDM map coordinates
#'
#' Draws the samples at their map coordinates, colored by a map coordinate,
#' a column of `data`, or a vector.
#'
#' @param cdm_map An object created by [map_comm()].
#' @param data `NULL` or a data frame or tibble with one row per sample,
#'   usually `cdm_data$env`, with the variables used for colors. It must
#'   contain every sample of the map; extra rows are ignored. Samples are
#'   matched by `sample_id` when the map and `data` both have it, otherwise
#'   by order.
#' @param x Name of the map coordinate used on the x-axis.
#' @param y Name of the map coordinate used on the y-axis.
#' @param color `NULL`, the name of a column of `cdm_map$map` or of `data`,
#'   or a vector with one value per row of `cdm_map$map`, in the same order.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_map <- function(cdm_map,
                     data = NULL,
                     x = "dm_1",
                     y = "dm_2",
                     color = NULL) {
  if (!inherits(cdm_map, "cdm_map")) {
    stop("cdm_map must be an object created by map_comm().", call. = FALSE)
  }
  if (!is.null(data) && !is.data.frame(data)) {
    stop("data must be a data frame with one row per sample, or NULL.", call. = FALSE)
  }
  map_data <- cdm_map$map
  if (!all(c(x, y) %in% setdiff(names(map_data), "sample_id"))) {
    stop("x and y must be coordinate names in cdm_map$map, such as \"dm_1\".", call. = FALSE)
  }

  plot_data <- data.frame(x_value = map_data[[x]], y_value = map_data[[y]])
  if (is.null(color)) {
    return(
      ggplot2::ggplot(plot_data, ggplot2::aes(x = x_value, y = y_value)) +
        ggplot2::geom_point(size = 2.2, alpha = 0.9) +
        ggplot2::labs(x = x, y = y) +
        ggplot2::theme_classic(base_size = 12)
    )
  }

  # A single string is always a column name; other values are colors per sample.
  if (is.character(color) && length(color) == 1L) {
    color_name <- color
    if (color %in% names(map_data)) {
      plot_data$color_value <- map_data[[color]]
    } else if (!is.null(data) && color %in% names(data)) {
      data <- as.data.frame(data, check.names = FALSE)
      data_rows <- match_sample_rows(map_data, data, "the map", "data")
      plot_data$color_value <- data[[color]][data_rows]
    } else {
      stop("color must be a column name of cdm_map$map or of data.", call. = FALSE)
    }
  } else {
    if (length(color) != nrow(map_data)) {
      stop("A color vector must have one value per sample of the map (", nrow(map_data), ").", call. = FALSE)
    }
    color_name <- "color"
    plot_data$color_value <- unname(color)
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = x_value, y = y_value, color = color_value)
  ) +
    ggplot2::geom_point(size = 2.2, alpha = 0.9) +
    ggplot2::labs(x = x, y = y, title = color_name, color = color_name) +
    ggplot2::theme_classic(base_size = 12)

  if (is.numeric(plot_data$color_value)) {
    p <- p + ggplot2::scale_color_gradientn(
      colors = viridisLite::turbo(100),
      name = color_name
    )
  }

  p
}

# Columns of plot_data used in ggplot2::aes().
utils::globalVariables(c("x_value", "y_value", "color_value"))
