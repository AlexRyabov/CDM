#' Plot community composition
#'
#' Draws one box plot for each row of a community deviation table. Row names
#' identify the plotted groups and their plotting order; columns contain
#' observations from samples.
#'
#' @param composition An object returned by `community_deviation_indexes()`.
#'
#' @return A `ggplot` object. The long-format plotting data are stored in
#'   `plot$data` with columns `group`, `sample`, and `value`.
#'
#' @export
plot_community_composition <- function(composition) {
  validate_plot_community_composition_input(composition)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot community composition.", call. = FALSE)
  }

  composition_matrix <- as.matrix(composition$deviation)
  group_order <- rownames(composition_matrix)
  plot_data <- data.frame(
    group = factor(
      rep(group_order, each = ncol(composition_matrix)),
      levels = group_order
    ),
    sample = rep(colnames(composition_matrix), times = nrow(composition_matrix)),
    value = as.vector(t(composition_matrix)),
    stringsAsFactors = FALSE
  )

  finite_values <- is.finite(plot_data$value)
  if (any(!finite_values)) {
    warning(
      sum(!finite_values),
      " non-finite community deviation values were omitted from the plot.",
      call. = FALSE
    )
    plot_data <- plot_data[finite_values, , drop = FALSE]
  }
  if (nrow(plot_data) == 0L) {
    stop("No finite community deviation values are available to plot.", call. = FALSE)
  }

  fill_colors <- colors_fresh(
    factor(group_order, levels = group_order),
    order = "convenient"
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = group, y = value, fill = group)
  ) +
    ggplot2::geom_boxplot(
      width = 0.65,
      color = "grey20",
      linewidth = 0.35
    ) +
    ggplot2::scale_fill_manual(values = fill_colors, guide = "none") +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = NULL, y = "Community deviation") +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )
}

# Columns of plot_data used in ggplot2::aes().
utils::globalVariables(c("group", "value"))

validate_plot_community_composition_input <- function(composition) {
  if (!inherits(composition, "cdm_community_deviation_indexes")) {
    stop(
      "composition must be an object returned by community_deviation_indexes().",
      call. = FALSE
    )
  }
  if (!is.list(composition) || !all(c("reference", "deviation") %in% names(composition))) {
    stop("composition must contain reference and deviation.", call. = FALSE)
  }
  if (!is.numeric(composition$reference) ||
      is.null(names(composition$reference)) ||
      any(is.na(composition$reference)) ||
      any(!is.finite(composition$reference))) {
    stop("composition$reference must be a named finite numeric vector.", call. = FALSE)
  }
  if (!is.data.frame(composition$deviation) && !is.matrix(composition$deviation)) {
    stop("composition$deviation must be a data frame or matrix.", call. = FALSE)
  }
  if (nrow(composition$deviation) < 1L || ncol(composition$deviation) < 1L) {
    stop("composition$deviation must contain at least one group and one sample.", call. = FALSE)
  }

  group_names <- rownames(composition$deviation)
  sample_names <- colnames(composition$deviation)
  if (is.null(group_names) ||
      any(is.na(group_names) | group_names == "") ||
      anyDuplicated(group_names)) {
    stop("composition$deviation must have unique non-empty row names.", call. = FALSE)
  }
  if (is.null(sample_names) ||
      any(is.na(sample_names) | sample_names == "") ||
      anyDuplicated(sample_names)) {
    stop("composition$deviation must have unique non-empty sample column names.", call. = FALSE)
  }
  if (!identical(names(composition$reference), group_names)) {
    stop(
      "Names and order of composition$reference must match composition$deviation row names.",
      call. = FALSE
    )
  }

  numeric_columns <- if (is.data.frame(composition$deviation)) {
    vapply(composition$deviation, is.numeric, logical(1))
  } else {
    is.numeric(composition$deviation)
  }
  if (!all(numeric_columns)) {
    stop("All values in composition$deviation must be numeric.", call. = FALSE)
  }

  invisible(TRUE)
}
