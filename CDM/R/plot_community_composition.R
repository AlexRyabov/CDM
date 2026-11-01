#' Plot community composition
#'
#' Draws one box plot per taxonomic group of a community deviation table,
#' with one value per sample. The groups keep the order of
#' `composition$reference`.
#'
#' @param composition An object returned by [community_deviation_indexes()].
#'
#' @return A `ggplot` object. The plotted data are in `plot$data` with the
#'   columns `group`, `sample`, and `value`.
#'
#' @export
plot_community_composition <- function(composition) {
  validate_plot_community_composition_input(composition)

  group_order <- names(composition$reference)
  deviation <- composition$deviation
  sample_labels <- if ("sample_id" %in% names(deviation)) deviation$sample_id else as.character(seq_len(nrow(deviation)))
  values <- as.matrix(deviation[group_order])
  plot_data <- data.frame(
    group = factor(rep(group_order, each = nrow(values)), levels = group_order),
    sample = rep(sample_labels, times = ncol(values)),
    value = as.vector(values),
    stringsAsFactors = FALSE
  )

  finite_values <- is.finite(plot_data$value)
  if (any(!finite_values)) {
    warning(
      sum(!finite_values),
      " non-finite community deviation values were left out of the plot.",
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
  if (!inherits(composition, "cdm_community_deviation_indexes") ||
      !all(c("reference", "deviation") %in% names(composition))) {
    stop("composition must be an object returned by community_deviation_indexes().", call. = FALSE)
  }
  reference <- composition$reference
  if (!is.numeric(reference) || length(reference) == 0L || is.null(names(reference)) ||
      anyNA(reference) || any(!is.finite(reference))) {
    stop("composition$reference must be a named numeric vector of finite values.", call. = FALSE)
  }
  deviation <- composition$deviation
  if (!is.data.frame(deviation) || nrow(deviation) == 0L) {
    stop("composition$deviation must be a data frame with at least one sample.", call. = FALSE)
  }
  if (!identical(setdiff(names(deviation), "sample_id"), names(reference))) {
    stop("The group columns of composition$deviation must match names(composition$reference).", call. = FALSE)
  }
  if (!all(vapply(deviation[names(reference)], is.numeric, logical(1)))) {
    stop("All group columns of composition$deviation must be numeric.", call. = FALSE)
  }
  invisible(TRUE)
}
