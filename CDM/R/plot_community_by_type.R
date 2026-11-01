#' Plot community composition by community type
#'
#' Splits the samples of a community deviation table by their community type,
#' draws one box plot panel per type with [plot_community_composition()], and
#' combines the panels into one figure. All panels keep the group order of
#' `composition$reference`.
#'
#' @param composition An object returned by [community_deviation_indexes()].
#' @param data A data frame or tibble with one row per sample and the
#'   community type, usually `cdm_data$env`. It must contain every sample of
#'   `composition`; extra rows are ignored. Samples are matched by
#'   `sample_id` when both tables have it, otherwise by order.
#' @param community_type Column of `data` with the community types, given as
#'   a name or a string.
#'
#' @return A patchwork plot object. Its panels and community-type names are
#'   stored in the attributes `plots` and `community_types`.
#'
#' @export
plot_community_by_type <- function(composition, data, community_type = "community_type") {
  validate_plot_community_composition_input(composition)
  if (!is.data.frame(data)) {
    stop("data must be a data frame with one row per sample, such as cdm_data$env.", call. = FALSE)
  }
  data <- as.data.frame(data, check.names = FALSE)
  type_column <- resolve_column_argument(rlang::enquo(community_type), data, "community_type")
  if (is.null(type_column)) {
    stop("community_type must name the column of data with the community types.", call. = FALSE)
  }
  data_rows <- match_sample_rows(composition$deviation, data, "the composition", "data")
  types <- data[[type_column]][data_rows]
  # Without sample IDs the panels label the samples by their row in composition.
  if (!"sample_id" %in% names(composition$deviation)) {
    composition$deviation <- data.frame(
      sample_id = as.character(seq_len(nrow(composition$deviation))),
      composition$deviation,
      check.names = FALSE
    )
  }
  type_levels <- if (is.factor(types)) levels(droplevels(types)) else sort(unique(as.character(types)))

  type_plots <- lapply(type_levels, function(type_name) {
    type_composition <- composition
    in_type <- as.character(types) == type_name
    type_composition$deviation <- composition$deviation[in_type, , drop = FALSE]
    plot_community_composition(type_composition) +
      ggplot2::labs(title = paste0(type_name, " (n = ", sum(in_type), ")")) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(size = 7, angle = 45, hjust = 1)
      )
  })
  names(type_plots) <- type_levels

  result <- patchwork::wrap_plots(type_plots, ncol = ceiling(sqrt(length(type_plots))))
  attr(result, "plots") <- type_plots
  attr(result, "community_types") <- type_levels
  result
}
