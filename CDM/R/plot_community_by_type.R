#' Plot community composition by community type
#'
#' Splits the samples in a community composition object according to
#' `cdm_data$env$community_type`, draws one community-deviation box-plot
#' panel for each observed type, and combines the panels into one figure. All
#' panels preserve the group order from `composition_data$deviation` row names.
#'
#' @param cdm_data An object created by `prepare_data()` with a
#'   `community_type` column in `cdm_data$env`.
#' @param composition_data An object returned by `community_deviation_indexes()` for
#'   the same `cdm_data` object.
#'
#' @return A patchwork plot object. Its component plots and community-type
#'   names are stored in the `plots` and `community_types` attributes.
#'
#' @export
plot_community_by_type <- function(cdm_data, composition_data) {
  validate_plot_community_by_type_input(cdm_data, composition_data)

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required to combine community-type plots.", call. = FALSE)
  }

  community_types <- community_type_levels(cdm_data$env$community_type)
  community_type_plots <- lapply(
    community_types,
    function(type_name) {
      type_samples <- cdm_data$sample_names[
        !is.na(cdm_data$env$community_type) &
          as.character(cdm_data$env$community_type) == type_name
      ]
      type_composition <- composition_data
      type_composition$deviation <- composition_data$deviation[
        ,
        type_samples,
        drop = FALSE
      ]

      plot_community_composition(type_composition) +
        ggplot2::labs(
          title = paste0(type_name, " (n = ", length(type_samples), ")")
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 10),
          axis.text.x = ggplot2::element_text(size = 7, angle = 45, hjust = 1)
        )
    }
  )
  names(community_type_plots) <- community_types

  result <- patchwork::wrap_plots(
    community_type_plots,
    ncol = ceiling(sqrt(length(community_type_plots)))
  )
  attr(result, "plots") <- community_type_plots
  attr(result, "community_types") <- community_types
  result
}

validate_plot_community_by_type_input <- function(cdm_data, composition_data) {
  if (!inherits(cdm_data, "cdm_data")) {
    stop("cdm_data must be an object created by prepare_data().", call. = FALSE)
  }
  if (!is.data.frame(cdm_data$env)) {
    stop("cdm_data$env must be a data frame.", call. = FALSE)
  }
  if (!"community_type" %in% names(cdm_data$env)) {
    stop(
      "cdm_data$env must contain community_type. Run update_community_type() first.",
      call. = FALSE
    )
  }
  if (!is.character(cdm_data$sample_names) ||
      length(cdm_data$sample_names) != nrow(cdm_data$env) ||
      any(is.na(cdm_data$sample_names) | cdm_data$sample_names == "") ||
      anyDuplicated(cdm_data$sample_names)) {
    stop("cdm_data$sample_names must contain one unique ID per env row.", call. = FALSE)
  }
  if (!identical(rownames(cdm_data$env), cdm_data$sample_names)) {
    stop(
      "cdm_data$env row names must match cdm_data$sample_names in the same order.",
      call. = FALSE
    )
  }

  validate_plot_community_composition_input(composition_data)
  composition_samples <- colnames(composition_data$deviation)
  missing_samples <- setdiff(cdm_data$sample_names, composition_samples)
  extra_samples <- setdiff(composition_samples, cdm_data$sample_names)
  if (length(composition_samples) != length(cdm_data$sample_names) ||
      length(missing_samples) > 0L ||
      length(extra_samples) > 0L) {
    stop(
      "composition_data deviation columns must match cdm_data$sample_names. Missing: ",
      paste(missing_samples, collapse = ", "),
      "; extra: ",
      paste(extra_samples, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

community_type_levels <- function(community_type) {
  observed_types <- as.character(community_type)
  observed_types <- observed_types[!is.na(observed_types) & observed_types != ""]

  if (is.factor(community_type)) {
    community_types <- levels(community_type)
    community_types <- community_types[community_types %in% observed_types]
  } else {
    community_types <- sort(unique(observed_types))
  }
  if (length(community_types) == 0L) {
    stop(
      "cdm_data$env$community_type must contain at least one non-missing value.",
      call. = FALSE
    )
  }

  community_types
}
