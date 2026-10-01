#' Calculate community deviation indexes
#'
#' Aggregates taxon contributions into groups and returns either their absolute
#' sample values or their log10 deviations from a dataset-wide reference.
#'
#' For sample `j` and taxonomic group `k`, the grouped contribution `S_kj` is
#' the sum of relative abundances when `measure = "abundance"`. When
#' `measure = "richness"`, it is the number of taxa in the group with positive
#' abundance. Taxa are assigned to groups using a column of
#' `cdm_data$species_names` selected by `group_by`.
#'
#' The reference `S*_k` is the mean or median of `S_kj` across all samples,
#' including samples with zero contribution. With `scale = "relative"`, the
#' deviation index is `D_kj = log10(S_kj / S*_k)`. Positive values indicate
#' contributions above the reference, negative values indicate contributions
#' below it, and values of 1 and -1 represent ten-fold enrichment and depletion.
#'
#' To make relative indexes finite when `S_kj` is zero, zeros are replaced by
#' one-tenth of the smallest positive value in the complete grouped matrix.
#' Abundance compositions are then renormalized to sum to one within samples.
#' A group with a zero reference remains undefined and is returned as `NA`.
#'
#' @param cdm_data An object created by `prepare_data()`.
#' @param group_by Name of the grouping column in `cdm_data$species_names`, for
#'   example `"Phylum"`.
#' @param measure Group contribution to calculate: summed relative abundance or
#'   the number of taxa with positive abundance.
#' @param scale Output scale. `"relative"` returns log10 deviation indexes;
#'   `"absolute"` returns the grouped contributions `S_kj`.
#' @param reference Dataset-wide reference function, either `"median"` or
#'   `"mean"`.
#' @param max_groups Maximum number of groups to return. Groups are selected by
#'   decreasing reference value.
#'
#' @return An object of class `cdm_community_deviation_indexes`, containing:
#' \describe{
#'   \item{reference}{A numeric vector whose names identify the taxonomic
#'     groups. Groups are sorted by decreasing reference value.}
#'   \item{deviation}{A data frame with taxonomic groups in rows and samples in
#'     columns. Row names identify the groups and follow the same decreasing
#'     reference order; column names are sample IDs. On the absolute scale the
#'     table contains `S_kj`; on the relative scale it contains `D_kj`.}
#' }
#'
#' @export
community_deviation_indexes <- function(cdm_data,
                                        group_by,
                                        measure = c("abundance", "richness"),
                                        scale = c("relative", "absolute"),
                                        reference = c("median", "mean"),
                                        max_groups = 10L) {
  measure <- match.arg(measure)
  scale <- match.arg(scale)
  reference <- match.arg(reference)
  validate_community_deviation_input(cdm_data, group_by, max_groups)

  grouped_matrix <- as.matrix(cdm_data$relative_abundance)
  if (identical(measure, "richness")) {
    grouped_matrix <- 1L * (grouped_matrix > 0)
  }
  grouped_matrix <- rowsum(
    grouped_matrix,
    group = as.character(cdm_data$species_names[[group_by]]),
    reorder = FALSE
  )

  group_reference <- if (identical(reference, "mean")) {
    rowMeans(grouped_matrix)
  } else {
    apply(grouped_matrix, 1L, stats::median)
  }
  result_order <- utils::head(
    order(group_reference, decreasing = TRUE),
    max_groups
  )
  group_reference <- group_reference[result_order]

  if (identical(scale, "absolute")) {
    deviation_matrix <- grouped_matrix[result_order, , drop = FALSE]
  } else {
    adjusted_matrix <- grouped_matrix
    positive_values <- adjusted_matrix[adjusted_matrix > 0]
    if (length(positive_values) > 0L) {
      pseudocount <- min(positive_values) / 10
      adjusted_matrix[adjusted_matrix == 0] <- pseudocount

      if (identical(measure, "abundance")) {
        adjusted_matrix <- sweep(
          adjusted_matrix,
          2L,
          colSums(adjusted_matrix),
          FUN = "/"
        )
      }
    } else {
      adjusted_matrix[] <- NA_real_
    }

    deviation_matrix <- log10(sweep(
      adjusted_matrix[result_order, , drop = FALSE],
      1L,
      group_reference,
      FUN = "/"
    ))
    undefined_groups <- names(group_reference)[group_reference == 0]
    if (length(undefined_groups) > 0L) {
      deviation_matrix[undefined_groups, ] <- NA_real_
      warning(
        "Relative deviation is undefined for groups with zero reference: ",
        paste(undefined_groups, collapse = ", "),
        call. = FALSE
      )
    }
  }

  result <- list(
    reference = group_reference,
    deviation = as.data.frame(deviation_matrix, check.names = FALSE)
  )
  class(result) <- "cdm_community_deviation_indexes"
  result
}

validate_community_deviation_input <- function(cdm_data, group_by, max_groups) {
  if (!inherits(cdm_data, "cdm_data")) {
    stop("cdm_data must be an object created by prepare_data().", call. = FALSE)
  }

  required_fields <- c("species_names", "relative_abundance", "sample_names")
  missing_fields <- setdiff(required_fields, names(cdm_data))
  if (length(missing_fields) > 0L) {
    stop(
      "cdm_data is missing fields: ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.character(group_by) ||
      length(group_by) != 1L ||
      is.na(group_by) ||
      group_by == "") {
    stop("group_by must be a single non-empty column name.", call. = FALSE)
  }
  if (length(max_groups) != 1L ||
      is.na(max_groups) ||
      !is.numeric(max_groups) ||
      !is.finite(max_groups) ||
      max_groups < 1 ||
      max_groups != as.integer(max_groups)) {
    stop("max_groups must be a single positive integer.", call. = FALSE)
  }

  species_names <- cdm_data$species_names
  if (!is.data.frame(species_names)) {
    stop("cdm_data$species_names must be a data frame.", call. = FALSE)
  }
  if (!group_by %in% names(species_names)) {
    stop("group_by column not found in cdm_data$species_names: ", group_by, call. = FALSE)
  }

  group_values <- as.character(species_names[[group_by]])
  invalid_groups <- is.na(group_values) | group_values == ""
  if (any(invalid_groups)) {
    stop(
      "group_by column contains missing or empty values at rows: ",
      paste(which(invalid_groups), collapse = ", "),
      call. = FALSE
    )
  }

  relative_abundance <- as.data.frame(cdm_data$relative_abundance, check.names = FALSE)
  if (nrow(relative_abundance) != nrow(species_names)) {
    stop(
      "cdm_data$relative_abundance must have one row for each row in cdm_data$species_names.",
      call. = FALSE
    )
  }

  sample_names <- cdm_data$sample_names
  if (!is.character(sample_names) ||
      length(sample_names) != ncol(relative_abundance) ||
      any(is.na(sample_names) | sample_names == "") ||
      anyDuplicated(sample_names)) {
    stop(
      "cdm_data$sample_names must be unique non-empty names for relative_abundance columns.",
      call. = FALSE
    )
  }
  if (!identical(names(relative_abundance), sample_names)) {
    stop(
      "cdm_data$relative_abundance column names must match cdm_data$sample_names.",
      call. = FALSE
    )
  }

  numeric_columns <- vapply(relative_abundance, is.numeric, logical(1))
  if (!all(numeric_columns)) {
    stop("All cdm_data$relative_abundance columns must be numeric.", call. = FALSE)
  }

  abundance_matrix <- as.matrix(relative_abundance)
  if (anyNA(abundance_matrix)) {
    stop("cdm_data$relative_abundance must not contain missing values.", call. = FALSE)
  }
  if (any(!is.finite(abundance_matrix))) {
    stop("cdm_data$relative_abundance must contain only finite values.", call. = FALSE)
  }
  if (any(abundance_matrix < 0)) {
    stop("cdm_data$relative_abundance must not contain negative values.", call. = FALSE)
  }

  invisible(TRUE)
}
