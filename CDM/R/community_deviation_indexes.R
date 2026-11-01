#' Calculate community deviation indexes
#'
#' Sums the contributions of the taxa in every taxonomic group and returns
#' either these sums per sample or their log10 deviations from a reference
#' over all samples.
#'
#' For sample `j` and taxonomic group `k`, the grouped contribution `S_jk` is
#' the sum of relative abundances when `measure = "abundance"`. When
#' `measure = "richness"`, it is the number of taxa in the group with positive
#' abundance. The group of every taxon is taken from the column `group_by` of
#' `taxa`.
#'
#' The reference `S*_k` is the mean or median of `S_jk` over all samples,
#' including samples with zero contribution. With `scale = "relative"`, the
#' deviation index is `D_jk = log10(S_jk / S*_k)`. Positive values mean a
#' contribution above the reference, negative values a contribution below it;
#' 1 and -1 mean ten times more and ten times less.
#'
#' To keep relative indexes finite when `S_jk` is zero, zeros are replaced by
#' one tenth of the smallest positive value of all grouped contributions.
#' Abundance compositions are then scaled again to sum to one in every
#' sample. A group with a zero reference has no defined deviation and gets
#' `NA`.
#'
#' @param x A data frame, tibble, or matrix of relative abundances with
#'   samples in rows and one column per taxon named by its `taxon_id`,
#'   usually `cdm_data$relative_abundance`. A column `sample_id` is not used
#'   in the calculation; it is copied to the result.
#' @param taxa A data frame with the column `taxon_id` and the column
#'   `group_by`, usually `cdm_data$taxa`. Every taxon column of `x` must be
#'   found in `taxa$taxon_id`.
#' @param group_by Name of the grouping column in `taxa`, for example
#'   `"Phylum"`.
#' @param measure Group contribution to calculate: summed relative abundance or
#'   the number of taxa with positive abundance.
#' @param scale `"relative"` returns log10 deviation indexes; `"absolute"`
#'   returns the grouped contributions `S_jk`.
#' @param reference Reference over all samples: `"median"` or `"mean"`.
#' @param max_groups Maximum number of groups to return. Groups with the
#'   largest reference values are kept.
#'
#' @return An object of class `cdm_community_deviation_indexes`, a list with:
#' \describe{
#'   \item{reference}{A numeric vector named by taxonomic group, sorted by
#'     decreasing reference value.}
#'   \item{deviation}{A data frame with one row per sample in the order of
#'     `x`: `sample_id` (if `x` has it), then one column per taxonomic group
#'     in the order of `reference`. On the absolute scale it contains `S_jk`;
#'     on the relative scale it contains `D_jk`.}
#' }
#'
#' @export
community_deviation_indexes <- function(x,
                                        taxa,
                                        group_by,
                                        measure = c("abundance", "richness"),
                                        scale = c("relative", "absolute"),
                                        reference = c("median", "mean"),
                                        max_groups = 10L) {
  measure <- match.arg(measure)
  scale <- match.arg(scale)
  reference <- match.arg(reference)
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop("x must be a data frame or a matrix with samples in rows and taxa in columns.", call. = FALSE)
  }
  if (!is.data.frame(taxa) || !"taxon_id" %in% names(taxa)) {
    stop("taxa must be a data frame with the column taxon_id, such as cdm_data$taxa.", call. = FALSE)
  }
  if (!is.character(group_by) || length(group_by) != 1L || is.na(group_by) || !nzchar(group_by)) {
    stop("group_by must be a single column name.", call. = FALSE)
  }
  if (!group_by %in% names(taxa)) {
    stop("taxa has no group_by column '", group_by, "'.", call. = FALSE)
  }
  if (length(max_groups) != 1L || !is.numeric(max_groups) || !is.finite(max_groups) ||
      max_groups < 1 || max_groups != round(max_groups)) {
    stop("max_groups must be a single positive integer.", call. = FALSE)
  }

  x <- as.data.frame(x, check.names = FALSE)
  sample_ids <- if ("sample_id" %in% names(x)) table_sample_ids(x) else NULL
  x$sample_id <- NULL
  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop("x must have at least one sample (row) and one taxon (column).", call. = FALSE)
  }
  taxon_rows <- match(names(x), id_text(taxa$taxon_id))
  if (anyNA(taxon_rows)) {
    missing_taxa <- names(x)[is.na(taxon_rows)]
    stop(
      "taxa$taxon_id is missing ", length(missing_taxa), " taxa of x: ",
      paste(utils::head(missing_taxa, 10L), collapse = ", "), if (length(missing_taxa) > 10L) ", ...", ".",
      call. = FALSE
    )
  }
  groups <- as.character(taxa[[group_by]][taxon_rows])
  if (anyNA(groups) || any(!nzchar(groups))) {
    stop("The group_by column of taxa has missing or empty values for some taxa of x.", call. = FALSE)
  }
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All columns of x except sample_id must be numeric.", call. = FALSE)
  }
  values <- as.matrix(x)
  if (anyNA(values) || any(!is.finite(values)) || any(values < 0)) {
    stop("x must contain only finite, non-negative values without NA.", call. = FALSE)
  }

  if (identical(measure, "richness")) {
    values <- 1 * (values > 0)
  }
  # Samples in rows, groups in columns.
  grouped <- t(rowsum(t(values), group = groups, reorder = FALSE))

  group_reference <- if (identical(reference, "mean")) colMeans(grouped) else apply(grouped, 2L, stats::median)
  kept <- utils::head(order(group_reference, decreasing = TRUE), max_groups)
  group_reference <- group_reference[kept]

  if (identical(scale, "absolute")) {
    deviation <- grouped[, kept, drop = FALSE]
  } else {
    adjusted <- grouped
    positive_values <- adjusted[adjusted > 0]
    if (length(positive_values) > 0L) {
      adjusted[adjusted == 0] <- min(positive_values) / 10
      if (identical(measure, "abundance")) {
        adjusted <- adjusted / rowSums(adjusted)
      }
    } else {
      adjusted[] <- NA_real_
    }
    deviation <- log10(sweep(adjusted[, kept, drop = FALSE], 2L, group_reference, FUN = "/"))
    undefined_groups <- names(group_reference)[group_reference == 0]
    if (length(undefined_groups) > 0L) {
      deviation[, undefined_groups] <- NA_real_
      warning(
        "The relative deviation is not defined for groups with zero reference: ",
        paste(undefined_groups, collapse = ", "),
        call. = FALSE
      )
    }
  }

  deviation <- as.data.frame(deviation, check.names = FALSE)
  rownames(deviation) <- NULL
  if (!is.null(sample_ids)) {
    deviation <- data.frame(sample_id = sample_ids, deviation, check.names = FALSE, stringsAsFactors = FALSE)
  }
  result <- list(reference = group_reference, deviation = deviation)
  class(result) <- "cdm_community_deviation_indexes"
  result
}
