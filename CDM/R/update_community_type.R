#' Add community types to CDM community data
#'
#' Adds per-sample community type assignments from a clustered map to
#' `cdm_data$env$community_type`. Samples are matched by their IDs, not by
#' position. The function does not save anything; save the returned object
#' explicitly with `saveRDS()` if needed.
#'
#' @param cdm_data An object created by `prepare_data()`.
#' @param clustered_map A `cdm_map` object returned by `cluster_comm()`.
#'
#' @return The updated `cdm_data` object.
#'
#' @export
update_community_type <- function(cdm_data, clustered_map) {
  if (!inherits(cdm_data, "cdm_data")) {
    stop("cdm_data must be an object created by prepare_data().", call. = FALSE)
  }
  if (!inherits(clustered_map, "cdm_map") || is.null(clustered_map$community_type)) {
    stop("clustered_map must be a cdm_map object returned by cluster_comm().", call. = FALSE)
  }

  community_type <- clustered_map$community_type
  type_ids <- names(community_type)
  if (is.null(type_ids) || any(is.na(type_ids) | type_ids == "")) {
    stop("clustered_map$community_type must be named by sample ID.", call. = FALSE)
  }
  if (anyDuplicated(type_ids)) {
    stop("clustered_map$community_type sample IDs must be unique.", call. = FALSE)
  }

  missing_cluster_samples <- setdiff(cdm_data$sample_names, type_ids)
  extra_cluster_samples <- setdiff(type_ids, cdm_data$sample_names)
  if (length(missing_cluster_samples) > 0L || length(extra_cluster_samples) > 0L) {
    stop(
      "Clustered map samples must match community data samples. ",
      "Missing in clustered map: ", paste(missing_cluster_samples, collapse = ", "),
      ". Extra in clustered map: ", paste(extra_cluster_samples, collapse = ", "),
      call. = FALSE
    )
  }

  community_type <- community_type[rownames(cdm_data$env)]
  if (any(is.na(community_type))) {
    stop("Every community data sample must have a community type.", call. = FALSE)
  }

  cdm_data$env$community_type <- unname(community_type)
  cdm_data
}
