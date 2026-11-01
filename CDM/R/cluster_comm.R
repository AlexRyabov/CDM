#' Cluster communities in a CDM map
#'
#' Clusters the samples by their map coordinates and adds the community type
#' of every sample to the map. Currently the supported clustering method is
#' `"mclust"`.
#'
#' @param cdm_map An object created by [map_comm()].
#' @param options Clustering options created by [cluster_options()].
#'
#' @return The input `cdm_map` object with two added fields:
#' \describe{
#'   \item{community_type}{A data frame with one row per sample in the order
#'     of `cdm_map$map`: `sample_id` (if the map has it), then the factor
#'     `community_type`.}
#'   \item{cluster_result}{An object of class `cdm_cluster` with the cluster
#'     of every sample, membership probabilities, the selected number of
#'     clusters, BIC values, and the fitted clustering object.}
#' }
#'
#' @export
cluster_comm <- function(cdm_map, options = cluster_options()) {
  if (!inherits(cdm_map, "cdm_map")) {
    stop("cdm_map must be an object created by map_comm().", call. = FALSE)
  }
  if (!inherits(options, "cluster_options")) {
    stop("options must be created by cluster_options().", call. = FALSE)
  }

  map_data <- as.data.frame(cdm_map$map, check.names = FALSE)
  sample_ids <- if ("sample_id" %in% names(map_data)) table_sample_ids(map_data) else NULL
  map_data$sample_id <- NULL
  if (nrow(map_data) < 2L || ncol(map_data) < 1L) {
    stop("cdm_map$map must contain at least two samples and one coordinate.", call. = FALSE)
  }
  if (!all(vapply(map_data, is.numeric, logical(1)))) {
    stop("All coordinates in cdm_map$map must be numeric.", call. = FALSE)
  }
  map_matrix <- as.matrix(map_data)
  if (any(!is.finite(map_matrix))) {
    stop("cdm_map$map must contain only finite values.", call. = FALSE)
  }

  if (identical(options$method, "mclust")) {
    cluster_result <- cluster_mclust(map_matrix, sample_ids, options)
    community_type <- data.frame(community_type = factor(cluster_result$clusters))
    if (!is.null(sample_ids)) {
      community_type <- data.frame(sample_id = sample_ids, community_type, stringsAsFactors = FALSE)
    }
    cdm_map$community_type <- community_type
    cdm_map$cluster_result <- cluster_result
    return(cdm_map)
  }

  stop("Unsupported clustering method: ", options$method, call. = FALSE)
}

cluster_mclust <- function(map_matrix, sample_ids, options) {
  # Mclust() evaluates mclustBIC() in the caller environment.
  mclustBIC <- mclust::mclustBIC

  if (identical(options$k, "best")) {
    cluster_object <- mclust::Mclust(map_matrix)
  } else {
    cluster_object <- mclust::Mclust(map_matrix, G = options$k)
  }

  if (is.null(cluster_object)) {
    stop("mclust could not fit a clustering model.", call. = FALSE)
  }

  probabilities <- as.data.frame(cluster_object$z, check.names = FALSE)
  names(probabilities) <- paste0("cluster_", seq_len(ncol(probabilities)))
  if (!is.null(sample_ids)) {
    probabilities <- data.frame(sample_id = sample_ids, probabilities, stringsAsFactors = FALSE)
  }

  result <- list(
    clusters = unname(cluster_object$classification),
    probabilities = probabilities,
    k = cluster_object$G,
    method = options$method,
    bic = cluster_object$bic,
    bic_table = cluster_object$BIC,
    cluster_object = cluster_object,
    options = options,
    sample_id = sample_ids
  )

  class(result) <- "cdm_cluster"
  result
}
