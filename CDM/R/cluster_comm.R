#' Cluster communities in a CDM map
#'
#' Clusters samples using coordinates from a `cdm_map` object and adds
#' community type IDs to that object. Currently the supported clustering method
#' is `"mclust"`.
#'
#' @param cdm_map An object created by `map_comm()`.
#' @param options Clustering options created by `cluster_options()`.
#'
#' @return The input `cdm_map` object with added fields:
#' \describe{
#'   \item{community_type}{Factor of community types named by sample ID.}
#'   \item{cluster_result}{An object of class `cdm_cluster` with cluster
#'     assignments, membership probabilities, the selected number of clusters,
#'     BIC values, and the fitted clustering object.}
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
  if (nrow(map_data) < 2L || ncol(map_data) < 1L) {
    stop("cdm_map$map must contain at least two samples and one coordinate.", call. = FALSE)
  }

  sample_names <- as.character(cdm_map$sample_names)
  if (length(sample_names) == 0L ||
      any(is.na(sample_names) | sample_names == "") ||
      anyDuplicated(sample_names)) {
    stop("cdm_map$sample_names must contain unique non-empty sample IDs.", call. = FALSE)
  }
  if (!identical(rownames(map_data), sample_names)) {
    stop(
      "Row names of cdm_map$map must be the sample IDs in cdm_map$sample_names.",
      call. = FALSE
    )
  }

  numeric_cols <- vapply(map_data, is.numeric, logical(1))
  if (!all(numeric_cols)) {
    stop("All cdm_map$map columns must be numeric.", call. = FALSE)
  }

  map_matrix <- as.matrix(map_data)
  if (any(!is.finite(map_matrix))) {
    stop("cdm_map$map must contain only finite values.", call. = FALSE)
  }

  if (identical(options$method, "mclust")) {
    cluster_result <- cluster_mclust(map_matrix, sample_names, options)
    cdm_map$community_type <- factor(cluster_result$clusters)
    names(cdm_map$community_type) <- sample_names
    cdm_map$cluster_result <- cluster_result
    return(cdm_map)
  }

  stop("Unsupported clustering method: ", options$method, call. = FALSE)
}

cluster_mclust <- function(map_matrix, sample_names, options) {
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package 'mclust' is required for method = 'mclust'.", call. = FALSE)
  }

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

  clusters <- cluster_object$classification
  names(clusters) <- sample_names

  probabilities <- as.data.frame(cluster_object$z, check.names = FALSE)
  names(probabilities) <- paste0("cluster_", seq_len(ncol(probabilities)))
  rownames(probabilities) <- sample_names

  result <- list(
    clusters = clusters,
    probabilities = probabilities,
    k = cluster_object$G,
    method = options$method,
    bic = cluster_object$bic,
    bic_table = cluster_object$BIC,
    cluster_object = cluster_object,
    options = options,
    sample_names = sample_names
  )

  class(result) <- "cdm_cluster"
  result
}
