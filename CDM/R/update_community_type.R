#' Add community types to environmental data
#'
#' Adds the community type of every sample from a clustered map to a table
#' with one row per sample, usually `cdm_data$env`, as the column
#' `community_type`. An existing `community_type` column is replaced. The
#' tables are matched by `sample_id` when both have it, otherwise by order.
#'
#' @param env A data frame or tibble with one row per sample, usually
#'   `cdm_data$env`. It must contain the same samples as the clustered map.
#' @param clustered_map A `cdm_map` object returned by [cluster_comm()].
#'
#' @return `env` as a data frame with the column `community_type`.
#'
#' @export
update_community_type <- function(env, clustered_map) {
  if (!is.data.frame(env)) {
    stop("env must be a data frame with one row per sample.", call. = FALSE)
  }
  if (!inherits(clustered_map, "cdm_map") || !is.data.frame(clustered_map$community_type)) {
    stop("clustered_map must be a cdm_map object returned by cluster_comm().", call. = FALSE)
  }
  env <- as.data.frame(env, check.names = FALSE)
  types <- clustered_map$community_type
  type_rows <- match_sample_rows(env, types, "env", "the clustered map", same_samples = TRUE)
  env$community_type <- types$community_type[type_rows]
  env
}
