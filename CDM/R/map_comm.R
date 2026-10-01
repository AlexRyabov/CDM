#' Map communities to a low-dimensional space
#'
#' Embeds the samples of a `cdm_data` object with the mapping method defined by
#' `options` and returns a compact mapping result. Currently the supported
#' method is `"diffmap"`.
#'
#' @param cdm_data An object created by `prepare_data()`.
#' @param options Mapping options created by `map_options()`.
#'
#' @return An object of class `cdm_map`, a list with:
#' \describe{
#'   \item{map}{Mapping coordinates as a data frame with samples in rows and
#'     dimensions in columns. Row names are sample IDs.}
#'   \item{eigen_values}{Eigenvalues returned by the mapping method.}
#'   \item{map_object}{Original result returned by the mapping method.}
#'   \item{options}{Mapping options used for the run.}
#'   \item{sample_names}{Character vector of sample IDs.}
#' }
#'
#' @export
map_comm <- function(cdm_data, options = map_options()) {
  if (!inherits(cdm_data, "cdm_data")) {
    stop("cdm_data must be an object created by prepare_data().", call. = FALSE)
  }
  if (!inherits(options, "map_options")) {
    stop("options must be created by map_options().", call. = FALSE)
  }

  if (identical(options$method, "diffmap")) {
    sample_matrix <- t(as.matrix(cdm_data$relative_abundance[, cdm_data$sample_names, drop = FALSE]))
    storage.mode(sample_matrix) <- "double"

    k_min <- options$k_min
    if (identical(k_min, "sqrt")) {
      k_min <- ceiling(sqrt(nrow(sample_matrix)))
    }

    DM <- diffmap(
      sample_matrix,
      metric = options$metric,
      k_min = k_min,
      dm_variables = options$dm_variables
    )

    map_data <- as.data.frame(DM$DM, check.names = FALSE)
    names(map_data) <- paste0("dm_", seq_len(ncol(map_data)))
    rownames(map_data) <- cdm_data$sample_names

    result <- list(
      map = map_data,
      eigen_values = DM$eigv,
      map_object = DM,
      options = options,
      sample_names = cdm_data$sample_names
    )

    class(result) <- "cdm_map"
    return(result)
  }

  stop("Unsupported mapping method: ", options$method, call. = FALSE)
}
