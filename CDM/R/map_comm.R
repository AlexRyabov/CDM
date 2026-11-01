#' Map communities to a low-dimensional space
#'
#' Embeds the samples in a low-dimensional space with the method defined by
#' `options`. Currently the supported method is `"diffmap"` (diffusion map).
#'
#' @param x A data frame, tibble, or matrix with samples in rows and one
#'   numeric column per taxon, usually `cdm_data$relative_abundance`. A
#'   column `sample_id` is not used in the calculation; it is copied to the
#'   result.
#' @param options Mapping options created by [map_options()].
#'
#' @return An object of class `cdm_map`, a list with:
#' \describe{
#'   \item{map}{A data frame with one row per sample in the order of `x`:
#'     `sample_id` (if `x` has it), then the coordinates `dm_1`, `dm_2`, ...}
#'   \item{eigen_values}{Eigenvalues returned by the mapping method.}
#'   \item{map_object}{Original result returned by the mapping method.}
#'   \item{options}{Mapping options used for the run.}
#' }
#'
#' @export
map_comm <- function(x, options = map_options()) {
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop("x must be a data frame or a matrix with samples in rows and taxa in columns.", call. = FALSE)
  }
  if (!inherits(options, "map_options")) {
    stop("options must be created by map_options().", call. = FALSE)
  }
  x <- as.data.frame(x, check.names = FALSE)
  sample_ids <- if ("sample_id" %in% names(x)) table_sample_ids(x) else NULL
  x$sample_id <- NULL
  if (nrow(x) < 2L || ncol(x) < 1L) {
    stop("x must have at least two samples (rows) and one taxon (column).", call. = FALSE)
  }
  if (!all(vapply(x, is.numeric, logical(1)))) {
    stop("All columns of x except sample_id must be numeric.", call. = FALSE)
  }
  sample_matrix <- as.matrix(x)
  storage.mode(sample_matrix) <- "double"
  dimnames(sample_matrix) <- NULL

  if (identical(options$method, "diffmap")) {
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

    coordinates <- as.data.frame(DM$DM, check.names = FALSE)
    names(coordinates) <- paste0("dm_", seq_len(ncol(coordinates)))
    map_data <- if (is.null(sample_ids)) coordinates else
      data.frame(sample_id = sample_ids, coordinates, stringsAsFactors = FALSE)

    result <- list(
      map = map_data,
      eigen_values = DM$eigv,
      map_object = DM,
      options = options
    )
    class(result) <- "cdm_map"
    return(result)
  }

  stop("Unsupported mapping method: ", options$method, call. = FALSE)
}
