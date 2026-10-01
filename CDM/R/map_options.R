#' Create mapping options
#'
#' Creates a small options object for `map_comm()`. Currently the supported
#' mapping method is `"diffmap"`.
#'
#' @param method Mapping method. Currently only `"diffmap"` is supported.
#' @param metric Similarity metric used by the mapping method. One of
#'   `"jaccard"`, `"euclidean"`, `"pearson"`, or `"spearman"`.
#' @param k_min Minimum number of nearest neighbors. Can be a positive number
#'   or the string `"sqrt"`, which uses the square root of the number of
#'   samples.
#' @param dm_variables Number of diffusion map variables.
#'
#' @return An object of class `map_options`.
#'
#' @export
map_options <- function(method = "diffmap",
                        metric = c("jaccard", "euclidean", "pearson", "spearman"),
                        k_min = "sqrt",
                        dm_variables = 10L) {
  method <- match.arg(method, choices = "diffmap")
  metric <- match.arg(metric)

  if (!(is.numeric(k_min) && length(k_min) == 1L && is.finite(k_min) && k_min > 0) &&
      !identical(k_min, "sqrt")) {
    stop("k_min must be a positive number or the string 'sqrt'.", call. = FALSE)
  }

  dm_variables <- as.integer(dm_variables)
  if (length(dm_variables) != 1L || is.na(dm_variables) || dm_variables < 1L) {
    stop("dm_variables must be a positive integer.", call. = FALSE)
  }

  options <- list(
    method = method,
    metric = metric,
    k_min = k_min,
    dm_variables = dm_variables
  )

  class(options) <- c("map_options", "list")
  options
}
