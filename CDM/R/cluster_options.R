#' Create clustering options
#'
#' Creates a small options object for `cluster_comm()`. Currently the supported
#' method is `"mclust"`.
#'
#' @param method Clustering method. Currently only `"mclust"` is supported.
#' @param k Number of clusters. Can be a positive integer, a vector of positive
#'   integers, or the string `"best"` to select the best model by BIC.
#'
#' @return An object of class `cluster_options`.
#'
#' @export
cluster_options <- function(method = "mclust", k = "best") {
  method <- match.arg(method, choices = "mclust")

  if (!identical(k, "best")) {
    if (!is.numeric(k) || length(k) < 1L || any(!is.finite(k)) ||
        any(k <= 0) || any(abs(k - round(k)) > .Machine$double.eps^0.5)) {
      stop("k must be 'best' or a positive integer vector.", call. = FALSE)
    }
    k <- as.integer(round(k))
  }

  options <- list(
    method = method,
    k = k
  )

  class(options) <- c("cluster_options", "list")
  options
}
