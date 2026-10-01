#' Diffusion map
#'
#' Computes a diffusion map for the rows of a numeric data matrix. Rows are
#' treated as objects, and columns are treated as variables describing those
#' objects.
#'
#' The function first converts the data matrix to a row-wise similarity matrix,
#' keeps the strongest nearest-neighbor similarities in each column, symmetrizes
#' the cleaned matrix, and computes diffusion coordinates from the
#' row-normalized graph Laplacian.
#'
#' @param X Numeric matrix. Rows are objects, columns are variables.
#' @param metric Similarity metric. One of `"euclidean"`, `"pearson"`,
#'   `"spearman"`, or `"jaccard"`.
#' @param k_min Minimum number of positive nearest-neighbor similarities per
#'   column before symmetrization. Ties at the threshold are retained.
#' @param dm_variables Number of non-trivial diffusion map variables to return.
#'   If `NULL`, all available non-trivial variables are returned.
#'
#' @return A list with:
#' \describe{
#'   \item{DM}{Diffusion map coordinates. Rows match rows of `X`; columns are
#'   diffusion map variables.}
#'   \item{eigv}{Eigenvalues corresponding to the returned diffusion map
#'   variables.}
#' }
#'
#' @author Alexey Ryabov, 2026
#'
#' @examples
#' set.seed(1)
#' X <- matrix(runif(20 * 10), nrow = 20, ncol = 10)
#' X <- X / rowSums(X)
#'
#' DM <- diffmap(X, metric = "spearman", k_min = 5, dm_variables = 3)
#'
#' plot(DM$DM[, 1], DM$DM[, 2],
#'   pch = 19,
#'   xlab = "DM1",
#'   ylab = "DM2"
#' )
#'
#' @export
diffmap <- function(X,
                    metric = c("euclidean", "pearson", "spearman", "jaccard"),
                    k_min = 10L,
                    dm_variables = NULL) {
  metric <- match.arg(metric)
  sim <- diffmap_similarity(X, metric = metric)
  diffmap_eigv(sim, k_min = k_min, dm_variables = dm_variables)
}

#' Compute a row-wise similarity matrix
#'
#' `diffmap_similarity()` is a helper for `diffmap()`. It returns only the full
#' similarity matrix; nearest-neighbor cleaning is done by `diffmap_eigv()`.
#'
#' @param X Numeric matrix. Rows are objects, columns are variables.
#' @param metric Similarity metric. One of `"euclidean"`, `"pearson"`,
#'   `"spearman"`, or `"jaccard"`.
#'
#' @return A numeric square similarity matrix.
#'
#'
#' @export
diffmap_similarity <- function(X,
                               metric = c("euclidean", "pearson", "spearman", "jaccard")) {
  metric <- match.arg(metric)
  X <- as.matrix(X)

  if (!is.numeric(X)) {
    stop("X must be numeric.", call. = FALSE)
  }
  if (nrow(X) == 0L || ncol(X) == 0L) {
    stop("X must not be empty.", call. = FALSE)
  }

  S <- switch(
    metric,
    euclidean = {
      D <- as.matrix(stats::dist(X, method = "euclidean"))
      out <- 1 / D
      diag(out) <- 0
      if (any(is.infinite(out))) {
        finite_values <- out[is.finite(out)]
        replacement <- if (length(finite_values)) max(finite_values) else 0
        out[is.infinite(out)] <- replacement
      }
      out
    },
    pearson = correlation_similarity(X, method = "pearson"),
    spearman = correlation_similarity(X, method = "spearman"),
    jaccard = jaccard_similarity(X)
  )

  S[!is.finite(S)] <- 0
  S
}

#' Compute diffusion map coordinates from a similarity matrix
#'
#' `diffmap_eigv()` cleans a similarity matrix, constructs the row-normalized
#' graph Laplacian, and returns diffusion coordinates and eigenvalues.
#'
#' @param sim Numeric square similarity matrix.
#' @param k_min Minimum number of positive nearest-neighbor similarities per
#'   column before symmetrization. Ties at the threshold are retained.
#' @param dm_variables Number of non-trivial diffusion map variables to return.
#'
#' @return A list with `DM` containing diffusion map coordinates
#'   `eigVect / eigValues` and `eigv` containing corresponding eigenvalues of
#'   the row-normalized Laplacian.
#'
#'
#' @export
diffmap_eigv <- function(sim, k_min = 10L, dm_variables = NULL) {
  S <- as.matrix(sim)

  if (!is.numeric(S) || any(!is.finite(S))) {
    stop("sim must be a numeric finite matrix.", call. = FALSE)
  }
  if (nrow(S) == 0L || ncol(S) == 0L || nrow(S) != ncol(S)) {
    stop("sim must be a non-empty square matrix.", call. = FALSE)
  }

  n <- nrow(S)
  if (n < 2L) {
    stop("sim must contain at least two objects.", call. = FALSE)
  }

  k_min <- as.integer(k_min)
  if (length(k_min) != 1L || is.na(k_min) || k_min < 1L) {
    stop("k_min must be a positive integer.", call. = FALSE)
  }
  k_min <- min(k_min, n - 1L)

  if (is.null(dm_variables) || is.na(dm_variables)) {
    dm_variables <- n - 1L
  }
  dm_variables <- as.integer(dm_variables)
  if (length(dm_variables) != 1L || is.na(dm_variables) || dm_variables < 1L) {
    stop("dm_variables must be a positive integer.", call. = FALSE)
  }
  dm_variables <- min(dm_variables, n - 1L)

  S <- clean_similarity(S, k_min = k_min)
  degrees <- rowSums(S)
  if (any(degrees <= 0)) {
    stop("The cleaned similarity matrix contains rows with zero degree.", call. = FALSE)
  }

  L <- diag(n) - S / degrees
  eig <- eigen(L)
  eigv <- eig$values
  eigvec <- eig$vectors

  nearly_real <- abs(Im(eigv)) < 1e-8
  eigv[nearly_real] <- Re(eigv[nearly_real])
  eigvec[, nearly_real] <- Re(eigvec[, nearly_real])

  ord <- order(Re(eigv), abs(Im(eigv)))
  eigv <- eigv[ord]
  eigvec <- eigvec[, ord, drop = FALSE]

  nonzero <- abs(eigv) >= 1e-8
  eigv <- eigv[nonzero]
  eigvec <- eigvec[, nonzero, drop = FALSE]

  keep <- seq_len(min(dm_variables, length(eigv)))
  eigv <- eigv[keep]
  eigvec <- eigvec[, keep, drop = FALSE]

  list(
    DM = sweep(eigvec, 2L, eigv, "/"),
    eigv = eigv
  )
}

correlation_similarity <- function(X, method) {
  out <- (stats::cor(t(X), method = method, use = "pairwise.complete.obs") + 1) / 2
  out[is.na(out)] <- 0
  diag(out) <- 0
  out
}

jaccard_similarity <- function(X) {
  X <- X > 0
  intersection <- tcrossprod(X)
  row_counts <- rowSums(X)
  union <- outer(row_counts, row_counts, "+") - intersection
  out <- intersection / union
  out[is.na(out)] <- 0
  diag(out) <- 0
  out
}

clean_similarity <- function(S, k_min) {
  S[!is.finite(S)] <- 0
  diag(S) <- 0

  S_clean <- S
  for (j in seq_len(ncol(S))) {
    positive_values <- S[S[, j] > 0, j]
    if (length(positive_values)) {
      threshold <- sort(positive_values, decreasing = TRUE)[min(k_min, length(positive_values))]
      S_clean[S[, j] < threshold, j] <- 0
    }
  }

  pmax(S_clean, t(S_clean))
}
