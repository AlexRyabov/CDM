#' Select samples of community data
#'
#' Keeps the samples of a `cdm_data` object for which a condition on the
#' environmental variables is true. The samples are removed from `env`,
#' `abundance`, and `relative_abundance` together; `taxa` is unchanged.
#'
#' @param x An object created by [prepare_data()].
#' @param condition A logical expression evaluated with the columns of
#'   `x$env`, for example `Depth < 105`. It must give one value per sample;
#'   missing values count as `FALSE`. A quoted condition, such as
#'   `quote(Depth < 105)` stored in the project settings, also works.
#' @param ... Not used.
#'
#' @return A `cdm_data` object with the selected samples.
#'
#' @examples
#' abundance <- transpose_abundance(data.frame(s1 = c(1, 2), s2 = c(3, 4), s3 = c(5, 6)))
#' env <- data.frame(Sample = c("s1", "s2", "s3"), Depth = c(25, 80, 120))
#' cdm_data <- prepare_data(abundance, env, sample_id = "Sample", taxon_id = NULL)
#'
#' shallow <- subset(cdm_data, Depth < 105)
#' shallow$env
#'
#' depth_filter <- quote(Depth < 105)
#' shallow <- subset(cdm_data, depth_filter)
#'
#' @export
subset.cdm_data <- function(x, condition, ...) {
  if (missing(condition)) {
    stop("condition must be given, for example subset(cdm_data, Depth < 105).", call. = FALSE)
  }
  validate_cdm_data(x)
  condition_expr <- substitute(condition)
  caller_env <- parent.frame()
  keep <- tryCatch(
    {
      keep <- eval(condition_expr, envir = x$env, enclos = caller_env)
      # A quoted condition, such as a sample filter stored in the settings.
      if (is.language(keep)) {
        keep <- eval(keep, envir = x$env, enclos = caller_env)
      }
      keep
    },
    error = function(error) {
      stop("Could not evaluate the condition with the columns of env: ", conditionMessage(error), call. = FALSE)
    }
  )
  if (!is.logical(keep) || !is.null(dim(keep)) || length(keep) != nrow(x$env)) {
    stop("condition must give one TRUE or FALSE value per sample.", call. = FALSE)
  }
  keep <- !is.na(keep) & keep

  for (part in c("env", "abundance", "relative_abundance")) {
    x[[part]] <- x[[part]][keep, , drop = FALSE]
    rownames(x[[part]]) <- NULL
  }
  x
}
