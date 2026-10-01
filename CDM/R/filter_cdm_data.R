#' Filter CDM community data by environmental variables
#'
#' Evaluates a logical condition using columns from `cdm_data$env`, then keeps
#' the corresponding samples in all sample-level parts of the object. Samples
#' are aligned by their explicit identifiers, not by row or column position.
#'
#' @param cdm_data An object created by `prepare_data()`.
#' @param condition A logical expression evaluated using columns from
#'   `cdm_data$env`. It must return one value per sample. Missing values in the
#'   result are treated as `FALSE`. The condition can also be a quoted
#'   expression, for example `quote(Depth < 105)` stored in project settings.
#'
#' @return A filtered `cdm_data` object. `species_names` is unchanged, while
#'   `env`, `abundance`, `relative_abundance`, and `sample_names` contain only
#'   selected samples.
#'
#' @examples
#' species_names <- data.frame(species = c("sp1", "sp2"))
#' abundance <- data.frame(
#'   sample_a = c(1, 2),
#'   sample_b = c(3, 4),
#'   sample_c = c(5, 6)
#' )
#' env <- data.frame(
#'   Sample = c("sample_a", "sample_b", "sample_c"),
#'   Depth = c(25, 80, 120),
#'   Lat = c(74, 77, 81),
#'   community_type = c("type_1", "type_2", "type_1")
#' )
#' cdm_data <- prepare_data(species_names, abundance, env)
#'
#' shallow_data <- filter_cdm_data(cdm_data, Depth < 105)
#' latitude_data <- filter_cdm_data(cdm_data, Lat >= 75 & Lat <= 80)
#' sample_filter <- quote(Depth < 105)
#' quoted_data <- filter_cdm_data(cdm_data, sample_filter)
#' selected_types <- filter_cdm_data(
#'   cdm_data,
#'   community_type == "type_1"
#' )
#'
#' @export
filter_cdm_data <- function(cdm_data, condition) {
  if (missing(condition)) {
    stop("condition must be provided.", call. = FALSE)
  }

  cdm_data <- align_cdm_data_samples(cdm_data)
  condition_expr <- substitute(condition)
  caller_env <- parent.frame()
  keep <- tryCatch(
    {
      keep <- eval(condition_expr, envir = cdm_data$env, enclos = caller_env)
      # A quoted condition, such as a sample filter stored in the settings.
      if (is.language(keep)) {
        keep <- eval(keep, envir = cdm_data$env, enclos = caller_env)
      }
      keep
    },
    error = function(error) {
      stop(
        "Could not evaluate condition using cdm_data$env: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  if (!is.logical(keep) || !is.null(dim(keep)) || length(keep) != nrow(cdm_data$env)) {
    stop(
      "condition must return one logical value per sample in cdm_data$env.",
      call. = FALSE
    )
  }

  keep[is.na(keep)] <- FALSE
  sample_ids <- cdm_data$sample_names[keep]

  cdm_data$env <- cdm_data$env[sample_ids, , drop = FALSE]
  cdm_data$abundance <- cdm_data$abundance[, sample_ids, drop = FALSE]
  cdm_data$relative_abundance <-
    cdm_data$relative_abundance[, sample_ids, drop = FALSE]
  cdm_data$sample_names <- sample_ids

  cdm_data
}

align_cdm_data_samples <- function(cdm_data) {
  if (!inherits(cdm_data, "cdm_data")) {
    stop("cdm_data must be an object created by prepare_data().", call. = FALSE)
  }

  required_fields <- c(
    "species_names",
    "abundance",
    "relative_abundance",
    "env",
    "sample_names"
  )
  missing_fields <- setdiff(required_fields, names(cdm_data))
  if (length(missing_fields) > 0L) {
    stop(
      "cdm_data is missing fields: ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.data.frame(cdm_data$species_names) ||
      !is.data.frame(cdm_data$abundance) ||
      !is.data.frame(cdm_data$relative_abundance) ||
      !is.data.frame(cdm_data$env)) {
    stop(
      "cdm_data species_names, abundance, relative_abundance, and env fields must be data frames.",
      call. = FALSE
    )
  }

  sample_names <- cdm_data$sample_names
  if (!is.character(sample_names) ||
      any(is.na(sample_names) | sample_names == "") ||
      anyDuplicated(sample_names)) {
    stop("cdm_data$sample_names must contain unique non-empty sample IDs.", call. = FALSE)
  }

  env_ids <- rownames(cdm_data$env)
  sample_fields <- list(
    env = env_ids,
    abundance = names(cdm_data$abundance),
    relative_abundance = names(cdm_data$relative_abundance)
  )

  for (field_name in names(sample_fields)) {
    field_ids <- sample_fields[[field_name]]
    if (is.null(field_ids) ||
        any(is.na(field_ids) | field_ids == "") ||
        anyDuplicated(field_ids)) {
      stop(
        "cdm_data$", field_name,
        " must have unique non-empty sample IDs.",
        call. = FALSE
      )
    }

    missing_ids <- setdiff(sample_names, field_ids)
    extra_ids <- setdiff(field_ids, sample_names)
    if (length(field_ids) != length(sample_names) ||
        length(missing_ids) > 0L ||
        length(extra_ids) > 0L) {
      stop(
        "Sample IDs in cdm_data$", field_name,
        " must match cdm_data$sample_names. Missing: ",
        paste(missing_ids, collapse = ", "),
        ". Extra: ",
        paste(extra_ids, collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (nrow(cdm_data$species_names) != nrow(cdm_data$abundance) ||
      nrow(cdm_data$species_names) != nrow(cdm_data$relative_abundance)) {
    stop(
      "species_names, abundance, and relative_abundance must have matching taxon rows.",
      call. = FALSE
    )
  }

  cdm_data$env <- cdm_data$env[sample_names, , drop = FALSE]
  cdm_data$abundance <- cdm_data$abundance[, sample_names, drop = FALSE]
  cdm_data$relative_abundance <-
    cdm_data$relative_abundance[, sample_names, drop = FALSE]

  cdm_data
}
