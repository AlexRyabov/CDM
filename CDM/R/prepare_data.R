#' Prepare input data for CDM analyses
#'
#' Combines species metadata, abundance data, environmental data, and relative
#' abundance into a single `cdm_data` object.
#'
#' Data frames can also be tibbles; all parts of the result are stored as
#' plain data frames.
#'
#' @param species_names Data frame with species or taxonomic metadata. Rows must
#'   match rows of `abundance`.
#' @param abundance Numeric matrix or data frame with species/taxa in rows and
#'   samples in columns.
#' @param env Data frame with samples in rows and environmental parameters in
#'   columns. By default, it must contain a `Sample` column with explicit sample
#'   identifiers.
#' @param sample_names Optional character vector with sample names. If `NULL`
#'   and `abundance` has column names, those names are used. If provided, these
#'   names are assigned to abundance columns and used to align `env`.
#' @param env_sample_col Character string naming the environmental-data column
#'   with sample identifiers.
#'
#' @return An object of class `cdm_data`, a list with:
#' \describe{
#'   \item{species_names}{Species or taxonomic metadata.}
#'   \item{abundance}{Abundance data as a data frame.}
#'   \item{relative_abundance}{Column-normalized relative abundance.}
#'   \item{env}{Environmental data with sample names as row names.}
#'   \item{sample_names}{Character vector of sample names.}
#' }
#'
#' @export
prepare_data <- function(species_names,
                         abundance,
                         env,
                         sample_names = NULL,
                         env_sample_col = "Sample") {
  if (!is.data.frame(species_names)) {
    stop("species_names must be a data frame.", call. = FALSE)
  }
  # Tibbles are stored as plain data frames, like abundance and env below.
  species_names <- as.data.frame(species_names)

  abundance_was_matrix <- is.matrix(abundance)
  if (!abundance_was_matrix && !is.data.frame(abundance)) {
    stop("abundance must be a matrix or data frame.", call. = FALSE)
  }

  if (!is.data.frame(env)) {
    stop("env must be a data frame.", call. = FALSE)
  }

  if (nrow(species_names) != nrow(abundance)) {
    stop(
      "species_names and abundance must have the same number of rows.",
      call. = FALSE
    )
  }

  sample_names <- resolve_sample_names(
    abundance = abundance,
    sample_names = sample_names,
    abundance_was_matrix = abundance_was_matrix
  )

  abundance_df <- as.data.frame(abundance, check.names = FALSE)
  names(abundance_df) <- sample_names

  numeric_cols <- vapply(abundance_df, is.numeric, logical(1))
  if (!all(numeric_cols)) {
    stop("All abundance columns must be numeric.", call. = FALSE)
  }

  env <- as.data.frame(env, check.names = FALSE)
  env <- align_env_samples(
    env = env,
    sample_names = sample_names,
    env_sample_col = env_sample_col
  )
  rownames(env) <- sample_names

  relative_abundance <- compute_relative_abundance(abundance_df)

  cdm_data <- list(
    species_names = species_names,
    abundance = abundance_df,
    relative_abundance = relative_abundance,
    env = env,
    sample_names = sample_names
  )

  class(cdm_data) <- "cdm_data"
  cdm_data
}

resolve_sample_names <- function(abundance, sample_names, abundance_was_matrix) {
  n_samples <- ncol(abundance)

  if (!is.null(sample_names)) {
    sample_names <- as.character(sample_names)
    if (length(sample_names) != n_samples) {
      stop(
        "sample_names must have one value for each sample column in abundance.",
        call. = FALSE
      )
    }
    if (any(is.na(sample_names) | sample_names == "")) {
      stop("sample_names must not contain missing or empty values.", call. = FALSE)
    }
    if (anyDuplicated(sample_names)) {
      stop("sample_names must be unique.", call. = FALSE)
    }
    return(sample_names)
  }

  sample_names <- colnames(abundance)
  if (is.null(sample_names) || any(is.na(sample_names) | sample_names == "")) {
    if (abundance_was_matrix) {
      stop(
        "abundance matrix must have column names when sample_names is NULL.",
        call. = FALSE
      )
    }

    stop(
      "abundance data frame must have column names when sample_names is NULL.",
      call. = FALSE
    )
  }

  if (anyDuplicated(sample_names)) {
    stop("abundance column names must be unique.", call. = FALSE)
  }

  sample_names
}

align_env_samples <- function(env, sample_names, env_sample_col) {
  if (!is.character(env_sample_col) || length(env_sample_col) != 1L ||
      is.na(env_sample_col) || env_sample_col == "") {
    stop("env_sample_col must be a single non-empty string.", call. = FALSE)
  }

  if (!env_sample_col %in% names(env)) {
    stop(
      "env must contain a '", env_sample_col,
      "' column with explicit sample identifiers.",
      call. = FALSE
    )
  }

  env_sample_names <- as.character(env[[env_sample_col]])
  if (any(is.na(env_sample_names) | env_sample_names == "")) {
    stop(
      "env$", env_sample_col, " must not contain missing or empty values.",
      call. = FALSE
    )
  }
  if (anyDuplicated(env_sample_names)) {
    stop("env$", env_sample_col, " values must be unique.", call. = FALSE)
  }

  missing_env <- setdiff(sample_names, env_sample_names)
  extra_env <- setdiff(env_sample_names, sample_names)
  if (length(missing_env) > 0L || length(extra_env) > 0L) {
    msg <- "env sample IDs must exactly match abundance sample names."
    if (length(missing_env) > 0L) {
      msg <- paste0(
        msg,
        " Missing in env: ",
        paste(missing_env, collapse = ", "),
        "."
      )
    }
    if (length(extra_env) > 0L) {
      msg <- paste0(
        msg,
        " Missing in abundance: ",
        paste(extra_env, collapse = ", "),
        "."
      )
    }
    stop(msg, call. = FALSE)
  }

  env[match(sample_names, env_sample_names), , drop = FALSE]
}

compute_relative_abundance <- function(abundance_df) {
  column_totals <- colSums(abundance_df, na.rm = TRUE)
  zero_columns <- column_totals == 0 | is.na(column_totals)

  if (any(zero_columns)) {
    warning(
      sum(zero_columns),
      " sample columns have zero total abundance and were left as zero.",
      call. = FALSE
    )
  }

  relative_abundance <- abundance_df
  nonzero_columns <- !zero_columns
  relative_abundance[nonzero_columns] <- sweep(
    abundance_df[nonzero_columns],
    2L,
    column_totals[nonzero_columns],
    "/"
  )
  relative_abundance[zero_columns] <- 0

  relative_abundance
}
