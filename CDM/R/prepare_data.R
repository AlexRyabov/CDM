#' Turn a species table into a table with samples in rows
#'
#' Species tables often have taxa in rows and one abundance column per sample.
#' `transpose_abundance()` turns such a table into the layout used by
#' [prepare_data()]: samples in rows, the sample IDs in the column
#' `sample_id`, and one column per taxon.
#'
#' @param x A data frame, tibble, or matrix with taxa in rows and one numeric
#'   column per sample. The column names are the sample IDs.
#' @param taxon_id `NULL` or a vector with one ID per row of `x`, for example
#'   the column `ASV` of the taxonomy table. The taxon columns of the result
#'   are named by these IDs. With `NULL` the taxa are numbered `taxon_0001`,
#'   `taxon_0002`, and so on, in the order of the rows of `x`.
#'
#' @return A data frame with one row per sample: the column `sample_id`
#'   followed by one numeric column per taxon.
#'
#' @examples
#' x <- data.frame(s1 = c(5, 0, 1), s2 = c(2, 3, 0))
#' transpose_abundance(x, taxon_id = c("ASV1", "ASV2", "ASV3"))
#' transpose_abundance(x)
#'
#' @export
transpose_abundance <- function(x, taxon_id = NULL) {
  if (!is.data.frame(x) && !is.matrix(x)) {
    stop("x must be a data frame or a matrix with taxa in rows and samples in columns.", call. = FALSE)
  }
  x <- as.data.frame(x, check.names = FALSE)
  if (nrow(x) == 0L || ncol(x) == 0L) {
    stop("x must have at least one taxon (row) and one sample (column).", call. = FALSE)
  }
  check_ids(names(x), "The column names of x (sample IDs)")
  non_numeric <- names(x)[!vapply(x, is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      "All columns of x must be numeric abundances. Not numeric: ",
      paste(utils::head(non_numeric, 10L), collapse = ", "),
      if (length(non_numeric) > 10L) ", ...", ".",
      call. = FALSE
    )
  }

  if (is.null(taxon_id)) {
    taxon_id <- number_ids("taxon", nrow(x))
  }
  if (length(taxon_id) != nrow(x)) {
    stop("taxon_id must have one value per row of x (", nrow(x), ").", call. = FALSE)
  }
  taxon_id <- id_text(taxon_id)
  check_ids(taxon_id, "taxon_id")
  if ("sample_id" %in% taxon_id) {
    stop("taxon_id must not contain \"sample_id\", the name of the sample ID column.", call. = FALSE)
  }

  abundance <- as.data.frame(t(as.matrix(x)), check.names = FALSE)
  names(abundance) <- taxon_id
  rownames(abundance) <- NULL
  data.frame(sample_id = names(x), abundance, check.names = FALSE, stringsAsFactors = FALSE)
}

#' Prepare community data
#'
#' Checks and combines the abundance table, the environmental table, and the
#' taxonomy into one `cdm_data` object. All tables have samples (or taxa) in
#' rows. Tibbles are accepted.
#'
#' Samples and taxa are matched by ID when the ID columns are named, and by
#' order when the corresponding argument is `NULL`:
#'
#' * `sample_id = "Sample"`: the column `Sample` of `env` is matched with
#'   `abundance$sample_id`. Both tables must contain the same sample IDs; the
#'   rows of `env` are put in the order of `abundance`.
#' * `sample_id = NULL`: `env` has no ID column. Its rows are matched with the
#'   rows of `abundance` by order, so both tables must have the same number of
#'   rows. The IDs come from `abundance$sample_id`, or the samples are
#'   numbered `sample_0001`, ... if `abundance` has no `sample_id` column
#'   either.
#' * `taxon_id = "ASV"`: the column `ASV` of `taxa` is matched with the taxon
#'   columns of `abundance` (all columns except `sample_id`).
#' * `taxon_id = NULL`: the rows of `taxa` are matched with the taxon columns
#'   of `abundance` by order, and take their names as IDs.
#'
#' @param abundance A data frame, tibble, or matrix with samples in rows and
#'   one numeric column per taxon, named by the taxon ID. An optional column
#'   `sample_id` holds the sample IDs; [transpose_abundance()] creates it.
#' @param env A data frame or tibble with one row per sample and the
#'   environmental variables.
#' @param taxa `NULL` or a data frame or tibble with one row per taxon and the
#'   taxonomy. With `NULL`, `taxa` contains only the taxon IDs.
#' @param sample_id Name of the sample ID column of `env`, or `NULL` to match
#'   the rows of `env` and `abundance` by order.
#' @param taxon_id Name of the taxon ID column of `taxa`, or `NULL` to match
#'   the rows of `taxa` and the taxon columns of `abundance` by order.
#'
#' @return An object of class `cdm_data`, a list of data frames without row
#'   names:
#' \describe{
#'   \item{abundance}{One row per sample: `sample_id`, then one column per
#'     taxon named by its `taxon_id`.}
#'   \item{relative_abundance}{The same table with the abundances of every
#'     sample divided by their sum.}
#'   \item{env}{One row per sample in the same order: `sample_id`, then the
#'     environmental variables.}
#'   \item{taxa}{One row per taxon in the order of the abundance columns:
#'     `taxon_id`, then the taxonomy.}
#' }
#'
#' @examples
#' abundance <- transpose_abundance(
#'   data.frame(s1 = c(5, 0, 1), s2 = c(2, 3, 0)),
#'   taxon_id = c("ASV1", "ASV2", "ASV3")
#' )
#' env <- data.frame(Sample = c("s2", "s1"), Depth = c(50, 10))
#' taxa <- data.frame(ASV = c("ASV1", "ASV2", "ASV3"), Phylum = c("P1", "P1", "P2"))
#' cdm_data <- prepare_data(abundance, env, taxa, sample_id = "Sample", taxon_id = "ASV")
#' cdm_data$env
#'
#' @export
prepare_data <- function(abundance,
                         env,
                         taxa = NULL,
                         sample_id = "sample_id",
                         taxon_id = "taxon_id") {
  if (!is.data.frame(abundance) && !is.matrix(abundance)) {
    stop("abundance must be a data frame or a matrix with samples in rows.", call. = FALSE)
  }
  if (!is.data.frame(env)) {
    stop("env must be a data frame with one row per sample.", call. = FALSE)
  }
  if (is.matrix(abundance) && is.null(colnames(abundance))) {
    stop("The abundance matrix has no column names; name its columns by taxon ID.", call. = FALSE)
  }
  for (argument in list(list("sample_id", sample_id), list("taxon_id", taxon_id))) {
    value <- argument[[2L]]
    if (!is.null(value) && (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))) {
      stop(argument[[1L]], " must be a column name or NULL.", call. = FALSE)
    }
  }
  abundance <- as.data.frame(abundance, check.names = FALSE)
  env <- as.data.frame(env, check.names = FALSE)
  rownames(abundance) <- NULL
  rownames(env) <- NULL

  # Sample IDs of abundance.
  abundance_ids <- NULL
  if ("sample_id" %in% names(abundance)) {
    abundance_ids <- id_text(abundance$sample_id)
    check_ids(abundance_ids, "abundance$sample_id")
    abundance$sample_id <- NULL
  }
  if (ncol(abundance) == 0L || nrow(abundance) == 0L) {
    stop("abundance must have at least one sample (row) and one taxon (column).", call. = FALSE)
  }
  taxon_ids <- names(abundance)
  check_ids(taxon_ids, "The taxon column names of abundance")
  non_numeric <- taxon_ids[!vapply(abundance, is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    stop(
      "All taxon columns of abundance must be numeric. Not numeric: ",
      paste(utils::head(non_numeric, 10L), collapse = ", "), ".",
      call. = FALSE
    )
  }
  values <- as.matrix(abundance)
  if (anyNA(values) || any(!is.finite(values)) || any(values < 0)) {
    stop("abundance must contain only finite, non-negative values without NA.", call. = FALSE)
  }

  # Sample IDs of env, and the order of its rows.
  if (is.null(sample_id)) {
    if (nrow(env) != nrow(abundance)) {
      stop(
        "With sample_id = NULL, env and abundance are matched by order and must have the same ",
        "number of rows (", nrow(env), " and ", nrow(abundance), ").",
        call. = FALSE
      )
    }
    if (is.null(abundance_ids)) {
      abundance_ids <- number_ids("sample", nrow(abundance))
    }
    if ("sample_id" %in% names(env)) {
      stop("env already has a column sample_id; give sample_id = \"sample_id\" to match by it.", call. = FALSE)
    }
  } else {
    if (!sample_id %in% names(env)) {
      stop(
        "env has no column '", sample_id, "': give the name of its sample ID column in sample_id, ",
        "or sample_id = NULL to match the rows by order.",
        call. = FALSE
      )
    }
    if (sample_id != "sample_id" && "sample_id" %in% names(env)) {
      stop("env has both '", sample_id, "' and a column sample_id; keep only one of them.", call. = FALSE)
    }
    env_ids <- id_text(env[[sample_id]])
    check_ids(env_ids, paste0("env$", sample_id))
    env[[sample_id]] <- NULL
    if (is.null(abundance_ids)) {
      stop(
        "abundance has no column sample_id, so env cannot be matched by ID. Add the column ",
        "sample_id to abundance (transpose_abundance() creates it), or use sample_id = NULL.",
        call. = FALSE
      )
    }
    report_unmatched(abundance_ids, env_ids, "abundance", "env", "sample")
    env <- env[match(abundance_ids, env_ids), , drop = FALSE]
    rownames(env) <- NULL
  }

  # Taxonomy, in the order of the abundance columns. Without taxa, the
  # taxonomy table holds only the taxon IDs.
  if (is.null(taxa)) {
    taxa <- data.frame(row.names = seq_along(taxon_ids))
  } else if (!is.data.frame(taxa)) {
    stop("taxa must be a data frame with one row per taxon, or NULL.", call. = FALSE)
  } else if (is.null(taxon_id)) {
    taxa <- as.data.frame(taxa, check.names = FALSE)
    if (nrow(taxa) != length(taxon_ids)) {
      stop(
        "With taxon_id = NULL, the rows of taxa are matched with the taxon columns of abundance ",
        "by order; their numbers differ (", nrow(taxa), " and ", length(taxon_ids), ").",
        call. = FALSE
      )
    }
    if ("taxon_id" %in% names(taxa)) {
      stop("taxa already has a column taxon_id; give taxon_id = \"taxon_id\" to match by it.", call. = FALSE)
    }
  } else {
    taxa <- as.data.frame(taxa, check.names = FALSE)
    if (!taxon_id %in% names(taxa)) {
      stop(
        "taxa has no column '", taxon_id, "': give the name of its taxon ID column in taxon_id, ",
        "or taxon_id = NULL to match the taxa by order.",
        call. = FALSE
      )
    }
    if (taxon_id != "taxon_id" && "taxon_id" %in% names(taxa)) {
      stop("taxa has both '", taxon_id, "' and a column taxon_id; keep only one of them.", call. = FALSE)
    }
    taxa_ids <- id_text(taxa[[taxon_id]])
    check_ids(taxa_ids, paste0("taxa$", taxon_id))
    taxa[[taxon_id]] <- NULL
    report_unmatched(taxon_ids, taxa_ids, "abundance", "taxa", "taxon")
    taxa <- taxa[match(taxon_ids, taxa_ids), , drop = FALSE]
  }
  rownames(taxa) <- NULL

  totals <- rowSums(values)
  zero_samples <- totals == 0
  if (any(zero_samples)) {
    warning(
      sum(zero_samples), " samples have zero total abundance; their relative abundances are 0.",
      call. = FALSE
    )
    totals[zero_samples] <- 1
  }
  relative <- as.data.frame(values / totals, check.names = FALSE)

  cdm_data <- list(
    abundance = data.frame(sample_id = abundance_ids, abundance, check.names = FALSE, stringsAsFactors = FALSE),
    relative_abundance = data.frame(sample_id = abundance_ids, relative, check.names = FALSE, stringsAsFactors = FALSE),
    env = data.frame(sample_id = abundance_ids, env, check.names = FALSE, stringsAsFactors = FALSE),
    taxa = data.frame(taxon_id = taxon_ids, taxa, check.names = FALSE, stringsAsFactors = FALSE)
  )
  class(cdm_data) <- "cdm_data"
  cdm_data
}

# Stops unless ids are unique, non-missing, and non-empty.
check_ids <- function(ids, label) {
  if (is.null(ids) || anyNA(ids) || any(!nzchar(ids))) {
    stop(label, " must not contain missing or empty values.", call. = FALSE)
  }
  duplicated_ids <- unique(ids[duplicated(ids)])
  if (length(duplicated_ids) > 0L) {
    stop(
      label, " must be unique. Duplicated: ",
      paste(utils::head(duplicated_ids, 10L), collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# IDs as text. Numeric IDs, often read from Excel, are written one by one
# without scientific notation, so that 100000 stays "100000" (not "1e+05")
# and 2 stays "2" even when another ID is 1.5.
id_text <- function(ids) {
  text <- if (is.numeric(ids)) trimws(formatC(ids, format = "fg", digits = 15)) else as.character(ids)
  text[is.na(ids)] <- NA_character_
  text
}

# Numbered IDs such as sample_0001, with at least four digits. Used for
# samples and for taxa in two functions.
number_ids <- function(prefix, n) {
  sprintf(paste0("%s_%0", max(4L, nchar(n)), "d"), prefix, seq_len(n))
}

# Stops with the IDs that are found in only one of two tables.
report_unmatched <- function(ids_1, ids_2, table_1, table_2, what) {
  missing_2 <- setdiff(ids_1, ids_2)
  missing_1 <- setdiff(ids_2, ids_1)
  if (length(missing_1) > 0L || length(missing_2) > 0L) {
    stop(
      "The ", what, " IDs of ", table_1, " and ", table_2, " must be the same.",
      if (length(missing_2) > 0L) paste0(
        " Missing in ", table_2, ": ", paste(utils::head(missing_2, 10L), collapse = ", "),
        if (length(missing_2) > 10L) ", ...", "."
      ),
      if (length(missing_1) > 0L) paste0(
        " Missing in ", table_1, ": ", paste(utils::head(missing_1, 10L), collapse = ", "),
        if (length(missing_1) > 10L) ", ...", "."
      ),
      call. = FALSE
    )
  }
}

# Checks that a cdm_data object has the agreed structure: the same sample IDs
# in the same order in env, abundance, and relative_abundance, and the taxon
# IDs of taxa equal to the taxon columns of the abundance tables.
validate_cdm_data <- function(cdm_data) {
  if (!inherits(cdm_data, "cdm_data")) {
    stop("cdm_data must be an object created by prepare_data().", call. = FALSE)
  }
  parts <- c("abundance", "relative_abundance", "env", "taxa")
  missing_parts <- setdiff(parts, names(cdm_data))
  if (length(missing_parts) > 0L) {
    stop(
      "cdm_data is missing: ", paste(missing_parts, collapse = ", "),
      ". Create it again with prepare_data().",
      call. = FALSE
    )
  }
  for (part in setdiff(parts, "taxa")) {
    if (!is.data.frame(cdm_data[[part]]) || !identical(names(cdm_data[[part]])[1L], "sample_id")) {
      stop("cdm_data$", part, " must be a data frame whose first column is sample_id.", call. = FALSE)
    }
  }
  ids <- cdm_data$env$sample_id
  check_ids(ids, "cdm_data$env$sample_id")
  if (!identical(cdm_data$abundance$sample_id, ids) || !identical(cdm_data$relative_abundance$sample_id, ids)) {
    stop("The sample_id columns of cdm_data$env, abundance, and relative_abundance must be equal.", call. = FALSE)
  }
  if (!is.data.frame(cdm_data$taxa) || !identical(names(cdm_data$taxa)[1L], "taxon_id") ||
      !identical(cdm_data$taxa$taxon_id, names(cdm_data$abundance)[-1L]) ||
      !identical(names(cdm_data$relative_abundance), names(cdm_data$abundance))) {
    stop(
      "cdm_data$taxa$taxon_id must equal the taxon columns of cdm_data$abundance and relative_abundance.",
      call. = FALSE
    )
  }
  invisible(cdm_data)
}
