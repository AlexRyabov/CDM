# Load project data for CSDM analyses
#
# This file is a user-editable analysis script. It expects a configuration list
# created by setup_project() and adds loaded input tables to cfg$data.

load_data <- function(cfg) {
  # Validate the required configuration fields.
  if (!is.list(cfg)) {
    stop("cfg must be a list created by setup_project().", call. = FALSE)
  }
  if (is.null(cfg$files$env) || is.null(cfg$files$species)) {
    stop(
      "cfg$files must contain both 'env' and 'species' file paths.",
      call. = FALSE
    )
  }

  # Read supported table formats by file extension.
  read_datafile <- function(file) {
    if (!file.exists(file)) {
      stop("Input data file does not exist: ", file, call. = FALSE)
    }

    extension <- tolower(tools::file_ext(file))

    if (extension == "csv") {
      return(read.csv(file, stringsAsFactors = FALSE, check.names = FALSE))
    }

    if (extension == "tsv") {
      return(read.delim(file, stringsAsFactors = FALSE, check.names = FALSE))
    }

    if (extension == "txt") {
      return(read.table(
        file,
        header = TRUE,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ))
    }

    if (extension %in% c("xls", "xlsx")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop(
          "Package 'readxl' is required to read Excel files: ", file,
          call. = FALSE
        )
      }
      return(as.data.frame(readxl::read_excel(file), check.names = FALSE))
    }

    stop("Unsupported input file extension: ", extension, " in ", file, call. = FALSE)
  }

  # Clean taxonomic labels before comparisons with option rules.
  clean_taxon_names <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- sub("^[A-Za-z]_{1,2}\\s*", "", x)
    x <- trimws(x)
    x[is.na(x) | x == "" | toupper(x) == "NA"] <- "NA"
    x
  }

  # Move TopName before Kingdom for a stable species table layout.
  move_top_name <- function(x) {
    if (!all(c("TopName", "Kingdom") %in% names(x))) {
      return(x)
    }

    kingdom_pos <- match("Kingdom", names(x))
    before_kingdom <- names(x)[seq_len(kingdom_pos - 1)]
    before_kingdom <- setdiff(before_kingdom, "TopName")
    after_kingdom <- names(x)[kingdom_pos:length(names(x))]
    after_kingdom <- setdiff(after_kingdom, "TopName")
    x[c(before_kingdom, "TopName", after_kingdom)]
  }

  # Sum sample columns after grouping by the requested taxonomic level.
  group_species_table <- function(t_samples, group_vars) {
    sample_vars <- setdiff(names(t_samples), group_vars)
    numeric_sample_vars <- sample_vars[vapply(t_samples[sample_vars], is.numeric, logical(1))]

    if (length(numeric_sample_vars) == 0) {
      warning("No numeric sample columns were found for species grouping.", call. = FALSE)
      return(unique(t_samples[group_vars]))
    }

    stats_table <- aggregate(
      t_samples[numeric_sample_vars],
      by = t_samples[group_vars],
      FUN = sum,
      na.rm = TRUE
    )

    move_top_name(stats_table)
  }

  # Apply Fram Strait specific species-table cleanup and grouping.
  process_fram_strait_species <- function(t_samples, options) {
    if (isTRUE(options$rename_calanus)) {
      ind <- grepl("Calanus_helgolandicus", t_samples$Species, fixed = TRUE)
      t_samples$Species[ind] <- "Calanus_spp."
    }

    if (isTRUE(options$split_syndiniales)) {
      clean_class <- clean_taxon_names(t_samples$Class)
      t_samples$Phylum[clean_class == "Syndiniales"] <- "Syndiniales"
      t_samples$Phylum[clean_class == "Dinophyceae"] <- "Dinophyceae"
    }

    if (isTRUE(options$filter_metazoa)) {
      ind_metazoa <- t_samples$Phylum %in% c("p_ Metazoa", "Metazoa")
      t_samples <- t_samples[!ind_metazoa, , drop = FALSE]
      if (sum(ind_metazoa) > 0) {
        warning(
          sum(ind_metazoa), " metazoa ASVs were removed from the sample list",
          call. = FALSE
        )
      }
    }

    group_by <- options$group_by
    if (is.null(group_by)) {
      group_by <- "ASV"
    }
    group_by <- match.arg(group_by, choices = c("ASV", "Species", "Genus"))

    if (isTRUE(options$remove_undefined_taxa)) {
      ind_undef <- clean_taxon_names(t_samples[[group_by]]) == "NA"
      t_samples <- t_samples[!ind_undef, , drop = FALSE]
      if (sum(ind_undef) > 0) {
        warning(
          sum(ind_undef), " NA ASVs were removed from the sample list",
          call. = FALSE
        )
      }
    }

    if ("DNA" %in% names(t_samples)) {
      t_samples$DNA <- NULL
    }

    if (group_by == "ASV") {
      t_samples$TopName <- t_samples$ASV
      t_samples$ASV <- NULL
      return(move_top_name(t_samples))
    }

    if (group_by == "Species") {
      t_samples$TopName <- t_samples$Species
      t_samples$ASV <- NULL
      group_vars <- c("TopName", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
      return(group_species_table(t_samples, group_vars))
    }

    t_samples$TopName <- t_samples$Genus
    t_samples$ASV <- NULL
    t_samples$Species <- NULL
    group_vars <- c("TopName", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus")
    group_species_table(t_samples, group_vars)
  }

  # Sample columns start after the last taxonomic column kept in the table.
  get_sample_columns <- function(t_samples) {
    leading_cols <- c(
      "TopName", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus",
      "Species", "ASV", "DNA"
    )
    leading_positions <- match(leading_cols, names(t_samples), nomatch = 0)
    leading_positions <- leading_positions[leading_positions > 0]

    if (length(leading_positions) == 0) {
      stop(
        "No taxonomic/name columns were found before the species sample columns.",
        call. = FALSE
      )
    }

    first_sample_col <- max(leading_positions) + 1
    if (first_sample_col > ncol(t_samples)) {
      stop("No sample columns were found after the taxonomic/name columns.", call. = FALSE)
    }

    seq.int(first_sample_col, ncol(t_samples))
  }

  # Keep environmental data only for samples present in the species table and
  # order rows to match the species sample columns.
  align_env_to_species_samples <- function(env, species, sample_cols) {
    if (!"Sample" %in% names(env)) {
      stop("Environmental table must contain a 'Sample' column.", call. = FALSE)
    }

    sample_names <- names(species)[sample_cols]
    env_sample <- as.character(env$Sample)
    match_idx <- match(sample_names, env_sample)

    missing_env <- is.na(match_idx)
    if (any(missing_env)) {
      warning(
        sum(missing_env), " species sample columns have no matching env$Sample: ",
        paste(sample_names[missing_env], collapse = ", "),
        call. = FALSE
      )
    }

    duplicate_env <- duplicated(env_sample) & !is.na(env_sample)
    if (any(duplicate_env)) {
      warning(
        sum(duplicate_env), " duplicate env$Sample rows found; using first match.",
        call. = FALSE
      )
    }

    matched_idx <- match_idx[!missing_env]
    env[matched_idx, , drop = FALSE]
  }

  # Convert sample counts in each taxon row to proportional abundances.
  normalize_species_rows <- function(species, sample_cols) {
    sample_data <- species[sample_cols]
    numeric_cols <- vapply(sample_data, is.numeric, logical(1))
    if (!all(numeric_cols)) {
      stop(
        "All species sample columns must be numeric before row normalization.",
        call. = FALSE
      )
    }

    row_totals <- rowSums(sample_data, na.rm = TRUE)
    zero_rows <- row_totals == 0 | is.na(row_totals)
    if (any(zero_rows)) {
      warning(
        sum(zero_rows), " species rows have zero total abundance and were not normalized.",
        call. = FALSE
      )
    }

    nonzero_rows <- !zero_rows
    species[nonzero_rows, sample_cols] <- sample_data[nonzero_rows, , drop = FALSE] /
      row_totals[nonzero_rows]

    species
  }

  # Load environmental and species data into cfg$data.
  cfg$data <- list(
    env = read_datafile(cfg$files$env),
    species = read_datafile(cfg$files$species)
  )

  if (identical(cfg$project$id, "fram_strait")) {
    cfg$data$species <- process_fram_strait_species(
      cfg$data$species,
      cfg$options$preprocessing
    )
  }

  cfg$data$idx_sample_cols <- get_sample_columns(cfg$data$species)
  cfg$data$sample_cols <- names(cfg$data$species)[cfg$data$idx_sample_cols]
  cfg$data$species <- normalize_species_rows(
    cfg$data$species,
    cfg$data$idx_sample_cols
  )
  cfg$data$env <- align_env_to_species_samples(
    cfg$data$env,
    cfg$data$species,
    cfg$data$idx_sample_cols
  )

  cfg
}

# Example:
# source("setup_project.R")
# source("load_data.R")
# cfg <- setup_project("fram_strait")
# cfg <- load_data(cfg)
# df_env <- cfg$data$env
# df_species <- cfg$data$species
