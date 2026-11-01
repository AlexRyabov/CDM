# Define project name, folders and options in config.R.
source(here::here("analysis", "scripts", "config.R"))

# Load the species table (taxa in rows) and the environmental table.
species_table <- as.data.frame(
  read_excel(cdm_config$files$species),
  check.names = FALSE
)
env <- as.data.frame(
  read_excel(cdm_config$files$environment),
  check.names = FALSE
)

# The taxonomy columns end with columns$last_taxonomy; one abundance column
# per sample follows.
last_taxonomy_col <- match(cdm_config$columns$last_taxonomy, names(species_table))
if (is.na(last_taxonomy_col) || last_taxonomy_col == ncol(species_table)) {
  stop(
    basename(cdm_config$files$species), " must contain a '",
    cdm_config$columns$last_taxonomy,
    "' column followed by abundance columns.",
    call. = FALSE
  )
}
taxa <- species_table[seq_len(last_taxonomy_col)]

# Put the samples in rows; the taxon columns are named by their IDs.
taxon_ids <- if (is.null(cdm_config$columns$taxon_id)) NULL else taxa[[cdm_config$columns$taxon_id]]
abundance <- transpose_abundance(
  species_table[(last_taxonomy_col + 1L):ncol(species_table)],
  taxon_id = taxon_ids
)

# Build the community data object; samples and taxa are matched by ID.
cdm_data <- prepare_data(
  abundance,
  env,
  taxa,
  sample_id = cdm_config$columns$sample_id,
  taxon_id = cdm_config$columns$taxon_id
)

# Keep the samples selected by the project sample filter.
if (!is.null(cdm_config$sample_filter)) {
  cdm_data <- subset(cdm_data, cdm_config$sample_filter)
}

# Save community data.
saveRDS(cdm_data, cdm_config$files$community_data)
