# Define project name, folders and options in config.R.
source("config.R")

# Load species abundance and environmental tables.
species_table <- as.data.frame(
  read_excel(cdm_config$files$species),
  check.names = FALSE
)
env <- as.data.frame(
  read_excel(cdm_config$files$environment),
  check.names = FALSE
)

# Split taxonomy columns from abundance columns.
last_taxonomy_col <- match(cdm_config$columns$last_taxonomy, names(species_table))
if (is.na(last_taxonomy_col) || last_taxonomy_col == ncol(species_table)) {
  stop(
    basename(cdm_config$files$species), " must contain a '",
    cdm_config$columns$last_taxonomy,
    "' column followed by abundance columns.",
    call. = FALSE
  )
}
species_names <- species_table[seq_len(last_taxonomy_col)]
abundance <- species_table[(last_taxonomy_col + 1L):ncol(species_table)]

# Build the community data object; samples are matched by ID.
cdm_data <- prepare_data(
  species_names,
  abundance,
  env,
  env_sample_col = cdm_config$columns$sample_id
)

# Keep the samples selected by the project sample filter.
if (!is.null(cdm_config$sample_filter)) {
  cdm_data <- filter_cdm_data(cdm_data, cdm_config$sample_filter)
}

# Save community data.
saveRDS(cdm_data, cdm_config$files$community_data)
