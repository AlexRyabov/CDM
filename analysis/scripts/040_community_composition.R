source("config.R")

# Load prepared community data.
cdm_data <- readRDS(cdm_config$files$community_data)

# Community composition options from config.R.
group_by <- cdm_config$composition$group_by
max_groups <- cdm_config$composition$max_groups

if (!group_by %in% names(cdm_data$species_names)) {
  stop(
    "cdm_data$species_names is missing column: ",
    group_by,
    call. = FALSE
  )
}
if (!"community_type" %in% names(cdm_data$env)) {
  stop(
    "cdm_data$env must contain community_type. Run 020_cluster.R before this script.",
    call. = FALSE
  )
}

# Relative abundance deviations within each community type.
abundance_composition <- community_deviation_indexes(
  cdm_data,
  group_by = group_by,
  measure = "abundance",
  scale = "relative",
  max_groups = max_groups
)

community_abundance_by_type_plot <- plot_community_by_type(
  cdm_data,
  abundance_composition
)

# Relative richness deviations within each community type.
richness_composition <- community_deviation_indexes(
  cdm_data,
  group_by = group_by,
  measure = "richness",
  scale = "relative",
  max_groups = max_groups
)

community_richness_by_type_plot <- plot_community_by_type(
  cdm_data,
  richness_composition
)

if (interactive()) {
  print(community_abundance_by_type_plot)
  #print(community_richness_by_type_plot)
}
