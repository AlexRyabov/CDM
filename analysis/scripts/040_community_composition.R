source(here::here("analysis", "scripts", "config.R"))

# Load prepared community data.
cdm_data <- readRDS(cdm_config$files$community_data)

# Community composition options from config.R.
group_by <- cdm_config$composition$group_by
max_groups <- cdm_config$composition$max_groups

if (!"community_type" %in% names(cdm_data$env)) {
  stop(
    "cdm_data$env must contain community_type. Run 020_cluster.R before this script.",
    call. = FALSE
  )
}

# Relative abundance deviations within each community type.
abundance_composition <- community_deviation_indexes(
  cdm_data$relative_abundance,
  cdm_data$taxa,
  group_by = group_by,
  measure = "abundance",
  scale = "relative",
  max_groups = max_groups
)

community_abundance_by_type_plot <- plot_community_by_type(
  abundance_composition,
  cdm_data$env
)

# Relative richness deviations within each community type.
richness_composition <- community_deviation_indexes(
  cdm_data$relative_abundance,
  cdm_data$taxa,
  group_by = group_by,
  measure = "richness",
  scale = "relative",
  max_groups = max_groups
)

community_richness_by_type_plot <- plot_community_by_type(
  richness_composition,
  cdm_data$env
)

if (interactive()) {
  print(community_abundance_by_type_plot)
  #print(community_richness_by_type_plot)
}
