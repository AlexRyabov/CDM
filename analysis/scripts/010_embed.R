source("config.R")

# Load prepared community data.
cdm_data <- readRDS(cdm_config$files$community_data)

# Define mapping options from config.R and run the embedding.
mapping_options <- map_options(
  method = cdm_config$embedding$method,
  metric = cdm_config$embedding$metric,
  k_min = cdm_config$embedding$k_min,
  dm_variables = cdm_config$embedding$dm_variables
)

map_result <- map_comm(cdm_data, mapping_options)

# Save embedding result.
saveRDS(map_result, cdm_config$files$embedding)

# Plot the embedding colored by selected variables.
map_plots <- lapply(cdm_config$embedding_plot_colors, function(v) {
  plot_map(map_result, cdm_data, x = "dm_1", y = "dm_2", color = v)
})

embedding_plot <- wrap_plots(map_plots, ncol = 2)

embedding_plot
