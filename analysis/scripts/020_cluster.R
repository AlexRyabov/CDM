source("config.R")

# Load prepared community data and saved embedding.
cdm_data <- readRDS(cdm_config$files$community_data)
map_result <- readRDS(cdm_config$files$embedding)

# Cluster samples and save the clustering result separately from the embedding.
clustering_options <- cluster_options(
  method = cdm_config$clustering$method,
  k = cdm_config$clustering$k
)
clustered_map <- cluster_comm(map_result, clustering_options)
cluster_result <- clustered_map$cluster_result
saveRDS(cluster_result, cdm_config$files$clustering)

# Store per-sample community types with environmental data and save them.
cdm_data <- update_community_type(cdm_data, clustered_map)
saveRDS(cdm_data, cdm_config$files$community_data)

# Plot community types with distinct colors.
community_colors <- colors_fresh(cdm_data$env$community_type, order = "my")

community_plot <- plot_map(
  map_result,
  cdm_data,
  x = "dm_1",
  y = "dm_2",
  color = "community_type"
) +
  scale_color_manual(values = community_colors)

community_plot
