# Project settings and packages used by all analysis scripts.
# Install the packages once with: Rscript install_libraries.R

library(here)
library(devtools)
library(readxl)
library(ggplot2)
library(patchwork)
library(mclust)
library(viridisLite)

# Load CDMr package functions.
load_all(here("CDM"), quiet = TRUE)

# Select the analysis project from the list `projects` below.
project_name <- "fram_strait"

# Base settings. Projects specify only the settings that differ from these.
default_settings <- list(
  description = "",

  # Folders with input data and results of the project.
  paths = list(
    data = here("analysis/data", project_name),
    results = here("analysis/results", project_name)
  ),

  # Input files, relative to paths$data.
  files = list(
    species = "species.xlsx",
    environment = "environment.xlsx"
  ),

  # Column names in the input files.
  columns = list(
    # Sample identifiers in the environmental table.
    sample_id = "Sample",
    # Last taxonomy column in the species table; abundance columns follow it.
    last_taxonomy = "Species"
  ),

  # Samples kept after import, as a condition on environmental columns,
  # e.g. quote(Depth < 105). NULL keeps all samples.
  sample_filter = NULL,

  # Embedding options for map_options() in 010_embed.R.
  embedding = list(
    method = "diffmap",
    metric = "jaccard",   # "jaccard", "euclidean", "pearson", or "spearman"
    k_min = "sqrt",       # number of nearest neighbours or "sqrt"
    dm_variables = 10L
  ),
  # Variables used to color the embedding plots in 010_embed.R.
  embedding_plot_colors = c("dm_3"),

  # Clustering options for cluster_options() in 020_cluster.R.
  clustering = list(
    method = "mclust",
    k = "best"            # number of clusters or "best" by BIC
  ),

  # Community composition options in 040_community_composition.R.
  composition = list(
    group_by = "Phylum",  # taxonomy column used to group taxa
    max_groups = 15L
  )
)

# Analysis projects. Change only the defaults that differ, e.g. k_min = 61.
projects <- list(
  fram_strait = list(
    description = "Fram Strait communities from samples shallower than 105 m.",
    files = list(
      species = "ASVSamples.xlsx",
      environment = "Samples_imp.xlsx"
    ),
    sample_filter = quote(Depth < 105),
    embedding = list(k_min = 61),
    embedding_plot_colors = c("dm_3", "MonthNum", "Depth", "Lon")
  )
)

# Combine the settings of the selected project and add result file paths.
cdm_config <- setup_project(project_name, default_settings, projects)
