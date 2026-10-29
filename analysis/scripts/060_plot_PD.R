# Partial dependence of the community-type probabilities on temperature and
# day of year, for three depth layers.
# Requires 050_fit_model.R (final model) and 020_cluster.R (community types).
source("config.R")

# Partial dependence settings ----------------------------------------------------

pd_settings <- list(
  pred1 = "temp_imputed",  # x axis
  pred2 = "Month_doy",     # y axis
  n_grid = 200L,           # grid points per axis
  layer_column = "Depth",  # env column that defines the layers
  # Depth layers [lower, upper) in m. The probabilities are averaged over the
  # observed samples of each layer.
  depth_layers = list(
    "Shallow (0-30 m)" = c(0, 30),
    "Middle (30-60 m)" = c(30, 60),
    "Deep (60-105 m)" = c(60, 105)
  )
)
contour_levels <- c(0.4, 0.6, 0.8)  # probability contours on the maps

# TRUE recalculates the partial dependence even if the model, the settings,
# and the data did not change.
recompute <- FALSE

# Data and model ------------------------------------------------------------------

model <- readRDS(cdm_config$files$model)
cdm_data <- readRDS(cdm_config$files$community_data)
env <- cdm_data$env

layer_values <- env[[pd_settings$layer_column]]
layer_data <- lapply(pd_settings$depth_layers, function(layer) {
  env[layer_values >= layer[1] & layer_values < layer[2], , drop = FALSE]
})

# Grids over the range of all observed values.
grid1 <- seq(min(env[[pd_settings$pred1]]), max(env[[pd_settings$pred1]]), length.out = pd_settings$n_grid)
grid2 <- seq(min(env[[pd_settings$pred2]]), max(env[[pd_settings$pred2]]), length.out = pd_settings$n_grid)

# Partial dependence --------------------------------------------------------------

# Saved results are reused if they were computed with the same model (its
# provenance), the same settings, and the same inputs: the grids and the
# predictor values of the samples of every layer (with their IDs).
pd_inputs <- list(
  grids = list(grid1, grid2),
  data = lapply(layer_data, function(data) data[model$predictors])
)
saved <- NULL
if (file.exists(cdm_config$files$partial_dependence)) {
  saved <- readRDS(cdm_config$files$partial_dependence)
}
is_current <- !is.null(saved) &&
  identical(saved$model_provenance, model$provenance) &&
  identical(saved$settings, pd_settings) &&
  identical(saved$inputs, pd_inputs)

if (recompute || !is_current) {
  message("Computing the partial dependence (", pd_settings$n_grid, " x ", pd_settings$n_grid, " grid)")
  # With cdm_config$workers > 1 the grid is computed in parallel R processes.
  # If the script is interrupted, stop the processes with stopCluster(cl).
  if (cdm_config$workers > 1) {
    cl <- makeClusterPSOCK(
      cdm_config$workers,
      rscript_startup = bquote(pkgload::load_all(.(here("CDM")), quiet = TRUE))
    )
    plan(cluster, workers = cl)
  }

  pds <- lapply(layer_data, function(data) {
    pd2D_find(
      model,
      data,
      pred1 = setNames(list(grid1), pd_settings$pred1),
      pred2 = setNames(list(grid2), pd_settings$pred2)
    )
  })

  if (cdm_config$workers > 1) {
    plan(sequential)
    stopCluster(cl)
  }

  saveRDS(
    list(
      pds = pds,
      settings = pd_settings,
      inputs = pd_inputs,
      model_provenance = model$provenance
    ),
    cdm_config$files$partial_dependence
  )
} else {
  message("Loading the saved partial dependence: model, settings, and inputs are unchanged")
  pds <- saved$pds
}

# Maps of the most probable community type ---------------------------------------

pd_plots <- lapply(names(pds), function(layer_name) {
  most_probable <- pd2D_mostprobable(pds[[layer_name]])
  pd2D_plot(
    most_probable,
    points = layer_data[[layer_name]],
    class_column = model$target,
    levels = contour_levels,
    title = layer_name
  )
})

print(wrap_plots(pd_plots, nrow = 1, guides = "collect"))
