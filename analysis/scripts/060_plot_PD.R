# Partial dependence maps: the most probable community type over temperature
# and day of year, for three depth layers.
# Requires 050_fit_model.R (final model).
source(here::here("analysis", "scripts", "config.R"))

# Settings ----------------------------------------------------------------------

compute_pd <- TRUE  # FALSE loads the saved partial dependence
n_grid <- 200L      # grid points per axis

# Data and model ----------------------------------------------------------------

model <- readRDS(cdm_config$files$model)
env <- readRDS(cdm_config$files$community_data)$env

# The probabilities are averaged over the samples of each depth layer.
layers <- list(
  "Shallow (0-30 m)" = subset(env, Depth < 30),
  "Middle (30-60 m)" = subset(env, Depth >= 30 & Depth < 60),
  "Deep (60-105 m)"  = subset(env, Depth >= 60)
)

# The same grid for all layers, over the range of all samples. The first
# predictor is the x axis of the maps, the second the y axis.
grid <- list(
  temp_imputed = seq(min(env$temp_imputed), max(env$temp_imputed), length.out = n_grid),
  Month_doy = seq(min(env$Month_doy), max(env$Month_doy), length.out = n_grid)
)

# Partial dependence ------------------------------------------------------------

if (compute_pd) {
  # With cdm_config$workers > 1 the grid is computed in parallel R processes.
  # If the script is interrupted, stop the processes with stopCluster(cl).
  if (cdm_config$workers > 1) {
    cl <- makeClusterPSOCK(
      cdm_config$workers,
      rscript_startup = bquote(pkgload::load_all(.(here("CDM")), quiet = TRUE))
    )
    plan(cluster, workers = cl)
  }

  pds <- lapply(layers, function(layer) pd2D_find(model, layer, grid))
  saveRDS(pds, cdm_config$files$partial_dependence)

  if (cdm_config$workers > 1) {
    plan(sequential)
    stopCluster(cl)
  }
} else {
  pds <- readRDS(cdm_config$files$partial_dependence)
  # The saved partial dependence is valid only for the current model and grid.
  # All layers were computed together, so the first layer is enough to check.
  pd <- pds[[1]]
  if (!identical(pd$model_provenance, model$provenance)) {
    stop("partial_dependence.rds was computed with another model: set compute_pd <- TRUE.")
  }
  if (!identical(setNames(list(pd$x, pd$y), c(pd$pred1, pd$pred2)), grid)) {
    stop("partial_dependence.rds was computed on another grid: set compute_pd <- TRUE.")
  }
}

# Maps of the most probable community type --------------------------------------

pd_plots <- lapply(names(pds), function(name) {
  plot(pds[[name]], points = layers[[name]], title = name)
})
print(wrap_plots(pd_plots, nrow = 1, guides = "collect"))
