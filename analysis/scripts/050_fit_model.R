# Predict community-type probabilities from environmental conditions.
# Requires 020_cluster.R, which stores community_type in the community data.
source("config.R")

# Model settings ----------------------------------------------------------------

model_settings <- list(
  target = "community_type",
  # Environmental columns used as predictors; they must not contain NA.
  predictors = c(
    "Depth", "Month_doy", "Lon","temp_imputed"
#    "Depth", "Month_doy", "Lat", "Lon", "sea_ice_monthly","temp_imputed", "mod_aq_par_imputed"
  ),
  n_predictors = 2:4,           # numbers of predictors to try
  v = 5L,                       # outer folds, or "leave_block_out"
  repeats = 1L,                 # repeated fold assignments
  selection_score = "imp_rf",   # predictor ranking score
  imbalance = "smotenc",        # "smotenc" or "none"
  metric = "macro_recall",      # "macro_recall", "macro_f1", or "roc_auc"
  grid_size = 10L,              # engine parameter combinations
  inner_v = 5L,                 # inner folds of the nested cross-validation
  correlation_threshold = 0.9,  # one predictor of a more correlated pair is removed
  min_class_size = 6L,          # smaller classes are left out of a training set
  final_engine = "svm",         # engine of the final model: "svm" or "rf"
  seed = 1L
)

# Parallel processes ------------------------------------------------------------

# With cdm_config$workers > 1 the model tuning runs in parallel R processes.
# Every process loads the CDMr source when it starts. If the script is
# interrupted, stop the processes with stopCluster(cl).
if (cdm_config$workers > 1) {
  cl <- makeClusterPSOCK(
    cdm_config$workers,
    rscript_startup = bquote(pkgload::load_all(.(here("CDM")), quiet = TRUE))
  )
  plan(cluster, workers = cl)
}

# Cross-validation folds --------------------------------------------------------

cdm_data <- readRDS(cdm_config$files$community_data)

# All samples of a cruise are held out together; folds are balanced by the
# season (Month_doy) and by the number of samples.
set.seed(model_settings$seed)
cv <- make_cv_folds(
  cdm_data$env,
  group = Cruise,
  v = model_settings$v,
  repeats = model_settings$repeats,
  balance = "observations",
  strata = Month_doy
)
print(summary(cv))
saveRDS(cv, cdm_config$files$cv_folds)

# Model evaluation --------------------------------------------------------------

svm_options <- model_options(
  predictors = model_settings$predictors,
  target = model_settings$target,
  engine = "svm",
  n_predictors = model_settings$n_predictors,
  selection_score = model_settings$selection_score,
  imbalance = model_settings$imbalance,
  metric = model_settings$metric,
  grid_size = model_settings$grid_size,
  inner_v = model_settings$inner_v,
  correlation_threshold = model_settings$correlation_threshold,
  min_class_size = model_settings$min_class_size,
  seed = model_settings$seed
)
rf_options <- update(svm_options, engine = "rf")

# Nested cross-validation of both models (the longest step).
svm_evaluation <- evaluate_model(cdm_data, cv, svm_options)
saveRDS(svm_evaluation, cdm_config$files$svm_evaluation)

rf_evaluation <- evaluate_model(cdm_data, cv, rf_options)
saveRDS(rf_evaluation, cdm_config$files$rf_evaluation)

print(compare_models(svm = svm_evaluation, rf = rf_evaluation))
print(summary(svm_evaluation))
print(summary(rf_evaluation))
print(plot(svm_evaluation))
print(plot(rf_evaluation))

# Final model -------------------------------------------------------------------

final_options <- update(svm_options, engine = model_settings$final_engine)
model <- fit_model(cdm_data, cv, final_options)
saveRDS(model, cdm_config$files$model)

final_evaluation <- if (identical(model_settings$final_engine, "svm")) svm_evaluation else rf_evaluation
print(summary(model, evaluation = final_evaluation))

if (cdm_config$workers > 1) {
  plan(sequential)
  stopCluster(cl)
}
