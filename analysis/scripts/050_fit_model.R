# Predict community-type probabilities from environmental conditions.
# Requires 020_cluster.R, which stores community_type in the community data.
source(here::here("analysis", "scripts", "config.R"))

# Model settings ----------------------------------------------------------------

model_settings <- list(
  target = "community_type",
  # Environmental columns used as predictors; they must not contain NA.
  predictors = c(
    "Depth", "Month_doy", "Lon", "temp_imputed"
#    "Depth", "Month_doy", "Lat", "Lon", "sea_ice_monthly","temp_imputed", "mod_aq_par_imputed"
  ),
  v = 5L,                       # outer folds, or "leave_block_out"
  repeats = 1L,                 # repeated fold assignments
  grid_size = 30L,              # engine parameter combinations tried in the tuning
  final_engine = "svm",         # engine of the final model: "svm" or "rf"
  seed = 1L
)
# All other model settings use the defaults of model_options(), for example
# 1 to all predictors, SMOTE-NC, macro recall, and 5 inner folds; see
# ?model_options to change them.

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

# Data and cross-validation folds -----------------------------------------------

cdm_data <- readRDS(cdm_config$files$community_data)

# Predictors and classes, as X and y in scikit-learn.
x <- cdm_data$env[model_settings$predictors]
y <- cdm_data$env[[model_settings$target]]

# All samples of a cruise are held out together; folds are balanced by the
# season (Month_doy) and by the number of samples. The folds are made from the
# same rows as x and y.
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
# this block is optiona, it compares SVM vs random forest ------------------------

svm_options <- model_options(
  engine = "svm",
  grid_size = model_settings$grid_size,
  seed = model_settings$seed
)
rf_options <- update(svm_options, engine = "rf")

# Nested cross-validation of both models (the longest step).
svm_evaluation <- cross_validate_model(x, y, cv, svm_options)
saveRDS(svm_evaluation, cdm_config$files$svm_evaluation)

rf_evaluation <- cross_validate_model(x, y, cv, rf_options)
saveRDS(rf_evaluation, cdm_config$files$rf_evaluation)

print(compare_models(svm = svm_evaluation, rf = rf_evaluation))
print(summary(svm_evaluation))
print(summary(rf_evaluation))
print(plot(svm_evaluation))
print(plot(rf_evaluation))

# Final model -------------------------------------------------------------------
# as model comparison for this data shows that SVM is better than RF we fit SVM as the final model------

final_options <- update(svm_options, engine = model_settings$final_engine)
model <- fit_model(x, y, cv, final_options)
saveRDS(model, cdm_config$files$model)

final_evaluation <- if (identical(model_settings$final_engine, "svm")) svm_evaluation else rf_evaluation
print(summary(model, evaluation = final_evaluation))

if (cdm_config$workers > 1) {
  plan(sequential)
  stopCluster(cl)
}
