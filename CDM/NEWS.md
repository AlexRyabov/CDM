# CDMr 0.0.0.9000

* `setup_project(project_name, default_settings, projects)` replaces
  `complete_config()`. It combines base settings with the settings of the
  selected project, rejects settings not defined in `default_settings`, adds
  full input and result file paths, and creates the results directory.
* `filter_cdm_data()` also accepts a quoted condition, such as
  `quote(Depth < 105)` stored in project settings.

* `map()` and `cluster()` are renamed to `map_comm()` and `cluster_comm()` to
  avoid conflicts with functions from other packages.
* Arguments use snake_case consistently: `map_options(method, metric, k_min,
  dm_variables)`, `cluster_options(method, k)`, `diffmap(X, metric, k_min,
  dm_variables)`, and `cdm_ml_cv(cdm_data, n_folds, ...)`.
* `update_community_type(cdm_data, clustered_map)` no longer saves files; the
  analysis script saves the updated community data explicitly.
* `cluster_comm()` and `plot_map()` align samples by explicit sample IDs.
* Tests use testthat; developer scripts for the MATLAB comparison are in `dev/`.
* License: MIT.
