# CDMr 0.0.0.9000

## New data format and explicit inputs

Saved results of earlier versions (`cdm_data.rds` and all later files) do
not work with this version; run the analysis scripts again from
`005_import_data.R`.

* `cdm_data` holds plain data frames with samples in rows and no row names:
  `abundance` and `relative_abundance` (`sample_id`, then one column per
  taxon named by its `taxon_id`), `env` (`sample_id`, then the environmental
  variables), and `taxa` (`taxon_id`, then the taxonomy; it replaces
  `species_names`). The field `sample_names` is removed. IDs are stored as
  text; numeric IDs are written without scientific notation.
* New `transpose_abundance(x, taxon_id = NULL)` turns a species table with
  taxa in rows into a table with samples in rows.
* `prepare_data(abundance, env, taxa = NULL, sample_id = "sample_id",
  taxon_id = "taxon_id")` has new arguments. It matches the tables by the
  named ID columns, or by order when an ID argument is `NULL`.
* `subset(cdm_data, condition)` replaces `filter_cdm_data()`.
* Functions take the parts of `cdm_data` they need instead of the whole
  object: `map_comm(x, options)`, `update_community_type(env,
  clustered_map)`, `plot_map(cdm_map, data, x, y, color)`,
  `community_deviation_indexes(x, taxa, group_by, ...)`, and
  `plot_community_by_type(composition, data, community_type)`. A column
  `sample_id` identifies the samples; tables without it are matched by order.
* `map_comm()` returns the coordinates with a `sample_id` column;
  `cluster_comm()` returns the community types as a table (`sample_id`,
  `community_type`). `community_deviation_indexes()` returns one row per
  sample and one column per taxonomic group.
* `evaluate_model()` is renamed to `cross_validate_model()`, because it
  estimates the prediction quality and does not return a fitted model.
* Models work like scikit-learn: `cross_validate_model(x, y, cv, options)` and
  `fit_model(x, y, cv, options)` take a predictor table `x`, the classes `y`,
  and folds `cv` made from the same rows. `model_options()` no longer has
  `predictors` and `target`; `n_predictors` is replaced by
  `n_predictors_min` (default 1) and `n_predictors_max` (default all columns
  of `x`). `predict(model, newdata)` returns one row per row of `newdata`.
* `make_cv_folds()`, `predict()`, and `pd2D_find()` name samples by the
  column `sample_id`, or by row numbers.
* Functions that take data frames also accept tibbles.
* `pd2D_find(model, data, grid)` takes both grids in one named list instead
  of `pred1` and `pred2`. `plot()` of its result replaces
  `pd2D_plot(pd2D_mostprobable(pd))`; `pd2D_plot()` is removed.
* ggplot2, mclust, patchwork, and viridisLite are now required packages
  (`Imports`), so they are installed with CDMr.

* Two-dimensional partial dependence: `pd2D_find()` (probabilities of all
  community types on a grid of two predictors, averaged over the samples of
  `data`, in parallel with future), `pd2D_mostprobable()`, and `pd2D_plot()`
  (map of the most probable type with probability contours, observed samples,
  and a mask outside their convex hull). `setup_project()` adds the result
  file `partial_dependence`.

* Community-type models: `model_options()`, `evaluate_model()` (nested,
  blocked cross-validation with predictor selection, SMOTE-NC, and tuning
  inside every training set), `fit_model()`, `predict()`, and
  `compare_models()`, with `print()`, `summary()`, and `plot()` methods.
  Engines: SVM (`e1071::svm()`, libsvm) and random forest (`ranger`). The
  predicted class is the model's own decision (one-vs-one voting, majority
  over trees); probabilities are libsvm's Platt probabilities combined over
  the class pairs, or ranger's tree frequencies. There is no additional
  probability calibration. kernlab is not used: in kernlab 0.9.33 `couple()`
  fills the upper and lower triangles of the pairwise probability matrix in
  different pair orders, so class probabilities are wrong for four or more
  classes.
* `make_cv_folds(data, group, v, repeats, balance, strata)` replaces
  `cdm_ml_cv()`. Like `rsample::group_vfold_cv()` it returns a standard
  `rset`; groups stay together, folds are balanced by strata and, with
  `balance = "observations"`, by size; `v = "leave_block_out"` gives
  leave-one-group-out (or leave-one-out without `group`). The stratum may vary
  within a group. `evaluate_model()` and `fit_model()` accept any `rset` and
  reject folds that hold out a sample twice or split a
  group (as `rsample::group_vfold_cv()` 1.3.2 can with `strata` and `pool > 0`).
* `setup_project()` adds the result files `cv_folds`, `svm_evaluation`,
  `rf_evaluation`, and `model`.

* `setup_project(project_name, default_settings, projects)` replaces
  `complete_config()`. It combines base settings with the settings of the
  selected project, rejects settings not defined in `default_settings`, adds
  full input and result file paths, and creates the results directory.
* `filter_cdm_data()` (now `subset()`) also accepts a quoted condition, such as
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
