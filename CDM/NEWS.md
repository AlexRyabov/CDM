# CDMr 0.0.0.9000

First development version. This file describes the current functions. Saved
results of earlier development states (`cdm_data.rds` and all later files)
do not work with this version; run the analysis scripts again from
`005_import_data.R`.

## Project settings

* `setup_project(project_name, default_settings, projects)` combines base
  settings with the settings of the selected project, rejects settings not
  defined in `default_settings`, adds full input and result file paths, and
  creates the results directory. Result files: `community_data`,
  `embedding`, `clustering`, `cv_folds`, `svm_evaluation`, `rf_evaluation`,
  `model`, and `partial_dependence`.

## Community data

* `cdm_data` holds plain data frames with samples in rows and no row names:
  `abundance` and `relative_abundance` (`sample_id`, then one column per
  taxon named by its `taxon_id`), `env` (`sample_id`, then the environmental
  variables), and `taxa` (`taxon_id`, then the taxonomy). IDs are stored as
  text; numeric IDs are written without scientific notation.
* `transpose_abundance(x, taxon_id = NULL)` turns a species table with taxa
  in rows into a table with samples in rows.
* `prepare_data(abundance, env, taxa = NULL, sample_id = "sample_id",
  taxon_id = "taxon_id")` checks the tables and matches them by the named ID
  columns, or by order when an ID argument is `NULL`.
* `subset(cdm_data, condition)` keeps the samples selected by a condition on
  the columns of `env`, also a quoted one such as `quote(Depth < 105)` stored
  in the project settings.
* Functions take the parts of `cdm_data` they need, not the whole object. A
  column `sample_id` identifies the samples; tables without it are matched
  by order. Functions that take data frames also accept tibbles.

## Embedding and community types

* `map_options()` and `map_comm(x, options)` embed the samples with diffusion
  maps (`diffmap()`, `diffmap_similarity()`, `diffmap_eigv()`). The result
  has the coordinates `dm_1`, `dm_2`, ... with a `sample_id` column.
* `cluster_options()` and `cluster_comm(cdm_map, options)` cluster the
  embedded samples with mclust and return the community types as a table
  (`sample_id`, `community_type`). `update_community_type(env,
  clustered_map)` adds them to `env`.
* `plot_map(cdm_map, data, x, y, color)` plots the embedding;
  `colors_fresh()` gives distinct colors for the community types.

## Community composition

* `community_deviation_indexes(x, taxa, group_by, ...)` gives, for every
  sample and taxonomic group, the deviation of the abundance or richness from
  a reference. `plot_community_by_type(composition, data, community_type)`
  and `plot_community_composition()` plot them.

## Community-type models

* `make_cv_folds(data, group, v, repeats, balance, strata)` returns a
  standard `rsample` object, like `rsample::group_vfold_cv()`. Groups stay
  together, folds are balanced by strata and, with `balance =
  "observations"`, by size; `v = "leave_block_out"` gives
  leave-one-group-out (or leave-one-out without `group`). The stratum may
  vary within a group.
* Models work like scikit-learn: `model_options()` holds the method settings,
  `cross_validate_model(x, y, cv, options)` estimates the prediction quality
  with nested, blocked cross-validation (predictor selection, SMOTE-NC, and
  tuning inside every training set), `fit_model(x, y, cv, options)` tunes
  and fits the final model, and `predict(model, newdata)` returns one row
  per row of `newdata`. `compare_models()` compares evaluations made on the
  same folds. `print()`, `summary()`, and `plot()` methods are provided.
* `cross_validate_model()` and `fit_model()` accept any `rset` and reject
  folds that hold out a sample twice or split a group (as
  `rsample::group_vfold_cv()` 1.3.2 can with `strata` and `pool > 0`).
* Engines: SVM (`e1071::svm()`, libsvm) and random forest (`ranger`). The
  predicted class is the model's own decision (one-vs-one voting, majority
  over trees); probabilities are libsvm's Platt probabilities combined over
  the class pairs, or ranger's tree frequencies. There is no additional
  probability calibration. kernlab is not used: in kernlab 0.9.33 `couple()`
  fills the upper and lower triangles of the pairwise probability matrix in
  different pair orders, so class probabilities are wrong for four or more
  classes.

## Partial dependence

* `pd2D_find(model, data, grid)` computes the probabilities of all community
  types on a grid of two predictors, given as a named list, averaged over the
  samples of `data`, in parallel with future. `plot()` of the result draws
  the most probable type (`pd2D_mostprobable()`) with probability contours,
  the observed samples, and a mask outside their convex hull.

## Packages

* ggplot2, mclust, patchwork, viridisLite, and the modelling packages are
  required packages (`Imports`), so they are installed with CDMr. future,
  parallelly, and pkgload are needed only for parallel runs.
* License: MIT.

## Renamed and removed functions

For scripts written for earlier development states:

* `filter_cdm_data()` is replaced by `subset()`.
* `map()` and `cluster()` are renamed to `map_comm()` and `cluster_comm()`.
* `complete_config()` is replaced by `setup_project()`.
* `cdm_ml_cv()` is replaced by `make_cv_folds()`.
* `evaluate_model()` is renamed to `cross_validate_model()`.
* `pd2D_plot()` is removed; use `plot()` of the result of `pd2D_find()`.
* `pd2D_find()` takes both grids in one named list instead of `pred1` and
  `pred2`.
* `cdm_data$species_names` is replaced by `taxa`; `cdm_data$sample_names` is
  removed. `model_options()` no longer has `predictors`, `target`, and
  `n_predictors`; use `x`, `y`, `n_predictors_min`, and `n_predictors_max`.
