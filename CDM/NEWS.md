# CDMr 0.0.0.9000

* Functions that take data frames also accept tibbles: `prepare_data()`,
  `make_cv_folds()`, `predict()` for `cdm_model`, `pd2D_find()`, and
  `pd2D_plot()`. Tibbles are converted to plain data frames; all parts of a
  `cdm_data` object are plain data frames.
* `make_cv_folds()` gains `sample_id`, the column with the sample IDs, for
  data without sample IDs as row names, such as tibbles. `make_cv_folds()`
  and `predict()` now stop with an error for data without sample IDs instead
  of using row numbers as IDs.

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
  within a group. `evaluate_model()` and `fit_model()` accept any `rset` made
  from `cdm_data$env` and reject folds that hold out a sample twice or split a
  group (as `rsample::group_vfold_cv()` 1.3.2 can with `strata` and `pool > 0`).
* `model_options()` has a `target` argument.
* `setup_project()` adds the result files `cv_folds`, `svm_evaluation`,
  `rf_evaluation`, and `model`.

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
