# CDMr: Community Distribution Modelling in R

This project turns community composition data and environmental observations
into community types and models that predict them. It first imports species
abundances and environmental variables and combines them into one community
data object, matching samples by their identifiers. The communities are then
embedded in a low-dimensional space with diffusion maps, and the embedded
samples are clustered into community types. The composition of each community
type is characterized by taxonomic groups. Later stages train machine-learning
models that predict community-type probabilities from environmental conditions.

The project has two parts: the reusable R package `CDMr` in `CDM/` and the user
analysis workflow in `analysis/`.

```text
CDMr/
  CDM/                         R package source (functions and documentation)
  analysis/
    scripts/
      install_libraries.R      installs all required packages (run once)
      config.R                 project selection, settings, and package loading
      005_import_data.R        import, validate, filter, and save community data
      010_embed.R              build and save the embedding
      020_cluster.R            cluster the embedding and save community types
      040_community_composition.R  composition of community types
      050_fit_model.R          evaluate and fit community-type models
      060_plot_PD.R            partial dependence maps of community types
    data/<project_name>/       input data of each project
    results/<project_name>/    saved results of each project
```

## Steps

1. Install the required packages once. From the project folder (the folder
   with `CDM/` and `analysis/`):

   ```text
   Rscript analysis/scripts/install_libraries.R
   ```

   This installs the packages that CDMr and the scripts use, not CDMr
   itself: `config.R` loads the CDMr functions from the folder `CDM/` with
   `devtools::load_all()`, so changes in `CDM/R/` take effect at once.

2. Select the project and its settings in `analysis/scripts/config.R`.
   `default_settings` contains the base settings: the data and results
   folders, input files, column names, the sample filter, the embedding,
   clustering, and composition options, and the number of parallel R
   processes (`workers`).
   Model settings are defined at the top of `050_fit_model.R`.
   `projects` lists the analysis projects; each project changes only the
   settings that differ from the defaults. The active project is selected with
   `project_name <- "fram_strait"`. `config.R` also loads all packages and the
   CDMr package functions; every analysis script sources it.

3. Put the input files in `analysis/data/<project_name>/`.

4. Run the analysis scripts in order. Each script loads the results of the
   previous steps, so a completed step does not have to be repeated. The
   scripts find `config.R` and all files from the project folder with
   `here::here()`, so the working directory can be the project folder or any
   folder inside it. In RStudio or Positron, open the project folder and run
   a script with its Source button, or:

   ```r
   source("analysis/scripts/005_import_data.R")            # once, or when the input files change
   source("analysis/scripts/010_embed.R")                  # after importing data
   source("analysis/scripts/020_cluster.R")                # after the embedding
   source("analysis/scripts/040_community_composition.R")  # after clustering
   source("analysis/scripts/050_fit_model.R")              # after clustering; takes long
   source("analysis/scripts/060_plot_PD.R")                # after the final model
   ```

   or from a terminal in the project folder:

   ```text
   Rscript analysis/scripts/005_import_data.R
   ```

   The empty file `.here` marks the project folder for `here`.

Results are saved in `analysis/results/<project_name>/`:

- `cdm_data.rds`: community data, including `env$community_type` after
  clustering;
- `embedding.rds` and `clustering.rds`;
- from `050_fit_model.R`: the folds `cv_folds.rds`, the model evaluations
  `evaluation_svm.rds` and `evaluation_rf.rds`, and the final model
  `model.rds`;
- from `060_plot_PD.R`: the partial dependence `partial_dependence.rds`.

## Community data

`005_import_data.R` builds the community data object `cdm_data`. Its tables
are plain data frames with samples (or taxa) in rows:

- `abundance` and `relative_abundance`: one row per sample, the column
  `sample_id`, then one column per taxon named by its taxon ID;
- `env`: one row per sample in the same order, `sample_id`, then the
  environmental variables (and `community_type` after `020_cluster.R`);
- `taxa`: one row per taxon, `taxon_id`, then the taxonomy.

The import has three steps:

1. The species table has taxa in rows: the taxonomy columns up to
   `columns$last_taxonomy`, then one abundance column per sample.
   `transpose_abundance(x, taxon_id)` turns the abundance columns into a table
   with samples in rows; the sample IDs are the column names, and the taxon
   columns are named by `taxon_id` (for example the `ASV` column), or
   numbered `taxon_0001`, ... if `taxon_id = NULL`. Data that already have
   samples in rows do not need this step.
2. `prepare_data(abundance, env, taxa, sample_id = "Sample", taxon_id = "ASV")`
   checks the tables and matches them. `sample_id` and `taxon_id` name the ID
   columns of `env` and `taxa` (set in `columns` in `config.R`); the tables
   are matched by these IDs. With `NULL`, the tables are matched by order and
   must have the same size.
3. `subset(cdm_data, condition)` keeps the samples selected by the project
   sample filter, for example `Depth < 105`.

The later scripts pass the parts of `cdm_data` to the package functions, for
example `map_comm(cdm_data$relative_abundance, options)`,
`update_community_type(cdm_data$env, clustered_map)`,
`plot_map(map, cdm_data$env, color = "Depth")`, and
`community_deviation_indexes(cdm_data$relative_abundance, cdm_data$taxa,
group_by = "Phylum")`. Tables are matched by the column `sample_id`; tables
without it are matched by order. Functions that take data frames also accept
tibbles.

## Community-type models

`050_fit_model.R` predicts the probabilities of the community types from
environmental variables. Its settings — target, predictors, numbers of folds,
and the few model options that differ from the defaults — are defined in the
list `model_settings` at the top of the script. The functions work like
scikit-learn: `x` is the table of predictors (`cdm_data$env[predictors]`),
`y` the vector of community types, and `cv` the cross-validation folds made
from the same rows. The predictors must not contain missing values; impute
them before this step.

1. `make_cv_folds()` splits the samples into cross-validation folds and
   returns a standard `rsample` object, like `rsample::group_vfold_cv()`:

   ```r
   cv <- make_cv_folds(cdm_data$env, group = Cruise, v = 5,
                       balance = "observations", strata = Month_doy)
   ```

   All samples of a group (here a cruise) stay in the same fold. The folds are
   balanced by the distribution of `strata` (numeric strata are binned into
   quartiles) and, with `balance = "observations"`, by the number of samples.
   `v = "leave_block_out"` makes every group its own fold; without `group`
   the folds are random (`rsample::vfold_cv()`), and `v = "leave_block_out"`
   gives leave-one-out. `summary(cv)` shows the size, the groups, and the
   strata of every fold. Call `set.seed()` before `make_cv_folds()` for
   reproducible folds. The folds keep the data they were made from, as all
   rsample objects do; the inner folds of `cross_validate_model()` use their
   group and strata columns, and the results name the samples by their
   `sample_id`.
2. `model_options()` defines a support vector machine (`e1071::svm()`,
   libsvm); a random forest (`ranger`) is obtained with
   `update(svm_options, engine = "rf")`. All settings have defaults. Inside
   every training set the best predictors are selected (every number from
   `n_predictors_min`, default 1, to `n_predictors_max`, default all columns
   of `x`, is tried), the classes are balanced with SMOTE-NC (new samples of
   the rare community types are made between their nearest neighbours; "NC"
   means that it also handles categorical predictors), and the number of
   predictors and the engine parameters are tuned. The predicted community
   type is the model's own decision (one-vs-one voting for the SVM, majority
   over trees for the random forest). The probabilities are libsvm's Platt
   probabilities (a logistic curve fitted to the SVM decision values for
   every pair of classes) combined over the class pairs (similar to
   `'FitPosterior'` in MATLAB, whose default
   pair coupling differs, so the values do not match exactly) or the
   forest's tree frequencies. With SMOTE-NC they refer to balanced classes,
   not to the observed frequencies of the types.
3. `cross_validate_model(x, y, cv, options)` estimates the prediction
   quality with nested cross-validation. The inner folds are made with the
   same group, strata, and balance, and tuning and predictor selection use
   only the training part of each outer fold. Folds that hold out a sample
   more than once or split a group are rejected. The main metric is the macro recall
   (mean recall over community types). `summary()` lists the metrics, the
   classes in every fold, the parameters and predictors selected in every
   outer fold, and the problems found, for example classes missing from a
   training part; `plot()` shows the confusion matrix and the recall per
   class. `compare_models()` compares the SVM and the random forest on the
   same folds. This step is optional: it helps to choose the engine, and
   step 4 does not need its results.
4. `fit_model(x, y, cv, options)` tunes the final model with the engine
   `final_engine` on the folds and fits it on all samples. It tunes the
   parameters again and does not use the results of step 3: the
   cross-validation only estimates how well the final model will predict new
   samples and helps to choose `final_engine`.
   `predict(model, newdata)` returns, for every row of the data frame or
   tibble `newdata` (for example `cdm_data$env`) and in the same order, the
   predicted community type and the probability of every community type.

The evaluation is the longest step: its duration grows with `v`, `inner_v`,
the number of predictors tried, and `grid_size`. The tuning can run in
parallel R processes: `workers` in `config.R` sets their number, and 1 runs
sequentially. The tuning consists of one independent task per inner fold and
number of predictors tried (for example 5 inner folds * 4 numbers of
predictors = 20 tasks), so the most efficient number of workers is the number
of tasks or a divisor of it, at most the number of CPU cores. The results do
not depend on the number of workers. If `050_fit_model.R` is interrupted,
stop the worker processes with `stopCluster(cl)`.

## Partial dependence maps

`060_plot_PD.R` shows how the probabilities of the community types depend on
two predictors of the final model, in separate maps for depth layers. The
predictors, the grid, and the depth layers are written directly in the
script.

1. `pd2D_find(model, data, grid)` computes the partial dependence on a grid
   of two predictors, given as a named list such as
   `list(temp_imputed = ..., Month_doy = ...)`. At every grid point both
   predictors are set in all samples of `data` (here the samples of one
   depth layer), the probabilities of all types are predicted, and they are
   averaged over the samples. The other predictors keep their observed
   values.
2. `plot(pd, points = ...)` draws the map of the most probable type at every
   grid point (found by `pd2D_mostprobable(pd)`), contour lines of its
   probability, the observed samples colored by their type, and a grey mask
   outside the convex hull of the samples, where the model has no data.

The partial dependence is saved in `partial_dependence.rds`. With
`compute_pd <- FALSE` the script loads it and only draws the maps. It stops
with an error if the saved partial dependence was computed with another model
(for example after `050_fit_model.R` was run again) or on another grid; then
set `compute_pd <- TRUE`. Changes of the depth layers are not checked. The
calculation runs in parallel with `workers` from `config.R`; it needs
`n_grid^2` predictions for every sample of a layer.
With SMOTE-NC (`imbalance = "smotenc"`, the default of `model_options()`)
the probabilities refer to balanced types, not to their observed frequencies.

## Adding a project

Add an entry to `projects` in `config.R` that lists only the changed settings,
for example:

```r
projects <- list(
  fram_strait = list(...),
  my_project = list(
    description = "My communities.",
    files = list(species = "species.xlsx", environment = "env.xlsx"),
    embedding = list(metric = "spearman")
  )
)
```

and select it with `project_name <- "my_project"`. A setting that is not defined
in `default_settings`, or an unknown project name, produces an error.

## Package development

Package functions are in `CDM/R/`. From the `CDM/` folder, regenerate the
documentation with `devtools::document()`.
