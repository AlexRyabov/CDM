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

1. Install the required packages once. From `analysis/scripts/`:

   ```text
   Rscript install_libraries.R
   ```

2. Select the project and its settings in `analysis/scripts/config.R`.
   `default_settings` contains the base settings: input files, column names,
   the sample filter, and the embedding, clustering, and composition options.
   Model settings are defined at the top of `050_fit_model.R`.
   `projects` lists the analysis projects; each project changes only the
   settings that differ from the defaults. The active project is selected with
   `project_name <- "fram_strait"`. `config.R` also loads all packages and the
   CDMr package functions; every analysis script sources it.

3. Put the input files in `analysis/data/<project_name>/`.

4. Run the analysis scripts in order with `analysis/scripts/` as the working
   directory. Each script loads the results of the previous steps, so a
   completed step does not have to be repeated:

   ```r
   source("005_import_data.R")              # once, or when the input files change
   source("010_embed.R")                    # after importing data
   source("020_cluster.R")                  # after the embedding
   source("040_community_composition.R")    # after clustering
   source("050_fit_model.R")                # after clustering; takes long
   source("060_plot_PD.R")                  # after the final model
   ```

   or from a terminal:

   ```text
   Rscript 005_import_data.R
   ```

Results are saved in `analysis/results/<project_name>/`: `cdm_data.rds`
(community data, including `env$community_type` after clustering),
`embedding.rds`, `clustering.rds`, and from `050_fit_model.R` the folds
`cv_folds.rds`, the model evaluations `evaluation_svm.rds` and
`evaluation_rf.rds`, and the final model `model.rds`, and from
`060_plot_PD.R` the partial dependence `partial_dependence.rds`.

## Community-type models

`050_fit_model.R` predicts the probabilities of the community types from
environmental variables. Its settings — target, predictors, numbers of folds,
and model options — are defined in the list `model_settings` at the top of the
script. The predictors must not contain missing values; impute them before
this step.

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
   reproducible folds. The samples are identified by the row names of
   `cdm_data$env`; for other data, such as a tibble, give the column with the
   sample IDs in `sample_id`.
2. `model_options()` defines a support vector machine (`e1071::svm()`,
   libsvm); a random forest (`ranger`) is obtained with
   `update(svm_options, engine = "rf")`. Inside every training set the best
   `n_predictors` predictors are selected, the classes are balanced with
   SMOTE-NC, and the number of predictors and the engine parameters are
   tuned. The predicted community type is the model's own decision
   (one-vs-one voting for the SVM, majority over trees for the random
   forest). The probabilities are libsvm's Platt probabilities combined over
   the class pairs (similar to `'FitPosterior'` in MATLAB, whose default
   pair coupling differs, so the values do not match exactly) or the
   forest's tree frequencies. With SMOTE-NC they refer to balanced classes,
   not to the observed frequencies of the types.
3. `evaluate_model()` estimates the prediction quality with nested
   cross-validation: the inner folds are made with the same group, strata,
   and balance, and tuning and predictor selection use only the training part
   of each outer fold. Folds that hold out a sample more than
   once or split a group are rejected. The main metric is the macro recall
   (mean recall over community types). `summary()` lists the metrics, the
   classes in every fold, the parameters and predictors selected in every
   outer fold, and the problems found, for example classes missing from a
   training part; `plot()` shows the confusion matrix and the recall per
   class. `compare_models()` compares the SVM and the random forest on the
   same folds.
4. `fit_model()` fits the final model with the engine `final_engine` on all
   samples. `predict(model, newdata)` returns the predicted community type
   and the probability of every community type for each sample ID. `newdata`
   is a `cdm_data` object or a data frame or tibble; without sample IDs as row
   names, give their column in `sample_id`.

Functions that take data frames also accept tibbles.

The evaluation is the longest step: its duration grows with `v`, `inner_v`,
the number of values in `n_predictors`, and `grid_size`. The tuning can run
in parallel R processes: set `workers` in `config.R` to more than 1. The
tuning consists of `inner_v * length(n_predictors)` independent tasks (for
example 5 * 3 = 15), so the most efficient number of workers is the number of
tasks or a divisor of it, at most the number of CPU cores. The results do not
depend on the number of workers. If `050_fit_model.R` is interrupted, stop the
worker processes with `stopCluster(cl)`.

## Partial dependence maps

`060_plot_PD.R` shows how the probabilities of the community types depend on
two predictors of the final model (now temperature and day of year), in
separate maps for depth layers. Its settings (`pd_settings`) are at the top of
the script.

1. `pd2D_find(model, data, pred1, pred2)` computes the partial dependence on a
   grid of the two predictors: at every grid point both predictors are set in
   all samples of `data` (here the samples of one depth layer), the
   probabilities of all types are predicted, and they are averaged over the
   samples. The other predictors keep their observed values.
2. `pd2D_mostprobable(pd)` finds the most probable type at every grid point
   and its probability.
3. `pd2D_plot(most_probable, points = ...)` draws the map of the most probable
   type, contour lines of its probability, the observed samples colored by
   their type, and a grey mask outside the convex hull of the samples, where
   the model has no data.

The partial dependence is saved in `partial_dependence.rds` and reused if the
model, the settings, and the samples are unchanged; set `recompute <- TRUE` to
compute it again. The calculation runs in parallel with `workers` from
`config.R`; it needs `n_grid^2` predictions for every sample of a layer.
With SMOTE-NC (`imbalance = "smotenc"` in `050_fit_model.R`) the
probabilities refer to balanced types, not to their observed frequencies.

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
