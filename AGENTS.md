# Working Instructions for the CDM R Project

## Project purpose

This repository develops an R package and a transparent user analysis workflow
for Community Distribution Modelling (CDM). The project turns community
composition data and environmental observations into interpretable community
types and predictive models.

The main analytical sequence is:

1. Import and validate raw species-abundance and environmental data.
2. Preprocess the data into a central community-data object.
3. Embed communities in a lower-dimensional space, initially with diffusion
   maps but with support for alternative methods.
4. Cluster the embedded samples to define community types.
5. Characterize the composition, prevalence, and ecological properties of the
   resulting community types.
6. Train machine-learning models that predict community-type probabilities from
   environmental conditions.
7. Use the fitted models for prediction, interpretation, partial dependence,
   and, where supported, expected community composition.

Scientific correctness and a clear user experience are equally important.

## Repository layout

in the root folder you can create your artifacts for testing. 
The project in not on GitHub yet

The repository has two distinct parts:

```text
CDM/                       R package source
analysis/                  User analysis project
  scripts/                 Sequential user-facing R scripts and config
    install_libraries.R    Installs all project packages (run once, not sourced)
    config.R               Project selection, settings, and package loading
    005_import_data.R      Import, validate, and save community data
    010_embed.R            Build and save the embedding
    020_cluster.R          Cluster the embedding and save community types
    040_community_composition.R  Characterize composition of community types
    .......
  data/                    Input data
  results/                 Generated and saved results
```
results have a subfolders with the project name, where results for each project 
are stored. 

Treat names and capitalization as significant.

### `CDM/`

This directory contains reusable package code. Package functions must be
general and must not contain Fram Strait-specific filenames, absolute paths,
column names, depth filters, or other project-specific assumptions unless they
are explicit function arguments.

### `analysis/`

This directory contains the concrete user workflow. Project-specific choices
belong here, especially in `analysis/scripts/config.R` or in clearly identified
analysis steps.

- `analysis/scripts/config.R` is the single source of truth for the project name,
  project options, input-file paths, output paths, sheet names, and relevant
  column names. The `fram_strait` project name is defined there; scripts must
  not redefine it.
- `config.R` follows a fixed structure: package loading, `load_all()` of the
  CDMr package, selection of the active project (`project_name`),
  `default_settings`, the `projects` registry, and a final call
  `cdm_config <- setup_project(project_name, default_settings, projects)`.
  `default_settings` is the base list of all settings, not a user project; it
  holds the data and results paths, input files (relative to `paths$data`),
  column names, the sample filter (a quoted condition or `NULL`), and the
  embedding, clustering, and composition options. Each entry of `projects`
  specifies only the settings that differ from `default_settings` and may
  change only fields that already exist there; `setup_project()` reports unknown
  fields and unknown project names. New project options must be added to
  `default_settings` with a comment, and scripts read them from `cdm_config`
  instead of hard-coding values. Do not duplicate the current setting values
  in documentation.
- Packages are loaded only in `config.R`, with simple `library()` calls at
  its beginning. It loads every package needed to run the analysis
  scripts, including those used only inside CDMr functions, so a missing
  package is reported at once. Analysis scripts call package functions without
  `package::` and do not call `library()`, `requireNamespace()`, or install
  packages. Package code in `CDM/R/` keeps using `package::` and declares its
  dependencies in `CDM/DESCRIPTION`.
- `analysis/scripts/install_libraries.R` is a separate user script that installs
  all packages of the project with `Rscript install_libraries.R`; it is never
  sourced automatically. The current list of project packages is `here`,
  `devtools`, `readxl`, `ggplot2`, `patchwork`, `mclust`, `viridisLite`,
  `testthat`, and `roxygen2`. When a dependency is added, update this list in
  `install_libraries.R`, the `library()` calls in `config.R` if the analysis
  needs it at run time,
  `CDM/DESCRIPTION` if package code uses it, and this file.
- The top-level `README.md` describes the user workflow step by step; update it
  when scripts, steps, or settings change.
- `analysis/scripts/` contains user-facing scripts that run in a clear,
  sequential order. Current scripts are intended to be run with
  `analysis/scripts/` as the working directory, because they source
  `config.R` by filename. Each script must be understandable on its own and
  runnable from a fresh R session after its prerequisites have been completed.
- `analysis/data/` contains source/input data. Do not modify or overwrite data
  unless the user explicitly requests it.
- `analysis/results/` contains generated artifacts. Heavy computations must save
  their results here through paths defined by `config.R`.

## Architecture and data flow

Keep the distinction between source data, working data, and fitted artifacts
clear.

- Excel or other user files remain the source of truth.
- The import script reads those files, validates sample identifiers, applies
  project preprocessing, and constructs the central community-data object.
- A processed RDS file is a cache/checkpoint, not a replacement source of truth.
- Every sample must have a stable, explicit sample identifier. Never rely only
  on row order to align abundance, environmental, embedding, or clustering
  data.
- Community-type assignments are small per-sample annotations and should be
  readily accessible together with community data.
- The complete fitted embedding, clustering object, diagnostics, model, and
  interpretation results remain separate artifacts. Do not copy all upstream
  data into every downstream object.
- Heavy stages such as embedding, clustering, model tuning, fitting, and partial
  dependence must be independently saveable and reloadable.
- Results of each project are stored in `analysis/results/<project_name>/`,
  so results of different projects are never mixed. Saved derived objects
  (embedding, clustering) keep their sample IDs and options; downstream
  functions must check that sample IDs match the community data before
  combining them.

The intended dependency structure is:

```text
raw data -> community data -> embedding -> clustering/community types
                         + community types -> community characterization
environment + community types -> machine-learning model -> predictions/effects
community profiles + type probabilities -> predicted composition (optional)
```

## User-facing code

User scripts are part of the product. Optimize them for clarity, not cleverness.

- Use short, explicit, sequential code.
- Use descriptive verb-based function names.
- Prefer one obvious way to perform each common operation.
- Keep package internals out of analysis scripts.
- Do not require users to manually copy objects or repeat paths between scripts.
- Source `config.R` from `analysis/scripts/` rather than hard-coding project
  names or paths.
- Use `file.path()` or configured paths; never use machine-specific absolute
  paths or `setwd()` inside package functions.
- Use standard R conventions such as `plot()` and `predict()` methods where
  appropriate.
- Provide informative validation errors, especially for missing columns,
  duplicated sample IDs, unmatched samples, invalid abundance values, and
  incompatible saved artifacts.
- Preserve data-frame structure explicitly where needed, for example with
  `drop = FALSE`.

Analysis scripts should normally follow a numbered workflow such as:

```text
005_import_data.R
010_embed.R
020_cluster.R
040_community_composition.R
050_fit_model.R
060_model_interpretation.R
070_prediction.R
```

Do not force all stages into a single script. A completed heavy stage must be
reusable without recomputing earlier stages.

## Implementation standard

Avoide too simple helper functions. If a function contains 1-2 statements 
it can be included in the code. 

If there were some problems or uncertainties during coding, you need 
to report them in output and describe in detail how you solved them 

Do not solve structural problems with superficial patches, accumulating special
cases, or duplicated code. If the existing design cannot support the requested
behavior cleanly, refactor or rewrite the affected component. A coherent
solution is preferred even when it takes longer.

At the same time, keep the scope controlled:

- Inspect the relevant package functions, analysis scripts, configuration, and
  tests before changing behavior.
- Preserve unrelated user work and unrelated behavior.
- Update all affected callers when an interface changes.
- Remove obsolete paths or implementations after a replacement is complete;
  do not leave two competing workflows without a documented reason.
- Do not retain a confusing interface solely to avoid editing downstream code,
  unless backward compatibility is an explicit requirement.
- Do not stop at a partial workaround, an unexplained TODO, or code that only
  works for the current example dataset.

## Package development

- Put reusable logic in `CDM/R/`, not in user scripts.
- Keep project-specific import decisions and filters in `analysis/`.
- Document exported functions and important object classes.
- Declare dependencies in the package metadata and use namespaces explicitly.
- Avoid hidden global state and side effects in package functions.
- Keep plotting, prediction, validation, and persistence behavior consistent
  across object classes.
- Add or update tests whenever package behavior changes.
- Test edge cases involving missing values, duplicate IDs, unmatched samples,
  one-column subsets, empty subsets, and factor levels in prediction data.

## Validation before completion

For every substantive change:

1. Run the most relevant focused tests.
2. Run broader package checks when feasible.
3. Run or parse the affected analysis script from a clean session with
   `analysis/scripts/` as the working directory when feasible.
4. Confirm that no input path or project name was hard-coded outside
   `config.R`.
5. Confirm that sample alignment is based on explicit identifiers.
6. Confirm that heavy results can be saved and loaded by the following script.
7. Update documentation and examples when the user-facing workflow changes.

Do not claim completion without stating what was validated and what could not be
validated in the current environment.

## Decision making

When requirements are genuinely ambiguous and different choices would alter the
scientific result or public API, ask the user. Otherwise, make the smallest
coherent design decision consistent with these instructions, implement it
fully, and explain the resulting user workflow clearly.
