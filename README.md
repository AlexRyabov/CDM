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
  CDM/                         R package source (functions, tests, documentation)
  analysis/
    scripts/
      install_libraries.R      installs all required packages (run once)
      config.R                 project selection, settings, and package loading
      005_import_data.R        import, validate, filter, and save community data
      010_embed.R              build and save the embedding
      020_cluster.R            cluster the embedding and save community types
      040_community_composition.R  composition of community types
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
   ```

   or from a terminal:

   ```text
   Rscript 005_import_data.R
   ```

Results are saved in `analysis/results/<project_name>/`: `cdm_data.rds`
(community data, including `env$community_type` after clustering),
`embedding.rds`, and `clustering.rds`.

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

Package functions are in `CDM/R/` and tests in `CDM/tests/testthat/`. From the
`CDM/` folder, run the tests with `devtools::test()` and regenerate the
documentation with `devtools::document()`.
