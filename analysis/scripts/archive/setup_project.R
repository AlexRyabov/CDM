# Project setup for CSDM analyses
#
# This file is a user-editable project configuration script. It is not part of
# the CDMr package API. Edit the paths below to point to input data and result
# folders for each analysis project.
#
# Source this file from an analysis script, then call:
#
# cfg <- setup_project("fram_strait")

setup_project <- function(project_id) {
  # Find the analysis root for interactive use from the repository,
  # analysis, or package folder.
  analysis_root <- getOption("CDMr.analysis_root")
  if (is.null(analysis_root)) {
    cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    if (basename(cwd) == "scripts" && basename(dirname(cwd)) == "analysis") {
      analysis_root <- dirname(cwd)
    } else if (basename(cwd) == "analysis") {
      analysis_root <- cwd
    } else if (dir.exists(file.path(cwd, "analysis"))) {
      analysis_root <- file.path(cwd, "analysis")
    } else if (basename(cwd) == "CDM" && dir.exists(file.path(dirname(cwd), "analysis"))) {
      analysis_root <- file.path(dirname(cwd), "analysis")
    } else {
      stop(
        "Could not locate the analysis folder. Run this script from the project folder.",
        call. = FALSE
      )
    }
  }

  analysis_root <- normalizePath(analysis_root, winslash = "/", mustWork = TRUE)
  project_root <- normalizePath(file.path(analysis_root, ".."), winslash = "/", mustWork = TRUE)
  package_root <- getOption("CDMr.package_root", file.path(project_root, "CDM"))

  # Define available analysis projects.
  projects <- list(
    # Use this if you want to define data folder relative to your scripts.
    fram_strait = list(
      # Define source files
      files = list(
        species = file.path(analysis_root, "data/processed/ASVNoMetazoaJ", "ASVNoMetazoaJ_Spec_norm.xlsx"),
        env = file.path(analysis_root, "data/processed/ASVNoMetazoaJ", "ASVNoMetazoaJ_env.xlsx")
      ),
      # Folder to save results.
      dirs = list(
        results = file.path(analysis_root, "results", project_id)
      ),
      # Data processing options.
      options = list(
        diffmap = list(
          metric = "jaccard"
        ),
        preprocessing = list(
          rename_calanus = TRUE,
          split_syndiniales = TRUE,
          filter_metazoa = TRUE,
          remove_undefined_taxa = TRUE,
          group_by = "Species"
        )
      )
    ),
    # Use this style if you define an absolute path to your data.
    proj1 = list(
       # Define source files
      files = list(
        env = "C:/mydata/proj1/dfEnv.csv",
        species = "C:/mydata/proj1/dfSpecies.csv"
      ),
      # Folder to save results.
      dirs = list(
        results = "C:/mydata/proj1/results"
      ),
      # Data processing options.
      options = list(
        diffmap = list(
          metric = "spearman"
        ),
        preprocessing = list(
          rename_calanus = FALSE,
          split_syndiniales = FALSE,
          filter_metazoa = FALSE,
          remove_undefined_taxa = FALSE,
          group_by = "ASV"
        )
      )
    )
  )

  # Validate requested project ID.
  if (!project_id %in% names(projects)) {
    available_projects <- paste(names(projects), collapse = ", ")
    stop(
      "Unknown project_id '", project_id, "'. Available project IDs: ",
      available_projects,
      call. = FALSE
    )
  }

  # Add standard output folders under the project-specific results directory.
  cfg <- projects[[project_id]]
  cfg$project <- list(
    id = project_id,
    root = analysis_root,
    package_root = normalizePath(package_root, winslash = "/", mustWork = TRUE)
  )
  cfg$dirs$figures <- file.path(cfg$dirs$results, "figures")
  cfg$dirs$tables <- file.path(cfg$dirs$results, "tables")
  cfg$dirs$models <- file.path(cfg$dirs$results, "models")

  output_dirs <- unlist(cfg$dirs, use.names = FALSE)

  # Create output folders when needed.
  for (output_dir in output_dirs) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Warn about missing input files without stopping project setup.
  input_files <- unlist(cfg$files, use.names = TRUE)
  for (input_name in names(input_files)) {
    if (!file.exists(input_files[[input_name]])) {
      warning(
        input_name, " does not exist: ", input_files[[input_name]],
        call. = FALSE
      )
    }
  }

  cfg
}

# Example:
# source("setup_project.R")
# cfg <- setup_project("fram_strait")
# df_env <- read.csv(cfg$files$env)
# df_species <- read.csv(cfg$files$species)
