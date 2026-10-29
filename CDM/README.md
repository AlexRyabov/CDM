# CDM

Community Distribution Modelling in R.

## Project Structure

- `R/` contains functions that are part of the CDMr package.
- `tests/testthat/` (local only, not in git) contains package tests; run them with `devtools::test()`.
- `dev/` (local only, not in git) contains developer scripts for the MATLAB comparison.
- `../analysis/scripts/` contains user-editable analysis scripts and project settings.
- `../analysis/data/` contains user data files.
- `../analysis/results/` contains analysis outputs.

Projects and their settings (input files, column names, sample filter, and
analysis options) are defined in `../analysis/scripts/config.R` and combined by
`setup_project()`. Results are saved in `../analysis/results/<project_name>/`.
Required packages are installed with `../analysis/scripts/install_libraries.R`.
The steps of the user workflow are described in `../README.md`.
