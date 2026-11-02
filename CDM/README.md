# CDMr

Community Distribution Modelling in R.

## Project Structure

- `R/` contains the functions of the CDMr package.
- `man/` contains their help pages, made from the comments in `R/` with
  `devtools::document()`.
- `../analysis/scripts/` contains user-editable analysis scripts and project settings.
- `../analysis/data/` contains user data files.
- `../analysis/results/` contains analysis outputs; the analysis scripts create it.

Projects and their settings (input files, column names, sample filter, and
analysis options) are defined in `../analysis/scripts/config.R` and combined by
`setup_project()`. Results are saved in `../analysis/results/<project_name>/`.
Required packages are installed with `../analysis/scripts/install_libraries.R`.
The steps of the user workflow are described in `../README.md`.
