# Install all packages used by the CDMr package and the analysis scripts.
# Run once from analysis/scripts: Rscript install_libraries.R
# This script is not sourced by config.R or by the analysis scripts.

packages <- c(
  # Analysis workflow
  "here",
  "devtools",
  "readxl",
  # CDMr plots and clustering
  "ggplot2",
  "patchwork",
  "mclust",
  "viridisLite",
  # Package development
  "testthat",
  "roxygen2"
)

installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(packages, installed_packages)

if (length(missing_packages) == 0L) {
  message("All project packages are already installed.")
} else {
  install.packages(missing_packages, repos = "https://CRAN.R-project.org")
}
