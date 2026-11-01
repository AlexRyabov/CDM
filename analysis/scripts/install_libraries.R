# Install all packages used by the CDMr package and the analysis scripts.
# Run once from the project folder: Rscript analysis/scripts/install_libraries.R
# This script is not sourced by config.R or by the analysis scripts.

options(repos = c(CRAN = "https://CRAN.R-project.org"))

# Packages of the analysis workflow that CDMr itself does not use.
workflow_packages <- c("devtools", "here", "readxl")
missing_packages <- setdiff(workflow_packages, rownames(installed.packages()))
if (length(missing_packages) > 0L) {
  install.packages(missing_packages)
}

# Packages of CDMr, as listed in CDM/DESCRIPTION. dependencies = TRUE also
# installs the suggested packages for parallel runs and tests.
devtools::install_deps(here::here("CDM"), dependencies = TRUE, upgrade = "never")
