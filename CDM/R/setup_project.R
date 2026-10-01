#' Set up the settings of an analysis project
#'
#' Combines the base settings with the settings of the selected project, adds
#' full input and result file paths, and creates the results directory if
#' needed. A project specifies only the settings that differ from
#' `default_settings`; it may change only fields that already exist there.
#' Nested lists, such as `files` or `embedding`, are combined field by field
#' with [utils::modifyList()].
#'
#' @param project_name Name of the selected project in `projects`.
#' @param default_settings Named list of base settings. It must contain
#'   `paths$data` and `paths$results`, and `files` with at least `species`
#'   and `environment`. Input file names in `files` are relative to
#'   `paths$data`.
#' @param projects Named list of projects. Each project is a named list of
#'   settings that replace the corresponding values in `default_settings`.
#'
#' @return A list with the complete settings: `project_name`, all fields of
#'   `default_settings` with project values applied, full input file paths in
#'   `files`, and the result files `files$community_data`, `files$embedding`,
#'   and `files$clustering` in `paths$results`.
#'
#' @examples
#' default_settings <- list(
#'   description = "",
#'   paths = list(data = "data", results = file.path(tempdir(), "results")),
#'   files = list(species = "species.xlsx", environment = "environment.xlsx"),
#'   embedding = list(metric = "jaccard", k_min = "sqrt")
#' )
#' projects <- list(
#'   example = list(
#'     files = list(species = "example_species.xlsx"),
#'     embedding = list(k_min = 20)
#'   )
#' )
#' settings <- setup_project("example", default_settings, projects)
#' settings$files$species
#' settings$embedding
#'
#' @export
setup_project <- function(project_name, default_settings, projects) {
  if (!is.character(project_name) || length(project_name) != 1L ||
      is.na(project_name) || !nzchar(project_name)) {
    stop("project_name must be a single non-empty string.", call. = FALSE)
  }
  if (!is.list(default_settings) || is.null(names(default_settings)) ||
      any(!nzchar(names(default_settings)))) {
    stop("default_settings must be a named list.", call. = FALSE)
  }
  if ("project_name" %in% names(default_settings)) {
    stop(
      "default_settings must not contain project_name; it is set from the project_name argument.",
      call. = FALSE
    )
  }
  if (!is.list(projects) || is.null(names(projects)) ||
      any(!nzchar(names(projects)))) {
    stop("projects must be a named list.", call. = FALSE)
  }
  if (!project_name %in% names(projects)) {
    stop(
      "Unknown project '", project_name, "'. Available projects: ",
      paste(names(projects), collapse = ", "), ".",
      call. = FALSE
    )
  }

  project_settings <- projects[[project_name]]
  if (!is.list(project_settings)) {
    stop("Project '", project_name, "' must be a list of settings.", call. = FALSE)
  }
  unknown_fields <- find_unknown_settings(default_settings, project_settings)
  if (length(unknown_fields) > 0L) {
    stop(
      "Project '", project_name, "' has settings that are not defined in ",
      "default_settings: ", paste(unknown_fields, collapse = ", "), ".",
      call. = FALSE
    )
  }

  settings <- c(
    list(project_name = project_name),
    utils::modifyList(default_settings, project_settings)
  )

  required_fields <- list(paths = c("data", "results"), files = c("species", "environment"))
  for (group in names(required_fields)) {
    if (!is.list(settings[[group]])) {
      stop("Settings field '", group, "' must be a list.", call. = FALSE)
    }
    missing_fields <- setdiff(required_fields[[group]], names(settings[[group]]))
    if (length(missing_fields) > 0L) {
      stop(
        "Settings field '", group, "' is missing: ",
        paste(missing_fields, collapse = ", "), ".",
        call. = FALSE
      )
    }
    for (name in names(settings[[group]])) {
      value <- settings[[group]][[name]]
      if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
        stop(
          "Settings field '", group, "$", name, "' must be a single non-empty string.",
          call. = FALSE
        )
      }
    }
  }

  result_files <- list(
    community_data = file.path(settings$paths$results, "cdm_data.rds"),
    embedding = file.path(settings$paths$results, "embedding.rds"),
    clustering = file.path(settings$paths$results, "clustering.rds")
  )
  reserved_names <- intersect(names(settings$files), names(result_files))
  if (length(reserved_names) > 0L) {
    stop(
      "Input files must not use the result file names: ",
      paste(reserved_names, collapse = ", "), ".",
      call. = FALSE
    )
  }

  input_files <- lapply(settings$files, function(file) file.path(settings$paths$data, file))
  settings$files <- c(input_files, result_files)

  dir.create(settings$paths$results, recursive = TRUE, showWarnings = FALSE)

  settings
}

# Returns the fields of `overrides` that are absent from `defaults`, for
# example "embedding$kmin". Nested lists are checked field by field.
find_unknown_settings <- function(defaults, overrides, prefix = "") {
  override_names <- names(overrides)
  if (is.null(override_names)) {
    override_names <- rep("", length(overrides))
  }

  unknown_fields <- character()
  for (index in seq_along(overrides)) {
    name <- override_names[[index]]
    field <- paste0(prefix, if (nzchar(name)) name else paste0("[[", index, "]]"))
    if (!nzchar(name) || !name %in% names(defaults)) {
      unknown_fields <- c(unknown_fields, field)
    } else if (is.list(defaults[[name]]) && is.list(overrides[[index]])) {
      unknown_fields <- c(
        unknown_fields,
        find_unknown_settings(defaults[[name]], overrides[[index]], paste0(field, "$"))
      )
    }
  }

  unknown_fields
}
