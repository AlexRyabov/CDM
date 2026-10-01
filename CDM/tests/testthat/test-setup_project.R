make_test_settings <- function(results_dir) {
  list(
    description = "",
    paths = list(data = "data_dir", results = results_dir),
    files = list(species = "species.xlsx", environment = "environment.xlsx"),
    sample_filter = NULL,
    embedding = list(method = "diffmap", metric = "jaccard", k_min = "sqrt")
  )
}

test_that("setup_project() applies project settings and adds file paths", {
  results_dir <- file.path(tempdir(), "cdm_setup_project_test")
  unlink(results_dir, recursive = TRUE)
  projects <- list(
    other = list(description = "Other project"),
    example = list(
      description = "Example project",
      files = list(species = "example_species.xlsx"),
      sample_filter = quote(Depth < 105),
      embedding = list(k_min = 20)
    )
  )

  settings <- setup_project("example", make_test_settings(results_dir), projects)

  expect_identical(settings$project_name, "example")
  expect_identical(settings$description, "Example project")
  expect_identical(settings$files$species, file.path("data_dir", "example_species.xlsx"))
  expect_identical(settings$files$environment, file.path("data_dir", "environment.xlsx"))
  expect_identical(settings$files$community_data, file.path(results_dir, "cdm_data.rds"))
  expect_identical(settings$files$embedding, file.path(results_dir, "embedding.rds"))
  expect_identical(settings$files$clustering, file.path(results_dir, "clustering.rds"))
  expect_identical(settings$sample_filter, quote(Depth < 105))
  expect_identical(
    settings$embedding,
    list(method = "diffmap", metric = "jaccard", k_min = 20)
  )
  expect_true(dir.exists(results_dir))

  default_project <- setup_project("other", make_test_settings(results_dir), projects)
  expect_null(default_project$sample_filter)
  expect_identical(default_project$embedding$k_min, "sqrt")
})

test_that("setup_project() reports unknown projects and settings", {
  default_settings <- make_test_settings(tempdir())
  projects <- list(example = list(embedding = list(kmin = 20), sample_filtr = NULL))

  expect_error(
    setup_project("missing", default_settings, projects),
    "Unknown project 'missing'. Available projects: example."
  )
  expect_error(
    setup_project("example", default_settings, projects),
    "not defined in default_settings: embedding$kmin, sample_filtr.",
    fixed = TRUE
  )
  expect_error(
    setup_project("example", default_settings, list(example = list(list(k_min = 2)))),
    "not defined in default_settings: [[1]]",
    fixed = TRUE
  )
  expect_error(
    setup_project(c("a", "b"), default_settings, projects),
    "single non-empty string"
  )
  expect_error(
    setup_project("example", c(default_settings, project_name = "x"), projects),
    "must not contain project_name"
  )
})

test_that("setup_project() validates paths and input files", {
  default_settings <- make_test_settings(tempdir())

  missing_input <- default_settings
  missing_input$files$environment <- NULL
  expect_error(
    setup_project("example", missing_input, list(example = list())),
    "'files' is missing: environment"
  )

  expect_error(
    setup_project("example", default_settings, list(example = list(files = list(species = NA_character_)))),
    "'files$species' must be a single non-empty string",
    fixed = TRUE
  )

  reserved_input <- default_settings
  reserved_input$files$embedding <- "embedding.rds"
  expect_error(
    setup_project("example", reserved_input, list(example = list())),
    "must not use the result file names: embedding"
  )
})
