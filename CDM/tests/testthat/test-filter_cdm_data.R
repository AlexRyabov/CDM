make_filter_test_data <- function() {
  species_names <- data.frame(species = c("sp1", "sp2"))
  abundance <- data.frame(
    sample_a = c(1, 2),
    sample_b = c(3, 4),
    sample_c = c(5, 6),
    check.names = FALSE
  )
  env <- data.frame(
    Sample = c("sample_a", "sample_b", "sample_c"),
    Depth = c(10, 50, NA),
    Lat = c(76, 78, 80),
    community_type = factor(c("type_1", "type_2", "type_1"))
  )
  cdm_data <- prepare_data(species_names, abundance, env)

  # Sample components may arrive in different orders but are aligned by ID.
  cdm_data$env <- cdm_data$env[c("sample_c", "sample_a", "sample_b"), , drop = FALSE]
  cdm_data$abundance <-
    cdm_data$abundance[, c("sample_b", "sample_c", "sample_a"), drop = FALSE]
  cdm_data$relative_abundance <-
    cdm_data$relative_abundance[, c("sample_c", "sample_a", "sample_b"), drop = FALSE]
  cdm_data
}

test_that("filter_cdm_data() keeps selected samples in all sample-level fields", {
  cdm_data <- make_filter_test_data()
  max_depth <- 20

  shallow_data <- filter_cdm_data(cdm_data, Depth <= max_depth)

  expect_s3_class(shallow_data, "cdm_data")
  expect_identical(shallow_data$sample_names, "sample_a")
  expect_identical(rownames(shallow_data$env), "sample_a")
  expect_identical(names(shallow_data$abundance), "sample_a")
  expect_identical(names(shallow_data$relative_abundance), "sample_a")
  expect_identical(nrow(shallow_data$species_names), 2L)

  type_data <- filter_cdm_data(cdm_data, community_type == "type_1")
  expect_identical(type_data$sample_names, c("sample_a", "sample_c"))
})

test_that("filter_cdm_data() accepts quoted conditions", {
  cdm_data <- make_filter_test_data()
  settings <- list(sample_filter = quote(Depth < max_depth))
  max_depth <- 20

  quoted_data <- filter_cdm_data(cdm_data, settings$sample_filter)
  expect_identical(quoted_data$sample_names, "sample_a")

  expect_error(
    filter_cdm_data(cdm_data, quote(UnknownColumn > 0)),
    "Could not evaluate condition"
  )
})

test_that("filter_cdm_data() treats missing values as FALSE and allows empty results", {
  cdm_data <- make_filter_test_data()

  known_depth_data <- filter_cdm_data(cdm_data, Depth < 100)
  expect_identical(known_depth_data$sample_names, c("sample_a", "sample_b"))

  empty_data <- filter_cdm_data(cdm_data, Lat > 90)
  expect_length(empty_data$sample_names, 0L)
  expect_identical(nrow(empty_data$env), 0L)
  expect_identical(ncol(empty_data$abundance), 0L)
  expect_identical(ncol(empty_data$relative_abundance), 0L)
})

test_that("filter_cdm_data() validates conditions and sample alignment", {
  cdm_data <- make_filter_test_data()

  expect_error(filter_cdm_data(cdm_data, Depth[1]), "one logical value per sample")
  expect_error(filter_cdm_data(cdm_data, UnknownColumn > 0), "Could not evaluate condition")

  misaligned_data <- cdm_data
  names(misaligned_data$abundance)[1] <- "unknown_sample"
  expect_error(
    filter_cdm_data(misaligned_data, Depth < 100),
    "must match cdm_data$sample_names",
    fixed = TRUE
  )
})
