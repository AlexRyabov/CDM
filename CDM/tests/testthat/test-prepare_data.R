test_that("prepare_data() aligns environmental rows to abundance samples by ID", {
  species_names <- data.frame(species = c("sp1", "sp2"))
  abundance <- data.frame(sample_a = c(1, 2), sample_b = c(3, 4), check.names = FALSE)
  env <- data.frame(Sample = c("sample_b", "sample_a"), depth = c(20, 10))

  cdm_data <- prepare_data(species_names, abundance, env)

  expect_s3_class(cdm_data, "cdm_data")
  expect_identical(cdm_data$sample_names, c("sample_a", "sample_b"))
  expect_identical(rownames(cdm_data$env), c("sample_a", "sample_b"))
  expect_identical(cdm_data$env$depth, c(10, 20))
  expect_equal(unname(colSums(cdm_data$relative_abundance)), c(1, 1))

  renamed_data <- prepare_data(
    species_names,
    unname(as.matrix(abundance)),
    env,
    sample_names = c("sample_a", "sample_b")
  )
  expect_identical(names(renamed_data$abundance), c("sample_a", "sample_b"))
})

test_that("prepare_data() reports unmatched and missing sample IDs", {
  species_names <- data.frame(species = c("sp1", "sp2"))
  abundance <- data.frame(sample_a = c(1, 2), sample_b = c(3, 4), check.names = FALSE)

  expect_error(
    prepare_data(
      species_names,
      abundance,
      data.frame(Sample = c("sample_a", "sample_c"), depth = c(10, 30))
    ),
    "must exactly match.*Missing in env: sample_b.*Missing in abundance: sample_c"
  )
  expect_error(
    prepare_data(species_names, abundance, data.frame(depth = c(10, 20))),
    "explicit sample identifiers"
  )
  expect_error(
    prepare_data(
      species_names,
      abundance,
      data.frame(Site = c("sample_a", "sample_b")),
      env_sample_col = "Sample"
    ),
    "'Sample' column"
  )
  expect_error(
    prepare_data(
      species_names,
      abundance,
      data.frame(Sample = c("sample_a", "sample_a"))
    ),
    "must be unique"
  )
})

test_that("prepare_data() uses a custom sample ID column", {
  species_names <- data.frame(species = c("sp1", "sp2"))
  abundance <- data.frame(sample_a = c(1, 2), sample_b = c(3, 4), check.names = FALSE)
  env <- data.frame(Site = c("sample_b", "sample_a"), depth = c(20, 10))

  cdm_data <- prepare_data(species_names, abundance, env, env_sample_col = "Site")

  expect_identical(cdm_data$env$Site, c("sample_a", "sample_b"))
})
