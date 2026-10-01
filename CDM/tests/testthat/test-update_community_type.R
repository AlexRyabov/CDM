test_that("update_community_type() adds community types by sample ID without saving", {
  species_names <- data.frame(species = c("sp1", "sp2"))
  abundance <- data.frame(sample_a = c(1, 2), sample_b = c(3, 4), check.names = FALSE)
  env <- data.frame(Sample = c("sample_a", "sample_b"), depth = c(10, 20))
  cdm_data <- prepare_data(species_names, abundance, env)

  clustered_map <- structure(
    list(community_type = factor(c(sample_b = "type_2", sample_a = "type_1"))),
    class = "cdm_map"
  )

  updated_data <- update_community_type(cdm_data, clustered_map)

  expect_s3_class(updated_data, "cdm_data")
  expect_identical(as.character(updated_data$env$community_type), c("type_1", "type_2"))
  expect_s3_class(updated_data$env$community_type, "factor")
})

test_that("update_community_type() rejects unmatched or unnamed community types", {
  species_names <- data.frame(species = c("sp1", "sp2"))
  abundance <- data.frame(sample_a = c(1, 2), sample_b = c(3, 4), check.names = FALSE)
  env <- data.frame(Sample = c("sample_a", "sample_b"), depth = c(10, 20))
  cdm_data <- prepare_data(species_names, abundance, env)

  unmatched_map <- structure(
    list(community_type = factor(c(sample_a = "type_1", sample_c = "type_3"))),
    class = "cdm_map"
  )
  expect_error(
    update_community_type(cdm_data, unmatched_map),
    "Clustered map samples must match"
  )

  unnamed_map <- structure(
    list(community_type = factor(c("type_1", "type_2"))),
    class = "cdm_map"
  )
  expect_error(update_community_type(cdm_data, unnamed_map), "named by sample ID")
})
