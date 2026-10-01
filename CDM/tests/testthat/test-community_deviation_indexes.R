make_deviation_test_data <- function() {
  species_names <- data.frame(Phylum = c("p1", "p1", "p1", "p2", "p2"))
  abundance <- data.frame(
    sample_a = c(4, 0, 0, 1, 0),
    sample_b = c(0, 0, 2, 0, 0),
    sample_c = c(0, 0, 0, 1, 1),
    check.names = FALSE
  )
  env <- data.frame(Sample = names(abundance), depth = c(10, 20, 30))
  prepare_data(species_names, abundance, env)
}

test_that("community_deviation_indexes() returns grouped absolute contributions", {
  cdm_data <- make_deviation_test_data()

  absolute_result <- community_deviation_indexes(
    cdm_data, group_by = "Phylum", measure = "abundance", scale = "absolute"
  )

  expect_s3_class(absolute_result, "cdm_community_deviation_indexes")
  expect_named(absolute_result, c("reference", "deviation"))
  expect_identical(names(absolute_result$reference), c("p1", "p2"))
  expect_equal(unname(absolute_result$reference), c(0.8, 0.2))
  expect_identical(rownames(absolute_result$deviation), c("p1", "p2"))
  expect_identical(colnames(absolute_result$deviation), cdm_data$sample_names)
  expect_equal(
    unname(as.matrix(absolute_result$deviation)),
    matrix(c(0.8, 0.2, 1, 0, 0, 1), nrow = 2)
  )

  mean_result <- community_deviation_indexes(
    cdm_data, group_by = "Phylum", measure = "abundance", scale = "absolute",
    reference = "mean"
  )
  expect_equal(unname(mean_result$reference), c(0.6, 0.4))
})

test_that("community_deviation_indexes() computes finite relative deviations", {
  cdm_data <- make_deviation_test_data()
  absolute_result <- community_deviation_indexes(
    cdm_data, group_by = "Phylum", measure = "abundance", scale = "absolute"
  )

  relative_result <- community_deviation_indexes(
    cdm_data, group_by = "Phylum", measure = "abundance", scale = "relative"
  )

  grouped_abundance <- as.matrix(absolute_result$deviation)
  adjusted_abundance <- grouped_abundance
  adjusted_abundance[adjusted_abundance == 0] <-
    min(grouped_abundance[grouped_abundance > 0]) / 10
  adjusted_abundance <- sweep(adjusted_abundance, 2L, colSums(adjusted_abundance), "/")
  expected_relative <- log10(sweep(adjusted_abundance, 1L, absolute_result$reference, "/"))

  expect_equal(relative_result$reference, absolute_result$reference)
  expect_equal(unname(as.matrix(relative_result$deviation)), unname(expected_relative))
  expect_true(all(is.finite(as.matrix(relative_result$deviation))))

  richness_result <- community_deviation_indexes(
    cdm_data, group_by = "Phylum", measure = "richness", scale = "relative"
  )
  expect_equal(unname(richness_result$reference), c(1, 1))
  expect_equal(
    unname(as.matrix(richness_result$deviation)),
    log10(matrix(c(1, 1, 1, 0.1, 0.1, 2), nrow = 2))
  )
})

test_that("community_deviation_indexes() limits groups and validates inputs", {
  cdm_data <- make_deviation_test_data()

  limited_result <- community_deviation_indexes(
    cdm_data, group_by = "Phylum", scale = "absolute", max_groups = 1L
  )
  expect_identical(names(limited_result$reference), "p1")
  expect_identical(rownames(limited_result$deviation), "p1")

  expect_error(community_deviation_indexes(cdm_data, "Phylum", max_groups = 0L), "max_groups must be")
  expect_error(community_deviation_indexes(cdm_data, "missing_column"), "group_by column not found")
})
