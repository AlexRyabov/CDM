test_that("plot_community_by_type() draws one panel per community type", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  species_names <- data.frame(Phylum = c("p1", "p1", "p2"))
  abundance <- data.frame(
    sample_a = c(10, 0, 0),
    sample_b = c(2, 0, 8),
    sample_c = c(0, 0, 10),
    sample_d = c(4, 0, 6),
    check.names = FALSE
  )
  env <- data.frame(
    Sample = names(abundance),
    community_type = factor(c("type_1", "type_1", "type_2", "type_2"))
  )
  cdm_data <- prepare_data(species_names, abundance, env)
  composition_data <- community_deviation_indexes(
    cdm_data, group_by = "Phylum", measure = "abundance", scale = "absolute",
    max_groups = 2L
  )

  community_plot <- plot_community_by_type(cdm_data, composition_data)

  expect_s3_class(community_plot, "patchwork")
  expect_identical(attr(community_plot, "community_types"), c("type_1", "type_2"))
  expect_length(attr(community_plot, "plots"), 2L)

  type_1_data <- attr(community_plot, "plots")$type_1$data
  expect_identical(unique(type_1_data$sample), c("sample_a", "sample_b"))
  expect_equal(type_1_data$value, c(0, 0.8, 1, 0.2))

  type_2_data <- attr(community_plot, "plots")$type_2$data
  expect_identical(unique(type_2_data$sample), c("sample_c", "sample_d"))
  expect_equal(type_2_data$value, c(1, 0.6, 0, 0.4))

  no_type_data <- cdm_data
  no_type_data$env$community_type <- NULL
  expect_error(plot_community_by_type(no_type_data, composition_data), "community_type")

  misaligned_composition <- composition_data
  misaligned_composition$deviation <- misaligned_composition$deviation[, -1L, drop = FALSE]
  expect_error(
    plot_community_by_type(cdm_data, misaligned_composition),
    "deviation columns must match"
  )
})
