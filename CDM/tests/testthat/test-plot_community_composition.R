test_that("plot_community_composition() draws one box per group", {
  skip_if_not_installed("ggplot2")

  species_names <- data.frame(Phylum = c("p1", "p1", "p1", "p2", "p2"))
  abundance <- data.frame(
    sample_a = c(4, 0, 0, 1, 0),
    sample_b = c(0, 0, 2, 0, 0),
    sample_c = c(0, 0, 0, 1, 1),
    check.names = FALSE
  )
  env <- data.frame(Sample = names(abundance), depth = c(10, 20, 30))
  cdm_data <- prepare_data(species_names, abundance, env)

  composition <- community_deviation_indexes(
    cdm_data, group_by = "Phylum", measure = "abundance", scale = "absolute"
  )
  composition_plot <- plot_community_composition(composition)

  expect_s3_class(composition_plot, "ggplot")
  expect_identical(levels(composition_plot$data$group), c("p1", "p2"))
  expect_identical(nrow(composition_plot$data), 6L)
  expect_identical(
    as.character(composition_plot$data$sample),
    rep(c("sample_a", "sample_b", "sample_c"), 2L)
  )
  expect_equal(composition_plot$data$value, c(0.8, 1, 0, 0.2, 0, 1))
  expect_s3_class(composition_plot$layers[[1L]]$geom, "GeomBoxplot")

  relative_plot <- plot_community_composition(community_deviation_indexes(
    cdm_data, group_by = "Phylum", measure = "abundance", scale = "relative"
  ))
  expect_identical(nrow(relative_plot$data), 6L)
  expect_true(all(is.finite(relative_plot$data$value)))

  invalid_composition <- composition
  class(invalid_composition) <- NULL
  expect_error(
    plot_community_composition(invalid_composition),
    "returned by community_deviation_indexes"
  )

  misaligned_composition <- composition
  names(misaligned_composition$reference) <- rev(names(misaligned_composition$reference))
  expect_error(plot_community_composition(misaligned_composition), "must match")
})
