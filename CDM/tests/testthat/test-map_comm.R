make_map_test_data <- function() {
  x <- as.matrix(read.csv(test_path("fixtures", "random_matrix_20x10_rowsum1.csv")))
  sample_ids <- sprintf("sample_%02d", seq_len(nrow(x)))
  abundance <- as.data.frame(t(x))
  names(abundance) <- sample_ids
  species_names <- data.frame(taxon = colnames(x))
  env <- data.frame(Sample = rev(sample_ids), gradient = rev(seq_along(sample_ids)))
  prepare_data(species_names, abundance, env)
}

test_that("map_comm() returns diffusion map coordinates named by sample ID", {
  cdm_data <- make_map_test_data()
  options <- map_options(metric = "spearman", k_min = 10, dm_variables = 3)

  map_result <- map_comm(cdm_data, options)

  expect_s3_class(map_result, "cdm_map")
  expect_identical(rownames(map_result$map), cdm_data$sample_names)
  expect_identical(map_result$sample_names, cdm_data$sample_names)
  expect_named(map_result$map, c("dm_1", "dm_2", "dm_3"))
  expect_identical(map_result$options, options)

  direct <- diffmap(
    t(as.matrix(cdm_data$relative_abundance)),
    metric = "spearman",
    k_min = 10,
    dm_variables = 3
  )
  expect_equal(unname(as.matrix(map_result$map)), unname(direct$DM))
  expect_equal(map_result$eigen_values, direct$eigv)
})

test_that("map_options() validates its inputs", {
  expect_identical(map_options()$k_min, "sqrt")
  expect_identical(map_options()$metric, "jaccard")
  expect_error(map_options(k_min = -1), "k_min must be")
  expect_error(map_options(dm_variables = 0), "dm_variables must be")
  expect_error(map_options(metric = "unknown"), "'arg' should be one of", fixed = TRUE)
  expect_error(map_comm(make_map_test_data(), list(method = "diffmap")), "map_options")
})

test_that("cluster_comm() assigns community types by sample ID", {
  skip_if_not_installed("mclust")
  cdm_data <- make_map_test_data()
  map_result <- map_comm(cdm_data, map_options(metric = "spearman", k_min = 10, dm_variables = 2))

  clustered_map <- cluster_comm(map_result, cluster_options(k = 2))

  expect_s3_class(clustered_map$cluster_result, "cdm_cluster")
  expect_s3_class(clustered_map$community_type, "factor")
  expect_identical(names(clustered_map$community_type), cdm_data$sample_names)
  expect_identical(rownames(clustered_map$cluster_result$probabilities), cdm_data$sample_names)
  expect_equal(clustered_map$cluster_result$k, 2)

  updated_data <- update_community_type(cdm_data, clustered_map)
  expect_identical(
    as.character(updated_data$env$community_type),
    as.character(clustered_map$community_type[rownames(updated_data$env)])
  )
})

test_that("cluster_comm() rejects maps without matching sample IDs", {
  cdm_data <- make_map_test_data()
  map_result <- map_comm(cdm_data, map_options(metric = "spearman", k_min = 10, dm_variables = 2))

  unnamed_map <- map_result
  rownames(unnamed_map$map) <- NULL
  expect_error(cluster_comm(unnamed_map), "Row names of cdm_map$map", fixed = TRUE)

  expect_error(cluster_options(k = 0), "k must be 'best'")
  expect_error(cluster_options(k = 2.5), "k must be 'best'")
  expect_identical(cluster_options(k = c(2, 3))$k, c(2L, 3L))
})

test_that("plot_map() matches environmental variables by sample ID", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("viridisLite")
  cdm_data <- make_map_test_data()
  map_result <- map_comm(cdm_data, map_options(metric = "spearman", k_min = 10, dm_variables = 2))

  shuffled_data <- cdm_data
  shuffled_data$env <- cdm_data$env[rev(rownames(cdm_data$env)), , drop = FALSE]
  map_plot <- plot_map(map_result, shuffled_data, color = "gradient")

  expected <- cdm_data$env[rownames(map_result$map), "gradient"]
  expect_identical(map_plot$data$color_value, expected)

  named_color <- stats::setNames(seq_along(cdm_data$sample_names), rev(cdm_data$sample_names))
  named_plot <- plot_map(map_result, cdm_data, color = named_color)
  expect_identical(unname(named_plot$data$color_value), rev(seq_along(cdm_data$sample_names)))

  partial_data <- filter_cdm_data(cdm_data, gradient > 1)
  expect_error(plot_map(map_result, partial_data, color = "gradient"), "missing samples")
})
