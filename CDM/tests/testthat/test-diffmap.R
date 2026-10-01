test_that("diffmap() reproduces the MATLAB reference implementation", {
  x <- as.matrix(read.csv(test_path("fixtures", "random_matrix_20x10_rowsum1.csv")))
  reference <- unname(as.matrix(read.csv(test_path("fixtures", "random_matrix_dm_matlab.csv"))))

  # Variables 8-10 share one degenerate eigenvalue, so their eigenvectors are
  # not unique and are not compared.
  dm <- diffmap(x, metric = "spearman", k_min = 10L, dm_variables = 7L)

  expect_identical(dim(dm$DM), c(20L, 7L))
  expect_length(dm$eigv, 7L)
  expect_true(all(diff(dm$eigv) > 0))

  # Eigenvectors are defined up to their sign.
  column_signs <- sign(colSums(dm$DM * reference[, 1:7]))
  expect_equal(
    unname(sweep(dm$DM, 2L, column_signs, "*")),
    reference[, 1:7],
    tolerance = 1e-8
  )
})

test_that("diffmap helpers validate their inputs", {
  expect_error(diffmap_similarity(matrix(numeric(0), 0, 0)), "must not be empty")
  expect_error(diffmap_eigv(matrix(1, 2, 3)), "square matrix")
  expect_error(diffmap_eigv(diag(2), k_min = 0), "k_min must be a positive integer")
  expect_error(diffmap(diag(3), metric = "unknown"), "'arg' should be one of", fixed = TRUE)
})
