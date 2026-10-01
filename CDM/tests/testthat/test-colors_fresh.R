test_that("colors_fresh() returns stable named colors", {
  expect_identical(
    unname(colors_fresh(3, order = "convenient")),
    c("#E6194B", "#3CB44B", "#FFC819")
  )

  community_types <- factor(
    c("type_2", "type_1", "type_2"),
    levels = c("type_1", "type_2", "type_3")
  )
  type_colors <- colors_fresh(community_types, order = "convenient")
  expect_identical(names(type_colors), levels(community_types))
  expect_length(type_colors, 3L)
})

test_that("colors_fresh() supports RGB output and large palettes", {
  rgb_colors <- colors_fresh(2, order = "convenient", format = "rgb")
  expect_true(is.matrix(rgb_colors))
  expect_identical(dim(rgb_colors), c(2L, 3L))
  expect_true(all(rgb_colors >= 0 & rgb_colors <= 1))

  many_colors <- colors_fresh(25)
  expect_length(many_colors, 25L)
  expect_true(all(grepl("^#[0-9A-F]{6}$", many_colors)))
})

test_that("colors_fresh() validates its inputs", {
  expect_error(colors_fresh(0), "positive integer")
  expect_error(colors_fresh(3, order = "unknown"), "'arg' should be one of", fixed = TRUE)
})
