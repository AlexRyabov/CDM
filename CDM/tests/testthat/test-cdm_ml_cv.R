make_cv_test_data <- function() {
  species_names <- data.frame(species = c("sp1", "sp2"))
  abundance <- data.frame(
    sample_a = c(1, 0), sample_b = c(2, 1), sample_c = c(0, 3), sample_d = c(1, 1),
    sample_e = c(2, 0), sample_f = c(0, 2), sample_g = c(1, 3), sample_h = c(2, 2),
    check.names = FALSE
  )
  env <- data.frame(
    Sample = names(abundance),
    Year = c(2020, 2020, 2021, 2021, 2022, 2022, 2023, 2023),
    Month = c("Jun", "Jul", "Jun", "Aug", "Jul", "Aug", "Jun", "Aug")
  )
  prepare_data(species_names, abundance, env)
}

test_that("cdm_ml_cv() keeps blocking groups within one fold", {
  cdm_data <- make_cv_test_data()

  cv <- cdm_ml_cv(cdm_data, n_folds = 2, group_by = "Year", stratify_by = "Month")

  expect_s3_class(cv, "cdm_ml_cv")
  expect_named(cv, c("sample_folds", "group_folds", "fold_strata", "fold_summary", "score"))
  expect_identical(cv$sample_folds$sample_id, cdm_data$sample_names)
  expect_true(all(cv$sample_folds$fold %in% 1:2))
  expect_true(all(table(cv$sample_folds$fold) > 0))
  expect_identical(sort(unique(cv$group_folds$n_samples)), 2L)
  expect_identical(as.integer(sort(table(cv$group_folds$fold))), c(2L, 2L))

  folds_by_year <- split(cv$sample_folds$fold, cv$sample_folds$group)
  expect_true(all(vapply(folds_by_year, function(x) length(unique(x)) == 1L, logical(1))))
})

test_that("cdm_ml_cv() reports consistent scores and stratum tables", {
  cdm_data <- make_cv_test_data()

  cv <- cdm_ml_cv(cdm_data, n_folds = 2, group_by = "Year", stratify_by = "Month")

  expect_identical(cv$score$metric, "squared_euclidean")
  expect_true(all(cv$fold_summary$strata_distance >= 0))
  expect_equal(cv$score$total_distance, sum(cv$fold_summary$strata_distance))
  expect_equal(cv$score$mean_distance, mean(cv$fold_summary$strata_distance))

  fold_totals <- stats::aggregate(n ~ fold, cv$fold_strata, sum)
  expect_identical(
    fold_totals$n[match(cv$fold_summary$fold, fold_totals$fold)],
    cv$fold_summary$n_samples
  )
})

test_that("cdm_ml_cv() aligns named vectors by sample ID", {
  cdm_data <- make_cv_test_data()
  named_groups <- stats::setNames(cdm_data$env$Year, rownames(cdm_data$env))
  named_groups <- named_groups[rev(names(named_groups))]

  named_cv <- cdm_ml_cv(
    cdm_data,
    n_folds = 2,
    group_by = named_groups,
    stratify_by = cdm_data$env$Month
  )

  expect_identical(named_cv$sample_folds$group, as.character(cdm_data$env$Year))
})

test_that("cdm_ml_cv() creates sample-level folds without blocking", {
  cdm_data <- make_cv_test_data()

  sample_level_cv <- cdm_ml_cv(cdm_data, n_folds = 4)

  expect_identical(nrow(sample_level_cv$group_folds), length(cdm_data$sample_names))
  expect_true(all(sample_level_cv$fold_summary$n_groups == 2L))
  expect_identical(unique(sample_level_cv$fold_strata$stratum), "all")
})

test_that("cdm_ml_cv() validates its inputs", {
  cdm_data <- make_cv_test_data()

  expect_error(cdm_ml_cv(cdm_data, n_folds = 5, group_by = "Year"), "n_folds cannot be larger")
  expect_error(cdm_ml_cv(cdm_data, n_folds = 1), "n_folds must be a single integer")
  expect_error(
    cdm_ml_cv(cdm_data, stratify_by = c("a", NA, rep("b", 6))),
    "must not contain missing"
  )
})
