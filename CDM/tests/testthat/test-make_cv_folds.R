test_that("make_cv_folds() returns an rset that keeps groups together", {
  env <- make_model_test_data()$env

  cv <- withr::with_seed(1, make_cv_folds(env, group = site, v = 4, repeats = 2, strata = month))

  expect_s3_class(cv, "rset")
  expect_s3_class(cv, "cdm_group_vfold_cv")
  expect_named(cv, c("splits", "id", "id2"))
  expect_identical(nrow(cv), 8L)
  expect_identical(attr(cv, "group"), "site")
  expect_identical(attr(cv, "strata"), "month")
  expect_identical(attr(cv, "balance"), "observations")

  for (split in cv$splits) {
    test_sites <- unique(rsample::assessment(split)$site)
    expect_length(intersect(test_sites, unique(rsample::analysis(split)$site)), 0L)
  }
  info <- rset_folds(cv, rownames(env))
  expect_identical(nrow(info$folds), 2L * nrow(env))
  expect_identical(rownames(rsample::assessment(cv$splits[[1]])), info$folds$sample_id[info$folds$repeat_id == 1 & info$folds$fold == 1])
})

test_that("make_cv_folds() balances strata and fold sizes", {
  env <- make_model_test_data()$env

  # Each month occurs in two sites, so balanced folds hold one site per month.
  cv <- withr::with_seed(3, make_cv_folds(env, group = "site", v = 2, strata = "month"))
  months <- sapply(cv$splits, function(split) table(rsample::assessment(split)$month))
  expect_true(all(months == 20L))

  # Groups of very different size: balance by rows or by groups.
  env$site[env$site %in% c("site1", "site2", "site3")] <- "big"
  by_rows <- withr::with_seed(1, make_cv_folds(env, group = site, v = 2))
  expect_identical(sort(sapply(by_rows$splits, function(s) nrow(rsample::assessment(s)))), c(80L, 80L))
  by_groups <- withr::with_seed(1, make_cv_folds(env, group = site, v = 2, balance = "groups"))
  expect_identical(sort(sapply(by_groups$splits, function(s) length(unique(rsample::assessment(s)$site)))), c(3L, 3L))
})

test_that("make_cv_folds() is reproducible with set.seed()", {
  env <- make_model_test_data()$env

  cv_1 <- withr::with_seed(5, make_cv_folds(env, group = site, v = 3, strata = month))
  cv_2 <- withr::with_seed(5, make_cv_folds(env, group = site, v = 3, strata = month))
  expect_identical(rset_folds(cv_1, rownames(env))$folds, rset_folds(cv_2, rownames(env))$folds)
})

test_that("make_cv_folds() creates leave-block-out, leave-one-out, and random folds", {
  env <- make_model_test_data()$env

  leave_site_out <- make_cv_folds(env, group = site, v = "leave_block_out")
  expect_identical(nrow(leave_site_out), 8L)
  expect_true(all(sapply(leave_site_out$splits, function(s) length(unique(rsample::assessment(s)$site))) == 1L))

  small <- env[1:6, ]
  leave_one_out <- make_cv_folds(small, v = "leave_block_out")
  expect_identical(nrow(leave_one_out), 6L)
  expect_true(all(sapply(leave_one_out$splits, function(s) nrow(rsample::assessment(s))) == 1L))

  random <- withr::with_seed(1, make_cv_folds(env, v = 5, strata = month))
  expect_s3_class(random, "vfold_cv")
  expect_identical(nrow(rset_folds(random, rownames(env))$folds), nrow(env))
})

test_that("make_cv_folds() validates its inputs", {
  env <- make_model_test_data()$env

  expect_error(make_cv_folds(env, group = site, v = 9), "between 2 and the number of groups \\(8\\)")
  expect_error(make_cv_folds(env, group = cruise, v = 2), "no group column 'cruise'")
  expect_error(make_cv_folds(env, group = site, v = "leave_one_out"), "number or \"leave_block_out\"")
  expect_error(make_cv_folds(env, group = site, v = "leave_block_out", strata = month), "strata cannot be used")
  expect_error(make_cv_folds(env, group = site, v = "leave_block_out", repeats = 2), "repeats must be 1")
  expect_error(make_cv_folds(env, group = site, v = 1), "v must be a single integer")

  env$site[3] <- NA
  expect_error(make_cv_folds(env, group = site, v = 2), "missing or empty values in 1 rows")
})

test_that("rset_folds() rejects overlapping folds and split groups", {
  env <- make_model_test_data()$env
  cv <- withr::with_seed(1, make_cv_folds(env, group = site, v = 2))

  expect_error(rset_folds(cv, rownames(env)[-1]), "must be the samples of cdm_data")

  # The second split also holds out the first sample of the first split.
  broken <- cv
  first_test <- rsample::complement(cv$splits[[1]])[1]
  second <- cv$splits[[2]]
  broken$splits[[2]] <- rsample::make_splits(
    list(analysis = setdiff(second$in_id, first_test), assessment = c(rsample::complement(second), first_test)),
    data = env
  )
  expect_error(rset_folds(broken, rownames(env)), "held out exactly once")

  rows <- seq_len(nrow(env))
  split_group <- rsample::manual_rset(
    list(
      rsample::make_splits(list(analysis = rows[rows > 5], assessment = rows[rows <= 5]), data = env),
      rsample::make_splits(list(analysis = rows[rows <= 5], assessment = rows[rows > 5]), data = env)
    ),
    ids = c("Fold1", "Fold2")
  )
  attr(split_group, "group") <- "site"
  expect_error(rset_folds(split_group, rownames(env)), "groups of 'site' are split between folds: 1 site1")
})

test_that("summary() of folds reports sizes and strata", {
  env <- make_model_test_data()$env
  cv <- withr::with_seed(1, make_cv_folds(env, group = site, v = 2, strata = month))

  cv_summary <- summary(cv)

  expect_s3_class(cv_summary, "summary.cdm_group_vfold_cv")
  expect_identical(cv_summary$folds$n_train + cv_summary$folds$n_test, c(160L, 160L))
  expect_identical(cv_summary$strata$split, c("Fold1", "Fold2", "all rows"))
  expect_output(print(cv_summary), "Stratum proportions")
})
