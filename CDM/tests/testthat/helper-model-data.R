# Synthetic community data for cross-validation and model tests: 8 sites
# (blocks) with 20 samples each, the month is constant within a site, and the
# community type depends on `x1`. `x3` is almost identical to `x1`.
make_model_test_data <- function(n_sites = 8L, per_site = 20L, seed = 1L) {
  withr::with_seed(seed, {
    n <- n_sites * per_site
    sample_ids <- sprintf("s%03d", seq_len(n))
    site <- rep(sprintf("site%d", seq_len(n_sites)), each = per_site)
    month <- rep(rep(c(6, 7, 8, 9), length.out = n_sites), each = per_site)
    x1 <- stats::runif(n)
    community_type <- factor(
      ifelse(x1 < 1 / 3, "A", ifelse(x1 < 2 / 3, "B", "C")),
      levels = c("A", "B", "C")
    )
    env <- data.frame(
      Sample = sample_ids,
      site = site,
      month = month,
      x1 = x1,
      x2 = stats::rnorm(n),
      x3 = x1 + stats::rnorm(n, sd = 0.01),
      habitat = sample(c("shelf", "slope"), n, replace = TRUE),
      community_type = community_type,
      stringsAsFactors = FALSE
    )
    abundance <- as.data.frame(matrix(stats::rpois(2L * n, 5), nrow = 2L,
                                      dimnames = list(NULL, sample_ids)))
    prepare_data(data.frame(species = c("sp1", "sp2")), abundance, env)
  })
}

make_fast_options <- function(engine = "svm", ...) {
  model_options(
    predictors = c("x1", "x2", "x3", "habitat"),
    engine = engine,
    n_predictors = 1:2,
    grid_size = 1L,
    inner_v = 2L,
    seed = 1L,
    ...
  )
}
