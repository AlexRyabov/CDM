# Sets a future plan with two worker processes for the rest of the calling
# test. Under devtools::test() CDMr is not installed, so every worker loads
# the package source when it starts; under R CMD check the package is
# installed and a multisession plan is enough.
local_two_workers <- function(env = parent.frame()) {
  source_dir <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(source_dir, "DESCRIPTION"))) {
    cluster <- parallelly::makeClusterPSOCK(
      2L,
      rscript_startup = bquote(suppressMessages(pkgload::load_all(.(source_dir), quiet = TRUE)))
    )
    withr::defer(parallel::stopCluster(cluster), envir = env)
    future::plan(future::cluster, workers = cluster)
  } else {
    future::plan(future::multisession, workers = 2L)
  }
  withr::defer(future::plan(future::sequential), envir = env)
}
