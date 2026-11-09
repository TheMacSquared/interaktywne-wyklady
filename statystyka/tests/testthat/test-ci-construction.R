testthat::test_that("wykresy konstrukcji CI korzystają z dokładnych przedziałów i zachowują RNG", {
  env <- new.env(parent = globalenv())
  set.seed(51)
  before <- .Random.seed
  sys.source(file.path(stat_root, "04-przedzialy-ufnosci", "modules", "ci_construction.R"), env)
  testthat::expect_identical(.Random.seed, before)
  for (kind in c("mean", "diff")) {
    testthat::expect_length(env$ci_construction_bank[[kind]], 16)
    for (d in env$ci_construction_bank[[kind]]) {
      reference <- if (kind == "mean") stats::t.test(d$x) else stats::t.test(d$x, d$y)
      testthat::expect_equal(c(d$lower, d$upper), as.numeric(reference$conf.int), tolerance = 1e-10)
    }
  }
})
