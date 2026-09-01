env <- new.env(parent = globalenv())
sys.source(file.path(risk_root, "R", "risk_math.R"), envir = env)

testthat::test_that("Bayes i prawdopodobieństwo całkowite mają poprawne granice", {
  testthat::expect_equal(env$risk_bayes(.01, .95, .05), .0095 / .059, tolerance = 1e-12)
  testthat::expect_equal(env$risk_bayes(1, .8, .2), 1)
  testthat::expect_true(is.na(env$risk_bayes(0, 1, 0)))
  testthat::expect_equal(env$risk_total_probability(.2, .4, .1), .16)
  testthat::expect_error(env$risk_bayes(-.1, .9, .1))
})

testthat::test_that("modele liczby prób odpowiadają funkcjom bazowym R", {
  testthat::expect_equal(env$risk_binomial_probability(100, .02, 2, "exactly"), dbinom(2, 100, .02))
  testthat::expect_equal(env$risk_binomial_probability(100, .02, 1, "at_least"), 1 - (1 - .02)^100)
  testthat::expect_equal(env$risk_at_least_one(1, .2), .2)
  testthat::expect_equal(env$risk_negative_binomial_total_pmf(10, 3, .1), dnbinom(7, 3, .1))
  testthat::expect_equal(env$risk_negative_binomial_finish(40, 3, .1), pnbinom(37, 3, .1))
  testthat::expect_equal(env$risk_negative_binomial_total_pmf(2, 3, .1), 0)
})

testthat::test_that("ogon normalny i funkcje czasu życia są spójne", {
  testthat::expect_equal(env$risk_normal_exceedance(85, 82, 3), pnorm(1, lower.tail = FALSE))
  testthat::expect_equal(env$risk_stress_strength_normal(0, 1, 0, 1), .5)
  e <- env$risk_exponential(c(0, 1000), 1 / 1500)
  testthat::expect_equal(e$cdf + e$reliability, c(1, 1))
  testthat::expect_equal(e$hazard, rep(1 / 1500, 2))
  w <- env$risk_weibull(c(0, 1000), 2, 1700)
  testthat::expect_equal(w$cdf + w$reliability, c(1, 1))
})

testthat::test_that("systemy i bramki FTA respektują logikę", {
  testthat::expect_equal(env$risk_series_reliability(c(.9, .8)), .72)
  testthat::expect_equal(env$risk_parallel_reliability(c(.9, .8)), .98)
  testthat::expect_equal(env$risk_gate_and(c(.1, .2)), .02)
  testthat::expect_equal(env$risk_gate_or(c(.1, .2)), .28)
  testthat::expect_equal(env$risk_fta_top(.005, .05, .08), .005 * (1 - .95 * .92))
  testthat::expect_equal(env$risk_common_cause_reliability(.98, .01), .9702)
  testthat::expect_equal(
    env$risk_common_cause_reliability(env$risk_parallel_reliability(c(.92, .95)), .01),
    .99 * (1 - .08 * .05)
  )
})
