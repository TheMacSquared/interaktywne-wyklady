testthat::test_that("bootstrap jest odtwarzalny i zachowuje kontrakt wyniku", {
  h <- load_module_helpers("07-symulacje-statystyczne/modules/helpers.R")
  x <- c(2, 4, 5, 9, 10)

  a <- h$run_bootstrap(x, mean, B = 200, seed = 123)
  b <- h$run_bootstrap(x, mean, B = 200, seed = 123)

  testthat::expect_equal(a$boot_stats, b$boot_stats)
  testthat::expect_equal(a$observed, mean(x))
  testthat::expect_length(a$boot_stats, 200)
  testthat::expect_equal(a$B, 200)
  testthat::expect_equal(a$se, stats::sd(a$boot_stats))
})

testthat::test_that("przedziały bootstrapowe mają poprawne granice", {
  h <- load_module_helpers("07-symulacje-statystyczne/modules/helpers.R")
  boot <- list(boot_stats = 1:100, observed = 60)

  percentile <- h$bootstrap_ci_percentile(boot, conf_level = 0.90)
  basic <- h$bootstrap_ci_basic(boot, conf_level = 0.90)
  q <- unname(stats::quantile(1:100, c(0.05, 0.95)))

  testthat::expect_equal(c(percentile$lower, percentile$upper), q)
  testthat::expect_equal(c(basic$lower, basic$upper),
                         c(2 * boot$observed - q[2], 2 * boot$observed - q[1]))
  testthat::expect_gt(percentile$upper, percentile$lower)
  testthat::expect_gt(basic$upper, basic$lower)
})

testthat::test_that("Wilson zachowuje poprawny zakres przy skrajnej proporcji", {
  h <- load_module_helpers("07-symulacje-statystyczne/modules/helpers.R")
  ci <- h$classical_ci_proportion(phat = 0, n = 20, conf_level = 0.95)
  wilson <- ci[ci$method == "Wilson", ]

  testthat::expect_gte(wilson$lower, 0)
  testthat::expect_lte(wilson$upper, 1)
  testthat::expect_gt(wilson$upper, wilson$lower)
})

testthat::test_that("jackknife średniej odtwarza klasyczny błąd standardowy", {
  h <- load_module_helpers("07-symulacje-statystyczne/modules/helpers.R")
  x <- c(3, 4, 8, 11, 14, 15)
  result <- h$run_jackknife(x, mean)

  testthat::expect_equal(result$observed, mean(x))
  testthat::expect_equal(result$bias, 0, tolerance = 1e-12)
  testthat::expect_equal(result$se, stats::sd(x) / sqrt(length(x)),
                         tolerance = 1e-12)
})

testthat::test_that("aktualizacja beta-dwumianowa ma poprawne parametry", {
  h <- load_module_helpers("08-metody-bayesowskie/modules/helpers.R")
  posterior <- h$beta_binomial_posterior(
    successes = 7, trials = 10, alpha_prior = 2, beta_prior = 3,
    grid_size = 300
  )

  testthat::expect_true(all(posterior$alpha_post == 9))
  testthat::expect_true(all(posterior$beta_post == 6))
  testthat::expect_equal(max(posterior$prior), 1)
  testthat::expect_equal(max(posterior$likelihood), 1)
  testthat::expect_equal(max(posterior$posterior), 1)
})

testthat::test_that("HDI zawiera zadaną część uporządkowanych próbek", {
  h <- load_module_helpers("08-metody-bayesowskie/modules/helpers.R")
  samples <- seq(-3, 3, length.out = 1001)
  interval <- h$hdi_from_samples(samples, prob = 0.80)
  inside <- mean(samples >= interval["lower"] & samples <= interval["upper"])

  testthat::expect_true(all(is.finite(interval)))
  testthat::expect_lte(interval["lower"], interval["upper"])
  testthat::expect_gte(inside, 0.80)
  testthat::expect_lt(inside, 0.81)
})
