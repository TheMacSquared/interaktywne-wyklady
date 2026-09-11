testthat::test_that("losowe p-wartości zachowują granice i rozdzielczość", {
  h <- load_module_helpers("01-symulacje-statystyczne/modules/helpers.R")
  d <- h$generate_two_groups_data(50, effect = 20, dist = "normal", seed = 1)
  for (alternative in c("two.sided", "greater")) {
    result <- h$run_permutation_test_twosample(d, B = 1000, seed = 1,
                                              alternative = alternative)
    testthat::expect_equal(result$p_value, 1 / 1001)
    testthat::expect_equal(result$extreme_count, 0)
    testthat::expect_equal(unname(result$mc_interval[1]), 0)
    testthat::expect_gt(result$mc_interval[2], result$p_value)
  }
  result <- h$run_permutation_test_twosample(d, B = 1000, seed = 1,
                                            alternative = "less")
  testthat::expect_equal(result$p_value, 1)
  # Wszystkie permutacje remisują: p musi być równe 1.
  tied <- data.frame(value = rep(1, 8), group = factor(rep(c("A", "B"), each = 4)))
  testthat::expect_equal(h$run_permutation_test_twosample(tied, B = 99)$p_value, 1)
  correlation <- h$run_permutation_test_correlation(data.frame(x = 1:20, y = 1:20),
                                                   B = 99, seed = 1)
  testthat::expect_equal(correlation$p_value, .01)
  null <- h$run_mc_null(rep(1, 50), "proportion", B = 99, seed = 1)
  testthat::expect_equal(null$p_value_mc, .01)
  testthat::expect_gt(null$mc_interval[2], 0)
})

testthat::test_that("widget permutacji pokazuje precyzję i resetuje wynik po zmianie danych", {
  testthat::skip_if_not_installed("callr")
  result <- callr::r(function(path) {
    setwd(path)
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    shiny::testServer(app$ch4_server, {
      session$setInputs(ch4_n_per_group = 50, ch4_true_diff = 20, ch4_dist = "normal",
                        ch4_n_perms = 200)
      set.seed(1)
      session$setInputs(ch4_perm_step1 = 1)
      session$setInputs(ch4_perm_step3 = 1)
      stopifnot(ch4_perm_res()$p_value > 0)
      stopifnot(grepl("b + 1", output$ch4_perm_result$html, fixed = TRUE))
      session$setInputs(ch4_true_diff = 0)
      stopifnot(is.null(ch4_perm_res()), ch4_step() == 0)
      session$setInputs(ch4_cor_n = 30, ch4_cor_true_r = .5, ch4_cor_B = 200,
                        ch4_cor_run = 1)
      stopifnot(grepl("b + 1", output$ch4_cor_result$html, fixed = TRUE))
    })
    TRUE
  }, args = list(path = file.path(stat_root, "01-symulacje-statystyczne")), timeout = 60)
  testthat::expect_true(result)
})
