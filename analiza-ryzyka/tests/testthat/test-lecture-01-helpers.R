testthat::test_that("klasyfikacja sytuacji ryzykownej jest punktowana", {
  env <- load_lecture_helpers()
  correct <- stats::setNames(env$risk_scenario_items$correct,
                             env$risk_scenario_items$id)

  result <- env$score_risk_classification(correct)
  testthat::expect_equal(result$score, 5)
  testthat::expect_true(all(result$correct))

  correct[["peel"]] <- "event"
  result <- env$score_risk_classification(correct)
  testthat::expect_equal(result$score, 4)
  testthat::expect_false(result$correct[[1]])
})

testthat::test_that("historia Bernoulliego i częstości są deterministycznie testowalne", {
  env <- load_lecture_helpers()
  history <- env$append_bernoulli_history(
    history = c(1, 0),
    n = 3,
    probability = 0.2,
    draws = c(0, 1, 0)
  )
  testthat::expect_equal(history, c(1L, 0L, 0L, 1L, 0L))

  frequencies <- env$cumulative_frequency(history)
  testthat::expect_equal(frequencies$frequency,
                         c(1, 0.5, 1 / 3, 0.5, 0.4))
  testthat::expect_error(env$append_bernoulli_history(integer(), 1, -0.1))
  testthat::expect_error(env$append_bernoulli_history(integer(), 0, 0.1))
})

testthat::test_that("definicja klasyczna obsługuje wartości brzegowe", {
  env <- load_lecture_helpers()
  testthat::expect_equal(env$classical_probability(0, 24), 0)
  testthat::expect_equal(env$classical_probability(24, 24), 1)
  testthat::expect_equal(env$classical_probability(6, 24), 0.25)
  testthat::expect_error(env$classical_probability(25, 24))
  testthat::expect_error(env$classical_probability(1, 0))

  grid <- env$build_pallet_grid(6, total = 24, columns = 6)
  testthat::expect_equal(nrow(grid), 24)
  testthat::expect_equal(sum(grid$favourable), 6)
})

testthat::test_that("działania na zdarzeniach zachowują liczebność przestrzeni", {
  env <- load_lecture_helpers()
  counts <- env$event_set_counts(total = 100, n_a = 30, n_b = 20, overlap = 8)

  testthat::expect_equal(unname(counts), c(8, 22, 12, 58))
  testthat::expect_equal(sum(counts), 100)
  testthat::expect_equal(sum(counts[c("A i B", "Tylko A", "Tylko B")]), 42)

  grid <- env$build_event_grid(100, 30, 20, 8)
  testthat::expect_equal(nrow(grid), 100)
  testthat::expect_equal(as.integer(table(grid$status)), unname(counts))
  testthat::expect_error(env$event_set_counts(100, 10, 20, 11))
  testthat::expect_error(env$event_set_counts(100, 80, 40, 10))
})

testthat::test_that("quiz ma poprawne klucze odpowiedzi", {
  env <- load_lecture_helpers()
  testthat::expect_gte(length(env$quiz_questions), 5)
  for (question in env$quiz_questions) {
    testthat::expect_true(question$correct %in% names(question$options))
    testthat::expect_gte(length(question$options), 3)
    testthat::expect_true(nzchar(question$explanation))
  }
})
