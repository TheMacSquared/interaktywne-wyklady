testthat::test_that("tabele zadania 1 mają kompletne odpowiedzi dla każdego kierunku", {
  testthat::skip_if_not_installed("callr")

  result <- callr::r(
    function(app_dir) {
      setwd(app_dir)
      env <- new.env(parent = globalenv())
      sys.source("app.R", envir = env)

      specs <- env$.ch8_task1_specs
      complete <- vapply(specs, function(x) {
        identical(names(x), c("variable", "type", "stats", "plot")) &&
          nrow(x) > 0 && all(vapply(x, function(col) all(nzchar(col)), logical(1)))
      }, logical(1))

      hidden <- as.character(env$.ch8_task1_table("sat", reveal = FALSE))
      shown <- as.character(env$.ch8_task1_table("sat", reveal = TRUE))

      list(
        directions = names(specs),
        complete = complete,
        hidden = hidden,
        shown = shown,
        old_solution_is_empty = all(vapply(env$.ch8_solutions,
                                            function(x) is.null(x$sol1), logical(1)))
      )
    },
    args = list(app_dir = file.path(stat_root, "01-typy-danych")),
    timeout = 60,
    spinner = FALSE,
    show = FALSE
  )

  testthat::expect_setequal(result$directions, c("bhp", "rol", "zyw", "sat"))
  testthat::expect_true(all(result$complete))
  testthat::expect_false(grepl("Identyfikator — nie analizujemy", result$hidden,
                               fixed = TRUE))
  testthat::expect_false(grepl(">?</td>", result$hidden, fixed = TRUE))
  testthat::expect_true(grepl("Identyfikator — nie analizujemy", result$shown,
                              fixed = TRUE))
  testthat::expect_true(grepl("Nie dotyczy", result$shown, fixed = TRUE))
  testthat::expect_true(result$old_solution_is_empty)
})
