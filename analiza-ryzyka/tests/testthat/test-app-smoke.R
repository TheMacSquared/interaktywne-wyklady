testthat::test_that("wszystkie wykłady ładują UI i kompletną listę rozdziałów", {
  testthat::skip_if_not_installed("callr")

  results <- lapply(names(expected_apps), function(app_name) {
    callr::r(
      function(path, expected_count) {
        setwd(path)
        env <- new.env(parent = globalenv())
        sys.source("app.R", envir = env)
        stopifnot(exists("ui", envir = env, inherits = FALSE))
        stopifnot(is.function(env$server))
        stopifnot(length(env$.chapters) == expected_count)
        ids <- vapply(env$.chapters, function(chapter) chapter$id, character(1))
        stopifnot(all(nzchar(ids)), !anyDuplicated(ids))
        TRUE
      },
      args = list(
        path = file.path(risk_root, app_name),
        expected_count = unname(expected_apps[[app_name]])
      ),
      timeout = 60,
      spinner = FALSE,
      show = FALSE
    )
  })
  testthat::expect_true(all(vapply(results, isTRUE, logical(1))))
})
