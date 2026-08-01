testthat::test_that("zdarzenia Bananpolu mają jawne metadane", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(risk_root, "R", "bananpol.R"), envir = env)

  testthat::expect_true(env$bananpol$company$fictional)
  for (event in env$bananpol$events) {
    testthat::expect_true(nzchar(event$label))
    testthat::expect_true(nzchar(event$unit))
    testthat::expect_true(nzchar(event$horizon))
    testthat::expect_true(nzchar(event$source))
  }
  testthat::expect_error(env$bananpol_event_meta("nie-istnieje"))
})
