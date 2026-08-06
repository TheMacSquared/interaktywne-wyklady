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

testthat::test_that("każdy liczbowy parametr ma pochodzenie i zakres", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(risk_root, "R", "bananpol.R"), envir = env)

  testthat::expect_silent(env$bananpol_validate_parameters())
  p <- env$bananpol_parameters
  testthat::expect_false(anyDuplicated(p$id) > 0)
  testthat::expect_true(all(p$fictional))
  testthat::expect_true(all(p$value >= p$minimum & p$value <= p$maximum))
  testthat::expect_true(all(nzchar(p$unit) & nzchar(p$horizon) & nzchar(p$source)))
})
