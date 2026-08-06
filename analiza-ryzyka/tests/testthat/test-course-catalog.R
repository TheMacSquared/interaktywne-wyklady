testthat::test_that("katalog obejmuje wykłady 02–10 i kompletne rozdziały", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(risk_root, "R", "course_catalog.R"), envir = env)

  testthat::expect_equal(length(env$risk_course_catalog), 9)
  testthat::expect_equal(anyDuplicated(vapply(
    env$risk_course_catalog, function(config) config$lecture_id, character(1)
  )), 0L)

  for (config in env$risk_course_catalog) {
    testthat::expect_true(dir.exists(file.path(risk_root, config$folder)))
    testthat::expect_gte(config$chapter_count, 8)
    testthat::expect_true(config$meetings %in% 1:2)
    testthat::expect_true(file.exists(file.path(risk_root, config$folder, "modules", "block.R")))
  }
})
