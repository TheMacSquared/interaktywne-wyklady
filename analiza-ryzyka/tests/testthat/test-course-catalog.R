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

testthat::test_that("nagłówki app.R są zgodne z katalogiem i tytułami bloków", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(risk_root, "R", "course_catalog.R"), envir = env)

  extract <- function(text, field) {
    match <- regmatches(text, regexpr(paste0(field, '\\s*=\\s*"[^"]*"'), text))
    sub(paste0(field, '\\s*=\\s*"'), "", sub('"$', "", match))
  }

  for (config in env$risk_course_catalog) {
    app_text <- paste(readLines(file.path(risk_root, config$folder, "app.R"),
      warn = FALSE, encoding = "UTF-8"
    ), collapse = "\n")
    testthat::expect_identical(extract(app_text, "lecture_title"), config$title)
    testthat::expect_identical(extract(app_text, "lecture_num"), config$num)
    testthat::expect_identical(extract(app_text, "lecture_id"), config$lecture_id)
    testthat::expect_identical(
      extract(app_text, "module_label"), "Analiza ryzyka · Bananpol"
    )

    block_text <- paste(readLines(file.path(risk_root, config$folder, "modules", "block.R"),
      warn = FALSE, encoding = "UTF-8"
    ), collapse = "\n")
    testthat::expect_true(grepl(paste0('title = "', config$title, '"'), block_text, fixed = TRUE))
  }
})
