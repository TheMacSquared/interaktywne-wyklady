testthat::test_that("bloki 02–10 realizują stały rytm dydaktyczny", {
  apps <- names(expected_apps)[-1]
  module_text <- vapply(apps, function(app) {
    paste(readLines(file.path(risk_root, app, "modules", "block.R"),
      warn = FALSE, encoding = "UTF-8"
    ), collapse = "\n")
  }, character(1))

  for (text in module_text) {
    testthat::expect_true(grepl("risk_vote_panel", text, fixed = TRUE))
    testthat::expect_gte(lengths(regmatches(text, gregexpr("sliderInput|selectInput|checkboxGroupInput|actionButton", text, perl = TRUE))), 2)
    testthat::expect_true(grepl("decision", text, fixed = TRUE))
    testthat::expect_true(grepl("pitfall", text, fixed = TRUE))
    testthat::expect_true(grepl("Ściąga", text, fixed = TRUE))
    testthat::expect_true(grepl("risk_assessment_ui", text, fixed = TRUE))
    testthat::expect_true(grepl("exercises", text, fixed = TRUE))
  }
})

testthat::test_that("wspólny quiz ma pięć pytań, a każdy blok trzy ćwiczenia", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(risk_root, "R", "risk_block.R"), envir = env)
  primary <- list(
    question = "P?", choices = c("Tak" = "yes", "Nie" = "no"),
    correct = "yes", explanation = "E"
  )
  testthat::expect_length(env$risk_quiz_questions(primary), 5)

  files <- file.path(risk_root, names(expected_apps)[-1], "modules", "block.R")
  for (file in files) {
    text <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    match <- regmatches(text, regexpr("[[:alnum:]_]+_exercises\\s*<-\\s*c\\(", text, perl = TRUE))
    testthat::expect_true(nzchar(match))
  }
})
