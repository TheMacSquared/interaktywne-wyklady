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

testthat::test_that("każdy blok ma pięć własnych pytań z poprawnymi kluczami", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(risk_root, "R", "risk_block.R"), envir = env)
  files <- file.path(risk_root, names(expected_apps)[-1], "modules", "block.R")
  all_questions <- character()
  for (file in files) {
    # Definicja quizu poprzedza komponenty Shiny; oceniamy sam zestaw treści.
    expressions <- parse(file)
    eval(expressions[[1]], envir = env)
    quiz <- get(as.character(expressions[[1]][[2]]), envir = env)
    questions <- env$risk_quiz_questions(quiz)
    testthat::expect_length(questions, 5)
    for (question in questions) {
      testthat::expect_true(question$correct %in% unname(question$choices))
      testthat::expect_true(nzchar(question$explanation))
      testthat::expect_true(!anyDuplicated(unname(question$choices)))
      all_questions <- c(all_questions, question$question)
    }
  }
  testthat::expect_false(anyDuplicated(all_questions) > 0)
})
