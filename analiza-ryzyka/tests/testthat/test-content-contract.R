testthat::test_that("bloki 01–10 realizują stały rytm dydaktyczny", {
  apps <- names(expected_apps)
  module_text <- vapply(apps, function(app) {
    paste(readLines(file.path(risk_root, app, "modules", "block.R"),
      warn = FALSE, encoding = "UTF-8"
    ), collapse = "\n")
  }, character(1))

  # 01 i 02 celowo nie mają panelu głosowania: rozdział 1 prowadzi do pytań prozą
  # (w 01 pytanie na start stoi na marginesie).
  no_vote <- c("01-jezyk-ryzyka", "02-warunki")
  # 01 kończy rozdział o macierzy ryzyka „Dobrą praktyką” zamiast pola decision.
  no_decision <- c("01-jezyk-ryzyka")
  for (app in apps) {
    text <- module_text[[app]]
    if (!app %in% no_vote) testthat::expect_true(grepl("risk_vote_panel", text, fixed = TRUE))
    # Kontrolki Shiny albo przyciski widgetów działających w przeglądarce (klasa lc-action).
    # Liczone we wszystkich plikach modules/*.R: przyjęte punkty stop mają kontrolki
    # we własnych plikach (stop_proto_*.R), nie w block.R.
    controls <- paste(unlist(lapply(Sys.glob(file.path(risk_root, app, "modules", "*.R")),
      readLines, warn = FALSE, encoding = "UTF-8")), collapse = "\n")
    testthat::expect_gte(lengths(regmatches(controls, gregexpr("sliderInput|selectInput|checkboxGroupInput|actionButton|lc_slider|lc_sf_slider|lc_segmented|lc_action|lc-action|lc_sf_action|_seg\\(|_button\\(", controls, perl = TRUE))), 2, label = app)
    if (!app %in% no_decision) testthat::expect_true(grepl("decision", text, fixed = TRUE))
    testthat::expect_true(grepl("Ściąga", text, fixed = TRUE))
    testthat::expect_true(grepl("risk_assessment_ui", text, fixed = TRUE))
    testthat::expect_true(grepl("exercises", text, fixed = TRUE))
  }
})

testthat::test_that("każdy blok ma pięć własnych pytań z poprawnymi kluczami", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(risk_root, "R", "risk_block.R"), envir = env)
  files <- file.path(risk_root, names(expected_apps), "modules", "block.R")
  all_questions <- character()
  for (file in files) {
    # Oceniamy sam zestaw treści: przypisanie obiektu o nazwie *_quiz. Nie musi być
    # pierwszym wyrażeniem pliku (w 05 przed quizem stoją obiekty karty planu).
    expressions <- parse(file)
    is_quiz <- vapply(expressions, function(e) {
      is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && grepl("_quiz$", as.character(e[[2]]))
    }, logical(1))
    testthat::expect_true(any(is_quiz), info = file)
    quiz_expr <- expressions[[which(is_quiz)[1]]]
    eval(quiz_expr, envir = env)
    quiz <- get(as.character(quiz_expr[[2]]), envir = env)
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
