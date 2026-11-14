testthat::test_that("tryb statyczny: wykład 02 ma wszystkie rozdziały w dokumencie i nie potrzebuje serwera", {
  testthat::skip_if_not_installed("callr")
  res <- callr::r(function(path) {
    setwd(path)
    options(lc.static = TRUE, lc.static.lectures = c("02-warunki" = "warunki", "03-alarm-i-prawda" = "alarm-i-prawda"))
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    t <- htmltools::renderTags(app$ui)
    doc <- xml2::read_html(paste0("<div>", t$html, "</div>"))
    sections <- xml2::xml_find_all(doc, "//section[@data-lc-static-chapter]")
    list(
      ids = xml2::xml_attr(sections, "id"),
      hidden = !is.na(xml2::xml_attr(sections, "hidden")),
      active = !is.na(xml2::xml_attr(sections, "data-lc-chapter-content")),
      chapters = vapply(app$.chapters, function(ch) ch$id, character(1)),
      shiny_outputs = length(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output') or contains(@class, 'shiny-plot-output')]")),
      next_links = xml2::xml_attr(xml2::xml_find_all(doc, "//a[@data-lc-next-target]"), "onclick"),
      quiz_button = xml2::xml_attr(xml2::xml_find_all(doc, "//button[@data-lc-quiz-check]"), "data-lc-quiz-check"),
      quiz_data = xml2::xml_text(xml2::xml_find_all(doc, "//script[@data-lc-quiz-data='w2']")),
      quiz_feedback = length(xml2::xml_find_all(doc, "//div[@id='w2_quiz_feedback']")),
      tabs = xml2::xml_attr(xml2::xml_find_all(doc, "//a[contains(@class, 'lc-tab')]"), "href")
    )
  }, args = list(path = file.path(risk_root, "02-warunki")), timeout = 60)

  testthat::expect_equal(res$ids, res$chapters)
  testthat::expect_equal(res$hidden, c(FALSE, rep(TRUE, length(res$ids) - 1)))
  testthat::expect_equal(res$active, c(TRUE, rep(FALSE, length(res$ids) - 1)))
  testthat::expect_equal(res$shiny_outputs, 0L)
  testthat::expect_true(length(res$next_links) > 0)
  testthat::expect_true(all(grepl("lcGoToChapter(", res$next_links, fixed = TRUE)))
  testthat::expect_false(any(grepl("Shiny", res$next_links, fixed = TRUE)))

  # Quiz: przycisk, dane pytań i miejsce na omówienie dla shared/lc_widgets.js.
  testthat::expect_equal(res$quiz_button, "w2")
  testthat::expect_equal(res$quiz_feedback, 1L)
  quiz <- jsonlite::fromJSON(res$quiz_data)
  testthat::expect_equal(nrow(quiz), 5L)
  testthat::expect_setequal(names(quiz), c("name", "question", "correct", "correct_label", "explanation"))
  testthat::expect_equal(quiz$name, paste0("w2_quiz_", 1:5))
  testthat::expect_false(anyNA(quiz$correct_label))

  # Zakładki prowadzą tylko do wykładów zbudowanych jako strony statyczne.
  testthat::expect_true("03-alarm-i-prawda.html" %in% res$tabs)
  testthat::expect_false(any(grepl("01-jezyk-ryzyka", res$tabs, fixed = TRUE)))
})

testthat::test_that("z serwerem Shiny lecture_page() zostawia miejsce na rozdział z renderUI", {
  testthat::skip_if_not_installed("callr")
  res <- callr::r(function(path) {
    setwd(path)
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    html <- htmltools::renderTags(app$ui)$html
    c(static = grepl("data-lc-static-chapter", html, fixed = TRUE),
      output = grepl("id=\"lc__chapter_content\"", html, fixed = TRUE))
  }, args = list(path = file.path(risk_root, "02-warunki")), timeout = 60)
  testthat::expect_false(res[["static"]])
  testthat::expect_true(res[["output"]])
})

testthat::test_that("głosowanie z komentarzami działa bez serwera (wykłady 03, 04 i 05)", {
  env <- new.env(parent = asNamespace("shiny"))
  env$shared_root <- file.path(dirname(risk_root), "shared")
  sys.source(file.path(env$shared_root, "palette.R"), envir = env)
  sys.source(file.path(env$shared_root, "lecture_layout.R"), envir = env)
  sys.source(file.path(risk_root, "R", "risk_block.R"), envir = env)
  choices <- c("A" = "a", "B" = "b", "C" = "c")
  html <- as.character(env$risk_vote_panel("v", "v_out", "Pytanie?", choices, correct = "a",
    feedback = list("a" = env$lc_status("Dobrze."), ".other" = env$lc_status("Nie."))))
  testthat::expect_match(html, 'data-lc-vote-check="v"', fixed = TRUE)
  testthat::expect_match(html, 'id="v_out"', fixed = TRUE)
  testthat::expect_false(grepl("shiny-html-output", html, fixed = TRUE))
  json <- sub('.*<script type="application/json" data-lc-vote-data="v">(.*?)</script>.*', "\\1", html)
  cfg <- jsonlite::fromJSON(gsub("<\\/", "</", json, fixed = TRUE))
  testthat::expect_equal(cfg$output, "v_out")
  testthat::expect_setequal(names(cfg$feedback), c("a", ".other", ".none"))
  testthat::expect_match(cfg$feedback$.none, "Najpierw zaznacz", fixed = TRUE)
  # Komentarz do nieistniejącej odpowiedzi to błąd autora.
  testthat::expect_error(env$risk_vote_panel("v", "v_out", "Pytanie?", choices,
    feedback = list("x" = "?")))
  # Bez feedback komentarz nadal renderuje serwer (wykłady nieprzeniesione).
  legacy <- as.character(env$risk_vote_panel("v", "v_out", "Pytanie?", choices))
  testthat::expect_match(legacy, "shiny-html-output", fixed = TRUE)

  for (lecture in c("01-jezyk-ryzyka", "03-alarm-i-prawda", "04-wiele-prob", "05-do-zdarzenia")) {
    text <- paste(readLines(file.path(risk_root, lecture, "modules", "block.R"), warn = FALSE), collapse = "\n")
    testthat::expect_false(grepl("renderUI|observeEvent|reactiveVal", text), info = lecture)
  }
})

testthat::test_that("wykład 04: scenariusze, przykłady i seria kontroli są w dokumencie, bez wyjść serwera", {
  testthat::skip_if_not_installed("callr")
  res <- callr::r(function(path) {
    setwd(path)
    options(lc.static = TRUE)
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    t <- htmltools::renderTags(app$ui)
    doc <- xml2::read_html(paste0("<div>", t$html, "</div>"))
    variants <- function(id) xml2::xml_find_all(doc, sprintf("//div[@data-lc-variants='%s']", id))
    values <- function(node) xml2::xml_attr(xml2::xml_find_all(node, "./div[@data-lc-variant]"), "data-lc-variant")
    list(
      outputs = length(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output') or contains(@class, 'shiny-text-output') or contains(@class, 'shiny-plot-output')]")),
      scenario_boxes = length(variants("p4_scenario")),
      scenario_values = values(variants("p4_scenario")[[1]]),
      scenario_reveal = xml2::xml_attr(variants("p4_scenario"), "data-lc-variants-reveal"),
      scenario_options = xml2::xml_attr(xml2::xml_find_all(doc, "//select[@id='p4_scenario']/option"), "value"),
      example_values = values(variants("p4p_ex")[[1]]),
      example_choices = xml2::xml_attr(xml2::xml_find_all(doc, "//input[@name='p4p_ex']"), "value"),
      example_cards = length(xml2::xml_find_all(doc, "//div[@data-lc-variants='p4p_ex']//div[@class='p4p-card']")),
      series = xml2::xml_attrs(xml2::xml_find_first(doc, "//pre[@data-p4-series-root]")),
      series_reads = xml2::xml_attr(xml2::xml_find_all(doc, "//*[@data-p4-series]"), "data-p4-series"),
      has_slider = length(xml2::xml_find_all(doc, "//input[@id='p4_series_n']")),
      has_button = length(xml2::xml_find_all(doc, "//button[@id='p4_run']")),
      script = grepl("lcP4Seria", t$head, fixed = TRUE)
    )
  }, args = list(path = file.path(risk_root, "04-wiele-prob")), timeout = 60)

  testthat::expect_equal(res$outputs, 0L)
  # Opis scenariusza widać od razu, diagnozę dopiero po kliknięciu przycisku.
  testthat::expect_equal(res$scenario_boxes, 2L)
  testthat::expect_equal(res$scenario_reveal, c(NA, "p4_scenario_show"))
  testthat::expect_equal(res$scenario_values, res$scenario_options)
  # Każda pozycja segmentu przykładów ma swój wariant.
  testthat::expect_setequal(res$example_values, res$example_choices)
  testthat::expect_equal(res$example_cards, length(res$example_choices) - 1L)
  testthat::expect_equal(res$series[["data-slider"]], "p4_series_n")
  testthat::expect_equal(res$series[["data-button"]], "p4_run")
  testthat::expect_equal(as.numeric(res$series[["data-p"]]), 0.02)
  testthat::expect_setequal(res$series_reads, c("bad", "expected"))
  testthat::expect_equal(c(res$has_slider, res$has_button), c(1L, 1L))
  testthat::expect_true(res$script)
})

testthat::test_that("wykład 01: decyzja i ćwiczenia mają dane dla przeglądarki, bez wyjść serwera", {
  testthat::skip_if_not_installed("callr")
  res <- callr::r(function(path) {
    setwd(path)
    options(lc.static = TRUE)
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    t <- htmltools::renderTags(app$ui)
    doc <- xml2::read_html(paste0("<div>", t$html, "</div>"))
    cfg <- function(xpath) jsonlite::fromJSON(xml2::xml_attr(xml2::xml_find_first(doc, xpath), "data-config"))
    ids <- function(xpath) xml2::xml_attr(xml2::xml_find_all(doc, xpath), "id")
    list(
      outputs = length(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output')]")),
      selectize = length(xml2::xml_find_all(doc, "//script[@data-for]")),
      report = cfg("//div[@data-j1-report]"),
      report_fields = xml2::xml_attr(xml2::xml_find_all(doc, "//input[@name='ch8_fields']"), "value"),
      report_conclusions = xml2::xml_attr(xml2::xml_find_all(doc, "//input[@name='ch8_conclusion']"), "value"),
      models = cfg("//div[@data-j1-models]"),
      models_hidden = !is.na(xml2::xml_attr(xml2::xml_find_first(doc, "//div[@data-j1-models]"), "hidden")),
      model_options = lapply(c("ch8_model_1", "ch8_model_2", "ch8_model_3"), function(id) {
        xml2::xml_attr(xml2::xml_find_all(doc, sprintf("//select[@id='%s']/option", id)), "value")
      }),
      reveals = xml2::xml_attr(xml2::xml_find_all(doc, "//div[@data-lc-reveal]"), "data-lc-reveal"),
      buttons = ids("//button"),
      vote = jsonlite::fromJSON(xml2::xml_text(xml2::xml_find_first(doc, "//script[@data-lc-vote-data='ch5_priority']"))),
      vote_choices = xml2::xml_attr(xml2::xml_find_all(doc, "//input[@name='ch5_priority']"), "value"),
      vote_button = xml2::xml_attr(xml2::xml_find_first(doc, "//button[@id='ch5_check']"), "data-lc-vote-check")
    )
  }, args = list(path = file.path(risk_root, "01-jezyk-ryzyka")), timeout = 60)

  testthat::expect_equal(res$outputs, 0L)
  # Listy wyboru są zwykłymi <select>, bez skryptów selectize.
  testthat::expect_equal(res$selectize, 0L)

  # Ćwiczenie 1: pola i wniosek z konfiguracji istnieją w formularzu.
  testthat::expect_setequal(res$report$required, res$report_fields)
  testthat::expect_setequal(names(res$report$labels), res$report$required)
  testthat::expect_true(res$report$correct %in% res$report_conclusions)
  testthat::expect_true(res$report$button %in% res$buttons)

  # Ćwiczenie 3: każda poprawna odpowiedź jest opcją swojej listy.
  testthat::expect_true(res$models_hidden)
  testthat::expect_equal(length(res$models$inputs), length(res$models$correct))
  for (i in seq_along(res$models$correct)) {
    testthat::expect_true(res$models$correct[[i]] %in% res$model_options[[i]])
  }
  testthat::expect_true(res$models$button %in% res$buttons)

  # Ćwiczenia 2 i 4: treść odsłaniana istniejącym przyciskiem.
  testthat::expect_setequal(res$reveals, c("ch8_sets_solution", "ch8_transfer_rubric"))
  testthat::expect_true(all(res$reveals %in% res$buttons))

  # Decyzja z rozdziału 5: komentarz do dobrej, złej i braku odpowiedzi.
  testthat::expect_equal(res$vote_button, "ch5_priority")
  testthat::expect_equal(res$vote$output, "ch5_feedback")
  testthat::expect_true("insufficient" %in% res$vote_choices)
  testthat::expect_match(res$vote$feedback$insufficient, "Dobrze.", fixed = TRUE)
  testthat::expect_match(res$vote$feedback$.other, "To zbyt szybki ranking.", fixed = TRUE)
  testthat::expect_equal(res$vote$feedback$.none, res$vote$feedback$.other)
})

# Wykłady gotowe do budowania statycznego (scripts/check_static_ready.R): wspólne warunki.
static_ready_lectures <- c("01-jezyk-ryzyka", "02-warunki", "03-alarm-i-prawda", "04-wiele-prob",
                           "05-do-zdarzenia", "06-zmiennosc-i-prog", "07-czas-zycia",
                           "08-niezawodnosc-systemu", "09-drzewo-bledow", "10-model-do-decyzji")

for (lecture in static_ready_lectures) local({
  lecture <- lecture
  testthat::test_that(paste("tryb statyczny:", lecture, "ma komplet rozdziałów i nie czeka na serwer"), {
    testthat::skip_if_not_installed("callr")
    res <- callr::r(function(path) {
      setwd(path)
      options(lc.static = TRUE)
      app <- new.env(parent = globalenv())
      sys.source("app.R", envir = app)
      t <- htmltools::renderTags(app$ui)
      doc <- xml2::read_html(paste0("<div>", t$html, "</div>"))
      ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id")
      list(
        sections = xml2::xml_attr(xml2::xml_find_all(doc, "//section[@data-lc-static-chapter]"), "id"),
        chapters = vapply(app$.chapters, function(ch) ch$id, character(1)),
        outputs = xml2::xml_attr(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output') or contains(@class, 'shiny-plot-output') or contains(@class, 'shiny-text-output')]"), "id"),
        selectize = length(xml2::xml_find_all(doc, "//script[@data-for]")),
        duplicate_ids = unique(ids[duplicated(ids)])
      )
    }, args = list(path = file.path(risk_root, lecture)), timeout = 180)
    testthat::expect_equal(res$sections, res$chapters)
    # Klasę wyjścia Shiny mogą mieć tylko opisy kroków, które pisze przeglądarka: id kończy się na _text.
    testthat::expect_true(all(grepl("_text$", res$outputs)), info = paste(res$outputs, collapse = ", "))
    testthat::expect_equal(res$selectize, 0L)
    testthat::expect_length(res$duplicate_ids, 0L)
  })
})
