testthat::test_that("dwa alarmy wynikają z jawnego rozkładu wspólnego", {
  env <- new.env()
  sys.source(file.path(risk_root, "R", "risk_math.R"), env)
  for (d in c(0, .5, 1)) {
    # Enumeracja: stan instalacji, tryb kopiowania i wyniki niezależnych losowań.
    states <- expand.grid(a = 0:1, copy = 0:1, first = 0:1, second = 0:1)
    p <- ifelse(states$a == 1, .95, .05)
    weights <- ifelse(states$a == 1, .01, .99) *
      ifelse(states$copy == 1, d, 1-d) *
      ifelse(states$first == 1, p, 1-p) * ifelse(states$second == 1, p, 1-p)
    both <- states$first == 1 & (states$copy == 1 | states$second == 1)
    expected <- sum(weights[both & states$a == 1]) / sum(weights[both])
    testthat::expect_equal(env$risk_two_alarm_posterior(.01, .95, .05, d), expected)
  }
  testthat::expect_true(is.na(env$risk_two_alarm_posterior(0, 1, 0)))
})

# Odczyty detektora (posterior, dwa alarmy, tablica, drzewo) liczy od 2026-11-13
# modules/detektor.js: rachunek sprawdza tests/js/a3_detektor.test.js, a wpięcie
# test-a3-browser.R. Tu zostaje nawigacja: suwaki żyją dłużej niż rozdział.
testthat::test_that("zmiana rozdziału nie odtwarza suwaków detektora z wartościami początkowymi", {
  result <- callr::r(function(path) {
    setwd(path)
    app <- new.env()
    sys.source("app.R", app)
    # Zmiana rozdziału nie odtwarza kontrolek z wartościami początkowymi.
    shiny::testServer(app$server, {
      first <- output$lc__chapter_content$html
      session$setInputs(a3_prev = .023, lc__switch_chapter = "ch-baza")
      stopifnot(identical(first, output$lc__chapter_content$html))
      stopifnot(identical(input$a3_prev, .023))
      stopifnot(grepl('id="a3_prev"', first, fixed = TRUE))
      stopifnot(grepl('id="a3_dependence"', first, fixed = TRUE))
    })
    TRUE
  }, args = list(path = file.path(risk_root, "03-alarm-i-prawda")), timeout = 60)
  testthat::expect_true(result)
})

# Grę Monty Halla prowadzi od 2026-11-14 modules/monty.js (logika: tests/js/w2_monty.test.js).
testthat::test_that("wykład 02: Monty Hall, drzewo i tablica działają bez serwera", {
  testthat::skip_if_not_installed("xml2")
  res <- callr::r(function(path) {
    setwd(path)
    app <- new.env()
    sys.source("app.R", app)
    html <- paste(vapply(app$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    t <- htmltools::renderTags(app$ui)
    list(
      monty = length(xml2::xml_find_all(doc, "//div[@class='w2-monty']")),
      slots = xml2::xml_attr(xml2::xml_find_all(doc, "//div[@class='w2-monty']//*[@data-mt]"), "data-mt"),
      figs = xml2::xml_attr(xml2::xml_find_all(doc, "//div[contains(@class, 'lc-sf') and @data-fig]"), "data-fig"),
      tree = length(xml2::xml_find_all(doc, "//svg[starts-with(@aria-label, 'Drzewo dróg: 1000')]")),
      plot_outputs = length(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-plot-output')]")),
      html_outputs = xml2::xml_attr(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output')]"), "id"),
      head = paste(t$head, t$html),
      has_server = exists("warunki_monty_server", envir = app)
    )
  }, args = list(path = file.path(risk_root, "02-warunki")), timeout = 60)
  testthat::expect_equal(res$monty, 1L)
  testthat::expect_true(all(c("controls", "doors", "feedback", "sim", "tick") %in% res$slots))
  testthat::expect_equal(res$figs, "w2_monty")
  testthat::expect_equal(res$tree, 1L)
  testthat::expect_equal(res$plot_outputs, 0L)
  testthat::expect_equal(res$html_outputs, "w2_quiz_feedback")
  testthat::expect_true(grepl("lcW2Monty", res$head, fixed = TRUE))
  testthat::expect_false(res$has_server)
})
