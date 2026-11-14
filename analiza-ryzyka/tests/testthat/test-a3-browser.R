testthat::test_that("wykład 03: detektor, drzewo i ryciny działają w przeglądarce", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not_installed("xml2")

  res <- callr::r(function(path) {
    setwd(path)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    html <- paste(vapply(env$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    t <- htmltools::renderTags(env$ui)
    list(
      figs = xml2::xml_attr(xml2::xml_find_all(doc, "//div[contains(@class, 'lc-sf') and @data-fig]"), "data-fig"),
      a3 = xml2::xml_attr(xml2::xml_find_all(doc, "//*[@data-a3]"), "data-a3"),
      a3v = xml2::xml_attr(xml2::xml_find_all(doc, "//*[@data-a3v]"), "data-a3v"),
      sliders = xml2::xml_attr(xml2::xml_find_all(doc, "//input[@type='range']"), "id"),
      plot_outputs = length(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-plot-output')]")),
      html_outputs = xml2::xml_attr(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output')]"), "id"),
      cost = jsonlite::fromJSON(xml2::xml_attr(xml2::xml_find_first(doc, "//div[@data-fig='a3_cost']"), "data-config")),
      head = paste(t$head, t$html)
    )
  }, args = list(path = file.path(risk_root, "03-alarm-i-prawda")), show = FALSE)

  testthat::expect_setequal(res$figs, c("a3_double", "a3_cost", "a3_sp"))
  testthat::expect_setequal(res$a3, c("table", "tree"))
  testthat::expect_setequal(res$a3v, c("tp", "fp", "post"))
  testthat::expect_true(all(c("a3_prev", "a3_sens", "a3_fpr", "a3_dependence", "a3_cost_c", "a3_cost_L",
    "a3_sp_prior", "a3_sp_lr", "a3_sp_radar") %in% res$sliders))
  testthat::expect_equal(res$plot_outputs, 0L)
  testthat::expect_setequal(res$html_outputs, c("a3_vote_feedback", "a3_quiz_feedback"))
  for (s in c("lcA3", "window.lcSF", "echarts/6.1.0/echarts.min.js")) {
    testthat::expect_true(grepl(s, res$head, fixed = TRUE), info = s)
  }
  # Trzy sytuacje z przykładu 3.8 (posteriory z risk_bayes).
  testthat::expect_equal(res$cost$cases$label, c("hala 0.002", "hala 0.01", "dwa alarmy"))
  testthat::expect_equal(round(res$cost$cases$q, 3), c(0.037, 0.161, 0.420))
})
