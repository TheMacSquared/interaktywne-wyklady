testthat::test_that("wykład 01: łańcuch, ćwiczenia, częstość, Venn i siatka działają w przeglądarce", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not_installed("xml2")

  res <- callr::r(function(path) {
    setwd(path)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    html <- paste(vapply(env$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    t <- htmltools::renderTags(env$ui)
    fb <- xml2::xml_find_all(doc, "//div[@data-ch1-feedback]")
    list(
      figs = xml2::xml_attr(xml2::xml_find_all(doc, "//div[contains(@class, 'lc-sf') and @data-fig]"), "data-fig"),
      checks = xml2::xml_attr(xml2::xml_find_all(doc, "//button[@data-ch1-check]"), "data-ch1-check"),
      feedback = lapply(xml2::xml_attr(fb, "data-config"), jsonlite::fromJSON),
      chain = jsonlite::fromJSON(xml2::xml_attr(xml2::xml_find_first(doc, "//div[@data-ch1='chain-detail']"), "data-config")),
      venn = length(xml2::xml_find_all(doc, "//div[@class='j1-venn']//div[@data-venn]")),
      venn_text_output = grepl('id="ch4_venn_text" class="shiny-html-output', html, fixed = TRUE),
      ops = length(xml2::xml_find_all(doc, "//svg[starts-with(@aria-label, 'Cztery diagramy Venna')]")),
      plot_outputs = length(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-plot-output')]")),
      html_outputs = xml2::xml_attr(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output')]"), "id"),
      head = paste(t$head, t$html),
      servers = c("jezyk_sytuacja_server", "jezyk_czestosc_server", "jezyk_zbiory_server") %in% ls(env)
    )
  }, args = list(path = file.path(risk_root, "01-jezyk-ryzyka")), show = FALSE)

  testthat::expect_setequal(res$figs, c("j1_freq", "j1_events"))
  testthat::expect_setequal(res$checks, c("ch1_assign", "ch1_assign_rampa"))
  testthat::expect_equal(vapply(res$feedback, function(f) nrow(f$items), integer(1)), c(5L, 5L))
  testthat::expect_equal(vapply(res$feedback, function(f) f$rows, logical(1)), c(FALSE, TRUE))
  testthat::expect_setequal(names(res$chain), c("hazard", "exposure", "event", "consequence", "safeguard"))
  testthat::expect_equal(res$venn, 4L)
  testthat::expect_false(res$venn_text_output)
  testthat::expect_equal(res$ops, 1L)
  testthat::expect_equal(res$plot_outputs, 0L)
  # W R zostają: sprawdzenie decyzji, ćwiczenia z rozdziału „Ćwiczenia” i quiz.
  testthat::expect_setequal(res$html_outputs, c("ch5_feedback", "j1_quiz_feedback", "ch8_feedback",
    "ch8_sets_feedback", "ch8_models_feedback", "ch8_transfer_feedback"))
  testthat::expect_true(grepl("lcJ1", res$head, fixed = TRUE))
  testthat::expect_false(any(res$servers))
})
