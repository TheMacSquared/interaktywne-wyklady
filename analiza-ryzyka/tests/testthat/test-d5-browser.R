testthat::test_that("wykład 05: punkty stop i rozkład geometryczny działają w przeglądarce", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not_installed("xml2")

  res <- callr::r(function(path) {
    setwd(path)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    html <- paste(vapply(env$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    scenes <- xml2::xml_find_all(doc, "//div[@class='d5p-scene']")
    t <- htmltools::renderTags(env$ui)
    list(
      scenes = xml2::xml_attr(scenes, "data-scene"),
      configs = lapply(xml2::xml_attr(scenes, "data-config"), jsonlite::fromJSON, simplifyVector = FALSE),
      figs = xml2::xml_attr(xml2::xml_find_all(doc, "//div[contains(@class, 'lc-sf') and @data-fig]"), "data-fig"),
      plot_outputs = length(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-plot-output')]")),
      html_outputs = xml2::xml_attr(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-html-output')]"), "id"),
      shiny_scene_ids = grepl('id="d5p_(rule|geo_one|mem|one|sum|br|cut_new|dost)', html),
      head = paste(t$head, t$html),
      ec = vapply(c("lc-nb", "lc-plan", "lc-mix"), function(k) length(xml2::xml_find_all(doc,
        sprintf("//div[@class='%s']//div[@class='lc-ec' and @data-role='pmf']", k))), integer(1)),
      canvases = length(xml2::xml_find_all(doc, "//canvas")),
      seed_x = vapply(env$d5p_br_seed_tapes, length, integer(1))
    )
  }, args = list(path = file.path(risk_root, "05-do-zdarzenia")), show = FALSE)

  testthat::expect_equal(sort(res$scenes), sort(c("kadry", "geo", "kadry", "kadry", "kadry", "oddzialy", "ciecie", "kadry")))
  testthat::expect_equal(res$figs, "d5_geom")
  testthat::expect_equal(res$plot_outputs, 0L)
  testthat::expect_setequal(res$html_outputs, c("d5_vote_feedback", "d5_quiz_feedback"))
  testthat::expect_false(res$shiny_scene_ids)
  for (s in c("lcD5Scenes", "window.lcSF", "echarts/6.1.0/echarts.min.js", "lcD5Nb", "lcD5Mix")) {
    testthat::expect_true(grepl(s, res$head, fixed = TRUE), info = s)
  }
  # Piaskownice P7, „Limit, średnia i rezerwa” i P11 na ECharts (od 2026-11-14), bez Chart.js.
  testthat::expect_equal(unname(res$ec), c(1L, 1L, 1L))
  testthat::expect_equal(res$canvases, 0L)
  testthat::expect_false(grepl("chart.umd.js", res$head, fixed = TRUE))

  frames <- Filter(function(cfg) !is.null(cfg$frames), res$configs)
  testthat::expect_equal(sort(vapply(frames, function(cfg) length(cfg$frames), integer(1))), c(2L, 3L, 3L, 3L, 5L))
  for (cfg in frames) for (f in cfg$frames) {
    testthat::expect_true(startsWith(f$svg, "<svg class=\"d5p-svg\""))
    testthat::expect_true(nzchar(f$name) && grepl("d5p-caption", f$body, fixed = TRUE))
    # Bez TeX w kadrach: wersja offline nie ma MathJax, wzory są w HTML (d5p-math).
    testthat::expect_false(grepl("\\(", f$body, fixed = TRUE))
  }
  cfg <- stats::setNames(res$configs, res$scenes)
  testthat::expect_equal(unlist(cfg$oddzialy$seedX), res$seed_x)
  testthat::expect_equal(sum(unlist(cfg$oddzialy$seedX) > 30), 8L)
  testthat::expect_equal(cfg$oddzialy$model, "0.41")
  # Startowa taśma P9: trzecia wada w 24. kontroli.
  testthat::expect_equal(which(cumsum(unlist(cfg$ciecie$start)) >= 3)[1], 24L)
})
