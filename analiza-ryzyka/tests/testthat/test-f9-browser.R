testthat::test_that("wykład 09: prototypy punktów stop są wpięte i działają w przeglądarce", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not_installed("xml2")

  res <- callr::r(function(path) {
    setwd(path)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    html <- paste(vapply(env$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    scenes <- xml2::xml_find_all(doc, "//div[@class='f9p-scene']")
    t <- htmltools::renderTags(env$ui)
    list(
      protos = xml2::xml_attr(xml2::xml_find_all(doc, "//div[@data-proto]"), "data-proto"),
      pending = grepl("w budowie", html, fixed = TRUE),
      scenes = xml2::xml_attr(scenes, "data-scene"),
      configs = lapply(xml2::xml_attr(scenes, "data-config"), jsonlite::fromJSON, simplifyVector = FALSE),
      figs = xml2::xml_attr(xml2::xml_find_all(doc, "//div[contains(@class, 'lc-sf') and @data-fig]"), "data-fig"),
      ids = xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id"),
      head = paste(t$head, t$html),
      numbers = c(or = env$f9p_or, top = env$f9p_top, wrong = env$f9p13_wrong, right = env$f9p13_right,
        valve = env$f9p17_top, chain = env$f9p14_chain, par = env$f9p14_par)
    )
  }, args = list(path = file.path(risk_root, "09-drzewo-bledow")), show = FALSE)

  expected <- c("P1", "P2", "P3", "P4A", "P4B", "P5", "P6A", "P6B", "P7", "P8", "P9", "P10", "P11A", "P11B",
    "P12", "P13", "P14", "P15", "P16A", "P16B", "P17", "P18", "P19A", "P19B")
  testthat::expect_equal(sort(res$protos), sort(expected))
  testthat::expect_false(res$pending)

  # Sceny: kadry z R (P1, P2), drzewa (11, w tym P14 z ryciną), dwa światy (P4A, P4B) i trzy piaskownice.
  testthat::expect_equal(sort(res$scenes), sort(c(rep("kadry", 2), rep("drzewo", 11), rep("lancuch", 2),
    "konstruktor", "stany", "parametry")))
  for (cfg in res$configs[res$scenes == "kadry"]) {
    testthat::expect_gte(length(cfg$frames), 3L)
    for (fr in cfg$frames) testthat::expect_true(nzchar(fr$svg))
  }
  for (cfg in res$configs[res$scenes == "drzewo"]) {
    testthat::expect_true(cfg$nav %in% c("kadry", "seg", "none"))
    for (fr in cfg$frames) {
      testthat::expect_gte(length(fr$panels), 1L)
      # Każdy rysunek to gotowy SVG albo drzewo, które scena zna.
      for (p in fr$panels) testthat::expect_true(!is.null(p$svg) || p$tree %in% names(cfg$trees))
    }
  }
  testthat::expect_setequal(res$figs, c("f9_suma", "f9_powtorzenie", "f9_wspolna", "f9_ranking", "f9_pole", "f9_przedzial"))
  testthat::expect_equal(anyDuplicated(res$ids), 0L)

  # Liczby na obrazach zgadzają się z przykładami 9.3, 9.6, 9.7 i 9.9.
  testthat::expect_equal(unname(res$numbers["or"]), 0.126)
  testthat::expect_equal(unname(res$numbers["top"]), 0.00063)
  testthat::expect_equal(round(unname(res$numbers["wrong"]), 4), 0.0139)
  testthat::expect_equal(unname(res$numbers["right"]), 0.0298)
  testthat::expect_equal(round(unname(res$numbers["valve"]), 6), 0.000717)
  testthat::expect_equal(round(unname(res$numbers["chain"]), 6), 0.000674)
  testthat::expect_equal(round(unname(res$numbers["par"]), 7), 0.0000698)

  for (s in c("lcF9", "window.lcSF", "echarts/6.1.0/echarts.min.js", "f9_wspolna", "f9_ranking", "konstruktor", "lancuch")) {
    testthat::expect_true(grepl(s, res$head, fixed = TRUE), info = s)
  }
})
