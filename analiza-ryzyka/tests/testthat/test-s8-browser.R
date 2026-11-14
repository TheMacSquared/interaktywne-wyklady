testthat::test_that("wykład 08: prototypy punktów stop są wpięte i działają w przeglądarce", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not_installed("xml2")

  res <- callr::r(function(path) {
    setwd(path)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    html <- paste(vapply(env$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    scenes <- xml2::xml_find_all(doc, "//div[@class='s8p-scene']")
    t <- htmltools::renderTags(env$ui)
    list(
      protos = xml2::xml_attr(xml2::xml_find_all(doc, "//div[@data-proto]"), "data-proto"),
      pending = grepl("w budowie", html, fixed = TRUE),
      scenes = xml2::xml_attr(scenes, "data-scene"),
      configs = lapply(xml2::xml_attr(scenes, "data-config"), jsonlite::fromJSON, simplifyVector = FALSE),
      figs = xml2::xml_attr(xml2::xml_find_all(doc, "//div[contains(@class, 'lc-sf') and @data-fig]"), "data-fig"),
      ids = xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id"),
      head = paste(t$head, t$html),
      p2 = env$s8p2_n, p12 = env$s8p12_r_of("mixed"), p13 = vapply(c("series", "mixed", "parallel"), env$s8p12_r_of, 0),
      p15 = c(env$s8p15_pow, env$s8p15_fans), p20 = vapply(env$s8p20_views, function(v) v$r, 0),
      p22 = env$s8p22_ib, p24 = env$s8p24_r, works = c(
        env$s8p_rbd_works(list(1, c(2, 3)), c(TRUE, FALSE, TRUE)),
        env$s8p_rbd_works(list(1, c(2, 3)), c(FALSE, TRUE, TRUE)),
        env$s8p_rbd_works(list(c(1, 2, 3)), c(TRUE, FALSE, FALSE), k = 2L))
    )
  }, args = list(path = file.path(risk_root, "08-niezawodnosc-systemu")), show = FALSE)

  # P1 stoi dwa razy (przed głosowaniem i po nim), pozostałe po razie.
  expected <- c("P1", "P1", "P2A", "P2B", "P3", "P4A", "P4B", "P5A", "P5B", "P6", "P7", "P8", "P9A", "P9B",
    "P10", "P11A", "P11B", "P12", "P13", "P14", "P15", "P16A", "P16B", "P17", "P18", "P19A", "P19B",
    "P20", "P21", "P22", "P23", "P24A", "P24B")
  testthat::expect_equal(sort(res$protos), sort(expected))
  testthat::expect_false(res$pending)

  # Sceny: kadry (P1 ×2, P3, P4A, P10, P15, P18, P24A), widoki (P2A, P8, P13, P20, P21, P22),
  # klikalne schematy (P5A i trzy w P5B) oraz jedna losowa.
  testthat::expect_equal(sort(res$scenes), sort(c(rep("kadry", 8), rep("widoki", 6), rep("uklad", 4), "misje")))
  for (cfg in res$configs[res$scenes == "kadry"]) {
    testthat::expect_gte(length(cfg$frames), 4L)
    for (fr in cfg$frames) testthat::expect_true(grepl("^<svg", fr$svg))
  }
  for (cfg in res$configs[res$scenes == "widoki"]) {
    for (v in cfg$views) testthat::expect_true(grepl("^<svg", v$svg))
  }
  # Klikalny schemat ma widok dla każdej z 2ⁿ masek.
  for (cfg in res$configs[res$scenes == "uklad"]) {
    for (a in if (is.null(cfg$archs)) list(cfg) else cfg$archs) {
      testthat::expect_equal(length(a$views), 2^a$n)
    }
  }
  testthat::expect_setequal(res$figs, c("s8_lancuch", "s8_horyzont", "s8_kwadrat", "s8_architektury", "s8_czas",
    "s8_przewaga", "s8_wspolna", "s8_sklad", "s8_beta", "s8_galezie", "s8_galezie_log", "s8_poprawa"))
  testthat::expect_equal(anyDuplicated(res$ids), 0L)

  # Liczby na obrazach zgadzają się z tekstem wykładu.
  testthat::expect_equal(unname(res$p2), c(81, 9, 9, 1))                      # przykład 8.1
  testthat::expect_equal(res$p12, 0.97608)                                     # przykład 8.7
  testthat::expect_equal(unname(res$p13), c(0.85652, 0.97608, 0.99992))        # oszacowanie (8.9)
  testthat::expect_equal(res$p15, c(10, 4))                                    # przykład 8.8
  testthat::expect_equal(unname(res$p20), c(0.999, 0.972, 0.729))              # układ k z n
  testthat::expect_equal(unname(res$p22), c(0.996, 0.049, 0.0784))             # przykład 8.11
  testthat::expect_equal(res$p24, 0.99 * 0.97608)
  testthat::expect_equal(res$works, c(TRUE, FALSE, FALSE))

  for (s in c("lcS8", "window.lcSF", "echarts/6.1.0/echarts.min.js", "s8_lancuch", "misje", "s8_beta", "s8_galezie_log")) {
    testthat::expect_true(grepl(s, res$head, fixed = TRUE), info = s)
  }
})
