testthat::test_that("wykład 06: prototypy punktów stop są wpięte i działają w przeglądarce", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not_installed("xml2")

  res <- callr::r(function(path) {
    setwd(path)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    html <- paste(vapply(env$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    scenes <- xml2::xml_find_all(doc, "//div[@class='z6p-scene']")
    t <- htmltools::renderTags(env$ui)
    list(
      protos = xml2::xml_attr(xml2::xml_find_all(doc, "//div[@data-proto]"), "data-proto"),
      pending = grepl("w budowie", html, fixed = TRUE),
      scenes = xml2::xml_attr(scenes, "data-scene"),
      configs = lapply(xml2::xml_attr(scenes, "data-config"), jsonlite::fromJSON, simplifyVector = FALSE),
      figs = xml2::xml_attr(xml2::xml_find_all(doc, "//div[contains(@class, 'lc-sf') and @data-fig]"), "data-fig"),
      ids = xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id"),
      head = paste(t$head, t$html),
      p1_over = env$z6p1_over
    )
  }, args = list(path = file.path(risk_root, "06-zmiennosc-i-prog")), show = FALSE)

  # P1 stoi dwa razy (przed głosowaniem i po nim), pozostałe po razie.
  expected <- c("P1", "P1", "P2A", "P2B", "P3", "P4", "P5", "P6", "P7", "P8", "P8b", "P9A", "P9B",
    "P10A", "P10B", "P11", "P12", "P13", "P14A", "P14B", "P15A", "P15B", "P16A", "P16B")
  testthat::expect_equal(sort(res$protos), sort(expected))
  testthat::expect_false(res$pending)

  # Sceny: kadry (P1 ×2, P6, P12, P15A) i dwie losowe.
  testthat::expect_equal(sort(res$scenes), sort(c(rep("kadry", 5), "zmiany", "dzwig")))
  for (cfg in res$configs[res$scenes == "kadry"]) {
    testthat::expect_gte(length(cfg$frames), 3L)
    for (fr in cfg$frames) testthat::expect_true(grepl("^<svg", fr$svg))
  }
  testthat::expect_setequal(res$figs, c("z6_hist_a", "z6_hist_b", "z6_pole", "z6_linijka", "z6_musigma",
    "z6_prog", "z6_interwencje", "z6_interwencje_b", "z6_prog_ponizej", "z6_nakladanie",
    "z6_modele", "z6_modele_b"))
  # Rycina interwencji stoi dwa razy (P9A i P10B) z różnymi kontrolkami.
  testthat::expect_equal(sum(res$figs == "z6_interwencje"), 2L)
  testthat::expect_equal(anyDuplicated(res$ids), 0L)

  # Scena P1 pokazuje te same liczby co przykład 6.1.
  testthat::expect_equal(res$p1_over, c(1L, 159L, 274L))

  for (s in c("lcZ6", "window.lcSF", "echarts/6.1.0/echarts.min.js", "z6_musigma", "dzwig", "zmiany")) {
    testthat::expect_true(grepl(s, res$head, fixed = TRUE), info = s)
  }
})
