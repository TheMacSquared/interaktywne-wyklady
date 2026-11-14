testthat::test_that("wykład 07: prototypy punktów stop są wpięte i działają w przeglądarce", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not_installed("xml2")

  res <- callr::r(function(path) {
    setwd(path)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    html <- paste(vapply(env$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    scenes <- xml2::xml_find_all(doc, "//div[@class='c7p-scene']")
    t <- htmltools::renderTags(env$ui)
    list(
      protos = xml2::xml_attr(xml2::xml_find_all(doc, "//div[@data-proto]"), "data-proto"),
      pending = grepl("w budowie", html, fixed = TRUE),
      scenes = xml2::xml_attr(scenes, "data-scene"),
      configs = lapply(xml2::xml_attr(scenes, "data-config"), jsonlite::fromJSON, simplifyVector = FALSE),
      figs = xml2::xml_attr(xml2::xml_find_all(doc, "//div[contains(@class, 'lc-sf') and @data-fig]"), "data-fig"),
      ids = xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id"),
      head = paste(t$head, t$html),
      p1_1000 = env$c7p1_alive(1000), p1_3000 = env$c7p1_alive(3000),
      naive = env$c7pc_naive, est = env$c7pc_est, truth = env$c7pc_true,
      p11 = env$c7p11_cnt, p18 = tabulate(env$c7p18_who, 3), p13 = env$c7p13_config$p
    )
  }, args = list(path = file.path(risk_root, "07-czas-zycia")), show = FALSE)

  # P1 stoi dwa razy (na otwarcie rozdziału i po głosowaniu), pozostałe po razie.
  expected <- c("P1", "P1", "P2A", "P2B", paste0("P", 3:15), "P16A", "P16B", "P17A", "P17B",
    "P18", "P19", "P20", "P21A", "P21B", "P22", "P23A", "P23B")
  testthat::expect_equal(sort(res$protos), sort(expected))
  testthat::expect_false(res$pending)

  # Sceny: kadry (P1 ×2, P3, P5, P9, P11, P15, P18) i trzy sceny w JS.
  testthat::expect_equal(sort(res$scenes), sort(c(rep("kadry", 8), "okno", "parki", "magazyn")))
  for (cfg in res$configs[res$scenes == "kadry"]) {
    testthat::expect_gte(length(cfg$frames), 3L)
    for (fr in cfg$frames) testthat::expect_true(grepl("^<svg", fr$svg))
  }
  testthat::expect_setequal(res$figs, c("c7_staly", "c7_gestosci", "c7_gestosci_b", "c7_cztery",
    "c7_pole_hazardu", "c7_koniec", "c7_powtorki", "c7_gamma", "c7_weibull", "c7_rodzina",
    "c7_ranking", "c7_wanna", "c7_jeden_weibull", "c7_termin", "c7_termin_pole", "c7_stary_nowy"))
  testthat::expect_equal(anyDuplicated(res$ids), 0L)

  # Liczby scen zgadzają się z tekstem wykładu (przykłady 7.1, 7.2, 7.5, 7.6, 7.11).
  testthat::expect_equal(unname(res$p1_1000), c(51L, 71L))
  testthat::expect_equal(unname(res$p1_3000), c(14L, 4L))
  testthat::expect_equal(c(res$naive, res$est, res$truth), c(612.5, 1812.5, 1368.75))
  testthat::expect_equal(round(res$p11$a[["later"]] / res$p11$a[["alive"]], 2), 0.72)
  testthat::expect_equal(round(res$p11$b[["later"]] / res$p11$b[["alive"]], 2), 0.65)
  testthat::expect_equal(res$p18, c(4L, 3L, 5L))
  testthat::expect_equal(round(res$p13, 3), 0.677)

  for (s in c("lcC7", "window.lcSF", "echarts/6.1.0/echarts.min.js", "c7_staly", "magazyn", "okno", "parki")) {
    testthat::expect_true(grepl(s, res$head, fixed = TRUE), info = s)
  }
})
