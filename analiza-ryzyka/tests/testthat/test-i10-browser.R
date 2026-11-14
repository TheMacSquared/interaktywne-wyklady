testthat::test_that("wykład 10: prototypy punktów stop są wpięte i działają w przeglądarce", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not_installed("xml2")

  res <- callr::r(function(path) {
    setwd(path)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    html <- paste(vapply(env$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    scenes <- xml2::xml_find_all(doc, "//div[@class='i10p-scene']")
    t <- htmltools::renderTags(env$ui)
    list(
      protos = xml2::xml_attr(xml2::xml_find_all(doc, "//div[@data-proto]"), "data-proto"),
      pending = grepl("w budowie", html, fixed = TRUE),
      scenes = xml2::xml_attr(scenes, "data-scene"),
      configs = lapply(xml2::xml_attr(scenes, "data-config"), jsonlite::fromJSON, simplifyVector = FALSE),
      figs = xml2::xml_attr(xml2::xml_find_all(doc, "//div[contains(@class, 'lc-sf') and @data-fig]"), "data-fig"),
      ids = xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id"),
      head = paste(t$head, t$html),
      cfg = env$i10p_cfg,
      base = env$i10p_mission(),
      notes = vapply(env$i10p1_notes, function(n) n$num, "")
    )
  }, args = list(path = file.path(risk_root, "10-model-do-decyzji")), show = FALSE)

  # P1 stoi dwa razy (przed głosowaniem i po nim), pozostałe po razie.
  expected <- c("P1", "P1", "P2", "P3", "P4", "P5A", "P5B", "P6A", "P6B", "P7", "P8", "P9", "P10",
    "P11A", "P11B", "P12", "P13A", "P13B", "P14", "P15", "P16", "P17", "P18A", "P18B")
  testthat::expect_equal(sort(res$protos), sort(expected))
  testthat::expect_false(res$pending)

  # Sceny: kadry (P1 ×2, P2, P4, P5A, P9, P11B, P12, P18A) i interaktywne.
  testthat::expect_equal(sort(res$scenes),
    sort(c(rep("kadry", 9), "rok", "detektor", "uklad", "drzewo", "drzewo")))
  for (cfg in res$configs[res$scenes == "kadry"]) {
    testthat::expect_gte(length(cfg$frames), 2L)
    for (fr in cfg$frames) testthat::expect_true(grepl("^<svg", fr$svg))
  }
  # Sceny liczące misję dostają parametry z R (nic nie jest wpisane w JS).
  for (cfg in res$configs[res$scenes %in% c("uklad", "drzewo")]) {
    for (k in c("time", "base_time", "mttf", "beta", "eta", "power_r", "controller_r", "init", "sens", "efficacy")) {
      testthat::expect_equal(cfg[[k]], res$cfg[[k]], info = k)
    }
  }
  testthat::expect_setequal(res$figs, c("i10_modele", "i10_zero", "i10_zero_n", "i10_odnowa",
    "i10_opcje", "i10_scen", "i10_odwrocenie", "i10_kryterium"))
  testthat::expect_equal(anyDuplicated(res$ids), 0L)

  # Liczby sceny P1 i stan bazowy zgadzają się z tekstem wykładu.
  testthat::expect_equal(res$notes, c("91%", "29%", "64%", "16"))
  testthat::expect_equal(round(res$base$top, 5), 0.00162)
  testthat::expect_equal(round(res$base$system_r, 3), 0.711)

  for (s in c("lcI10", "window.lcSF", "echarts/6.1.0/echarts.min.js", "i10_modele", "i10_kryterium", "drzewo", "uklad")) {
    testthat::expect_true(grepl(s, res$head, fixed = TRUE), info = s)
  }
})
