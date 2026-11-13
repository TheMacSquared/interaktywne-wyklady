testthat::test_that("sceny wykładu 04 rysuje przeglądarka: kontenery z danymi, bez wyjść Shiny", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not_installed("xml2")

  res <- callr::r(function(path) {
    setwd(path)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    html <- paste(vapply(env$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    nodes <- xml2::xml_find_all(doc, "//div[@class='p4s-scene']")
    list(
      scenes = xml2::xml_attr(nodes, "data-scene"),
      configs = lapply(xml2::xml_attr(nodes, "data-config"), jsonlite::fromJSON, simplifyVector = FALSE),
      head = { t <- htmltools::renderTags(env$ui); paste(t$head, t$html) },
      shiny_ids = grepl('id="p4s_', html, fixed = TRUE),
      buttons = length(xml2::xml_find_all(doc, "//*[@data-p4s]"))
    )
  }, args = list(path = file.path(risk_root, "04-wiele-prob")), show = FALSE)

  testthat::expect_setequal(res$scenes, c("banany", "prog", "paleta", "tabliczka", "kierowcy", "odbior"))
  testthat::expect_false(res$shiny_ids)
  testthat::expect_gte(res$buttons, 15L)
  testthat::expect_true(grepl("lcP4Scenes", res$head, fixed = TRUE))

  cfg <- stats::setNames(res$configs, res$scenes)
  testthat::expect_length(cfg$banany$spider, 12L)
  testthat::expect_equal(sum(unlist(cfg$banany$spider)), 2L)
  testthat::expect_length(cfg$kierowcy$first, 100L)
  testthat::expect_equal(sum(unlist(cfg$kierowcy$first) <= 2500), 22L)
  testthat::expect_length(cfg$tabliczka$outcomes, 8L)
  testthat::expect_equal(vapply(cfg$tabliczka$outcomes, function(r) sum(unlist(r)), numeric(1)),
    c(0, 1, 1, 1, 2, 2, 2, 3))
  testthat::expect_equal(cfg$odbior$quality$weak, 0.08)
})

testthat::test_that("wykresy wykładu 04 to ryciny lcSF: bez plotOutput, z danymi z R", {
  testthat::skip_if_not_installed("callr")
  testthat::skip_if_not_installed("xml2")

  res <- callr::r(function(path) {
    setwd(path)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    html <- paste(vapply(env$.chapters, function(ch) htmltools::renderTags(ch$content)$html, ""), collapse = "\n")
    doc <- xml2::read_html(html)
    nodes <- xml2::xml_find_all(doc, "//div[contains(concat(' ', @class, ' '), ' lc-sf ')]")
    t <- htmltools::renderTags(env$ui)
    list(
      figs = xml2::xml_attr(nodes, "data-fig"),
      configs = lapply(xml2::xml_attr(nodes, "data-config"), function(x) if (is.na(x)) NULL else jsonlite::fromJSON(x)),
      plot_outputs = length(xml2::xml_find_all(doc, "//*[contains(@class, 'shiny-plot-output')]")),
      head = paste(t$head, t$html)
    )
  }, args = list(path = file.path(risk_root, "04-wiele-prob")), show = FALSE)

  testthat::expect_setequal(res$figs, c("p4_assume", "p4_bg", "p4_binom", "p4_batches", "p4_bt",
    "p4_one", "p4_np", "p4_oc", "p4_zero", "p4_closing"))
  testthat::expect_equal(res$plot_outputs, 0L)
  for (s in c("echarts/6.1.0/echarts.min.js", "window.lcSF", "window.lcDist", "lcP4Charts")) {
    testthat::expect_true(grepl(s, res$head, fixed = TRUE), info = s)
  }
  cfg <- stats::setNames(res$configs, res$figs)
  # Galeria założeń: udziały sumują się do 1, odczyty z symulacji o ziarnie 4403.
  for (v in c("stable", "mixture", "dependent")) testthat::expect_equal(sum(cfg$p4_assume$obs[[v]]), 1)
  testthat::expect_equal(cfg$p4_assume$reads$stable$p0, "0.133")
  # Powtórzenia: pierwsze losowanie jak set.seed(2404) w dawnym serwerze.
  testthat::expect_equal(cfg$p4_batches$first, local({ set.seed(2404); rbinom(500, 100, 0.02) }))
})
