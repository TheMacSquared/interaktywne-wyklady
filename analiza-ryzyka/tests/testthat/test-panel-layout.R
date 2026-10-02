testthat::test_that("tryby paneli zachowują zgodność istniejących wywołań", {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(risk_root, "R", "palette.R"), env)
  sys.source(file.path(risk_root, "R", "lecture_layout.R"), env)
  html <- function(x) htmltools::renderTags(x)$html
  testthat::expect_match(html(env$figure_panel("Test", full_width = TRUE)), "lc-full")
  testthat::expect_false(grepl("lc-panel-text", html(env$figure_panel("Test"))))
  for (mode in c("compact", "text", "wide")) {
    testthat::expect_match(html(env$figure_panel("Test", width_mode = mode)), paste0("lc-panel-", mode))
  }
  testthat::expect_error(env$figure_panel("Test", full_width = TRUE, width_mode = "text"), "Wybierz")
  testthat::expect_error(env$figure_panel("Test", width_mode = "unknown"))
  testthat::expect_error(env$lc_table_region(min_width = -1))
  testthat::expect_match(html(env$lc_table_region(label = "Wyniki", min_width = 600)), 'aria-label="Wyniki"')
  testthat::expect_match(html(env$lc_widget_layout("Sterowanie", "Wykres", "beside")), "lc-widget-beside")
})
