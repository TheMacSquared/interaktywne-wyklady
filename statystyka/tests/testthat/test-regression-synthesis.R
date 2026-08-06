testthat::test_that("kanoniczny wykład regresji zawiera mapę i przypadek pingwinów", {
  helpers <- readLines(
    file.path(stat_root, "06-regresja", "modules", "helpers.R"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  app <- readLines(
    file.path(stat_root, "06-regresja", "app.R"),
    warn = FALSE,
    encoding = "UTF-8"
  )

  testthat::expect_true(any(grepl(".penguins_data", helpers, fixed = TRUE)))
  testthat::expect_true(any(grepl("ch0_mapa.R", app, fixed = TRUE)))
  testthat::expect_true(any(grepl("ch3b_kontekst.R", app, fixed = TRUE)))
  testthat::expect_false(any(grepl("app_light", app, fixed = TRUE)))
  testthat::expect_false(any(grepl("06-regresja-pingwiny", app, fixed = TRUE)))
})

testthat::test_that("dane pingwinów są kompletne dla modeli dydaktycznych", {
  testthat::skip_if_not_installed("palmerpenguins")

  env <- new.env(parent = globalenv())
  env$app_dir <- file.path(stat_root, "06-regresja")
  sys.source(
    file.path(stat_root, "06-regresja", "modules", "helpers.R"),
    envir = env
  )

  required <- c(
    "species", "bill_length_mm", "bill_depth_mm",
    "flipper_length_mm", "body_mass_g"
  )
  testthat::expect_true(all(required %in% names(env$.penguins_data)))
  testthat::expect_gte(nrow(env$.penguins_data), 330)
  testthat::expect_setequal(
    levels(env$.penguins_data$species),
    c("Adelie", "Chinstrap", "Gentoo")
  )
})

testthat::test_that("synteza zachowuje dwanaście jawnych tematów", {
  env <- new.env(parent = globalenv())
  expressions <- parse(file.path(
    stat_root, "06-regresja", "modules", "ch0_mapa.R"
  ))
  eval(expressions[[1]], envir = env)

  testthat::expect_equal(nrow(env$.regression_topics), 12)
  testthat::expect_true(all(c("rdzeń", "pogłębienie") %in% env$.regression_topics$level))
  testthat::expect_false(anyDuplicated(env$.regression_topics$order) > 0)
})
