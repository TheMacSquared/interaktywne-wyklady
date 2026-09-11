testthat::test_that("generator odtwarza CSV bez recyklingu i zachowuje daty scenariusza", {
  data_dir <- file.path(stat_root, "04-szeregi-czasowe", "dane")
  target <- tempfile("series-data-")
  dir.create(target)
  withr::defer(unlink(target, recursive = TRUE))
  withr::local_envvar(c(TS_DATA_OUTPUT_DIR = target))
  generator <- new.env(parent = globalenv())
  testthat::expect_warning(
    capture.output(sys.source(file.path(data_dir, "generate_data.R"), envir = generator)),
    NA
  )
  testthat::expect_length(generator$trend_bezr, 288)
  testthat::expect_length(generator$base_price, 288)
  testthat::expect_equal(generator$base_price[generator$date_pszen == as.Date("2022-06-01")], 300)
  testthat::expect_equal(tail(generator$base_price, 1), 170)
  testthat::expect_equal(tail(generator$trend_bezr, 1), 3.5)
  testthat::expect_equal(generator$covid_factor[generator$date_noclegi == as.Date("2020-03-01")], .2)
  files <- list.files(target, pattern = "[.]csv$")
  testthat::expect_length(files, 7)
  for (file in files) {
    actual <- read.csv(file.path(target, file))
    expected <- read.csv(file.path(data_dir, file))
    testthat::expect_equal(actual, expected, info = file)
    testthat::expect_true(all(grepl("Dane syntetyczne", actual$data_origin)), info = file)
    # Pierwszy zwrot nie ma poprzedniej obserwacji: jest jawnie brakujący.
    if (file == "wig20_tygodniowy.csv") {
      testthat::expect_true(is.na(actual$log_return[1]))
      testthat::expect_true(all(is.finite(actual$log_return[-1])))
      actual$log_return <- NULL
    }
    numeric_cols <- vapply(actual, is.numeric, logical(1))
    testthat::expect_true(all(is.finite(as.matrix(actual[, numeric_cols]))), info = file)
  }
})

testthat::test_that("nowe szeregi pozwalają dopasować modele i zachowują podpisy", {
  testthat::skip_if_not_installed("callr")
  result <- callr::r(function(path) {
    setwd(path)
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    for (x in list(app$bezrobocie_ts(), app$pszenica_ts(), app$noclegi_ts())) {
      fit <- forecast::Arima(x, order = c(1, 1, 1))
      pred <- forecast::forecast(fit, h = 12)
      stopifnot(all(is.finite(pred$mean)), length(pred$mean) == 12)
    }
    plot <- app$ts_mark_synthetic(ggplot2::ggplot(data.frame(x = 1:3), ggplot2::aes(x, x)) +
                                  ggplot2::geom_line())
    stopifnot(grepl("Dane syntetyczne", plot$labels$caption))
    for (chapter in app$.chapters) {
      stopifnot(grepl("Dane syntetyczne", htmltools::renderTags(chapter$content)$html))
    }
    TRUE
  }, args = list(path = file.path(stat_root, "04-szeregi-czasowe")), timeout = 60)
  testthat::expect_true(result)
})
