testthat::test_that("sf_ggplot przenosi wykres ggplot do opisu ryciny lc_spec", {
  suppressPackageStartupMessages({ library(shiny); library(ggplot2) })
  env <- new.env(parent = globalenv())
  for (f in c("palette.R", "theme_upwr.R", "lecture_layout.R", "lc_stat_figures.R", "lc_sf_ggplot.R")) {
    sys.source(file.path(dirname(stat_root), "shared", f), env)
  }
  types <- function(s) vapply(s$layers, `[[`, "", "type")

  # Histogram: słupki i liczebności jak ggplot_build(), barwa palety jako token motywu.
  v <- c(5.011, 3.762, 4.583, 5.172, 2.967, 3.633, 3.720, 4.182)
  p <- ggplot(data.frame(v), aes(v)) + geom_histogram(bins = 5, fill = env$upwr_cat[["niebo"]], alpha = 0.8) +
    labs(x = "Średnia", y = NULL)
  s <- env$sf_ggplot(p)
  b <- ggplot_build(p)$data[[1]]
  testthat::expect_equal(types(s), "bins")
  testthat::expect_equal(vapply(s$layers[[1]]$data, `[`, 0, 3), b$count)
  testthat::expect_equal(vapply(s$layers[[1]]$data, `[`, 0, 1), b$xmin)
  testthat::expect_equal(s$layers[[1]]$colors[[1]], "@niebo/0.8")
  testthat::expect_equal(s$xName, "Średnia")
  testthat::expect_equal(s$yName, "")                       # labs(y = NULL): bez nazwy osi

  # Oś kategorii: podpisy z ggplot; legenda wypełnienia z dyskretnej skali.
  d <- data.frame(g = c("b", "a", "c"), n = c(3, 1, 2), k = c("x", "y", "x"))
  s <- env$sf_ggplot(ggplot(d, aes(g, n, fill = k)) + geom_col() +
                       scale_fill_manual(values = c(x = env$upwr_accent, y = env$upwr_secondary)))
  testthat::expect_equal(unname(unlist(s$xLabels)), c("a", "b", "c"))
  testthat::expect_equal(vapply(s$legend, `[[`, "", "text"), c("x", "y"))
  testthat::expect_equal(s$legend[[1]]$color, "@accent")

  # Brak wartości przerywa linię (dwa odcinki), punkty z brakami są pomijane.
  d <- data.frame(x = 1:6, y = c(1, 2, NA, 4, 5, 6))
  s <- env$sf_ggplot(ggplot(d, aes(x, y)) + geom_line(na.rm = TRUE) + geom_point(na.rm = TRUE))
  testthat::expect_equal(types(s), c("line", "line", "points"))
  testthat::expect_length(s$layers[[3]]$data, 5)

  # Panele facet_wrap(): osobne wykresy z nagłówkami.
  d <- data.frame(x = 1:4, y = 1:4, f = c("A", "A", "B", "B"))
  s <- env$sf_ggplot(ggplot(d, aes(x, y)) + geom_point() + facet_wrap(~f))
  testthat::expect_length(s$charts, 2)
  testthat::expect_equal(s$headings, c("A", "B"))

  # Duże liczby z odstępem tysięcy: podpisy rysuje silnik (yBig), nie tekst z ggplot.
  d <- data.frame(x = 1:3, y = c(-3e6, 1e6, 5e6))
  s <- env$sf_ggplot(ggplot(d, aes(x, y)) + geom_point() + scale_y_continuous(labels = scales::label_number(big.mark = " ")))
  testthat::expect_null(s$yLabels)
  testthat::expect_true(isTRUE(s$yBig))
  testthat::expect_true(isTRUE(s$yNegative))

  # Rycina z wariantów: opis każdego wariantu w data-config.
  html <- as.character(env$lc_sf_ggplot(list(a = ggplot(d, aes(x, y)) + geom_point(), b = ggplot(d, aes(y, x)) + geom_point()),
                                        aria = "Test", control = env$lc_segmented("v", NULL, choices = c("A" = "a", "B" = "b"))))
  testthat::expect_match(html, 'data-fig="lc_spec"', fixed = TRUE)
  testthat::expect_match(html, "&quot;variants&quot;", fixed = TRUE)
})
