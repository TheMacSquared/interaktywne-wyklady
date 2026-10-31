# Referencje liczy R przy każdym uruchomieniu. JS uruchamia te same funkcje,
# których używają widgety; brak kopii ich algorytmów w testach.
chart_numeric_cases <- function() {
  array <- function(x) unname(as.list(as.double(x)))
  stat_case <- function(x) list(
    x = array(x), p = array(c(0, 0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 1)),
    trim = array(c(0, 0.1, 0.2, 0.49, 0.5))
  )
  fit_case <- function(x, y) list(x = array(x), y = array(y))
  list(
    statistics = lapply(list(
      c(170.6, 170.7), c(4200, 3800, 5100, 4000, 100000),
      c(8, -3, 8, 0, 12, -3, 4), rep(7, 6), 42, numeric(),
      c(1e16, 1, 1, -1e16), c(1e9 + 0.25, 1e9 + 0.5, 1e9 + 0.75)
    ), stat_case),
    z = array(seq(-8, 8, length.out = 321)),
    intervals = list(list(a = -1, b = 1), list(a = -2, b = 2),
                     list(a = 0, b = 0.01), list(a = 2.5, b = 4),
                     list(a = -4, b = -2.5), list(a = 1, b = 1)),
    quantiles = unlist(lapply(list(c(0, 1), c(170, 8), c(50, 0.5)), function(par) {
      lapply(c(0, 0.03, 0.25, 0.5, 0.75, 0.97, 1), function(p) {
        list(p = p, mu = par[1], sd = par[2], lo = par[1] - 4 * par[2], hi = par[1] + 4 * par[2])
      })
    }), recursive = FALSE),
    fits = list(
      fit_case(c(1, 2, 3, 4, 5), c(2, 5, 4, 8, 9)),
      fit_case(c(1, 2, 3, 4, 5), c(9, 8, 4, 5, 2)),
      fit_case(c(-2, -1, 0, 1, 2), c(-3, -1, 1, 3, 5)),
      fit_case(c(1, 2, 3, 4), rep(5, 4)),
      fit_case(c(1, 2, 3, 4), c(1, 0, 0, 1)),
      fit_case(c(1, 2, 3, 4, 50), c(1, 3, 2, 4, -50)),
      fit_case(c(-2, -1, 0, 1, 2), 1000 * c(-2, -1, 0, 1, 2) + c(1, -2, 2, -2, 1) * 1e-7)
    ),
    kurtosis = lapply(c(seq(-1.2, 6, by = 0.2), -0.010001, -0.01, 0.01, 0.010001), function(ek) {
      list(ek = ek, x = array(c(-5, -2.5, -1, 0, 1, 2.5, 5)), cutoffs = array(c(0, 1, 2.5, 5)))
    }),
    sampling = list(
      list(kind = "normal", n = 20000, a = 170, b = 8),
      list(kind = "gamma", n = 20000, a = 1, b = 5),
      list(kind = "gamma", n = 20000, a = 4, b = 2),
      list(kind = "uniform", n = 20000, a = -2, b = 4),
      list(kind = "integer", n = 20000, a = 1, b = 6)
    )
  )
}

run_chart_numerics <- function(input) {
  node <- Sys.which("node")
  testthat::skip_if(!nzchar(node), "Porównania R–JS wymagają Node.js.")
  input_path <- tempfile(fileext = ".json")
  output_path <- tempfile(fileext = ".json")
  on.exit(unlink(c(input_path, output_path)), add = TRUE)
  jsonlite::write_json(input, input_path, auto_unbox = TRUE, digits = NA)
  result <- suppressWarnings(system2(node, c(
    shQuote(file.path(stat_root, "tests", "js", "chart_numerics.js")),
    shQuote(input_path), shQuote(output_path)
  ), stdout = TRUE, stderr = TRUE))
  status <- attr(result, "status")
  if (!is.null(status) && status != 0L) stop(paste(result, collapse = "\n"))
  jsonlite::read_json(output_path, simplifyVector = FALSE)
}

numeric_values <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (!is.list(x)) return(as.double(x))
  vapply(x, function(v) if (is.null(v)) NA_real_ else as.double(v), numeric(1))
}

expect_chart_close <- function(actual, expected, abs_tol, rel_tol = 0, label) {
  actual <- numeric_values(actual)
  expected <- as.double(expected)
  testthat::expect_equal(length(actual), length(expected), info = label)
  testthat::expect_identical(is.na(actual), is.na(expected), info = label)
  valid <- is.finite(expected)
  error <- abs(actual[valid] - expected[valid])
  tolerance <- abs_tol + rel_tol * abs(expected[valid])
  testthat::expect_true(all(is.finite(actual[valid]) & error <= tolerance), info = paste(
    label, "maksymalny błąd:", max(c(0, error)), "tolerancja bezwzględna:", abs_tol
  ))
}

testthat::test_that("statystyki opisowe JS są zgodne z R, także dla małych i nietypowych prób", {
  cases <- chart_numeric_cases()
  output <- run_chart_numerics(cases)
  for (i in seq_along(cases$statistics)) {
    case <- cases$statistics[[i]]
    x <- numeric_values(case$x)
    p <- numeric_values(case$p)
    trim <- numeric_values(case$trim)
    actual <- output$statistics[[i]]
    label <- paste("próba", i)
    expect_chart_close(actual$mean, mean(x), 1e-12, 1e-14, paste(label, "średnia"))
    expect_chart_close(actual$median, median(x), 1e-12, 1e-14, paste(label, "mediana"))
    expected <- quantile(x, p, type = 7, names = FALSE)
    expect_chart_close(actual$quantiles, expected, 1e-12, 1e-14, paste(label, "kwantyle"))
    expect_chart_close(actual$sortedQuantiles, expected, 1e-12, 1e-14, paste(label, "kwantyle posortowane"))
    expect_chart_close(actual$trimmedMeans, vapply(trim, function(t) mean(x, trim = t), numeric(1)),
                       1e-12, 1e-14, paste(label, "średnie ucięte"))
  }
})

testthat::test_that("dystrybuanta, pola i kwantyle normalne JS mają kontrolowany błąd względem R", {
  cases <- chart_numeric_cases()
  output <- run_chart_numerics(cases)
  # Abramowitz–Stegun: gwarancja błędu dotyczy prawdopodobieństwa, nie błędu względnego w ogonie.
  expect_chart_close(output$probabilities, pnorm(numeric_values(cases$z)), 8e-8, label = "pnorm")
  expected <- vapply(cases$intervals, function(case) pnorm(case$b) - pnorm(case$a), numeric(1))
  expect_chart_close(output$intervals, expected, 1.6e-7, label = "pola pod krzywą")
  for (i in seq_along(cases$quantiles)) {
    case <- cases$quantiles[[i]]
    expected <- min(case$hi, max(case$lo, qnorm(case$p, case$mu, case$sd)))
    # Widget ogranicza x do widocznego zakresu; błąd kwantyla podajemy w jednostkach σ.
    expect_chart_close(output$quantiles[[i]], expected, 2e-6 * case$sd,
                       label = paste("qnorm w zakresie rysunku", i))
  }
})

testthat::test_that("regresja, korelacja i SD reszt JS są zgodne z modelem R", {
  cases <- chart_numeric_cases()
  output <- run_chart_numerics(cases)
  for (i in seq_along(cases$fits)) {
    case <- cases$fits[[i]]
    x <- numeric_values(case$x)
    y <- numeric_values(case$y)
    model <- lm(y ~ x)
    actual <- output$fits[[i]]
    expect_chart_close(actual$a, coef(model)[1], 1e-10, 1e-12, paste("wyraz wolny", i))
    expect_chart_close(actual$b, coef(model)[2], 1e-10, 1e-12, paste("nachylenie", i))
    expected_r <- if (sd(y) == 0) NA_real_ else suppressWarnings(cor(x, y))
    expect_chart_close(actual$r, expected_r, 1e-12, label = paste("korelacja", i))
    # To SD reszt (n − 1), a nie błąd standardowy regresji z summary(lm) (n − 2).
    expect_chart_close(actual$sd, sd(residuals(model)), 1e-10, 1e-10, paste("SD reszt", i))
  }
})

testthat::test_that("gęstości i całkowanie widgetu kurtozy są zgodne z R", {
  cases <- chart_numeric_cases()
  output <- run_chart_numerics(cases)
  for (i in seq_along(cases$kurtosis)) {
    case <- cases$kurtosis[[i]]
    ek <- case$ek
    x <- numeric_values(case$x)
    cutoffs <- numeric_values(case$cutoffs)
    # Modele zdefiniowane w widgecie: symetryczna beta, normalny i skalowany t.
    if (ek < -0.01) {
      shape <- max(1.01, (-6 / ek - 3) / 2)
      support <- sqrt(2 * shape + 1)
      density <- dbeta((x / support + 1) / 2, shape, shape) / (2 * support)
      upper <- pbeta((cutoffs / support + 1) / 2, shape, shape, lower.tail = FALSE)
    } else if (ek <= 0.01) {
      density <- dnorm(x)
      upper <- pnorm(cutoffs, lower.tail = FALSE)
    } else {
      df <- max(4.5, 6 / ek + 4)
      scale <- sqrt(df / (df - 2))
      density <- dt(x * scale, df) * scale
      upper <- pt(cutoffs * scale, df, lower.tail = FALSE)
    }
    actual <- output$kurtosis[[i]]
    expect_chart_close(actual$density, density, 2e-10, 2e-8, paste("gęstość, kurtoza", ek))
    expect_chart_close(actual$upper, upper, 1e-9, 1e-7, paste("całka, kurtoza", ek))
  }
})

testthat::test_that("generatory JS zachowują parametry i kształt rozkładów R", {
  cases <- chart_numeric_cases()
  output <- run_chart_numerics(cases)
  probs <- c(0.1, 0.5, 0.9)
  for (i in seq_along(cases$sampling)) {
    case <- cases$sampling[[i]]
    a <- case$a
    b <- case$b
    if (case$kind == "normal") {
      mu <- a; variance <- b^2
      q <- qnorm(probs, a, b); density <- dnorm(q, a, b)
    } else if (case$kind == "gamma") {
      mu <- a * b; variance <- a * b^2
      q <- qgamma(probs, shape = a, scale = b); density <- dgamma(q, shape = a, scale = b)
    } else if (case$kind == "uniform") {
      mu <- (a + b) / 2; variance <- (b - a)^2 / 12
      q <- qunif(probs, a, b); density <- dunif(q, a, b)
    } else {
      mu <- mean(a:b); variance <- mean(((a:b) - mu)^2)
    }
    actual <- output$sampling[[i]]
    label <- paste("losowanie", case$kind, a, b)
    # Porównujemy rozkład z parametrami R, nie identyczność wylosowanych liczb.
    # Stały strumień Math.random w runnerze sprawia, że wynik jest deterministyczny.
    expect_chart_close(actual$mean, mu, 6 * sqrt(variance / case$n), label = paste(label, "średnia"))
    expect_chart_close(actual$variance, variance, 0, 0.05, paste(label, "wariancja"))
    if (case$kind == "integer") {
      k <- b - a + 1
      expect_chart_close(numeric_values(actual$counts) / case$n, rep(1 / k, k),
                         6 * sqrt((1 / k) * (1 - 1 / k) / case$n), label = paste(label, "częstości"))
    } else {
      for (j in seq_along(probs)) {
        expect_chart_close(actual$quantiles[[j]], q[j],
                           6 * sqrt(probs[j] * (1 - probs[j]) / case$n) / density[j],
                           label = paste(label, "kwantyl", probs[j]))
      }
    }
    if (case$kind %in% c("uniform", "integer")) {
      testthat::expect_true(actual$min >= a && actual$max <= b, info = label)
    }
    if (case$kind == "gamma") testthat::expect_true(actual$min >= 0, info = label)
  }
})
