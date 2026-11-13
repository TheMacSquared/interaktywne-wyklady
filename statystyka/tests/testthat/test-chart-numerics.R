# Referencje liczy R przy każdym uruchomieniu. JS uruchamia te same funkcje,
# których używają widgety; brak kopii ich algorytmów w testach.
# Rozkłady lcDist: nazwa funkcji JS, argumenty i odpowiednik w R.
chart_dist_specs <- function() {
  xs <- c(-6, -3.2, -1.5, -0.3, 0, 0.2, 1.1, 2.7, 5, 8.3)
  xp <- c(0.001, 0.01, 0.3, 1, 2.5, 6, 12, 25, 60)
  ps <- c(1e-10, 1e-4, 0.001, 0.02, 0.1, 0.3, 0.5, 0.7, 0.975, 0.99, 0.999, 0.99999)
  grid <- function(...) unname(as.list(as.data.frame(t(expand.grid(..., KEEP.OUT.ATTRS = FALSE)))))
  list(
    list(fn = "dnorm", args = grid(xs, c(0, 1.5), c(0.4, 2)), r = function(a) dnorm(a[1], a[2], a[3])),
    list(fn = "pnorm", args = grid(c(xs, -38, 38), 0, 1), r = function(a) pnorm(a[1], a[2], a[3])),
    list(fn = "qnorm", args = grid(ps, c(0, 100), c(1, 15)), r = function(a) qnorm(a[1], a[2], a[3])),
    list(fn = "dbinom", args = grid(0:50, c(10, 20, 50), c(0.1, 0.25, 0.5, 0.7)), r = function(a) dbinom(a[1], a[2], a[3])),
    list(fn = "dpois", args = grid(0:40, c(0.5, 2, 5, 10)), r = function(a) dpois(a[1], a[2])),
    list(fn = "qpois", args = grid(c(0.5, 0.99, 0.999), c(0.5, 2, 5, 10)), r = function(a) qpois(a[1], a[2])),
    list(fn = "dgeom", args = grid(1:40, c(0.05, round(1 / 6, 4), 0.3, 0.5)), r = function(a) dgeom(a[1] - 1, a[2])),
    list(fn = "qgeom", args = grid(c(0.5, 0.99, 0.999), c(0.05, round(1 / 6, 4), 0.2, 0.3, 0.5)), r = function(a) qgeom(a[1], a[2]) + 1),
    list(fn = "dexp", args = grid(xp, c(0.3, 1, 2, 5)), r = function(a) dexp(a[1], a[2])),
    list(fn = "qexp", args = grid(ps, c(0.3, 1, 5)), r = function(a) qexp(a[1], a[2])),
    list(fn = "dt", args = grid(xs, c(1, 3, 5, 30)), r = function(a) dt(a[1], a[2])),
    list(fn = "dchisq", args = grid(xp, c(1, 2, 3, 5, 10, 20)), r = function(a) dchisq(a[1], a[2])),
    list(fn = "pchisq", args = grid(xp, c(1, 2, 3, 5, 10, 20)), r = function(a) pchisq(a[1], a[2])),
    list(fn = "qchisq", args = grid(c(0.01, 0.5, 0.95, 0.99), c(1, 2, 3, 5, 10, 20)), r = function(a) qchisq(a[1], a[2])),
    list(fn = "dlnorm", args = grid(xp, c(0, 1, 2), c(0.3, 0.8, 1)), r = function(a) dlnorm(a[1], a[2], a[3])),
    list(fn = "qlnorm", args = grid(ps, c(0, 1, 8.5), c(0.3, 0.7, 1)), r = function(a) qlnorm(a[1], a[2], a[3])),
    list(fn = "lgamma", args = grid(c(0.1, 0.5, 1, 1.5, 3.3, 10.5, 25, 100, 171.5)), r = function(a) lgamma(a[1]))
  )
}
chart_dist_cases <- function() {
  lapply(chart_dist_specs(), function(s) list(fn = s$fn, args = lapply(s$args, function(a) as.list(as.double(a)))))
}

chart_numeric_cases <- function() {
  array <- function(x) unname(as.list(as.double(x)))
  stat_case <- function(x) list(
    x = array(x), p = array(c(0, 0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 1)),
    trim = array(c(0, 0.1, 0.2, 0.49, 0.5))
  )
  fit_case <- function(x, y) list(x = array(x), y = array(y))
  list(
    shape = lapply(list(
      c(1, 2, 3, 4, 10), c(170.6, 170.7, 168, 181.2, 175, 162.4),
      c(12, 15, 19, 22, 30, 41, 47, 55, 80, 120), c(-3, -1, 0, 0, 0, 1, 2, 8)
    ), function(x) list(x = array(x), grid = array(seq(min(x) - 3, max(x) + 3, length.out = 64)))),
    kde2d = list(list(x = array(c(160, 165, 170, 172, 181, 190, 168, 175)),
                      y = array(c(55, 60, 66, 70, 80, 92, 58, 74)), n = 7,
                      lims = array(c(155, 195, 50, 95)))),
    seeded = list(
      list(kind = "normal", seed = 42, n = 20000, a = 165, b = 6),
      list(kind = "gamma", seed = 42, n = 20000, a = 4, b = 1),
      list(kind = "gamma", seed = 7, n = 20000, a = 0.6, b = 2)
    ),
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
    dist = chart_dist_cases(),
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

# Dokładna suma (algorytm Shewchuka, jak math.fsum w Pythonie). mean() w R
# na procesorach bez rozszerzonej precyzji (np. Apple Silicon) traci 1 + 1
# w c(1e16, 1, 1, -1e16) i daje 0 zamiast 0.5; wzorzec nie może zależeć od platformy.
exact_sum <- function(x) {
  partials <- numeric()
  for (v in x) {
    kept <- numeric()
    for (p in partials) {
      if (abs(v) < abs(p)) { t <- v; v <- p; p <- t }
      hi <- v + p; lo <- p - (hi - v)
      if (lo != 0) kept <- c(kept, lo)
      v <- hi
    }
    partials <- c(kept, v)
  }
  sum(partials)
}
exact_mean <- function(x, trim = 0) {
  if (!length(x)) return(NaN)
  if (trim >= 0.5) return(median(x))
  n <- length(x); lo <- floor(n * trim) + 1
  s <- sort(x)[lo:(n + 1 - lo)]
  exact_sum(s) / length(s)
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
    expect_chart_close(actual$mean, exact_mean(x), 1e-12, 1e-14, paste(label, "średnia"))
    expect_chart_close(actual$median, median(x), 1e-12, 1e-14, paste(label, "mediana"))
    expected <- quantile(x, p, type = 7, names = FALSE)
    expect_chart_close(actual$quantiles, expected, 1e-12, 1e-14, paste(label, "kwantyle"))
    expect_chart_close(actual$sortedQuantiles, expected, 1e-12, 1e-14, paste(label, "kwantyle posortowane"))
    expect_chart_close(actual$trimmedMeans, vapply(trim, function(t) exact_mean(x, t), numeric(1)),
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

testthat::test_that("SD, skośność, kurtoza i krzywa gęstości JS są zgodne z R (e1071 typ 3, nrd0)", {
  cases <- chart_numeric_cases()
  output <- run_chart_numerics(cases)
  for (i in seq_along(cases$shape)) {
    x <- numeric_values(cases$shape[[i]]$x); grid <- numeric_values(cases$shape[[i]]$grid)
    got <- output$shape[[i]]; label <- paste("kształt", i)
    n <- length(x); m <- mean(x); s <- sd(x)
    expect_chart_close(got$sd, s, 1e-12, 1e-12, paste(label, "SD"))
    expect_chart_close(got$skewness, mean((x - m)^3) / s^3, 1e-12, 1e-10, paste(label, "skośność"))
    expect_chart_close(got$kurtosis, mean((x - m)^4) / s^4 - 3, 1e-12, 1e-10, paste(label, "kurtoza"))
    expect_chart_close(got$bw, bw.nrd0(x), 1e-12, 1e-12, paste(label, "pasmo nrd0"))
    bw <- bw.nrd0(x)
    exact <- vapply(grid, function(t) mean(dnorm(t, x, bw)), numeric(1))
    expect_chart_close(got$density, exact, 1e-12, 1e-10, paste(label, "gęstość dokładna"))
    # density() przybliża przez FFT; dopuszczamy różnicę niewidoczną na wykresie.
    approx <- stats::approx(density(x, bw = bw, n = 1024, from = min(grid), to = max(grid)), xout = grid)$y
    expect_chart_close(got$density, approx, 2e-3 * max(exact), 0, paste(label, "gęstość vs density()"))
  }
  expected <- list(c(165, 6, 0), c(4, 2, 1), c(1.2, sqrt(0.6) * 2, 2 / sqrt(0.6)))
  for (i in seq_along(cases$seeded)) {
    got <- output$seeded[[i]]; e <- expected[[i]]; label <- paste("losowanie z ziarnem", i)
    testthat::expect_true(isTRUE(got$repeatable), info = paste(label, "powtarzalność"))
    expect_chart_close(got$mean, e[1], 0, 0.02, paste(label, "średnia"))
    expect_chart_close(got$sd, e[2], 0, 0.03, paste(label, "SD"))
    expect_chart_close(got$skewness, e[3], 0.06, 0.08, paste(label, "skośność"))
  }
})

testthat::test_that("gęstość dwuwymiarowa JS jest zgodna z MASS::kde2d", {
  cases <- chart_numeric_cases()
  output <- run_chart_numerics(cases)
  case <- cases$kde2d[[1]]
  ref <- MASS::kde2d(numeric_values(case$x), numeric_values(case$y), n = case$n,
                     lims = numeric_values(case$lims))$z
  got <- do.call(rbind, lapply(output$kde2d[[1]], numeric_values))
  expect_chart_close(as.vector(got), as.vector(ref), 1e-15, 1e-10, "kde2d")
})

testthat::test_that("rozkłady lcDist (wykład 03) są zgodne z d*, p*, q* z R", {
  cases <- chart_numeric_cases()
  output <- run_chart_numerics(cases)
  specs <- chart_dist_specs()
  for (i in seq_along(specs)) {
    s <- specs[[i]]
    ref <- vapply(s$args, function(a) s$r(as.double(a)), numeric(1))
    got <- numeric_values(output$dist[[i]])
    # Kwantyle liczone bisekcją lub sumowaniem: błąd względny poniżej 1e-12.
    expect_chart_close(got, ref, 1e-300, 1e-12, s$fn)
  }
})
