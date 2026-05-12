# Wspolne helpery dla chapterow z ekonometrii.

eco_fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

eco_regression_data <- function(n = 80, beta0 = 10, beta1 = 1.5,
                                sigma = 6, seed = 1) {
  set.seed(seed)
  x <- runif(n, 0, 50)
  y <- beta0 + beta1 * x + rnorm(n, 0, sigma)
  data.frame(x = x, y = y)
}

eco_diagnostic_data <- function(kind = "ok", n = 120, seed = 2) {
  set.seed(seed)
  x <- runif(n, 0, 100)
  e <- switch(kind,
    ok = rnorm(n, 0, 8),
    hetero = rnorm(n, 0, 2 + 0.16 * x),
    nonlinear = 0.03 * (x - 50)^2 + rnorm(n, 0, 8),
    outliers = {
      z <- rnorm(n, 0, 8)
      idx <- sample(seq_len(n), 4)
      z[idx] <- z[idx] + sample(c(-45, 45), length(idx), TRUE)
      z
    },
    rnorm(n, 0, 8)
  )
  data.frame(x = x, y = 25 + 0.7 * x + e)
}

eco_ts_data <- function(n = 48, trend = 0.8, season = 8,
                        noise = 4, seed = 3) {
  set.seed(seed)
  t <- seq_len(n)
  quarter <- factor(rep(1:4, length.out = n))
  y <- 80 + trend * t + season * sin(2 * pi * t / 4) + rnorm(n, 0, noise)
  data.frame(t = t, quarter = quarter, y = y)
}

eco_lp_vertices <- function(a1 = 2, a2 = 1, b1 = 100,
                            c1 = 1, c2 = 2, b2 = 90,
                            z1 = 30, z2 = 25) {
  vertices <- rbind(
    data.frame(x = 0, y = 0),
    data.frame(x = 0, y = min(b1 / a2, b2 / c2)),
    data.frame(x = min(b1 / a1, b2 / c1), y = 0)
  )
  det <- a1 * c2 - c1 * a2
  if (abs(det) > .Machine$double.eps) {
    xi <- (b1 * c2 - b2 * a2) / det
    yi <- (a1 * b2 - c1 * b1) / det
    if (xi >= 0 && yi >= 0 &&
        a1 * xi + a2 * yi <= b1 + 1e-8 &&
        c1 * xi + c2 * yi <= b2 + 1e-8) {
      vertices <- rbind(vertices, data.frame(x = xi, y = yi))
    }
  }
  vertices$value <- z1 * vertices$x + z2 * vertices$y
  vertices
}
