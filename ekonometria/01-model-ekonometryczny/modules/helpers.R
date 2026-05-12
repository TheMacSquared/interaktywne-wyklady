# Lokalne helpery dla wykładu 01: Model ekonometryczny.

eco_make_regression_data <- function(n = 80, beta0 = 12, beta1 = 1.8,
                                     sigma = 6, seed = 123) {
  set.seed(seed)
  naklady <- runif(n, 5, 45)
  error <- rnorm(n, 0, sigma)
  sprzedaz <- beta0 + beta1 * naklady + error
  data.frame(
    naklady = naklady,
    sprzedaz = sprzedaz,
    fitted_true = beta0 + beta1 * naklady
  )
}

eco_make_diagnostic_data <- function(kind = "ok", n = 110, seed = 321) {
  set.seed(seed)
  x <- runif(n, 0, 100)
  eps <- switch(kind,
    ok = rnorm(n, 0, 8),
    hetero = rnorm(n, 0, 2 + 0.16 * x),
    nonlinear = 0.03 * (x - 50)^2 + rnorm(n, 0, 8),
    outlier = {
      e <- rnorm(n, 0, 8)
      idx <- sample(seq_len(n), 4)
      e[idx] <- e[idx] + sample(c(-45, 45), length(idx), replace = TRUE)
      e
    },
    rnorm(n, 0, 8)
  )
  y <- 25 + 0.65 * x + eps
  data.frame(x = x, y = y)
}

eco_make_ts_data <- function(n = 48, trend = 0.8, season = 8, noise = 4,
                             seed = 2024) {
  set.seed(seed)
  t <- seq_len(n)
  quarter <- factor(rep(1:4, length.out = n))
  y <- 80 + trend * t + season * sin(2 * pi * t / 4) + rnorm(n, 0, noise)
  data.frame(t = t, kwartal = quarter, y = y)
}

eco_forecast_trend <- function(df, h = 8) {
  fit <- lm(y ~ t + kwartal, data = df)
  future <- data.frame(
    t = (max(df$t) + 1):(max(df$t) + h),
    kwartal = factor(rep(1:4, length.out = h), levels = levels(df$kwartal))
  )
  pred <- predict(fit, newdata = future, interval = "prediction")
  cbind(future, as.data.frame(pred))
}

eco_lp_grid <- function(a1 = 2, a2 = 1, b1 = 100,
                        c1 = 1, c2 = 2, b2 = 90,
                        obj1 = 30, obj2 = 25) {
  x <- seq(0, 80, length.out = 300)
  y1 <- pmax(0, (b1 - a1 * x) / a2)
  y2 <- pmax(0, (b2 - c1 * x) / c2)
  boundary <- data.frame(x = x, y = pmin(y1, y2))
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
  vertices$value <- obj1 * vertices$x + obj2 * vertices$y
  list(boundary = boundary, vertices = vertices[order(vertices$x, vertices$y), ])
}

eco_metric <- function(label, value, caption = NULL, color = upwr_accent) {
  lc_stat_box(label = label, value = value, caption = caption, color = color)
}
