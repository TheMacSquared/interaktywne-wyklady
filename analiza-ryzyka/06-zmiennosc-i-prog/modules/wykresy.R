# Ryciny wykładu 06 liczone w przeglądarce (modules/wykresy.js, silnik lcSF).
# Tutaj: dane losowane ze stałym ziarnem — te same liczby co w dawnych wykresach
# ggplot — policzone w R przy budowaniu strony i przekazane rycinom w `config`.
# Wymaga: risk_math.R, risk_block.R (ładowany przed block.R).

# z6_hist: 5000 temperatur N(82, 3) z ziarnem 606; rycina bierze pierwsze n.
z6_hist_config <- local({
  set.seed(606)
  list(values = round(rnorm(5000, 82, 3), 3))
})

# z6_ls: 700 par (L, S). Standaryzowane odchylenia z ziarnem 607 — obciążenie
# to load + 8 · zl, wytrzymałość to strength + 7 · zs, jak rnorm(n, mean, sd).
z6_ls_config <- local({
  set.seed(607)
  n <- 700
  list(zl = round(rnorm(n), 4), zs = round(rnorm(n), 4), sd_load = 8, sd_strength = 7)
})

# z6_shapes: trzy rozkłady o średniej 82 i SD 3 (ziarno 608, 5000 obserwacji).
# Histogram: przedziały jak geom_histogram(bins = 60). Wykres kwantylowy: punkty
# stat_qq() — w ogonach (|z| > 1.8) wszystkie, w środku co dwunasty — i prosta
# stat_qq_line() przez kwartyle. Odczyty ogonów liczone z modelu, nie z próby.
z6_shapes_config <- local({
  gamma_scale <- 3 / sqrt(3)
  draw <- list(
    normal = function() rnorm(5000, 82, 3),
    skew   = function() 82 - 3 * gamma_scale + rgamma(5000, shape = 3, scale = gamma_scale),
    heavy  = function() 82 + 3 * rt(5000, df = 3) / sqrt(3)
  )
  tail_p <- list(
    normal = function(c) pnorm(c, 82, 3, lower.tail = FALSE),
    skew   = function(c) pgamma(c - (82 - 3 * gamma_scale), shape = 3, scale = gamma_scale, lower.tail = FALSE),
    heavy  = function(c) pt((c - 82) * sqrt(3) / 3, df = 3, lower.tail = FALSE)
  )
  lapply(stats::setNames(names(draw), names(draw)), function(shape) {
    set.seed(608)
    x <- draw[[shape]]()
    bins <- ggplot2::ggplot_build(
      ggplot2::ggplot(data.frame(x), ggplot2::aes(x)) + ggplot2::geom_histogram(bins = 60)
    )$data[[1]]
    n <- length(x)
    theoretical <- qnorm(ppoints(n))
    sorted <- sort(x)
    keep <- abs(theoretical) > 1.8 | seq_len(n) %% 12 == 0
    qx <- qnorm(c(.25, .75)); qy <- unname(quantile(x, c(.25, .75)))
    slope <- diff(qy) / diff(qx)
    list(
      bins = lapply(seq_len(nrow(bins)), function(i) c(round(bins$xmin[i], 4), round(bins$xmax[i], 4), bins$count[i])),
      qq = lapply(which(keep), function(i) c(round(theoretical[i], 4), round(sorted[i], 3))),
      qq_line = c(intercept = qy[1] - slope * qx[1], slope = slope),
      qq_range = c(range(theoretical), range(x)),
      p85 = risk_fmt_p(tail_p[[shape]](85)),
      p91 = formatC(tail_p[[shape]](91), format = "f", digits = 4)
    )
  })
})

# Interwencje (z6_action): μ, σ i próg każdego wariantu.
z6_action_config <- list(
  base = c(82, 3, 85), mean = c(80, 3, 85), sd = c(82, 2, 85), threshold = c(82, 3, 87)
)
