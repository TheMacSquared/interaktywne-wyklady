# Ryciny wykładu 06 liczone w przeglądarce (modules/wykresy.js, silnik lcSF).
# Tutaj: dane losowane ze stałym ziarnem — te same liczby co w dawnych wykresach
# ggplot — policzone w R przy budowaniu strony i przekazane rycinom w `config`.
# Wymaga: risk_math.R, risk_block.R (ładowany przed block.R).

# z6_shapes: trzy rozkłady o średniej 82 i SD 3 (ziarno 608, 5000 obserwacji).
# Wykres kwantylowy: punkty stat_qq() — w ogonach (|z| > 1.8) wszystkie, w środku
# co dwunasty — i prosta stat_qq_line() przez kwartyle.
z6_shapes_config <- local({
  gamma_scale <- 3 / sqrt(3)
  draw <- list(
    normal = function() rnorm(5000, 82, 3),
    skew   = function() 82 - 3 * gamma_scale + rgamma(5000, shape = 3, scale = gamma_scale),
    heavy  = function() 82 + 3 * rt(5000, df = 3) / sqrt(3)
  )
  lapply(stats::setNames(names(draw), names(draw)), function(shape) {
    set.seed(608)
    x <- draw[[shape]]()
    n <- length(x)
    theoretical <- qnorm(ppoints(n))
    sorted <- sort(x)
    keep <- abs(theoretical) > 1.8 | seq_len(n) %% 12 == 0
    qx <- qnorm(c(.25, .75)); qy <- unname(quantile(x, c(.25, .75)))
    slope <- diff(qy) / diff(qx)
    list(
      qq = lapply(which(keep), function(i) c(round(theoretical[i], 4), round(sorted[i], 3))),
      qq_line = c(intercept = qy[1] - slope * qx[1], slope = slope),
      qq_range = c(range(theoretical), range(x))
    )
  })
})
