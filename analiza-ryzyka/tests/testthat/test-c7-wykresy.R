# Wykresy wykładu 07 liczone w przeglądarce (modules/wykresy.js) względem R:
# te same liczby, które dawał serwer, dla każdego położenia suwaków.

c7_node <- function(script) {
  out <- system2("node", c("-e", shQuote(script)), stdout = TRUE, stderr = TRUE)
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = TRUE)
}

c7_env <- function() {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(risk_root, "R", "risk_math.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "palette.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "lecture_layout.R"), envir = env)
  sys.source(file.path(risk_root, "R", "risk_block.R"), envir = env)
  sys.source(file.path(risk_root, "07-czas-zycia", "modules", "wykresy.R"), envir = env)
  env
}

testthat::test_that("wykład 07: odczyty policzone w R przy budowaniu są tymi z dawnego serwera", {
  env <- c7_env()
  cfg <- env$c7_timeline_config
  testthat::expect_length(cfg$stats, length(seq(300, 2500, 50)))
  # Ustawienie domyślne (1200 h): cztery awarie, łączny czas 7250 h.
  at <- function(f) cfg$stats[[(f - cfg$min) / cfg$step + 1]]
  testthat::expect_equal(at(1200), c("4 z 8", "4 z 8", "7250 h", "612.5 h", "1812.5 h"))
  testthat::expect_equal(at(300), c("1 z 8", "7 z 8", "2320 h", "220 h", "2320 h"))
  testthat::expect_equal(at(2500)[1:2], c("7 z 8", "1 z 8"))

  testthat::expect_equal(env$c7_same_mean_config$scales, 1500 / gamma(1 + 1 / c(.7, 1, 2.5)))
  # Każdy model ma MTTF = 1500 h.
  testthat::expect_equal(env$c7_same_mean_config$scales * gamma(1 + 1 / env$c7_same_mean_config$shapes), rep(1500, 3))

  bottom <- env$c7_bathtub_config$bottom
  testthat::expect_length(bottom, 19)
  # Nasilenie 1 (pozycja 9): dno z panelu „Suma trzech składowych”.
  testthat::expect_equal(bottom[[9]]$t_text, "t ≈ 1310")
  testthat::expect_equal(bottom[[9]]$h_text, "0.184")
})

testthat::test_that("wykład 07: funkcje w przeglądarce dają liczby R", {
  testthat::skip_if(Sys.which("node") == "", "Node.js nie jest dostępny")
  env <- c7_env()
  root <- normalizePath(dirname(risk_root))
  res <- c7_node(sprintf('
    globalThis.window = globalThis;
    require("%s/shared/lc_dist.js"); require("%s/analiza-ryzyka/R/risk_figures.js");
    require("%s/analiza-ryzyka/07-czas-zycia/modules/wykresy.js");
    const Z = globalThis.lcC7Charts, R = globalThis.lcRisk, D = globalThis.lcDist, out = {};
    out.gamma = []; for (let k = 0.5; k <= 8; k += 0.5) out.gamma.push(Z.gammaReads(k));
    out.dgamma = [5, 250, 1000, 6000].map(t => D.dgamma(t, 2.5, 500));
    out.exp = [0, 1000, 4000].map(t => Z.exponential(t, 1500));
    out.expText = []; for (let t = 0; t <= 4000; t += 50) { const e = Z.exponential(t, 1500); out.expText.push([R.fmtP(e.cdf), R.fmtP(e.reliability), e.density.toFixed(6), e.hazard.toFixed(6)]); }
    out.weib = []; for (let b = 4; b <= 40; b++) out.weib.push(R.fmtP(R.weibull(1000, b / 10, 1700).reliability));
    out.plan = []; for (let t = 100; t <= 3000; t += 50) { const r = R.weibull(t, 2, 1700).reliability; out.plan.push([R.fmtP(r), R.fmtP(1 - r)]); }
    out.bath = Z.bathtub(1310, 1);
    out.obs = Z.observe([220, 480, 760, 990, 1350, 1750, 2300, 3100], 1200);
    out.idx = [Z.sliderIndex("0.2", 0.2, 0.1), Z.sliderIndex("1", 0.2, 0.1), Z.sliderIndex("2", 0.2, 0.1), Z.sliderIndex("1200", 300, 50)];
    console.log(JSON.stringify(out));', root, root, root))

  k <- seq(.5, 8, .5)
  testthat::expect_equal(res$gamma$mean, paste(k * 500, "h"))
  testthat::expect_equal(res$gamma$mode, paste(pmax(0, (k - 1) * 500), "h"))
  testthat::expect_equal(res$gamma$tail, vapply(k, function(x) env$risk_fmt_p(pgamma(1000, shape = x, rate = 1 / 500, lower.tail = FALSE)), ""))
  testthat::expect_equal(res$gamma$family, ifelse(k %% 1 == 0, "Erlang", "gamma"))
  testthat::expect_equal(res$dgamma, dgamma(c(5, 250, 1000, 6000), shape = 2.5, rate = 1 / 500), tolerance = 1e-10)

  e <- env$risk_exponential(c(0, 1000, 4000), 1 / 1500)
  testthat::expect_equal(res$exp$density, e$density, tolerance = 1e-12)
  testthat::expect_equal(res$exp$cdf, e$cdf, tolerance = 1e-12)
  testthat::expect_equal(res$exp$reliability, e$reliability, tolerance = 1e-12)
  t <- seq(0, 4000, 50); e <- env$risk_exponential(t, 1 / 1500)
  testthat::expect_equal(res$expText[, 1], vapply(e$cdf, env$risk_fmt_p, ""))
  testthat::expect_equal(res$expText[, 2], vapply(e$reliability, env$risk_fmt_p, ""))
  testthat::expect_equal(res$expText[, 3], formatC(e$density, format = "f", digits = 6))
  testthat::expect_equal(res$expText[, 4], formatC(e$hazard, format = "f", digits = 6))

  testthat::expect_equal(res$weib, vapply((4:40) / 10, function(b) env$risk_fmt_p(env$risk_weibull(1000, b, 1700)$reliability), ""))
  r <- env$risk_weibull(seq(100, 3000, 50), 2, 1700)$reliability
  testthat::expect_equal(res$plan[, 1], vapply(r, env$risk_fmt_p, ""))
  testthat::expect_equal(res$plan[, 2], vapply(1 - r, env$risk_fmt_p, ""))

  testthat::expect_equal(res$bath$total, 1.2 * exp(-1310 / 350) + .12 + (1310 / 4000)^3, tolerance = 1e-12)
  testthat::expect_equal(res$obs$time, pmin(c(220, 480, 760, 990, 1350, 1750, 2300, 3100), 1200))
  testthat::expect_equal(res$obs$failed, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  testthat::expect_equal(res$idx, c(0, 8, 18, 18))
})
