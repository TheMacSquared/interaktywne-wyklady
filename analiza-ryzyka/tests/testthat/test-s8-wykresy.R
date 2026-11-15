# Wykresy wykładu 08 liczone w przeglądarce (modules/wykresy.js) względem R
# oraz schematy i odczyty policzone przy budowaniu (modules/wykresy.R).

s8_node <- function(script) {
  out <- system2("node", c("-e", shQuote(script)), stdout = TRUE, stderr = TRUE)
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = TRUE)
}

s8_env <- function() {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(risk_root, "R", "risk_math.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "palette.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "lecture_layout.R"), envir = env)
  sys.source(file.path(risk_root, "R", "risk_block.R"), envir = env)
  # s8p_svg() ze wspólnych narzędzi prototypów: sam szkielet <svg>.
  env$s8p_svg <- function(w, h, aria, ..., max_width = w) {
    htmltools::HTML(paste0(sprintf('<svg viewBox="0 0 %d %d" aria-label="%s">', as.integer(w), as.integer(h), aria),
                           paste(unlist(list(...)), collapse = ""), "</svg>"))
  }
  sys.source(file.path(risk_root, "08-niezawodnosc-systemu", "modules", "wykresy.R"), envir = env)
  env
}

testthat::test_that("wykład 08: schematy i odczyty policzone w R przy budowaniu", {
  env <- s8_env()
  count <- function(svg, tag) lengths(regmatches(svg, gregexpr(paste0("<", tag, " "), svg)))
  # Liczba bloków i odcinków jak w dawnym wykresie ggplot.
  expected <- list(series = c(rect = 3, line = 4), parallel = c(rect = 2, line = 10), mixed = c(rect = 3, line = 11))
  for (kind in names(expected)) {
    svg <- as.character(env$s8_diagram_svg(kind))
    testthat::expect_equal(count(svg, "rect"), expected[[kind]][["rect"]], info = kind)
    testthat::expect_equal(count(svg, "line"), expected[[kind]][["line"]], info = kind)
    testthat::expect_equal(count(svg, "circle"), 2L, info = kind)
    testthat::expect_true(grepl("wejście", svg) && grepl("wyjście", svg), info = kind)
  }
  testthat::expect_true(grepl("Sterownik C", as.character(env$s8_diagram_svg("mixed"))))

  cfg <- env$s8_common_config
  testthat::expect_equal(cfg$base, 0.996)
  testthat::expect_equal(cfg$base_text, "bez wspólnej przyczyny: 0.996")
  testthat::expect_length(cfg$share, 31)
  testthat::expect_equal(cfg$share[c(1, 3)], c("0%", "72%"))    # suwak 0 i 0.01 (przykład 8.8)

  imp <- env$s8_improvement_config
  testthat::expect_equal(imp$text, c("0.0186", "0.0180", "0.0175"))
  testthat::expect_equal(imp$reads, c("0.019", "0.018", "0.017"))
})

testthat::test_that("wykład 08: funkcje w przeglądarce dają liczby R", {
  testthat::skip_if(Sys.which("node") == "", "Node.js nie jest dostępny")
  env <- s8_env()
  root <- normalizePath(dirname(risk_root))
  res <- s8_node(sprintf('
    globalThis.window = globalThis;
    require("%s/shared/lc_dist.js"); require("%s/analiza-ryzyka/R/risk_figures.js");
    require("%s/analiza-ryzyka/08-niezawodnosc-systemu/modules/wykresy.js");
    const Z = globalThis.lcS8Charts, R = globalThis.lcRisk, out = {};
    out.series = []; out.parallel = [];
    for (let a = 50; a <= 100; a += 5) for (let b = 50; b <= 100; b += 5) {
      out.series.push(R.fmtP(Z.series([a / 100, b / 100]))); out.parallel.push(R.fmtP(Z.parallel([a / 100, b / 100])));
    }
    out.time = []; for (let t = 100; t <= 3000; t += 50) { const v = Z.timeValues(t); out.time.push([v.a, v.b, v.c, v.system].map(R.fmtP)); }
    out.red = [];
    for (let n = 1; n <= 6; n++) for (let r = 50; r <= 99; r += 7) {
      const d = Z.redundancy(n, r / 100); out.red.push([R.fmtP(d.r), d.gain === null ? "—" : R.fmtP(d.gain), d.cost + " jedn."]);
    }
    console.log(JSON.stringify(out));', root, root, root))

  # Te same liczby co w przeglądarce: k / 100 (seq(.5, 1, .05) różni się o ostatni bit).
  grid <- expand.grid(b = seq(50, 100, 5) / 100, a = seq(50, 100, 5) / 100)
  testthat::expect_equal(res$series, mapply(function(a, b) env$risk_fmt_p(env$risk_series_reliability(c(a, b))), grid$a, grid$b))
  testthat::expect_equal(res$parallel, mapply(function(a, b) env$risk_fmt_p(env$risk_parallel_reliability(c(a, b))), grid$a, grid$b))

  t <- seq(100, 3000, 50)
  ra <- exp(-t / 1800); rb <- exp(-t / 2000); rc <- exp(-t / 2500)
  want <- cbind(ra, rb, rc, rc * (1 - (1 - ra) * (1 - rb)))
  testthat::expect_equal(res$time, unname(apply(want, 2, function(col) vapply(col, env$risk_fmt_p, ""))))

  red <- expand.grid(r = seq(50, 99, 7) / 100, n = 1:6)
  now <- 1 - (1 - red$r)^red$n
  testthat::expect_equal(res$red[, 1], vapply(now, env$risk_fmt_p, ""))
  testthat::expect_equal(res$red[, 2], ifelse(red$n > 1, vapply(now - (1 - (1 - red$r)^(red$n - 1)), env$risk_fmt_p, ""), "—"))
  testthat::expect_equal(res$red[, 3], paste(red$n * 100, "jedn."))
})
