# Wykresy wykładu 06 liczone w przeglądarce (modules/wykresy.js) względem R:
# te same dane (modules/wykresy.R) i te same liczby, które dawał serwer.

z6_node <- function(script) {
  out <- system2("node", c("-e", shQuote(script)), stdout = TRUE, stderr = TRUE)
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = TRUE)
}

testthat::test_that("wykład 06: histogram, odczyty próby i pary L–S jak w dawnych wykresach ggplot", {
  testthat::skip_if(Sys.which("node") == "", "Node.js nie jest dostępny")
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(risk_root, "R", "risk_math.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "palette.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "lecture_layout.R"), envir = env)
  sys.source(file.path(risk_root, "R", "risk_block.R"), envir = env)
  sys.source(file.path(risk_root, "06-zmiennosc-i-prog", "modules", "wykresy.R"), envir = env)

  # Dane ryciny to ten sam strumień losowy co w dawnym serwerze.
  set.seed(606)
  testthat::expect_equal(env$z6_hist_config$values[1:200], round(rnorm(200, 82, 3), 3))
  set.seed(607)
  l <- rnorm(700, 85, 8); s <- rnorm(700, 95, 7)
  testthat::expect_equal(85 + 8 * env$z6_ls_config$zl, l, tolerance = 1e-3)
  testthat::expect_equal(95 + 7 * env$z6_ls_config$zs, s, tolerance = 1e-3)

  cfg <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(hist = env$z6_hist_config, ls = env$z6_ls_config), auto_unbox = TRUE, digits = NA), cfg)
  root <- normalizePath(dirname(risk_root))
  res <- z6_node(sprintf('
    globalThis.window = globalThis;
    require("%s/shared/lc_dist.js"); require("%s/analiza-ryzyka/R/risk_figures.js");
    require("%s/analiza-ryzyka/06-zmiennosc-i-prog/modules/wykresy.js");
    const cfg = JSON.parse(require("fs").readFileSync("%s", "utf8")), Z = globalThis.lcZ6Charts;
    const out = {};
    for (const n of [30, 200, 5000]) {
      const v = cfg.hist.values.slice(0, n);
      out["n" + n] = { density: Z.densityBins(v, 68, 96, 1).map(b => b[2]), stats: Z.sampleStats(v, 85) };
    }
    const d = Z.loadStrength(cfg.ls, 85, 95);
    out.fail = d.fail.length; out.ok = d.ok.length;
    console.log(JSON.stringify(out));', root, root, root, cfg))

  for (n in c(30, 200, 5000)) {
    x <- env$z6_hist_config$values[seq_len(n)]
    h <- hist(x[x >= 68 & x <= 96], breaks = seq(68, 96, 1), plot = FALSE)   # (a, b], jak geom_histogram(breaks = )
    got <- res[[paste0("n", n)]]
    testthat::expect_equal(got$density, h$density, tolerance = 1e-9, info = paste("n =", n))
    testthat::expect_equal(got$stats$mean, as.character(round(mean(x), 2)))
    testthat::expect_equal(got$stats$sd, as.character(round(sd(x), 2)))
    testthat::expect_equal(got$stats$over, sprintf("%d z %d (%s%%)", sum(x > 85), n,
                                                   format(round(100 * mean(x > 85), 1), nsmall = 1)))
  }
  testthat::expect_equal(res$fail, sum(l > s))
  testthat::expect_equal(res$ok, sum(l <= s))
})

testthat::test_that("wykład 06: dane trzech kształtów i odczyty ogonów", {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(risk_root, "R", "risk_math.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "palette.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "lecture_layout.R"), envir = env)
  sys.source(file.path(risk_root, "R", "risk_block.R"), envir = env)
  sys.source(file.path(risk_root, "06-zmiennosc-i-prog", "modules", "wykresy.R"), envir = env)
  shapes <- env$z6_shapes_config
  testthat::expect_setequal(names(shapes), c("normal", "skew", "heavy"))
  for (s in shapes) {
    testthat::expect_equal(sum(vapply(s$bins, function(b) b[3], 0)), 5000)      # każda obserwacja w jednym przedziale
    testthat::expect_true(length(s$qq) > 500 && length(s$qq) < 1200)
    testthat::expect_true(s$qq_line[["slope"]] > 0)
  }
  # Te same liczby co w przykładzie 6.10 (model, nie próba).
  testthat::expect_equal(shapes$normal$p85, env$risk_fmt_p(pnorm(85, 82, 3, lower.tail = FALSE)))
  testthat::expect_equal(shapes$normal$p91, "0.0013")
  testthat::expect_true(as.numeric(shapes$heavy$p91) > as.numeric(shapes$normal$p91))
  testthat::expect_equal(env$z6_action_config$sd, c(82, 2, 85))
})
