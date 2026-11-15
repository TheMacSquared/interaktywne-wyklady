# Ryciny wykładu 09 liczone w przeglądarce (modules/wykresy.js) względem R
# oraz rysunek drzewa składany przy budowaniu (modules/wykresy.R).

f9_node <- function(script) {
  out <- system2("node", c("-e", shQuote(script)), stdout = TRUE, stderr = TRUE)
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = TRUE)
}

f9_env <- function() {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(risk_root, "R", "risk_math.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "palette.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "lecture_layout.R"), envir = env)
  sys.source(file.path(risk_root, "R", "risk_block.R"), envir = env)
  # f9p_svg() ze wspólnych narzędzi prototypów: sam szkielet <svg>.
  env$f9p_svg <- function(w, h, aria, ...) {
    htmltools::HTML(paste0(sprintf('<svg viewBox="0 0 %d %d" aria-label="%s">', w, h, aria),
                           paste(unlist(list(...)), collapse = ""), "</svg>"))
  }
  sys.source(file.path(risk_root, "09-drzewo-bledow", "modules", "wykresy.R"), envir = env)
  env
}

testthat::test_that("wykład 09: rysunek drzewa ma pięć węzłów z miejscami na liczby", {
  env <- f9_env()
  svg <- as.character(env$f9_tree_svg())
  count <- function(pattern) lengths(regmatches(svg, gregexpr(pattern, svg)))
  testthat::expect_equal(count("<line "), 4L)
  # Trzy liście (koła), bramka (trójkąt) i zdarzenie szczytowe (kwadrat) + po jednym znaku w legendzie.
  testthat::expect_equal(count("<circle "), 4L)
  testthat::expect_equal(count("<path "), 2L)
  testthat::expect_equal(count("<rect "), 2L)
  for (key in c("tree_top", "tree_init", "tree_or", "tree_detect", "tree_suppress")) {
    testthat::expect_true(grepl(sprintf('data-v="%s"', key), svg, fixed = TRUE), info = key)
  }
  testthat::expect_equal(env$f9_rank_config$top0, 0.00063)
})

testthat::test_that("wykład 09: funkcje w przeglądarce dają liczby R", {
  testthat::skip_if(Sys.which("node") == "", "Node.js nie jest dostępny")
  env <- f9_env()
  root <- normalizePath(dirname(risk_root))
  res <- f9_node(sprintf('
    globalThis.window = globalThis;
    require("%s/shared/lc_dist.js"); require("%s/analiza-ryzyka/R/risk_figures.js");
    require("%s/analiza-ryzyka/09-drzewo-bledow/modules/wykresy.js");
    const Z = globalThis.lcF9Charts, R = globalThis.lcRisk, out = { tree: [], repeat: [], common: [], rank: [] };
    for (let i = 0; i <= 30; i += 5) for (let d = 0; d <= 30; d += 6) for (let s = 0; s <= 30; s += 6) {
      const t = [i / 1000, d / 100, s / 100];
      out.tree.push([R.fmtPd(Z.ftaTop(...t), 6), R.fmtP(Z.ftaTop(...t)), R.fmtP(Z.gateOr([t[1], t[2]])),
                     R.fmtPd(Z.commonTop(...t, 0.05), 6)]);
    }
    for (let q = 0; q <= 20; q++) out.repeat.push([R.fmtP(q / 100), R.fmtP(Z.gateOr([q / 100, q / 100])), R.fmtP(q / 100 * q / 100)]);
    for (let q = 0; q <= 200; q += 5) out.common.push(R.fmtPd(Z.commonTop(0.005, 0.05, 0.08, q / 1000), 6));
    for (let r = 0; r <= 90; r += 5) out.rank.push(Z.rankGains({ init: 0.005, detect: 0.05, suppress: 0.08 }, r / 100).map((g) => R.fixed(g, 6)));
    console.log(JSON.stringify(out));', root, root, root))

  grid <- expand.grid(s = seq(0, 30, 6) / 100, d = seq(0, 30, 6) / 100, i = seq(0, 30, 5) / 1000)
  top <- mapply(env$risk_fta_top, grid$i, grid$d, grid$s)
  or <- mapply(function(d, s) env$risk_gate_or(c(d, s)), grid$d, grid$s)
  testthat::expect_equal(res$tree[, 1], vapply(top, env$risk_fmt_p, "", digits = 6))
  testthat::expect_equal(res$tree[, 2], vapply(top, env$risk_fmt_p, ""))
  testthat::expect_equal(res$tree[, 3], vapply(or, env$risk_fmt_p, ""))
  testthat::expect_equal(res$tree[, 4], vapply(grid$i * (.05 + .95 * or), env$risk_fmt_p, "", digits = 6))

  q <- (0:20) / 100
  testthat::expect_equal(res$`repeat`[, 1], vapply(q, env$risk_fmt_p, ""))
  testthat::expect_equal(res$`repeat`[, 2], vapply(q, function(x) env$risk_fmt_p(env$risk_gate_or(c(x, x))), ""))
  testthat::expect_equal(res$`repeat`[, 3], vapply((0:20) / 100 * (0:20) / 100, env$risk_fmt_p, ""))

  q <- seq(0, 200, 5) / 1000
  local_failure <- env$risk_gate_or(c(.05, .08))
  testthat::expect_equal(res$common, vapply(.005 * (q + (1 - q) * local_failure), env$risk_fmt_p, "", digits = 6))

  # Słupki rankingu: te same liczby co sprintf("%.6f") w dawnym wykresie.
  base <- c(init = .005, detect = .05, suppress = .08)
  top0 <- do.call(env$risk_fta_top, unname(as.list(base)))
  want <- t(vapply(seq(0, 90, 5) / 100, function(r) vapply(names(base), function(n) {
    x <- base; x[n] <- x[n] * (1 - r)
    sprintf("%.6f", top0 - do.call(env$risk_fta_top, unname(as.list(x))))
  }, ""), character(3)))
  testthat::expect_equal(res$rank, unname(want))
  # Ustawienie domyślne (redukcja 0.5): liczby z tekstu wykładu.
  testthat::expect_equal(res$rank[11, ], c("0.000315", "0.000115", "0.000190"))
})
