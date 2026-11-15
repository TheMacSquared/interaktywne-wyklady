# Ryciny wykładu 10 liczone w przeglądarce (10-projekt-badawczy/modules/figures.js)
# względem R: model własny na danych TeachingRatings i okno podglądu danych.

testthat::test_that("wykład 10: model własny w przeglądarce jak lm() na tych samych danych", {
  testthat::skip_if(Sys.which("node") == "", "Node.js nie jest dostępny")
  testthat::skip_if_not_installed("callr")
  root <- normalizePath(file.path(stat_root, ".."))
  res <- callr::r(function(app, root) {
    setwd(app)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    cfg <- tempfile(fileext = ".json")
    writeLines(jsonlite::toJSON(list(custom = env$s10_custom_config, sort = env$s10_data_config), auto_unbox = TRUE, digits = NA), cfg)
    sets <- list(character(), c("gender", "age", "native", "division", "credits", "response.rate"),
                 c("gender", "age", "minority", "native", "tenure", "division", "credits", "students", "response.rate"),
                 c("students", "tenure"))
    script <- sprintf('
      globalThis.window = globalThis;
      require("%s/shared/lc_dist.js"); require("%s/shared/lc_stats.js");
      require("%s/statystyka/10-projekt-badawczy/modules/figures.js");
      const cfg = JSON.parse(require("fs").readFileSync("%s", "utf8")), Z = globalThis.lcS10;
      const sets = %s;
      const out = { models: sets.map((v) => { const m = Z.customModel(cfg.custom, v);
        return { labels: m.terms.map((t) => t.label), est: m.terms.map((t) => t.estimate), se: m.terms.map((t) => t.se),
                 p: m.terms.map((t) => t.p), lo: m.terms.map((t) => t.lo), hi: m.terms.map((t) => t.hi), adj: m.fit.adjR2, aic: m.fit.aic }; }) };
      const w = Z.dataWindow(cfg.sort.sort.eval, cfg.sort.sort.eval.length, 11);
      out.window = { rows: w.order.slice(w.first, w.last + 1).map((i) => i + 1), none: Z.dataWindow(null, 463, 460) };
      out.p = [0.0004, 0.001, 0.0499, 0.5, NaN].map(Z.pLabel);
      console.log(JSON.stringify(out));', root, root, root, cfg, jsonlite::toJSON(sets))
    js <- jsonlite::fromJSON(paste(system2("node", c("-e", shQuote(script)), stdout = TRUE, stderr = TRUE), collapse = ""), simplifyVector = FALSE)
    want <- lapply(sets, function(v) {
      m <- lm(stats::reformulate(c("beauty", v), "eval"), data = env$tr_data)
      s <- summary(m)$coefficients[-1, , drop = FALSE]; ci <- confint(m)[-1, , drop = FALSE]
      list(labels = env$tr_label_term(rownames(s)), est = unname(s[, 1]), se = unname(s[, 2]), p = unname(s[, 4]),
           lo = unname(ci[, 1]), hi = unname(ci[, 2]), adj = summary(m)$adj.r.squared, aic = AIC(m))
    })
    list(js = js, want = want, sorted = as.integer(rownames(env$tr_data)[order(env$tr_data$eval, decreasing = TRUE)][11:20]))
  }, args = list(app = file.path(stat_root, "10-projekt-badawczy"), root = root), timeout = 180)

  for (i in seq_along(res$want)) {
    js <- res$js$models[[i]]; want <- res$want[[i]]
    testthat::expect_equal(unlist(js$labels), want$labels, info = paste("model", i))
    for (key in c("est", "se", "p", "lo", "hi")) {
      testthat::expect_equal(unlist(js[[key]]), want[[key]], tolerance = 1e-7, info = paste("model", i, key))
    }
    testthat::expect_equal(js$adj, want$adj, tolerance = 1e-10)
    testthat::expect_equal(js$aic, want$aic, tolerance = 1e-10)
  }
  # Okno podglądu: wiersze 11–20 po sortowaniu malejąco według eval (remisy w kolejności danych).
  testthat::expect_equal(unlist(res$js$window$rows), res$sorted)
  testthat::expect_equal(c(res$js$window$none$first, res$js$window$none$last), c(459L, 462L))
  testthat::expect_equal(unlist(res$js$p), c("< 0.001", "0.001", "0.050", "0.500", "—"))
})

testthat::test_that("wykład 10: dane rycin policzone w R przy budowaniu", {
  testthat::skip_if_not_installed("callr")
  res <- callr::r(function(app) {
    setwd(app)
    env <- new.env(parent = globalenv())
    sys.source("app.R", envir = env)
    box <- env$s10_box_config("gender", "eval", "Płeć", "Ocena", c("niebo", "wrzos"))
    sc <- env$s10_scatter_config("beauty", "Ocena atrakcyjności")
    women <- env$tr_data$eval[env$tr_data$gender == levels(factor(env$tr_data$gender))[1]]
    list(levels = box$levels, q = unlist(box$boxes[[1]][c("q1", "med", "q3")]), want_q = unname(quantile(women, c(.25, .5, .75))),
         n_points = length(box$points), fit_n = length(sc$fit),
         fit_ends = c(sc$fit[[1]][2], sc$fit[[80]][2]),
         want_ends = unname(predict(lm(eval ~ beauty, env$tr_data), data.frame(beauty = range(env$tr_data$beauty)))),
         band_ok = all(mapply(function(l, u) l[2] < u[2], sc$lwr, sc$upr)),
         series = env$ch7_model_series$beta_beauty, significant = env$s10_beta_config$significant)
  }, args = list(app = file.path(stat_root, "10-projekt-badawczy")), timeout = 180)
  testthat::expect_length(res$levels, 2)
  testthat::expect_equal(unname(res$q), res$want_q)
  testthat::expect_equal(res$n_points, 463L)
  testthat::expect_equal(res$fit_n, 80L)
  testthat::expect_equal(res$fit_ends, res$want_ends, tolerance = 1e-4)
  testthat::expect_true(res$band_ok)
  testthat::expect_equal(round(res$series, 3), c(0.133, 0.136, 0.156, 0.134))      # liczby z tekstu wykładu
  testthat::expect_true(all(res$significant))
})
