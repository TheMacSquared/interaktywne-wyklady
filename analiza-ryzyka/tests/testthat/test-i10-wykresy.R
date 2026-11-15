# Ryciny wykładu 10 liczone w przeglądarce (modules/wykresy.js) względem R:
# kontrakt misji, karty, scenariusze i notatka dają liczby dawnego serwera.

i10_node <- function(script) {
  out <- system2("node", c("-e", shQuote(script)), stdout = TRUE, stderr = TRUE)
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = TRUE)
}

i10_env <- function() {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(risk_root, "R", "bananpol.R"), envir = env)
  sys.source(file.path(risk_root, "R", "risk_math.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "palette.R"), envir = env)
  sys.source(file.path(dirname(risk_root), "shared", "lecture_layout.R"), envir = env)
  sys.source(file.path(risk_root, "R", "risk_block.R"), envir = env)
  sys.source(file.path(risk_root, "10-model-do-decyzji", "modules", "wykresy.R"), envir = env)
  env
}

testthat::test_that("wykład 10: dane rycin z tabeli interwencji Bananpolu", {
  env <- i10_env()
  opts <- env$i10_config$options
  testthat::expect_equal(vapply(opts, function(o) o$id, ""), env$bananpol$interventions$id)
  testthat::expect_equal(vapply(opts, function(o) o$cost_index, 0), env$bananpol$interventions$cost_index)
  testthat::expect_equal(opts[[2]]$wrapped, "Ograniczenie\nźródła ciepła")
  testthat::expect_setequal(names(env$i10_question_models),
    c("conditional", "bayes", "binomial", "geometric", "negative", "threshold", "survival", "system"))
})

testthat::test_that("wykład 10: kontrakt misji w przeglądarce jak risk_mission_analysis() w R", {
  testthat::skip_if(Sys.which("node") == "", "Node.js nie jest dostępny")
  env <- i10_env()
  root <- normalizePath(dirname(risk_root))
  cfg <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(env$i10_config, auto_unbox = TRUE, digits = NA), cfg)
  res <- i10_node(sprintf('
    globalThis.window = globalThis;
    require("%s/shared/lc_dist.js"); require("%s/analiza-ryzyka/R/risk_figures.js");
    require("%s/analiza-ryzyka/10-model-do-decyzji/modules/wykresy.js");
    const Z = globalThis.lcI10Charts, R = globalThis.lcRisk, out = { grid: [], cards: {}, text: {} };
    const options = JSON.parse(require("fs").readFileSync("%s", "utf8")).options, ids = options.map((o) => o.id);
    for (const model of ["exp", "weibull"]) for (const time of [100, 1000, 2000, 3000]) for (const stress of [0.5, 0.8, 1, 1.2, 1.5])
      for (const id of ["none"].concat(ids)) {
        const m = Z.mission({ time, model, power: 0.98, controller: 0.95, init: 0.005, sens: 0.95 }, id, stress);
        out.grid.push([m.fan_r, m.power_r, m.controller_r, m.parallel_r, m.system_r, m.initiation, m.miss, m.top]);
      }
    const base = { time: 1000, model: "exp", power: 0.98, controller: 0.95, init: 0.005, sens: 0.95 };
    out.cards.bayes = [[0.005, 0.95, 0.05], [0.005, 0.95, 0.02], [0.03, 0.5, 0.2], [0.001, 1, 0]].map((a) => R.fmtP(Z.bayes(...a)));
    out.cards.insp = [[100, 0.02], [1000, 0.02], [10, 0.001], [370, 0.047]].map(([n, p]) => [R.rnd(n * p, 2), R.fmtP(Z.atLeastOne(n, p)), R.fmtP(Z.zeroFailureUpper(n))]);
    out.cards.life = [100, 1000, 1950, 3000].map((t) => [R.fmtP(Z.lifeCurve("exp", t)), R.fmtP(Z.lifeCurve("weibull", t))]);
    out.text.label = Z.settingsLabel(base);
    out.text.format = [0.00162, 0.00230, 0.002, 0.0001, 0.01, 0.325, 0].map(Z.formatProbability);
    out.text.winners = [];
    for (const model of ["exp", "weibull"]) for (const budget of [1, 2, 3, 4]) for (const u of [0, 0.2, 0.5]) {
      const s = Object.assign({}, base, { model });
      out.text.winners.push(Z.scenarios(s, ids, u).map((row) => Z.winners(row, options, budget)));
    }
    out.text.worst = Math.max(...Z.scenarios(base, ids, 0.2).map((row) => row[0]));
    console.log(JSON.stringify(out));', root, root, root, cfg))

  ids <- env$bananpol$interventions$id
  want <- list()
  for (model in c("exp", "weibull")) for (time in c(100, 1000, 2000, 3000)) for (stress in c(.5, .8, 1, 1.2, 1.5))
    for (id in c("none", ids)) {
      m <- env$risk_mission_analysis(time, model, .98, .95, .005, .95, id, stress)
      want[[length(want) + 1]] <- c(m$fan_r, m$power_r, m$controller_r, m$parallel_r, m$system_r, m$initiation, m$miss, m$top)
    }
  testthat::expect_equal(res$grid, do.call(rbind, want), tolerance = 1e-13)

  testthat::expect_equal(res$cards$bayes, c(env$risk_fmt_p(env$risk_bayes(.005, .95, .05)), env$risk_fmt_p(env$risk_bayes(.005, .95, .02)),
                                            env$risk_fmt_p(env$risk_bayes(.03, .5, .2)), env$risk_fmt_p(env$risk_bayes(.001, 1, 0))))
  insp <- list(c(100, .02), c(1000, .02), c(10, .001), c(370, .047))
  testthat::expect_equal(res$cards$insp, do.call(rbind, lapply(insp, function(x) c(
    as.character(round(x[1] * x[2], 2)), env$risk_fmt_p(env$risk_at_least_one(x[1], x[2])), env$risk_fmt_p(env$risk_zero_failure_upper(x[1]))))))
  t <- c(100, 1000, 1950, 3000)
  testthat::expect_equal(res$cards$life, cbind(vapply(exp(-t / 1500), env$risk_fmt_p, ""), vapply(exp(-(t / 1700)^2), env$risk_fmt_p, "")))

  testthat::expect_equal(res$text$label, "wykładniczy · 1000 h · P(I) 0.00500 · czułość 0.950")
  testthat::expect_equal(res$text$format, vapply(c(.00162, .0023, .002, .0001, .01, .325, 0), env$risk_format_probability, ""))

  # Zwycięzcy scenariuszy jak w dawnym tekście pod wykresem.
  opts <- env$bananpol$interventions
  winners <- list()
  for (model in c("exp", "weibull")) for (budget in 1:4) for (u in c(0, .2, .5)) {
    winners[[length(winners) + 1]] <- vapply(c(1 - u, 1, 1 + u), function(m) {
      d <- opts[opts$cost_index <= budget, ]
      result <- vapply(d$id, function(id) env$risk_mission_analysis(1000, model, .98, .95, .005, .95, id, m)$top, 0)
      paste(d$label[abs(result - min(result)) < 1e-12], collapse = " / ")
    }, "")
  }
  testthat::expect_equal(res$text$winners, do.call(rbind, winners))
  # Przykład 10.11 (b): Weibull, budżet 3, u = 0.5 — w scenariuszu ostrożnym wygrywa dodatkowy wentylator.
  testthat::expect_equal(res$text$winners[21, ], c("Ograniczenie źródła ciepła", "Ograniczenie źródła ciepła", "Dodatkowy wentylator"))
  # Notatka: lepszy czujnik w najgorszym scenariuszu przekracza limit 0.002.
  testthat::expect_equal(env$risk_format_probability(res$text$worst), "0.00230 (0.230%)")
})
