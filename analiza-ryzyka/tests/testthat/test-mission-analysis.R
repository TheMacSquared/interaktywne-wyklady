env <- new.env(parent = globalenv())
sys.source(file.path(risk_root, "R", "risk_math.R"), envir = env)

testthat::test_that("granica dla zera awarii odwraca prawdopodobieństwo obserwacji", {
  for (n in c(10, 100, 1000)) {
    upper <- env$risk_zero_failure_upper(n)
    testthat::expect_equal((1 - upper)^n, .05, tolerance = 1e-12)
  }
  testthat::expect_error(env$risk_zero_failure_upper(0))
})

testthat::test_that("okres pracy łączy czas życia, system i warunkowy scenariusz", {
  b <- env$risk_mission_analysis(1000, "exp", .98, .95, .005, .95)
  fan <- exp(-1000 / 1500)
  testthat::expect_equal(b$fan_r, fan)
  testthat::expect_equal(b$system_r, .98 * .95 * (1 - (1 - fan)^2))
  testthat::expect_equal(b$top, .005 * (1 - .95 * b$system_r))
  long <- env$risk_mission_analysis(2000, "exp", .98, .95, .005, .95)
  testthat::expect_equal(long$power_r, .98^2)
  testthat::expect_equal(long$controller_r, .95^2)
  testthat::expect_gt(long$top, b$top)
  w <- env$risk_mission_analysis(1000, "weibull", .98, .95, .005, .95)
  testthat::expect_lt(w$top, b$top)
  testthat::expect_equal(env$risk_mission_analysis(1000, "exp", 1, 1, 0, 1)$top, 0)
})

testthat::test_that("interwencje są porównywane przy jednakowych scenariuszach", {
  for (model in c("exp", "weibull")) {
    for (stress in c(.5, 1, 1.5)) {
      baseline <- env$risk_mission_analysis(1000, model, .98, .95, .005, .95, stress = stress)
      for (id in c("detector", "prevention", "power", "fan")) {
        after <- env$risk_mission_analysis(1000, model, .98, .95, .005, .95, id, stress)
        testthat::expect_lt(after$top, baseline$top)
        testthat::expect_gte(after$top, 0)
      }
    }
  }
  baseline <- env$risk_mission_analysis(1000, "exp", .98, .95, .005, .95)
  prevention <- env$risk_mission_analysis(1000, "exp", .98, .95, .005, .95, "prevention")
  testthat::expect_equal(prevention$top, baseline$top / 2)
  testthat::expect_error(env$risk_mission_analysis(0, "exp", .98, .95, .005, .95))
})

testthat::test_that("karty, scenariusze i notatka reagują na ustawienia", {
  # Karty, scenariusze i notatkę liczy przeglądarka (10-model-do-decyzji/modules/wykresy.js);
  # test sprawdza te same zależności co dawniej na serwerze. Zgodność liczb z R: test-i10-wykresy.R.
  testthat::skip_if(Sys.which("node") == "", "Node.js nie jest dostępny")
  root <- normalizePath(dirname(risk_root))
  script <- sprintf('
    globalThis.window = globalThis;
    require("%s/shared/lc_dist.js"); require("%s/analiza-ryzyka/R/risk_figures.js");
    require("%s/analiza-ryzyka/10-model-do-decyzji/modules/wykresy.js");
    const Z = globalThis.lcI10Charts;
    const ids = ["detector", "prevention", "power", "fan"];
    const s = { time: 1000, model: "exp", power: 0.98, controller: 0.95, init: 0.005, sens: 0.95 };
    const top = (x) => Z.mission(x, "none").top;
    const worst = (x, id, u) => Math.max(...Z.scenarios(x, ids, u).map((row) => row[ids.indexOf(id)]));
    console.log(JSON.stringify({
      initial: top(s), fan: Z.mission(s, "none").fan_r, cells: Z.scenarios(s, ids, 0.2).flat().length,
      weibull: top(Object.assign({}, s, { model: "weibull" })),
      weibull2000: top(Object.assign({}, s, { model: "weibull", time: 2000 })),
      worst_power: worst(Object.assign({}, s, { model: "weibull", time: 2000 }), "power", 0.2)
    }));', root, root, root)
  res <- jsonlite::fromJSON(paste(system2("node", c("-e", shQuote(script)), stdout = TRUE, stderr = TRUE), collapse = ""))
  testthat::expect_equal(res$cells, 12L)                           # 4 działania × 3 scenariusze
  testthat::expect_equal(res$fan, exp(-1000 / 1500), tolerance = 1e-12)
  testthat::expect_lt(res$weibull, res$initial)                    # zmiana modelu obniża P(TOP)
  testthat::expect_gt(res$weibull2000, res$weibull)                # dłuższy okres pracy je podnosi
  # FPR nie wchodzi do kontraktu okresu pracy: P(TOP) od niego nie zależy (nie jest argumentem mission()).
  # Notatka: niezależne zasilanie (koszt 4) jest poza budżetem 1 i nie spełnia limitu 0.0001.
  testthat::expect_gt(res$worst_power, .0001)
})

testthat::test_that("FTA pokazuje oba błędy powtórzenia i przelicza wspólną przyczynę", {
  # Rachunek drzewa liczy przeglądarka (09-drzewo-bledow/modules/wykresy.js); test
  # sprawdza te same liczby co dawniej na serwerze.
  testthat::skip_if(Sys.which("node") == "", "Node.js nie jest dostępny")
  root <- normalizePath(dirname(risk_root))
  script <- sprintf('
    globalThis.window = globalThis;
    require("%s/shared/lc_dist.js"); require("%s/analiza-ryzyka/R/risk_figures.js");
    require("%s/analiza-ryzyka/09-drzewo-bledow/modules/wykresy.js");
    const Z = globalThis.lcF9Charts, R = globalThis.lcRisk;
    console.log(JSON.stringify({
      top: Z.ftaTop(0.005, 0.05, 0.08),
      common: Z.commonTop(0.005, 0.05, 0.08, 0.01), common_text: R.fmtPd(Z.commonTop(0.005, 0.05, 0.08, 0.01), 6),
      common_sure: R.fmtPd(Z.commonTop(0.005, 0.05, 0.08, 1), 6),
      repeat_or: Z.gateOr([0.05, 0.05]), repeat_and: 0.05 * 0.05
    }));', root, root, root)
  res <- jsonlite::fromJSON(paste(system2("node", c("-e", shQuote(script)), stdout = TRUE, stderr = TRUE), collapse = ""))
  testthat::expect_equal(res$top, .00063, tolerance = 1e-12)
  testthat::expect_equal(res$common_text, "0.000674")
  # Oba błędne rachunki powtórzonego liścia różnią się od jednego zdarzenia (0.05).
  testthat::expect_equal(res$repeat_or, 1 - .95^2, tolerance = 1e-12)
  testthat::expect_equal(res$repeat_and, .0025, tolerance = 1e-12)
  # Enumeracja zdarzeń bazowych niezależnie od wzoru ryciny.
  states <- expand.grid(i = 0:1, c = 0:1, d = 0:1, s = 0:1)
  weights <- apply(states, 1, function(x) prod(ifelse(x == 1,
    c(.005, .01, .05, .08), 1 - c(.005, .01, .05, .08))))
  active <- with(states, i == 1 & (c == 1 | d == 1 | s == 1))
  testthat::expect_equal(sum(weights[active]), .0006737, tolerance = 1e-12)
  testthat::expect_equal(res$common, sum(weights[active]), tolerance = 1e-12)
  testthat::expect_equal(res$common_sure, "0.005000")
})
