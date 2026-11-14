env <- new.env(parent = globalenv())
sys.source(file.path(risk_root, "R", "risk_math.R"), envir = env)

testthat::test_that("granica dla zera awarii odwraca prawdopodobieństwo obserwacji", {
  for (n in c(10, 100, 1000)) {
    upper <- env$risk_zero_failure_upper(n)
    testthat::expect_equal((1 - upper)^n, .05, tolerance = 1e-12)
  }
  testthat::expect_error(env$risk_zero_failure_upper(0))
})

testthat::test_that("misja łączy czas życia, system i warunkowy scenariusz", {
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

testthat::test_that("karty, scenariusze i notatka reagują na wejścia aplikacji", {
  testthat::skip_if_not_installed("callr")
  result <- callr::r(function(path) {
    setwd(path)
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    shiny::testServer(app$integracja_server, {
      session$setInputs(i10_time = 1000, i10_life_model = "exp", i10_power = .98,
        i10_controller = .95, i10_init = .005, i10_sens = .95, i10_fpr = .05,
        i10_n = 100, i10_p = .02, i10_question = "bayes", i10_budget = 2,
        i10_intervention = "prevention", i10_recommend = "prevention",
        i10_uncertainty = .2, i10_target = .002)
      initial <- top_p()
      stopifnot(nrow(scenario_results()) == 12)
      stopifnot(abs(life_r() - exp(-1000 / 1500)) < 1e-12)
      for (name in c("i10_alarm_result", "i10_inspection_result", "i10_system_result",
                     "i10_fta_stats", "i10_scenarios_stats", "i10_memo")) {
        stopifnot(length(output[[name]]) > 0)
      }
      session$setInputs(i10_life_model = "weibull")
      stopifnot(top_p() < initial)
      previous <- top_p()
      session$setInputs(i10_fpr = .2)
      stopifnot(identical(top_p(), previous))
      session$setInputs(i10_time = 2000)
      stopifnot(top_p() > previous)
      session$setInputs(i10_budget = 1, i10_recommend = "power")
      stopifnot(grepl("poza budżetem", output$i10_memo$html, fixed = TRUE))
      session$setInputs(i10_target = .0001)
      stopifnot(grepl("nie jest spełniony", output$i10_memo$html, fixed = TRUE))
    })
    TRUE
  }, args = list(path = file.path(risk_root, "10-model-do-decyzji")), timeout = 60)
  testthat::expect_true(result)
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

testthat::test_that("porównanie zmiennego p używa tych samych danych na wykresie i w średnich", {
  testthat::skip_if_not_installed("callr")
  result <- callr::r(function(path) {
    setwd(path)
    app <- new.env(parent = globalenv())
    sys.source("app.R", envir = app)
    shiny::testServer(app$dozd_server, {
      session$setInputs(d5_variation = .09, d5_geo_p = .1,
        d5_p = .1, d5_r = 3, d5_limit = 40)
      d <- failure_data()
      means <- tapply(d$x, d$model, mean)
      stopifnot(means[["Zmienne p między partiami"]] > means[["Stałe p"]])
      stopifnot(identical(failure_plot()$data, d))
      stopifnot(grepl(as.character(round(means[["Zmienne p między partiami"]], 1)),
        output$d5_failure_stats$html, fixed = TRUE))
    })
    TRUE
  }, args = list(path = file.path(risk_root, "05-do-zdarzenia")), timeout = 60)
  testthat::expect_true(result)
})
