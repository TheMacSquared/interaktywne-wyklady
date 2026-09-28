testthat::test_that("dwa alarmy wynikają z jawnego rozkładu wspólnego", {
  env <- new.env()
  sys.source(file.path(risk_root, "R", "risk_math.R"), env)
  for (d in c(0, .5, 1)) {
    # Enumeracja: stan instalacji, tryb kopiowania i wyniki niezależnych losowań.
    states <- expand.grid(a = 0:1, copy = 0:1, first = 0:1, second = 0:1)
    p <- ifelse(states$a == 1, .95, .05)
    weights <- ifelse(states$a == 1, .01, .99) *
      ifelse(states$copy == 1, d, 1-d) *
      ifelse(states$first == 1, p, 1-p) * ifelse(states$second == 1, p, 1-p)
    both <- states$first == 1 & (states$copy == 1 | states$second == 1)
    expected <- sum(weights[both & states$a == 1]) / sum(weights[both])
    testthat::expect_equal(env$risk_two_alarm_posterior(.01, .95, .05, d), expected)
  }
  testthat::expect_true(is.na(env$risk_two_alarm_posterior(0, 1, 0)))
})

testthat::test_that("alarm działa przed odwiedzeniem kontrolek i używa dokładnego posterioru", {
  result <- callr::r(function(path) {
    setwd(path)
    app <- new.env()
    sys.source("app.R", app)
    shiny::testServer(app$alarm_server, {
      session$setInputs(a3_dependence = .5)
      stopifnot(grepl("Po dwóch alarmach", output$a3_second$html))
      session$setInputs(a3_prev = .001, a3_sens = .95, a3_fpr = .05)
      stopifnot(abs(posterior() - .00095 / (.00095 + .04995)) < 1e-12)
      stopifnot(grepl(app$risk_format_probability(posterior()), output$a3_counts$html, fixed = TRUE))
    })
    # Zmiana rozdziału nie odtwarza kontrolek z wartościami początkowymi.
    shiny::testServer(app$server, {
      first <- output$lc__chapter_content$html
      session$setInputs(a3_prev = .023, lc__switch_chapter = "ch-baza")
      stopifnot(identical(first, output$lc__chapter_content$html))
      stopifnot(identical(input$a3_prev, .023))
      stopifnot(grepl('id="a3_prev"', first, fixed = TRUE))
      stopifnot(grepl('id="a3_dependence"', first, fixed = TRUE))
    })
    TRUE
  }, args = list(path = file.path(risk_root, "03-alarm-i-prawda")), timeout = 60)
  testthat::expect_true(result)
})

testthat::test_that("wydzielony Monty Hall sumuje gry i resetuje eksperyment", {
  result <- callr::r(function(path) {
    setwd(path)
    app <- new.env()
    sys.source("app.R", app)
    shiny::testServer(app$warunki_monty_server, {
      session$setInputs(w2_monty_door_1 = 0, w2_monty_switch = 0,
        w2_monty_sim_10 = 0, w2_monty_new = 0)
      session$setInputs(w2_monty_door_1 = 1)
      stopifnot(monty$opened != monty$prize, monty$opened != monty$chosen)
      session$setInputs(w2_monty_switch = 1)
      stopifnot(monty$final != monty$chosen, monty$final != monty$opened)
      session$setInputs(w2_monty_sim_10 = 1)
      session$setInputs(w2_monty_sim_10 = 2)
      stopifnot(monty_sim$n == 20, monty_sim$wins_stay + monty_sim$wins_switch == 20)
      session$setInputs(w2_monty_new = 1)
      stopifnot(is.null(monty$final), monty_sim$n == 0)
    })
    TRUE
  }, args = list(path = file.path(risk_root, "02-warunki")), timeout = 60)
  testthat::expect_true(result)
})
