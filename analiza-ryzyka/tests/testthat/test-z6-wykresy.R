# Wykresy wykładu 06 liczone w przeglądarce (modules/wykresy.js): dane z R
# (modules/wykresy.R) i te same liczby, które dawał serwer.

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
})
