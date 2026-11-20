# Pierwsze próby Ryc. 3.2 i 3.3 (rysuje je modules/fig_32.js); statystyki liczone w R dla porównania.
ci_construction_stats <- function(x, y = NULL) {
  center <- mean(x); se <- sd(x) / sqrt(length(x)); df <- length(x) - 1
  if (!is.null(y)) {
    a <- var(x) / length(x); b <- var(y) / length(y)
    center <- center - mean(y); se <- sqrt(a + b)
    df <- (a + b)^2 / (a^2 / (length(x) - 1) + b^2 / (length(y) - 1))
  }
  critical <- qt(.975, df)
  list(x = x, y = y, mean = center, se = se, df = df, t = critical,
       lower = center - critical * se, upper = center + critical * se)
}

# Pierwsza próba widgetu (kolejne losuje przeglądarka). Budowa UI nie zmienia globalnego RNG.
ci_construction_bank <- local({
  had_seed <- exists(".Random.seed", envir = .GlobalEnv)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(32033)
  bank <- list(
    mean = replicate(1, ci_construction_stats(rnorm(25, 170, 10)), simplify = FALSE),
    diff = replicate(1, ci_construction_stats(rnorm(25, 178, 7), rnorm(25, 165, 6)), simplify = FALSE)
  )
  if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  else rm(".Random.seed", envir = .GlobalEnv)
  bank
})
