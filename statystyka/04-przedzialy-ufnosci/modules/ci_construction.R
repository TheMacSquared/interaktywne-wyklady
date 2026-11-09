# Statystyki i kwantyle liczone w R; ECharts odpowiada za rysowanie.
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

# Gotowe próby dla wersji offline. Budowa UI nie zmienia globalnego RNG.
ci_construction_bank <- local({
  had_seed <- exists(".Random.seed", envir = .GlobalEnv)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  set.seed(32033)
  bank <- list(
    mean = replicate(16, ci_construction_stats(rnorm(25, 170, 10)), simplify = FALSE),
    diff = replicate(16, ci_construction_stats(rnorm(25, 178, 7), rnorm(25, 165, 6)), simplify = FALSE)
  )
  if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
  else rm(".Random.seed", envir = .GlobalEnv)
  bank
})

ci_construction_ui <- function(kind) {
  tags$div(class = "ci-construction", `data-kind` = kind,
    `data-config` = jsonlite::toJSON(ci_construction_bank[[kind]],
      auto_unbox = TRUE, null = "null", digits = 12),
    tags$div(class = "lc-ec", role = "img",
      `aria-label` = if (kind == "mean") "Konstrukcja przedziału ufności dla średniej" else
        "Konstrukcja przedziału ufności dla różnicy średnich")
  )
}
