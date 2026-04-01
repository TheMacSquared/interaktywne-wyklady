# Wspólne funkcje i kolory

library(ggplot2)
library(dplyr)

# Kolory
col_primary    <- "#3498db"
col_secondary  <- "#e74c3c"
col_success    <- "#27ae60"
col_warning    <- "#f39c12"
col_dark       <- "#2c3e50"
col_purple     <- "#9b59b6"
col_teal       <- "#1abc9c"
col_ci         <- "#3498db"
col_miss       <- "#e74c3c"
col_hit        <- "#27ae60"
col_estimate   <- "#f39c12"
col_true       <- "#9b59b6"

# Theme
theme_lecture <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "#7f8c8d"),
      panel.grid.minor = element_blank()
    )
}
theme_ci <- theme_lecture

# Stat box
stat_box <- function(text, color = col_primary) {
  tags$span(
    style = paste0(
      "display: inline-block; padding: 8px 16px; margin: 4px;",
      "border-radius: 6px; font-weight: bold; font-size: 16px;",
      "color: white; min-width: 100px; text-align: center;",
      "background: ", color, ";"
    ),
    text
  )
}

# Helper do wczytywania fragmentów HTML
include_content <- function(chapter, part) {
  path <- file.path("content_html", paste0(chapter, "_part", part, ".html"))
  if (file.exists(path)) {
    includeHTML(path)
  } else {
    p(style = "color: red;", paste0("Brak pliku: ", path))
  }
}

# Generowanie danych (z ch1)
generate_population_sample <- function(dist_type, n) {
  switch(dist_type,
    "normal"      = rnorm(n, mean = 170, sd = 10),
    "exponential" = rexp(n, rate = 0.5),
    "uniform"     = runif(n, min = 0, max = 10),
    "bimodal"     = {
      k <- rbinom(1, n, 0.5)
      c(rnorm(k, 3, 0.8), rnorm(n - k, 7, 0.8))
    },
    "skewed"      = rgamma(n, shape = 2, scale = 1.5)
  )
}

get_population_params <- function(dist_type) {
  switch(dist_type,
    "normal"      = list(mu = 170, sigma = 10),
    "exponential" = list(mu = 2,   sigma = 2),
    "uniform"     = list(mu = 5,   sigma = sqrt(100/12)),
    "bimodal"     = list(mu = 5,   sigma = sqrt(0.64 + 4)),
    "skewed"      = list(mu = 3,   sigma = sqrt(4.5))
  )
}

# Symulacja pokrycia CI
simulate_coverage <- function(dist_type, n, conf_level, n_sims, method = "t") {
  params <- get_population_params(dist_type)
  results <- lapply(seq_len(n_sims), function(i) {
    samp <- generate_population_sample(dist_type, n)
    xbar <- mean(samp)
    s <- sd(samp)
    if (method == "z") {
      crit <- qnorm(1 - (1 - conf_level) / 2)
      me <- crit * params$sigma / sqrt(n)
    } else {
      crit <- qt(1 - (1 - conf_level) / 2, df = n - 1)
      me <- crit * s / sqrt(n)
    }
    data.frame(sim = i, xbar = xbar, lower = xbar - me, upper = xbar + me,
               covers = (xbar - me <= params$mu) & (params$mu <= xbar + me))
  })
  do.call(rbind, results)
}
