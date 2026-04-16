# ============================================================================
# FUNKCJE POMOCNICZE - Przedzialy ufnosci
# ============================================================================

# Generowanie proby z wybranego rozkladu
generate_population_sample <- function(dist_type, n) {
  switch(dist_type,
    "normal"      = rnorm(n, mean = 170, sd = 10),
    "exponential" = rexp(n, rate = 0.5),
    "uniform"     = runif(n, min = 0, max = 10),
    "bimodal"     = {
      k <- rbinom(n, 1, 0.5)
      ifelse(k == 1, rnorm(n, mean = 3, sd = 0.8), rnorm(n, mean = 7, sd = 0.8))
    },
    "skewed"      = rgamma(n, shape = 2, scale = 1.5),
    rnorm(n)
  )
}

# Parametry populacji
get_population_params <- function(dist_type) {
  switch(dist_type,
    "normal"      = list(mu = 170, sigma = 10),
    "exponential" = list(mu = 2, sigma = 2),
    "uniform"     = list(mu = 5, sigma = sqrt(100/12)),
    "bimodal"     = list(mu = 5, sigma = sqrt(0.8^2 + 4)),
    "skewed"      = list(mu = 3, sigma = sqrt(2) * 1.5),
    list(mu = 0, sigma = 1)
  )
}

# Nazwy rozkladow po polsku
dist_names_pl <- c(
  "normal"      = "Normalny (wzrost)",
  "exponential" = "Wyk\u0142adniczy (prawosko\u015bny)",
  "uniform"     = "Jednostajny",
  "bimodal"     = "Dwumodalny",
  "skewed"      = "Prawoskos\u0144ny (Gamma)"
)

# Symulacja pokrycia przedzialow ufnosci
simulate_coverage <- function(dist_type, n, conf_level, n_sims = 100,
                              method = "t") {
  params <- get_population_params(dist_type)
  mu <- params$mu

  results <- lapply(seq_len(n_sims), function(i) {
    samp <- generate_population_sample(dist_type, n)
    xbar <- mean(samp)
    s <- sd(samp)

    if (method == "z") {
      sigma <- params$sigma
      z_star <- qnorm(1 - (1 - conf_level) / 2)
      me <- z_star * sigma / sqrt(n)
    } else {
      t_star <- qt(1 - (1 - conf_level) / 2, df = n - 1)
      me <- t_star * s / sqrt(n)
    }

    data.frame(
      sim = i,
      xbar = xbar,
      lower = xbar - me,
      upper = xbar + me,
      covers = (xbar - me <= mu) & (mu <= xbar + me)
    )
  })

  do.call(rbind, results)
}

# Symulacja pokrycia dla proporcji
simulate_coverage_prop <- function(true_p, n, conf_level, n_sims = 100,
                                    method = "wald") {
  results <- lapply(seq_len(n_sims), function(i) {
    x <- rbinom(1, n, true_p)
    phat <- x / n

    if (method == "wald") {
      z_star <- qnorm(1 - (1 - conf_level) / 2)
      me <- z_star * sqrt(phat * (1 - phat) / n)
      lower <- phat - me
      upper <- phat + me
    } else {
      # Wilson
      z_star <- qnorm(1 - (1 - conf_level) / 2)
      denom <- 1 + z_star^2 / n
      center <- (phat + z_star^2 / (2 * n)) / denom
      me <- (z_star / denom) * sqrt(phat * (1 - phat) / n + z_star^2 / (4 * n^2))
      lower <- center - me
      upper <- center + me
    }

    data.frame(
      sim = i,
      phat = phat,
      lower = lower,
      upper = upper,
      covers = (lower <= true_p) & (true_p <= upper)
    )
  })

  do.call(rbind, results)
}

