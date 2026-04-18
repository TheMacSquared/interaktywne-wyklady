# ============================================================================
# FUNKCJE POMOCNICZE - Przedzialy ufnosci
# generate_population_sample(), get_population_params(), dist_names_pl -> R/shared.R
# ============================================================================

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

