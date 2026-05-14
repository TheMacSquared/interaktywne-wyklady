# ============================================================================
# FUNKCJE POMOCNICZE - Symulacje statystyczne
# Bootstrap, Jackknife, Permutacje, Cross-Validation, Monte Carlo
# ============================================================================

# ============================================================================
# GENERATORY DANYCH
# ============================================================================

# Kolory semantyczne tej aplikacji oparte o wspólną paletę UPWr.
sim_bootstrap <- unname(upwr_cat["niebo"])
sim_classical <- unname(upwr_cat["bursztyn"])
sim_null_dist <- upwr_reference
sim_observed  <- upwr_accent
sim_resample  <- unname(upwr_cat["bursztyn"])
sim_cv_train  <- unname(upwr_cat["szalwia"])
sim_cv_test   <- unname(upwr_cat["wrzos"])
sim_success   <- unname(upwr_cat["szalwia"])
sim_warning   <- unname(upwr_cat["bursztyn"])
sim_secondary <- upwr_secondary

# Generuje probe z wybranego rozkladu
# dist: "normal", "skewed", "bimodal", "heavy_tail", "uniform"
generate_sample_data <- function(n, dist = "skewed", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  switch(dist,
    "normal"     = rnorm(n, mean = 50, sd = 10),
    "skewed"     = rgamma(n, shape = 2, scale = 5),     # prawosk., mean=10, silna skosnosc
    "bimodal"    = {
      k <- rbinom(n, 1, 0.5)
      ifelse(k == 1, rnorm(n, 30, 5), rnorm(n, 70, 5))
    },
    "heavy_tail" = {
      # t(3) przeskalowany: srednia ~50
      50 + rt(n, df = 3) * 10
    },
    "uniform"    = runif(n, min = 20, max = 80),
    rnorm(n, 50, 10)
  )
}

# Nazwy rozkladow po polsku
dist_names_pl_sim <- c(
  "normal"     = "Normalny",
  "skewed"     = "Prawoskętny (Gamma)",
  "bimodal"    = "Dwumodalny",
  "heavy_tail" = "Grube ogony (t-Studenta)",
  "uniform"    = "Jednostajny"
)

# Generuje dane dwugrupowe do testow permutacyjnych
# effect: prawdziwa roznica srednich (0 = H0 prawdziwe)
generate_two_groups_data <- function(n_per_group = 20, effect = 5,
                                      dist = "skewed", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  x_a <- generate_sample_data(n_per_group, dist)
  x_b <- generate_sample_data(n_per_group, dist) + effect
  data.frame(
    value = c(x_a, x_b),
    group = factor(rep(c("A", "B"), each = n_per_group))
  )
}

# Generuje dane dwuwymiarowe z konfigurowalna korelacja
generate_bivariate_data <- function(n = 30, true_r = 0.4, dist = "normal", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  # Generuje par (x, y) z korelacja true_r przez transformacje Cholesky'ego
  sigma <- matrix(c(1, true_r, true_r, 1), 2, 2)
  L <- chol(sigma)
  z <- matrix(rnorm(n * 2), n, 2)
  xy <- z %*% L
  if (dist == "skewed") {
    # Przeksztalc x przez exp, zachowujac korelacje w przyblizeniu
    xy[, 1] <- exp(xy[, 1] * 0.5)
    xy[, 2] <- exp(xy[, 2] * 0.5)
  }
  data.frame(x = xy[, 1] * 10 + 50, y = xy[, 2] * 10 + 50)
}

# Generuje dane do CV (regresja wielomianowa)
# degree_true: prawdziwy stopien wielomianu w danych (zwykle 2)
generate_regression_data <- function(n = 60, degree_true = 2, sigma = 8, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  x <- runif(n, -3, 3)
  y <- switch(as.character(degree_true),
    "1" = 2 * x + rnorm(n, 0, sigma),
    "2" = x^2 - x + rnorm(n, 0, sigma),
    "3" = x^3 - 2 * x + rnorm(n, 0, sigma),
    x^2 + rnorm(n, 0, sigma)
  )
  data.frame(x = x, y = y)
}

# ============================================================================
# STATYSTYKI OPISOWE (uzupelnienie base R)
# ============================================================================

# Skosnosc probki (momentowa, population-style: bez korekty n/(n-1)(n-2)).
# Zgodnie z konwencja uzywana w wiekszosci podrecznikow wprowadzajacych.
compute_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  if (s == 0) return(NA_real_)
  sum((x - m)^3) / (n * s^3)
}

# ============================================================================
# BOOTSTRAP - RDZEN
# ============================================================================

# Wykonuje B prob bootstrapowych i oblicza statystyke dla kazdej
# stat_fn: funkcja numeric -> scalar (np. mean, median, sd)
# Zwraca list(boot_stats, observed, se)
run_bootstrap <- function(data, stat_fn, B = 1000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  observed <- stat_fn(data)
  n <- length(data)
  boot_stats <- replicate(B, stat_fn(sample(data, size = n, replace = TRUE)))
  list(
    boot_stats = boot_stats,
    observed   = observed,
    se         = sd(boot_stats),
    B          = B
  )
}

# Bootstrap CI metoda percentylowa
# Zwraca data.frame(method, lower, upper, width)
bootstrap_ci_percentile <- function(boot_result, conf_level = 0.95) {
  alpha <- 1 - conf_level
  q <- quantile(boot_result$boot_stats, probs = c(alpha / 2, 1 - alpha / 2))
  data.frame(
    method = "Bootstrap (percentyl)",
    lower  = unname(q[1]),
    upper  = unname(q[2]),
    width  = unname(q[2] - q[1]),
    stringsAsFactors = FALSE
  )
}

# Bootstrap CI metoda basic (reflected)
bootstrap_ci_basic <- function(boot_result, conf_level = 0.95) {
  alpha <- 1 - conf_level
  q <- quantile(boot_result$boot_stats, probs = c(alpha / 2, 1 - alpha / 2))
  obs <- boot_result$observed
  data.frame(
    method = "Bootstrap (basic)",
    lower  = 2 * obs - unname(q[2]),
    upper  = 2 * obs - unname(q[1]),
    width  = unname(q[2] - q[1]),
    stringsAsFactors = FALSE
  )
}

# Klasyczny CI dla sredniej (t-Studenta)
classical_ci_mean <- function(x, conf_level = 0.95) {
  n     <- length(x)
  xbar  <- mean(x)
  s     <- sd(x)
  t_val <- qt(1 - (1 - conf_level) / 2, df = n - 1)
  me    <- t_val * s / sqrt(n)
  data.frame(
    method = "Klasyczny (t-Student)",
    lower  = xbar - me,
    upper  = xbar + me,
    width  = 2 * me,
    stringsAsFactors = FALSE
  )
}

# Klasyczny CI dla proporcji: Wald i Wilson
# Zwraca data.frame z 2 wierszami
classical_ci_proportion <- function(phat, n, conf_level = 0.95) {
  z_star <- qnorm(1 - (1 - conf_level) / 2)

  # Wald
  me_wald <- z_star * sqrt(phat * (1 - phat) / n)

  # Wilson
  denom  <- 1 + z_star^2 / n
  center <- (phat + z_star^2 / (2 * n)) / denom
  me_wilson <- (z_star / denom) * sqrt(phat * (1 - phat) / n + z_star^2 / (4 * n^2))

  rbind(
    data.frame(method = "Wald",   lower = phat - me_wald,    upper = phat + me_wald,
               width = 2 * me_wald,    stringsAsFactors = FALSE),
    data.frame(method = "Wilson", lower = center - me_wilson, upper = center + me_wilson,
               width = 2 * me_wilson, stringsAsFactors = FALSE)
  )
}

# Bootstrap CI dla proporcji
bootstrap_ci_proportion <- function(successes, n, B = 1000, conf_level = 0.95,
                                     seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  phat <- successes / n
  # Traktujemy jako wektor 0/1 o dlugosci n
  x <- c(rep(1, successes), rep(0, n - successes))
  boot_result <- run_bootstrap(x, mean, B = B)
  ci <- bootstrap_ci_percentile(boot_result, conf_level)
  ci$method <- "Bootstrap (percentyl)"
  ci
}

# ============================================================================
# JACKKNIFE
# ============================================================================

# Leave-one-out jackknife
# Zwraca list(pseudovalues, se, bias, bias_corrected, observed)
run_jackknife <- function(data, stat_fn) {
  n        <- length(data)
  observed <- stat_fn(data)
  # Pseudowartosci jackknife: theta_(-i)
  jack_vals <- vapply(seq_len(n), function(i) stat_fn(data[-i]), numeric(1))
  theta_bar <- mean(jack_vals)
  # Bias
  bias <- (n - 1) * (theta_bar - observed)
  # SE
  se   <- sqrt((n - 1) / n * sum((jack_vals - theta_bar)^2))
  list(
    pseudovalues    = jack_vals,
    observed        = observed,
    se              = se,
    bias            = bias,
    bias_corrected  = observed - bias
  )
}

# ============================================================================
# PERMUTACJE
# ============================================================================

# Test permutacyjny dla dwoch grup (roznica srednich)
# data: data.frame(value, group) gdzie group to factor z 2 poziomami
# Zwraca list(perm_diffs, observed_diff, p_value)
run_permutation_test_twosample <- function(data, B = 1000,
                                            alternative = "two.sided",
                                            seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  groups <- levels(data$group)
  x_a    <- data$value[data$group == groups[1]]
  x_b    <- data$value[data$group == groups[2]]
  obs_diff <- mean(x_b) - mean(x_a)
  n <- nrow(data)
  n_a <- length(x_a)

  perm_diffs <- replicate(B, {
    perm <- sample(data$value)
    mean(perm[(n_a + 1):n]) - mean(perm[1:n_a])
  })

  p_val <- switch(alternative,
    "two.sided" = mean(abs(perm_diffs) >= abs(obs_diff)),
    "greater"   = mean(perm_diffs >= obs_diff),
    "less"      = mean(perm_diffs <= obs_diff)
  )

  list(
    perm_diffs    = perm_diffs,
    observed_diff = obs_diff,
    p_value       = p_val,
    alternative   = alternative
  )
}

# Test permutacyjny dla korelacji
# data: data.frame(x, y)
# Zwraca list(perm_cors, observed_r, p_value)
run_permutation_test_correlation <- function(data, B = 1000,
                                              alternative = "two.sided",
                                              seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  obs_r <- cor(data$x, data$y)

  perm_cors <- replicate(B, {
    cor(sample(data$x), data$y)
  })

  p_val <- switch(alternative,
    "two.sided" = mean(abs(perm_cors) >= abs(obs_r)),
    "greater"   = mean(perm_cors >= obs_r),
    "less"      = mean(perm_cors <= obs_r)
  )

  list(
    perm_cors   = perm_cors,
    observed_r  = obs_r,
    p_value     = p_val,
    alternative = alternative
  )
}

# Klasyczny t-test dla dwoch grup (do porownania)
# Zwraca tibble z rstatix::t_test
classical_ttest_twosample <- function(data) {
  t_test(data, value ~ group, var.equal = FALSE)
}

# ============================================================================
# CROSS-VALIDATION
# ============================================================================

# K-Fold CV dla regresji wielomianowej
# data: data.frame(x, y)
# degree: stopien wielomianu
# k: liczba foldow (jesli k == nrow(data): LOOCV)
# Zwraca list(cv_mse, train_mse, fold_errors, k)
run_kfold_cv <- function(data, degree, k = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(data)
  k <- min(k, n)

  # Przypisz obserwacje do foldow
  fold_ids <- sample(rep(seq_len(k), length.out = n))

  fold_errors <- numeric(k)
  for (fold in seq_len(k)) {
    train <- data[fold_ids != fold, , drop = FALSE]
    test  <- data[fold_ids == fold, , drop = FALSE]
    fit   <- lm(y ~ poly(x, degree, raw = TRUE), data = train)
    preds <- predict(fit, newdata = test)
    fold_errors[fold] <- mean((test$y - preds)^2)
  }

  # Train MSE (na calym zbiorze)
  fit_full   <- lm(y ~ poly(x, degree, raw = TRUE), data = data)
  train_preds <- predict(fit_full)
  train_mse  <- mean((data$y - train_preds)^2)

  list(
    cv_mse      = mean(fold_errors),
    train_mse   = train_mse,
    fold_errors = fold_errors,
    k           = k,
    degree      = degree
  )
}

# ============================================================================
# MONTE CARLO
# ============================================================================

# Symulacja mocy testu t (dwie grupy, roznica srednich)
# n: n na grupe, delta: prawdziwa roznica srednich, B: liczba symulacji
# Zwraca list(p_values, power)
run_mc_power <- function(n, delta, alpha = 0.05, B = 1000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  p_values <- replicate(B, {
    x <- rnorm(n, mean = 0,     sd = 10)
    y <- rnorm(n, mean = delta, sd = 10)
    t.test(y, x, var.equal = FALSE)$p.value
  })
  list(
    p_values = p_values,
    power    = mean(p_values < alpha),
    alpha    = alpha,
    n        = n,
    delta    = delta
  )
}

# Krzywa mocy dla wektora wartosci delta
# Zwraca data.frame(delta, power)
compute_power_curve <- function(n, delta_seq, alpha = 0.05, B = 500, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  powers <- vapply(delta_seq, function(d) {
    p_vals <- replicate(B, {
      x <- rnorm(n, 0, 10)
      y <- rnorm(n, d, 10)
      t.test(y, x, var.equal = FALSE)$p.value
    })
    mean(p_vals < alpha)
  }, numeric(1))
  data.frame(delta = delta_seq, power = powers)
}

# Symulacja rozkladu pod H0 przez Monte Carlo
# scenario: "chisq", "ttest_one", "proportion"
# Zwraca list(null_stats, p_value_mc, observed_stat, classical_p)
run_mc_null <- function(observed_data, scenario, B = 5000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  if (scenario == "chisq") {
    # Test zgodnosci: czy dane sa z rozkladu jednostajnego?
    # observed_data: wektor kategorii
    obs_table <- table(observed_data)
    k         <- length(obs_table)
    n         <- sum(obs_table)
    exp_freq  <- rep(n / k, k)
    obs_stat  <- sum((obs_table - exp_freq)^2 / exp_freq)
    null_stats <- replicate(B, {
      sim <- sample(names(obs_table), n, replace = TRUE)
      sim_table <- table(factor(sim, levels = names(obs_table)))
      sum((sim_table - exp_freq)^2 / exp_freq)
    })
    classical_p <- pchisq(obs_stat, df = k - 1, lower.tail = FALSE)

  } else if (scenario == "ttest_one") {
    # Test jednej proby: H0: mu = mu0
    mu0       <- 0
    x         <- observed_data
    n         <- length(x)
    obs_stat  <- (mean(x) - mu0) / (sd(x) / sqrt(n))
    null_stats <- replicate(B, {
      x_sim <- rnorm(n, mean = mu0, sd = sd(x))
      (mean(x_sim) - mu0) / (sd(x_sim) / sqrt(n))
    })
    classical_p <- 2 * pt(-abs(obs_stat), df = n - 1)

  } else if (scenario == "proportion") {
    # Test proporcji: H0: p = 0.5
    p0        <- 0.5
    n         <- length(observed_data)
    k_obs     <- sum(observed_data)
    obs_stat  <- (k_obs / n - p0) / sqrt(p0 * (1 - p0) / n)
    null_stats <- replicate(B, {
      k_sim    <- rbinom(1, n, p0)
      (k_sim / n - p0) / sqrt(p0 * (1 - p0) / n)
    })
    classical_p <- 2 * pnorm(-abs(obs_stat))
  }

  p_value_mc <- mean(abs(null_stats) >= abs(obs_stat))

  list(
    null_stats    = null_stats,
    p_value_mc    = p_value_mc,
    observed_stat = obs_stat,
    classical_p   = classical_p,
    scenario      = scenario
  )
}

# ============================================================================
# HELPERY WIZUALIZACJI
# ============================================================================


# Histogram rozkladu bootstrapowego z zaznaczonymi granicami CI
plot_bootstrap_distribution <- function(boot_result, ci,
                                         stat_label = "Statystyka*",
                                         sim_bootstrap, sim_observed, sim_success,
                                         conf_level = 0.95) {
  df <- data.frame(stat = boot_result$boot_stats)
  ggplot(df, aes(x = stat)) +
    geom_histogram(bins = 40, fill = sim_bootstrap, color = "white", alpha = 0.8) +
    geom_vline(xintercept = boot_result$observed,
               color = sim_observed, linewidth = 1.5, linetype = "solid") +
    geom_vline(xintercept = ci$lower,
               color = sim_success, linewidth = 1.2, linetype = "dashed") +
    geom_vline(xintercept = ci$upper,
               color = sim_success, linewidth = 1.2, linetype = "dashed") +
    annotate("text", x = boot_result$observed, y = Inf,
             label = paste0("obs = ", round(boot_result$observed, 2)),
             vjust = -0.3, hjust = -0.1, color = sim_observed, size = 4) +
    labs(
      title   = paste0("Rozkład bootstrapowy (B = ", boot_result$B, ")"),
      subtitle = paste0(round(conf_level * 100), "% CI: [",
                        round(ci$lower, 2), ", ", round(ci$upper, 2), "]"),
      x = stat_label,
      y = "Liczba prób"
    ) +
    theme_upwr()
}

# Histogram rozkladu permutacyjnego z zaznaczona obserwacja i p-wartoscia
plot_permutation_distribution <- function(perm_result,
                                           stat_label = "Statystyka*",
                                           sim_bootstrap, sim_observed) {
  df    <- data.frame(stat = perm_result$perm_diffs %||% perm_result$perm_cors)
  obs   <- perm_result$observed_diff %||% perm_result$observed_r
  p_val <- perm_result$p_value
  extreme <- abs(df$stat) >= abs(obs)

  ggplot(df, aes(x = stat, fill = extreme)) +
    geom_histogram(bins = 40, color = "white", alpha = 0.85) +
    scale_fill_manual(values = c("FALSE" = sim_bootstrap, "TRUE" = sim_observed),
                      guide = "none") +
    geom_vline(xintercept = obs,
               color = sim_observed, linewidth = 1.5) +
    geom_vline(xintercept = -abs(obs),
               color = sim_observed, linewidth = 1.5, linetype = "dashed") +
    annotate("text", x = obs, y = Inf,
             label = paste0("obs = ", round(obs, 3)),
             vjust = -0.3, hjust = -0.1, color = sim_observed, size = 4) +
    labs(
      title    = paste0("Rozkład permutacyjny (B = ", length(df$stat), ")"),
      subtitle = paste0("p-wartość = ", round(p_val, 4),
                        " (czerwone = równie ekstremalne lub bardziej)"),
      x = stat_label,
      y = "Liczba permutacji"
    ) +
    theme_upwr()
}

# Nullowy operator koalescencji (jak w purrr)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Wykres porownaczy belek CI (wiele metod)
# ci_df: data.frame(method, lower, upper) - wiele wierszy
plot_ci_comparison <- function(ci_df, true_value = NULL,
                                sim_bootstrap, sim_observed, sim_success, sim_warning) {
  ci_df$method <- factor(ci_df$method, levels = rev(ci_df$method))
  n_methods    <- nrow(ci_df)
  cols         <- c(sim_bootstrap, sim_warning, sim_observed,
                    sim_success, sim_cv_test)[seq_len(n_methods)]

  p <- ggplot(ci_df, aes(y = method, color = method)) +
    geom_segment(aes(x = lower, xend = upper, yend = method),
                 linewidth = 2.5) +
    geom_point(aes(x = (lower + upper) / 2), size = 4) +
    scale_color_manual(values = setNames(cols, levels(ci_df$method)),
                       guide = "none") +
    labs(title = "Porównanie przedziałów ufności",
         x = "Wartość", y = NULL) +
    theme_upwr()

  if (!is.null(true_value)) {
    p <- p + geom_vline(xintercept = true_value,
                        color = upwr_secondary, linewidth = 1, linetype = "dotted") +
      annotate("text", x = true_value, y = Inf,
               label = paste0("μ = ", round(true_value, 2)),
               vjust = -0.3, hjust = -0.1, color = upwr_secondary, size = 3.5)
  }
  p
}

# Wykres jednego kroku bootstrapowego (dla ch1)
# Pokazuje oryginalna probe i jedna probe bootstrapowa z kolorowaniem wg czestosci
# resample_list: lista wektorow (kolejne proby bootstrapowe)
plot_bootstrap_step <- function(orig_data, resample_list, sim_bootstrap, sim_warning,
                                 sim_secondary = upwr_secondary) {
  n           <- length(orig_data)
  freq_labels <- c("Pominięty (0x)", "Raz (1x)", "Wielokrotnie (2x+)")
  last_rs     <- resample_list[[length(resample_list)]]

  # Kolorowanie oryginalu wg ostatniej proby bootstrapowej
  idx_match  <- match(last_rs, orig_data)
  freq_table <- tabulate(idx_match, nbins = n)

  df_orig <- data.frame(
    x    = orig_data,
    y    = 0,
    freq = factor(
      ifelse(freq_table == 0, "Pominięty (0x)",
      ifelse(freq_table == 1, "Raz (1x)", "Wielokrotnie (2x+)")),
      levels = freq_labels
    )
  )

  # Wszystkie proby bootstrapowe jako wiersze y = 1, 2, ...
  df_boot <- do.call(rbind, lapply(seq_along(resample_list), function(i) {
    data.frame(x = resample_list[[i]], y = i)
  }))

  y_labels <- c("Oryginalna próba",
                paste0("Bootstrap ", seq_along(resample_list)))
  y_breaks <- 0:length(resample_list)

  # Srednie wszystkich prob bootstrapowych
  boot_means <- sapply(resample_list, mean)

  # Wysokosc wykresu skaluje sie z liczba prob
  plot_height_ratio <- max(1, length(resample_list) * 0.6)

  p <- ggplot() +
    geom_jitter(data = df_orig, aes(x = x, y = y, color = freq),
                height = 0.12, size = 3, alpha = 0.9) +
    geom_jitter(data = df_boot, aes(x = x, y = y),
                color = sim_warning, height = 0.12, size = 2.5, alpha = 0.75) +
    geom_vline(xintercept = mean(orig_data), color = sim_secondary,
               linewidth = 1.2, linetype = "dashed") +
    geom_segment(
      data = data.frame(y = seq_along(resample_list), xmean = boot_means),
      aes(x = xmean, xend = xmean, y = y - 0.35, yend = y + 0.35),
      color = sim_observed, linewidth = 1.2, linetype = "dashed"
    ) +
    scale_color_manual(
      values = c("Pominięty (0x)"      = upwr_rule,
                 "Raz (1x)"                 = sim_bootstrap,
                 "Wielokrotnie (2x+)"       = sim_warning),
      name = "Częstość w ostatniej próbie:"
    ) +
    scale_y_continuous(breaks = y_breaks, labels = y_labels) +
    labs(
      title    = paste0(length(resample_list),
                        ifelse(length(resample_list) == 1,
                               " próba bootstrapowa",
                               " próby bootstrapowe")),
      subtitle = paste0("Oryginalna śr.: ", round(mean(orig_data), 2),
                        "  |  Ostatnia bootstrap śr.: ", round(mean(last_rs), 2)),
      x = "Wartość",
      y = NULL
    ) +
    theme_upwr() +
    theme(axis.text.y = element_text(size = 11))
  p
}

# Wykres pseudowartosci jackknife
plot_jackknife_pseudovalues <- function(jack_result, stat_label = "Statystyka",
                                         sim_bootstrap, sim_observed, sim_success) {
  n    <- length(jack_result$pseudovalues)
  df   <- data.frame(i = seq_len(n), pv = jack_result$pseudovalues)
  obs  <- jack_result$observed
  bc   <- jack_result$bias_corrected
  se   <- jack_result$se

  ggplot(df, aes(x = i, y = pv)) +
    geom_point(color = sim_bootstrap, size = 2.5, alpha = 0.8) +
    geom_hline(yintercept = obs, color = sim_observed,
               linewidth = 1.2, linetype = "dashed") +
    geom_hline(yintercept = bc, color = sim_success,
               linewidth = 1.2, linetype = "solid") +
    geom_hline(yintercept = obs + se, color = sim_bootstrap,
               linewidth = 0.8, linetype = "dotted") +
    geom_hline(yintercept = obs - se, color = sim_bootstrap,
               linewidth = 0.8, linetype = "dotted") +
    annotate("text", x = n * 0.02, y = obs,
             label = paste0("obs = ", round(obs, 3)),
             hjust = 0, vjust = -0.5, color = sim_observed, size = 3.5) +
    annotate("text", x = n * 0.02, y = bc,
             label = paste0("BC = ", round(bc, 3)),
             hjust = 0, vjust = -0.5, color = sim_success, size = 3.5) +
    labs(
      title    = paste0("Pseudowartości jackknife (n = ", n, ")"),
      subtitle = paste0("Obciążenie = ", round(jack_result$bias, 4),
                        "  |  SE = ", round(se, 4)),
      x = "Indeks pominiętej obserwacji",
      y = stat_label
    ) +
    theme_upwr()
}

# Wykres wynikow K-Fold CV
plot_cv_results <- function(cv_results_list, degree_labels = NULL,
                              sim_cv_train, sim_cv_test) {
  # cv_results_list: lista wynikow run_kfold_cv dla roznych stopni
  df <- do.call(rbind, lapply(cv_results_list, function(r) {
    data.frame(degree = r$degree, cv_mse = r$cv_mse, train_mse = r$train_mse)
  }))
  df_long <- tidyr::pivot_longer(df, cols = c("cv_mse", "train_mse"),
                                  names_to = "type", values_to = "mse")
  df_long$type <- factor(df_long$type,
                          levels = c("train_mse", "cv_mse"),
                          labels = c("MSE treningowy", "CV MSE (uogólnienie)"))

  ggplot(df_long, aes(x = degree, y = mse, color = type, group = type)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 3) +
    scale_color_manual(values = c("MSE treningowy" = sim_cv_train,
                                   "CV MSE (uogólnienie)" = sim_cv_test),
                       name = NULL) +
    labs(
      title    = "MSE treningowy vs CV MSE według stopnia wielomianu",
      subtitle = "Optymalny stopień: gdzie CV MSE jest najniższy",
      x        = "Stopień wielomianu",
      y        = "Błąd średniokwadratowy (MSE)"
    ) +
    theme_upwr() +
    theme(legend.position = "top")
}

# Histogram p-wartosci z MC (symulacja mocy)
plot_power_histogram <- function(mc_result, sim_bootstrap, sim_observed) {
  df      <- data.frame(p = mc_result$p_values)
  alpha   <- mc_result$alpha
  n_rej   <- sum(df$p < alpha)
  n_total <- length(df$p)

  ggplot(df, aes(x = p, fill = p < alpha)) +
    geom_histogram(breaks = seq(0, 1, by = 0.05), color = "white", alpha = 0.9) +
    scale_fill_manual(values = c("FALSE" = upwr_rule, "TRUE" = sim_observed),
                      guide = "none") +
    geom_vline(xintercept = alpha, color = sim_observed,
               linewidth = 1.5, linetype = "dashed") +
    annotate("text", x = alpha, y = Inf,
             label = paste0("α = ", alpha),
             vjust = -0.3, hjust = -0.1, color = sim_observed, size = 4) +
    labs(
      title    = paste0("Rozkład p-wartości z B = ", n_total, " symulacji"),
      subtitle = paste0("Moc = ", round(mc_result$power * 100, 1),
                        "% (odrzucono ", n_rej, " z ", n_total, ")"),
      x = "p-wartość",
      y = "Liczba symulacji"
    ) +
    theme_upwr()
}

# Krzywa mocy
plot_power_curve <- function(power_df, current_delta = NULL,
                               alpha = 0.05, sim_bootstrap, sim_observed) {
  p <- ggplot(power_df, aes(x = delta, y = power)) +
    geom_line(color = sim_bootstrap, linewidth = 1.8) +
    geom_point(color = sim_bootstrap, size = 2.5) +
    geom_hline(yintercept = alpha, color = upwr_rule,
               linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = 0.80, color = sim_observed,
               linetype = "dotted", linewidth = 1) +
    annotate("text", x = min(power_df$delta), y = 0.80,
             label = "Moc = 80% (konwencja)", hjust = 0, vjust = -0.4,
             color = sim_observed, size = 3.5) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    labs(
      title    = "Krzywa mocy testu",
      subtitle = paste0("n = ", attr(power_df, "n") %||%
                          "?", "  |  α = ", alpha),
      x        = "Wielkość efektu (δ)",
      y        = "Moc = P(odrzucenie H₀ | H₁ prawdziwa)"
    ) +
    theme_upwr()

  if (!is.null(current_delta)) {
    p <- p + geom_vline(xintercept = current_delta,
                        color = sim_observed, linewidth = 1.2, linetype = "dashed")
  }
  p
}

# ============================================================================
# GENERATORY DANYCH DO CWICZEN KIERUNKOWYCH
# ============================================================================

generate_exercise_data_rol <- function(n = 28, seed = 42) {
  set.seed(seed)
  data.frame(
    nawoz       = factor(sample(c("A", "B"), n, replace = TRUE)),
    plon_dt_ha  = round(rgamma(n, shape = 2.5, scale = 18), 1),  # prawosk., ~45 dt/ha
    odmiana     = factor(sample(c("Typ1", "Typ2", "Typ3"), n, replace = TRUE))
  )
}

generate_exercise_data_zyw <- function(n = 35, seed = 42) {
  set.seed(seed)
  data.frame(
    produkt             = factor(sample(c("A", "B"), n, replace = TRUE)),
    ocena_tekstury      = pmin(7, pmax(1, round(rgamma(n, 2, 0.4) + 1))),
    zawartosc_bialka    = round(rnorm(n, 18, 3.5), 1),
    czas_przechowywania = round(runif(n, 1, 90))
  )
}

generate_exercise_data_bhp <- function(n = 22, seed = 42) {
  set.seed(seed)
  data.frame(
    warunki         = factor(sample(c("normalny", "stres"), n, replace = TRUE)),
    czas_reakcji_ms = round(rlnorm(n, meanlog = 5.6, sdlog = 0.4)),  # silnie prawosk.
    bledy_count     = rpois(n, lambda = 2.5)
  )
}

generate_exercise_data_edu <- function(n = 45, seed = 42) {
  set.seed(seed)
  data.frame(
    program       = factor(sample(c("tradycyjny", "nowy"), n, replace = TRUE)),
    wynik_test    = pmin(100, pmax(0, round(rnorm(n, 68, 15)))),
    frekwencja_pct = round(pmin(100, pmax(50, 100 - rexp(n, rate = 0.04))), 1),
    klasa         = factor(sample(c("A", "B", "C"), n, replace = TRUE))
  )
}

# ============================================================================
# FORMATOWANIE WYNIKOW
# ============================================================================

# Formatuje p-wartosc z decyzja po polsku (tekst p-wartosci z shared.R)
format_pval_pl <- function(p, alpha = 0.05) {
  p_txt <- format_p(p)
  if (p < alpha) {
    list(
      decision    = paste0(p_txt, " — odrzucamy H₀ (α = ", alpha, ")"),
      color       = sim_success,
      explanation = "Wynik jest istotny statystycznie."
    )
  } else {
    list(
      decision    = paste0(p_txt, " — brak podstaw do odrzucenia H₀"),
      color       = sim_observed,
      explanation = "Brak istotności statystycznej."
    )
  }
}
