# ============================================================================
# FUNKCJE POMOCNICZE - Metody bayesowskie
# Prior/posterior, Bayes Factor, HDI, wrappery BayesFactor i rstanarm
# ============================================================================

# Nullowy operator koalescencji
`%||%` <- function(a, b) if (!is.null(a)) a else b

bayes_input <- function(x, default) {
  if (is.null(x) || length(x) != 1L || is.na(x)) default else x
}

# Kolory semantyczne tej aplikacji oparte o wspólną paletę UPWr.
bayes_primary    <- unname(upwr_cat["niebo"])
bayes_secondary  <- upwr_accent
bayes_success    <- unname(upwr_cat["szalwia"])
bayes_warning    <- unname(upwr_cat["bursztyn"])
bayes_reference  <- upwr_secondary
bayes_purple     <- unname(upwr_cat["wrzos"])
bayes_teal       <- unname(upwr_cat["turkus"])
bayes_freq       <- upwr_accent
bayes_bayes      <- unname(upwr_cat["wrzos"])
bayes_prior      <- upwr_reference
bayes_likelihood <- unname(upwr_cat["bursztyn"])
bayes_posterior  <- unname(upwr_cat["wrzos"])
bayes_hdi        <- unname(upwr_cat["bursztyn"])

# ============================================================================
# GENERATORY DANYCH
# ============================================================================

# Rozklad probki (znany ze symulacji)
generate_sample_data <- function(n, dist = "normal", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  switch(dist,
    "normal"     = rnorm(n, mean = 50, sd = 10),
    "skewed"     = rgamma(n, shape = 2, scale = 5),
    "bimodal"    = {
      k <- rbinom(n, 1, 0.5)
      ifelse(k == 1, rnorm(n, 30, 5), rnorm(n, 70, 5))
    },
    "heavy_tail" = 50 + rt(n, df = 3) * 10,
    "uniform"    = runif(n, min = 20, max = 80),
    rnorm(n, 50, 10)
  )
}

# Dane dwugrupowe
generate_two_groups_data <- function(n_per_group = 20, effect = 5,
                                      dist = "normal", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  x_a <- generate_sample_data(n_per_group, dist)
  x_b <- generate_sample_data(n_per_group, dist) + effect
  data.frame(
    value = c(x_a, x_b),
    group = factor(rep(c("A", "B"), each = n_per_group))
  )
}

# Dane wielogrupowe dla ANOVA
generate_multi_groups_data <- function(n_per_group = 20, means = c(0, 5, 10),
                                       sd = 10, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  k <- length(means)
  group_labels <- LETTERS[seq_len(k)]
  values <- unlist(lapply(means, function(m) rnorm(n_per_group, m, sd)))
  data.frame(
    value = values,
    group = factor(rep(group_labels, each = n_per_group))
  )
}

# Dane dwuwymiarowe (korelacja)
generate_bivariate_data <- function(n = 30, true_r = 0.4, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  sigma <- matrix(c(1, true_r, true_r, 1), 2, 2)
  L <- chol(sigma)
  z <- matrix(rnorm(n * 2), n, 2)
  xy <- z %*% L
  data.frame(x = xy[, 1] * 10 + 50, y = xy[, 2] * 10 + 50)
}

# Dane regresyjne (liniowa)
generate_regression_data <- function(n = 60, slope = 1.5, intercept = 2,
                                      sigma = 8, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  x <- runif(n, -3, 3)
  y <- intercept + slope * x + rnorm(n, 0, sigma)
  data.frame(x = x, y = y)
}

# Dane do regresji logistycznej
generate_logistic_data <- function(n = 80, beta0 = -1, beta1 = 1.5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  x <- runif(n, -3, 3)
  p <- 1 / (1 + exp(-(beta0 + beta1 * x)))
  y <- rbinom(n, 1, p)
  data.frame(x = x, y = y)
}

# ============================================================================
# BETA-BINOMIAL (rozdzial 1)
# ============================================================================

# Oblicza prior, likelihood i posterior na siatce theta
# Zwraca data.frame z kolumnami: theta, prior, likelihood, posterior (znormalizowane)
beta_binomial_posterior <- function(successes, trials, alpha_prior = 1, beta_prior = 1,
                                     grid_size = 200) {
  theta <- seq(0.001, 0.999, length.out = grid_size)
  prior <- dbeta(theta, alpha_prior, beta_prior)
  likelihood <- dbinom(successes, trials, theta)
  # Posterior analitycznie: Beta(alpha + s, beta + n - s)
  alpha_post <- alpha_prior + successes
  beta_post  <- beta_prior + trials - successes
  posterior <- dbeta(theta, alpha_post, beta_post)

  # Normalizacja do tej samej skali (max = 1) dla wizualizacji
  data.frame(
    theta      = theta,
    prior      = prior      / max(prior),
    likelihood = likelihood / max(likelihood),
    posterior  = posterior  / max(posterior),
    alpha_post = alpha_post,
    beta_post  = beta_post
  )
}

# Probki z posterior Beta (do obliczenia HDI)
beta_binomial_samples <- function(successes, trials, alpha_prior = 1, beta_prior = 1,
                                   n_samples = 4000) {
  rbeta(n_samples,
        alpha_prior + successes,
        beta_prior + trials - successes)
}

# ============================================================================
# HDI (Highest Density Interval)
# ============================================================================

# HDI z wektora probek (algorytm Chen-Shao: najkrotsze okno zawierajace prob)
hdi_from_samples <- function(samples, prob = 0.95) {
  samples <- sort(samples[is.finite(samples)])
  n <- length(samples)
  window_size <- floor(prob * n)
  if (window_size < 1) return(c(lower = NA, upper = NA))
  n_windows <- n - window_size
  widths <- samples[(window_size + 1):n] - samples[1:n_windows]
  i_min <- which.min(widths)
  c(lower = samples[i_min], upper = samples[i_min + window_size])
}

# ============================================================================
# BAYES FACTOR: wrappery
# ============================================================================

# Test jednej proby: H0: mu = mu0 vs H1: mu != mu0
# Zwraca list(bf10, posterior_samples, posterior_median, hdi, t_result, p_value, mean_x)
compute_bf_one_sample <- function(x, mu0 = 0, n_posterior = 4000) {
  bf_obj <- BayesFactor::ttestBF(x = x, mu = mu0)
  bf10   <- as.numeric(BayesFactor::extractBF(bf_obj)$bf)

  # Posterior dla (mu - mu0) / sigma (efekt standardyzowany)
  post_samples <- BayesFactor::posterior(bf_obj, iterations = n_posterior,
                                         progress = FALSE)
  # Kolumna "mu" to srednia (w oryginalnej skali, juz przesunieta o mu0 niestety nie,
  # wiec odejmujemy) - zaleznie od wersji; bezpieczniej: rekonstruuj z samples.
  mu_post    <- as.numeric(post_samples[, "mu"])
  sigma2_post<- as.numeric(post_samples[, "sig2"])

  # Klasyczny test t
  t_res <- t.test(x, mu = mu0)

  list(
    bf10             = bf10,
    posterior_mu     = mu_post,
    posterior_sigma2 = sigma2_post,
    posterior_median = median(mu_post),
    hdi              = hdi_from_samples(mu_post, prob = 0.95),
    t_statistic      = unname(t_res$statistic),
    df               = unname(t_res$parameter),
    p_value          = t_res$p.value,
    ci_freq          = as.numeric(t_res$conf.int),
    mean_x           = mean(x),
    n                = length(x),
    mu0              = mu0
  )
}

# Test dwoch prob niezaleznych (Welch freq + ttestBF)
# data: data.frame(value, group) z 2 poziomami factor
compute_bf_two_sample <- function(data, n_posterior = 4000) {
  groups <- levels(data$group)
  x_a <- data$value[data$group == groups[1]]
  x_b <- data$value[data$group == groups[2]]

  bf_obj <- BayesFactor::ttestBF(x = x_b, y = x_a)  # B - A
  bf10   <- as.numeric(BayesFactor::extractBF(bf_obj)$bf)

  post_samples <- BayesFactor::posterior(bf_obj, iterations = n_posterior,
                                         progress = FALSE)
  # Kolumny: beta (roznica srednich), mu (srednia globalna), sig2, delta (efekt)
  col_names <- colnames(post_samples)
  # "beta (x - y)" lub "beta" w zaleznosci od wersji
  beta_col <- grep("^beta", col_names, value = TRUE)[1]
  delta_col <- grep("^delta", col_names, value = TRUE)[1]

  diff_post  <- as.numeric(post_samples[, beta_col])
  delta_post <- if (!is.na(delta_col)) as.numeric(post_samples[, delta_col]) else NA

  # Klasyczny test t (Welch)
  t_res <- t.test(x_b, x_a, var.equal = FALSE)

  # Cohen's d (pooled, dla porownania)
  sd_pool <- sqrt(((length(x_a) - 1) * var(x_a) +
                    (length(x_b) - 1) * var(x_b)) /
                   (length(x_a) + length(x_b) - 2))
  cohen_d <- (mean(x_b) - mean(x_a)) / sd_pool

  list(
    bf10             = bf10,
    posterior_diff   = diff_post,
    posterior_delta  = delta_post,
    posterior_median = median(diff_post),
    hdi              = hdi_from_samples(diff_post, prob = 0.95),
    t_statistic      = unname(t_res$statistic),
    df               = unname(t_res$parameter),
    p_value          = t_res$p.value,
    ci_freq          = as.numeric(t_res$conf.int),
    mean_a           = mean(x_a),
    mean_b           = mean(x_b),
    obs_diff         = mean(x_b) - mean(x_a),
    cohen_d          = cohen_d,
    n_a              = length(x_a),
    n_b              = length(x_b),
    group_a          = groups[1],
    group_b          = groups[2]
  )
}

# ANOVA: BF dla modelu z czynnikiem vs null
compute_bf_anova <- function(data) {
  bf_obj <- BayesFactor::anovaBF(value ~ group, data = data, progress = FALSE)
  bf10 <- as.numeric(BayesFactor::extractBF(bf_obj)$bf[1])

  # Klasyczna ANOVA
  aov_res <- summary(aov(value ~ group, data = data))[[1]]
  f_stat  <- aov_res[1, "F value"]
  p_val   <- aov_res[1, "Pr(>F)"]
  df1     <- aov_res[1, "Df"]
  df2     <- aov_res[2, "Df"]

  # Statystyki grupowe
  group_stats <- aggregate(value ~ group, data = data,
                            FUN = function(v) c(mean = mean(v), sd = sd(v),
                                                n = length(v)))
  group_stats <- do.call(data.frame, group_stats)
  names(group_stats) <- c("group", "mean", "sd", "n")

  list(
    bf10        = bf10,
    f_statistic = f_stat,
    df1         = df1,
    df2         = df2,
    p_value     = p_val,
    group_stats = group_stats
  )
}

# Korelacja: BF + posterior rho
compute_bf_correlation <- function(data, n_posterior = 4000) {
  bf_obj <- BayesFactor::correlationBF(y = data$y, x = data$x, progress = FALSE)
  bf10 <- as.numeric(BayesFactor::extractBF(bf_obj)$bf)

  post_samples <- BayesFactor::posterior(bf_obj, iterations = n_posterior,
                                         progress = FALSE)
  rho_col <- grep("^rho", colnames(post_samples), value = TRUE)[1]
  rho_post <- as.numeric(post_samples[, rho_col])

  # Klasyczny test korelacji (Pearson)
  cor_res <- cor.test(data$x, data$y, method = "pearson")

  list(
    bf10             = bf10,
    posterior_rho    = rho_post,
    posterior_median = median(rho_post),
    hdi              = hdi_from_samples(rho_post, prob = 0.95),
    r_obs            = unname(cor_res$estimate),
    t_statistic      = unname(cor_res$statistic),
    df               = unname(cor_res$parameter),
    p_value          = cor_res$p.value,
    ci_freq          = as.numeric(cor_res$conf.int)
  )
}

# ============================================================================
# TABELE KRZYZOWE (ch7)
# ============================================================================

# BF dla tabeli krzyzowej + klasyczny chi-kwadrat
# table_matrix: macierz liczebnosci (r x k)
# sampling: "indepMulti" (domyslne dla "dwa czynniki, ustalone sumy wierszy"),
#           "jointMulti", "poisson", "hypergeom" (tylko 2x2)
compute_bf_contingency <- function(table_matrix, sampling = "indepMulti",
                                    fixed_margin = "rows") {
  # fixedMargin ma znaczenie tylko dla indepMulti
  bf_obj <- if (sampling == "indepMulti") {
    BayesFactor::contingencyTableBF(table_matrix,
                                     sampleType = "indepMulti",
                                     fixedMargin = fixed_margin)
  } else {
    BayesFactor::contingencyTableBF(table_matrix, sampleType = sampling)
  }
  bf10 <- as.numeric(BayesFactor::extractBF(bf_obj)$bf)

  # Klasyczny chi-kwadrat
  # suppressWarnings - dla malych liczebnosci pokazuje ostrzezenie aproksymacji
  chi_res <- suppressWarnings(chisq.test(table_matrix, correct = FALSE))

  # Expected counts warning (reguła Cochrana: >=80% komórek >=5)
  expected <- chi_res$expected
  low_expected_pct <- mean(expected < 5) * 100

  list(
    bf10              = bf10,
    chi_statistic     = unname(chi_res$statistic),
    df                = unname(chi_res$parameter),
    p_value           = chi_res$p.value,
    expected          = expected,
    observed          = table_matrix,
    low_expected_pct  = low_expected_pct,
    sampling          = sampling
  )
}

# Posterior dla proporcji (Beta-Binomial) i OR w tabeli 2x2
# table_2x2: [,1] = "sukces", [,2] = "porazka"; [1,] = grupa A, [2,] = grupa B
# (albo dowolne 2x2 - traktujemy kolumny jako 2 wyniki, wiersze jako 2 grupy)
posterior_2x2_or <- function(table_2x2, alpha_prior = 1, beta_prior = 1,
                              n_samples = 4000) {
  a1 <- table_2x2[1, 1]; b1 <- table_2x2[1, 2]
  a2 <- table_2x2[2, 1]; b2 <- table_2x2[2, 2]

  # Posteriory na proporcje: Beta(alpha + sukcesy, beta + porazki)
  p1_post <- rbeta(n_samples, alpha_prior + a1, beta_prior + b1)
  p2_post <- rbeta(n_samples, alpha_prior + a2, beta_prior + b2)

  diff_post <- p1_post - p2_post
  # Odds ratio: (p1/(1-p1)) / (p2/(1-p2))
  or_post <- (p1_post / (1 - p1_post)) / (p2_post / (1 - p2_post))
  # log-OR (lepiej symetryczny do HDI)
  log_or_post <- log(or_post)

  list(
    p1_samples     = p1_post,
    p2_samples     = p2_post,
    diff_samples   = diff_post,
    or_samples     = or_post,
    log_or_samples = log_or_post,
    diff_median    = median(diff_post),
    diff_hdi       = hdi_from_samples(diff_post, prob = 0.95),
    or_median      = median(or_post),
    or_hdi         = hdi_from_samples(or_post, prob = 0.95),
    log_or_median  = median(log_or_post),
    log_or_hdi     = hdi_from_samples(log_or_post, prob = 0.95),
    p_direction    = mean(diff_post > 0)  # prob. ze p1 > p2
  )
}

# Wykres tabeli krzyzowej: obserwowane vs oczekiwane (stacked bar lub mosaic-like)
plot_contingency_table <- function(observed, title = "Tabela 2x2 (obserwowane)",
                                    col_a = bayes_primary, col_b = bayes_warning) {
  rn <- rownames(observed); if (is.null(rn)) rn <- paste0("W", seq_len(nrow(observed)))
  cn <- colnames(observed); if (is.null(cn)) cn <- paste0("K", seq_len(ncol(observed)))

  df <- expand.grid(row = rn, col = cn, stringsAsFactors = FALSE)
  df$count <- as.vector(observed)
  df$row <- factor(df$row, levels = rn)
  df$col <- factor(df$col, levels = cn)

  ggplot(df, aes(x = row, y = count, fill = col)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
    geom_text(aes(label = count), position = position_dodge(width = 0.9),
              vjust = -0.3, size = 4, fontface = "bold") +
    scale_fill_manual(values = c(col_a, col_b), name = NULL) +
    labs(title = title, x = NULL, y = "Liczność") +
    theme_upwr() +
    theme(legend.position = "top")
}

# Wykres posterior dla OR (log-skali), z referencja OR = 1
plot_posterior_or <- function(post_or_result,
                               bayes_posterior = bayes_purple,
                               bayes_hdi = bayes_warning) {
  df <- data.frame(log_or = post_or_result$log_or_samples)
  d <- density(df$log_or)
  d_df <- data.frame(x = d$x, y = d$y)
  hdi <- post_or_result$log_or_hdi
  d_in_hdi <- d_df[d_df$x >= hdi["lower"] & d_df$x <= hdi["upper"], ]

  # Etykiety OR (a nie log-OR) w dolnej czesci
  or_breaks <- c(0.25, 0.5, 1, 2, 4)
  log_breaks <- log(or_breaks)

  ggplot(d_df, aes(x = x, y = y)) +
    geom_area(fill = bayes_posterior, alpha = 0.25) +
    geom_line(color = bayes_posterior, linewidth = 1.2) +
    geom_area(data = d_in_hdi, fill = bayes_hdi, alpha = 0.45) +
    geom_vline(xintercept = 0, color = bayes_reference,
               linetype = "dotted", linewidth = 1) +
    annotate("text", x = 0, y = Inf, label = "OR = 1",
             vjust = -0.3, hjust = -0.1, color = bayes_reference, size = 3.5) +
    scale_x_continuous(breaks = log_breaks, labels = or_breaks,
                       name = "Odds Ratio (OR)") +
    labs(
      title = "Posterior dla OR (skala log)",
      subtitle = paste0("Mediana OR = ", round(post_or_result$or_median, 2),
                        "  |  95% HDI OR: [",
                        round(post_or_result$or_hdi["lower"], 2), ", ",
                        round(post_or_result$or_hdi["upper"], 2), "]"),
      y = "Gęstość"
    ) +
    theme_upwr()
}

# ============================================================================
# REGRESJA BAYESOWSKA (rstanarm)
# ============================================================================

.bayes_fit_cache <- new.env(parent = emptyenv())

bayes_fit_cache_key <- function(kind, formula, data, chains, iter, prior_scale) {
  data_sig <- paste(
    vapply(data, function(x) paste(round(as.numeric(x), 8), collapse = ","), character(1)),
    collapse = "|"
  )
  paste(kind, deparse(formula), nrow(data), chains, iter, prior_scale, data_sig, sep = "::")
}

# Regresja liniowa: stan_glm vs lm
# Zwraca list ze spójnymi elementami: freq_coefs (df), bayes_coefs (df z HDI)
fit_bayes_lm <- function(formula, data, chains = 2, iter = 1000, prior_scale = 2.5) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("Pakiet 'rstanarm' jest potrzebny do regresji bayesowskiej. Zainstaluj go przez install.packages('rstanarm').", call. = FALSE)
  }

  cache_key <- bayes_fit_cache_key("lm", formula, data, chains, iter, prior_scale)
  if (exists(cache_key, envir = .bayes_fit_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .bayes_fit_cache, inherits = FALSE))
  }

  # Freq: lm
  freq_model <- lm(formula, data = data)
  freq_tidy  <- broom::tidy(freq_model, conf.int = TRUE, conf.level = 0.95)
  freq_coefs <- data.frame(
    term     = freq_tidy$term,
    estimate = freq_tidy$estimate,
    lower    = freq_tidy$conf.low,
    upper    = freq_tidy$conf.high,
    stringsAsFactors = FALSE
  )

  # Bayes: stan_glm
  bayes_model <- rstanarm::stan_glm(
    formula, data = data, family = gaussian(),
    prior = rstanarm::normal(0, prior_scale, autoscale = TRUE),
    prior_intercept = rstanarm::normal(0, 10, autoscale = TRUE),
    chains = chains, iter = iter, refresh = 0, cores = 1, seed = 123
  )

  # Posterior samples dla kazdego parametru
  post <- as.matrix(bayes_model)
  # Zostawiamy tylko wspolczynniki (bez sigma)
  coef_cols <- setdiff(colnames(post), c("sigma"))

  bayes_coefs <- do.call(rbind, lapply(coef_cols, function(cn) {
    samples <- post[, cn]
    hdi <- hdi_from_samples(samples, prob = 0.95)
    data.frame(
      term     = cn,
      estimate = median(samples),
      lower    = unname(hdi["lower"]),
      upper    = unname(hdi["upper"]),
      stringsAsFactors = FALSE
    )
  }))

  result <- list(
    freq_model    = freq_model,
    bayes_model   = bayes_model,
    freq_coefs    = freq_coefs,
    bayes_coefs   = bayes_coefs,
    posterior     = post,
    r_squared     = summary(freq_model)$r.squared,
    n             = nrow(data)
  )
  assign(cache_key, result, envir = .bayes_fit_cache)
  result
}

# Regresja logistyczna
fit_bayes_glm_logistic <- function(formula, data, chains = 2, iter = 1000,
                                    prior_scale = 2.5) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("Pakiet 'rstanarm' jest potrzebny do regresji bayesowskiej. Zainstaluj go przez install.packages('rstanarm').", call. = FALSE)
  }

  cache_key <- bayes_fit_cache_key("logistic", formula, data, chains, iter, prior_scale)
  if (exists(cache_key, envir = .bayes_fit_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .bayes_fit_cache, inherits = FALSE))
  }

  freq_model <- glm(formula, data = data, family = binomial())
  freq_tidy  <- broom::tidy(freq_model, conf.int = TRUE, conf.level = 0.95,
                             exponentiate = FALSE)
  freq_coefs <- data.frame(
    term     = freq_tidy$term,
    estimate = freq_tidy$estimate,
    lower    = freq_tidy$conf.low,
    upper    = freq_tidy$conf.high,
    or       = exp(freq_tidy$estimate),
    or_lower = exp(freq_tidy$conf.low),
    or_upper = exp(freq_tidy$conf.high),
    stringsAsFactors = FALSE
  )

  bayes_model <- rstanarm::stan_glm(
    formula, data = data, family = binomial(),
    prior = rstanarm::normal(0, prior_scale, autoscale = TRUE),
    prior_intercept = rstanarm::normal(0, 2.5, autoscale = TRUE),
    chains = chains, iter = iter, refresh = 0, cores = 1, seed = 123
  )

  post <- as.matrix(bayes_model)
  coef_cols <- colnames(post)

  bayes_coefs <- do.call(rbind, lapply(coef_cols, function(cn) {
    samples <- post[, cn]
    hdi <- hdi_from_samples(samples, prob = 0.95)
    or_samples <- exp(samples)
    hdi_or <- hdi_from_samples(or_samples, prob = 0.95)
    data.frame(
      term     = cn,
      estimate = median(samples),
      lower    = unname(hdi["lower"]),
      upper    = unname(hdi["upper"]),
      or       = median(or_samples),
      or_lower = unname(hdi_or["lower"]),
      or_upper = unname(hdi_or["upper"]),
      stringsAsFactors = FALSE
    )
  }))

  result <- list(
    freq_model  = freq_model,
    bayes_model = bayes_model,
    freq_coefs  = freq_coefs,
    bayes_coefs = bayes_coefs,
    posterior   = post,
    n           = nrow(data)
  )
  assign(cache_key, result, envir = .bayes_fit_cache)
  result
}

# ============================================================================
# INTERPRETACJA BAYES FACTOR
# ============================================================================

# Skala Jeffreysa / Lee & Wagenmakers (2013)
interpret_bf <- function(bf) {
  # Wartosci odnosza sie do log10(BF) wg Jeffreysa; podajemy w skali BF
  if (bf < 1) {
    inv <- 1 / bf
    dir <- "dla H₀"
  } else {
    inv <- bf
    dir <- "dla H₁"
  }
  level <- if (inv < 3) "anekdotyczny (słaby)"
           else if (inv < 10) "umiarkowany"
           else if (inv < 30) "silny"
           else if (inv < 100) "bardzo silny"
           else "ekstremalny"
  list(
    bf            = bf,
    bf_inv        = inv,
    direction     = dir,
    level         = level,
    short_summary = paste0(level, " dowód ", dir)
  )
}

# Formatowanie liczby BF do wyświetlenia
format_bf <- function(bf) {
  if (!is.finite(bf)) return("∞")
  if (bf >= 1000) return(format(bf, digits = 3, scientific = TRUE))
  if (bf >= 10)   return(format(round(bf, 1), nsmall = 1))
  if (bf >= 0.01) return(format(round(bf, 3), nsmall = 3))
  format(bf, digits = 3, scientific = TRUE)
}

# ============================================================================
# FORMATOWANIE p-wartosci (kopia z symulacji dla spojnosci)
# ============================================================================

format_pval_pl <- function(p, alpha = 0.05) {
  p_txt <- format_p(p)
  if (p < alpha) {
    list(
      decision    = paste0(p_txt, " — odrzucamy H₀ (α = ", alpha, ")"),
      color       = bayes_success,
      explanation = "Wynik jest istotny statystycznie."
    )
  } else {
    list(
      decision    = paste0(p_txt, " — brak podstaw do odrzucenia H₀"),
      color       = bayes_secondary,
      explanation = "Brak istotności statystycznej."
    )
  }
}

# ============================================================================
# HELPERY WIZUALIZACJI
# ============================================================================

# Prior, likelihood, posterior na jednej osi (ch1)
plot_prior_likelihood_posterior <- function(df, theta_label = "θ",
                                             show_prior = TRUE,
                                             show_likelihood = TRUE,
                                             show_posterior = TRUE) {
  long_df <- data.frame()
  if (show_prior)      long_df <- rbind(long_df,
                                         data.frame(theta = df$theta, y = df$prior,
                                                    type = "Prior"))
  if (show_likelihood) long_df <- rbind(long_df,
                                         data.frame(theta = df$theta, y = df$likelihood,
                                                    type = "Likelihood (dane)"))
  if (show_posterior)  long_df <- rbind(long_df,
                                         data.frame(theta = df$theta, y = df$posterior,
                                                    type = "Posterior"))

  long_df$type <- factor(long_df$type,
                         levels = c("Prior", "Likelihood (dane)", "Posterior"))

  ggplot(long_df, aes(x = theta, y = y, color = type, fill = type)) +
    geom_area(alpha = 0.25, position = "identity") +
    geom_line(linewidth = 1.3) +
    scale_color_manual(values = c("Prior" = bayes_prior,
                                   "Likelihood (dane)" = bayes_warning,
                                   "Posterior" = bayes_purple),
                       name = NULL) +
    scale_fill_manual(values = c("Prior" = bayes_prior,
                                  "Likelihood (dane)" = bayes_warning,
                                  "Posterior" = bayes_purple),
                      name = NULL) +
    labs(
      title = "Prior → Likelihood → Posterior",
      x = theta_label,
      y = "Gęstość (znormalizowana)"
    ) +
    theme_upwr() +
    theme(legend.position = "top")
}

# Posterior z HDI i punktem odniesienia (0 lub mu0)
plot_posterior_density <- function(samples, hdi, ref_value = 0,
                                    x_label = "Wartość parametru",
                                    title = "Rozkład a posteriori",
                                    bayes_posterior = bayes_purple,
                                    bayes_hdi = bayes_warning) {
  df <- data.frame(x = samples)
  d  <- density(samples)
  d_df <- data.frame(x = d$x, y = d$y)
  d_in_hdi <- d_df[d_df$x >= hdi["lower"] & d_df$x <= hdi["upper"], ]

  ggplot(d_df, aes(x = x, y = y)) +
    geom_area(fill = bayes_posterior, alpha = 0.25) +
    geom_line(color = bayes_posterior, linewidth = 1.2) +
    geom_area(data = d_in_hdi, fill = bayes_hdi, alpha = 0.45) +
    geom_vline(xintercept = ref_value, color = bayes_reference,
               linetype = "dotted", linewidth = 1) +
    geom_vline(xintercept = hdi["lower"], color = bayes_hdi,
               linewidth = 1, linetype = "dashed") +
    geom_vline(xintercept = hdi["upper"], color = bayes_hdi,
               linewidth = 1, linetype = "dashed") +
    annotate("text", x = ref_value, y = Inf,
             label = paste0("ref = ", round(ref_value, 2)),
             vjust = -0.3, hjust = -0.1, color = bayes_reference, size = 3.5) +
    labs(
      title    = title,
      subtitle = paste0("95% HDI: [", round(hdi["lower"], 2),
                        ", ", round(hdi["upper"], 2), "]"),
      x = x_label, y = "Gęstość"
    ) +
    theme_upwr()
}

# Pasek skali BF (Jeffreys)
plot_bf_scale <- function(bf) {
  # Skala w log10(BF), od -2 do 2
  bf_log <- log10(max(bf, 1e-10))
  bf_log <- max(-2.5, min(2.5, bf_log))

  segments_df <- data.frame(
    xmin = c(-2.5, -2, -log10(30), -log10(10), -log10(3), 0,
              log10(3), 1, log10(30), 2),
    xmax = c(-2, -log10(30), -log10(10), -log10(3), 0, log10(3),
              1, log10(30), 2, 2.5),
    fill = c(bayes_reference, bayes_purple, bayes_purple, upwr_seq_burgundy[2], upwr_panel,
             upwr_seq_gold[1], upwr_seq_gold[2], upwr_seq_gold[3], bayes_warning, bayes_reference),
    stringsAsFactors = FALSE
  )

  labels_df <- data.frame(
    x = c(-2, -log10(30), -log10(10), -log10(3), 0,
            log10(3), 1, log10(30), 2),
    label = c("100", "30", "10", "3", "1", "3", "10", "30", "100"),
    stringsAsFactors = FALSE
  )

  ggplot() +
    geom_rect(data = segments_df,
              aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = fill)) +
    scale_fill_identity() +
    geom_vline(xintercept = bf_log, color = bayes_secondary, linewidth = 2) +
    annotate("text", x = bf_log, y = 1.35,
             label = paste0("BF10 = ", format_bf(bf)),
             color = bayes_secondary, fontface = "bold", size = 5) +
    geom_text(data = labels_df, aes(x = x, y = -0.25, label = label), size = 3.3) +
    annotate("text", x = -1.5, y = 0.5, label = "dla H0",
             color = "white", fontface = "bold", size = 4) +
    annotate("text", x = 1.5, y = 0.5, label = "dla H1",
             color = "white", fontface = "bold", size = 4) +
    scale_x_continuous(limits = c(-2.7, 2.7), breaks = NULL) +
    scale_y_continuous(limits = c(-0.5, 1.6), breaks = NULL) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA))
}

# Panel danych (próby) dla porównania (lewy panel freq)
plot_sample_data <- function(x, mu0 = NULL, mean_obs = NULL,
                              title = "Dane (próba)",
                              col_freq = bayes_secondary) {
  df <- data.frame(x = x)
  mean_x <- mean_obs %||% mean(x)

  p <- ggplot(df, aes(x = x)) +
    geom_histogram(bins = 20, fill = col_freq, color = "white", alpha = 0.75) +
    geom_vline(xintercept = mean_x, color = bayes_reference,
               linewidth = 1.3, linetype = "solid") +
    annotate("text", x = mean_x, y = Inf,
             label = paste0("xbar = ", round(mean_x, 2)),
             vjust = -0.3, hjust = -0.1, color = bayes_reference, size = 4) +
    labs(title = title, x = "Wartość", y = "Liczność") +
    theme_upwr()

  if (!is.null(mu0)) {
    p <- p + geom_vline(xintercept = mu0, color = col_freq,
                         linewidth = 1.2, linetype = "dashed") +
      annotate("text", x = mu0, y = Inf,
               label = paste0("mu0 = ", round(mu0, 2)),
               vjust = -1.5, hjust = -0.1, color = col_freq, size = 4)
  }
  p
}

# Box plot dwóch grup (dla ch5)
plot_two_groups_box <- function(data, col_a = bayes_primary, col_b = bayes_warning,
                                 title = "Dane (2 grupy)") {
  ggplot(data, aes(x = group, y = value, fill = group)) +
    geom_jitter(width = 0.15, size = 2, alpha = 0.6,
                 aes(color = group), show.legend = FALSE) +
    geom_boxplot(alpha = 0.55, width = 0.5, outlier.shape = NA) +
    scale_fill_manual(values = c(col_a, col_b), guide = "none") +
    scale_color_manual(values = c(col_a, col_b), guide = "none") +
    labs(title = title, x = "Grupa", y = "Wartość") +
    theme_upwr()
}

# Scatter plot dla korelacji / regresji
plot_scatter_with_fit <- function(data, x_var = "x", y_var = "y",
                                   show_line = TRUE,
                                   col_point = bayes_primary,
                                   col_line = bayes_secondary,
                                   title = "Dane") {
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(color = col_point, size = 2.5, alpha = 0.7)
  if (show_line) {
    p <- p + geom_smooth(method = "lm", se = TRUE, color = col_line,
                          fill = col_line, alpha = 0.15, linewidth = 1.1)
  }
  p + labs(title = title, x = x_var, y = y_var) + theme_upwr()
}

# Forest plot wspolczynnikow (dla regresji: freq vs bayes obok siebie)
plot_coef_forest <- function(coefs_df, paradigm_label,
                              col_freq = bayes_secondary, col_bayes = bayes_purple,
                              exclude_intercept = FALSE) {
  if (exclude_intercept) {
    coefs_df <- coefs_df[!grepl("(Intercept)", coefs_df$term), , drop = FALSE]
  }
  coefs_df$term <- factor(coefs_df$term, levels = rev(coefs_df$term))
  col_use <- if (paradigm_label == "Częstościowo") col_freq else col_bayes

  ggplot(coefs_df, aes(y = term)) +
    geom_vline(xintercept = 0, color = bayes_reference,
                linetype = "dotted", linewidth = 0.8) +
    geom_segment(aes(x = lower, xend = upper, yend = term),
                  color = col_use, linewidth = 2.5) +
    geom_point(aes(x = estimate), color = col_use, size = 4) +
    geom_text(aes(x = estimate,
                   label = paste0(round(estimate, 2),
                                  " [", round(lower, 2), ", ",
                                  round(upper, 2), "]")),
               vjust = -0.8, size = 3.2, color = bayes_reference) +
    labs(title = paradigm_label, x = "Współczynnik", y = NULL) +
    theme_upwr()
}

# ============================================================================
# GENERATORY DANYCH DO CWICZEN KIERUNKOWYCH
# ============================================================================

generate_exercise_data_rol <- function(n = 30, seed = 42) {
  set.seed(seed)
  data.frame(
    nawoz      = factor(sample(c("A", "B"), n, replace = TRUE)),
    plon_dt_ha = round(rgamma(n, shape = 3, scale = 15), 1),
    uprawa     = factor(sample(c("Pszenica", "Zyto"), n, replace = TRUE))
  )
}

generate_exercise_data_zyw <- function(n = 40, seed = 42) {
  set.seed(seed)
  data.frame(
    receptura      = factor(sample(c("Stara", "Nowa"), n, replace = TRUE)),
    ocena_smaku    = pmin(10, pmax(1, round(rnorm(n, 6.5, 1.5)))),
    trwalosc_dni   = round(runif(n, 5, 40))
  )
}

generate_exercise_data_bhp <- function(n = 25, seed = 42) {
  set.seed(seed)
  data.frame(
    szkolenie     = factor(sample(c("Bez", "Po"), n, replace = TRUE)),
    czas_reakcji  = round(rlnorm(n, meanlog = 5.5, sdlog = 0.35)),
    incydent      = rbinom(n, 1, 0.3)
  )
}

generate_exercise_data_edu <- function(n = 50, seed = 42) {
  set.seed(seed)
  data.frame(
    metoda          = factor(sample(c("Klasyczna", "Aktywna"), n, replace = TRUE)),
    wynik           = pmin(100, pmax(0, round(rnorm(n, 70, 12)))),
    godziny_nauki   = round(runif(n, 1, 10), 1),
    zaliczenie      = rbinom(n, 1, 0.7)
  )
}
