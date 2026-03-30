# ============================================================================
# FUNKCJE POMOCNICZE - Regresja
# ============================================================================

# Generowanie danych do regresji liniowej prostej
generate_regression_data <- function(n = 100, beta0 = 10, beta1 = 2, sigma = 5,
                                      scenario = "custom") {
  set.seed(NULL)
  if (scenario == "height_weight") {
    x <- rnorm(n, mean = 170, sd = 10)
    y <- -100 + 1.0 * x + rnorm(n, 0, 8)
    data.frame(x = round(x, 1), y = round(y, 1),
               x_label = "Wzrost (cm)", y_label = "Waga (kg)")
  } else if (scenario == "study_grade") {
    x <- runif(n, 0, 40)
    y <- 2.0 + 0.06 * x + rnorm(n, 0, 0.5)
    y <- pmin(pmax(y, 2), 5)
    data.frame(x = round(x, 1), y = round(y, 2),
               x_label = "Godziny nauki/tydz.", y_label = "\u015arednia ocen")
  } else if (scenario == "temp_icecream") {
    x <- rnorm(n, mean = 20, sd = 7)
    y <- 50 + 15 * x + rnorm(n, 0, 40)
    y <- pmax(y, 0)
    data.frame(x = round(x, 1), y = round(y, 0),
               x_label = "Temperatura (\u00b0C)", y_label = "Sprzeda\u017c lod\u00f3w")
  } else {
    x <- rnorm(n, mean = 0, sd = 3)
    y <- beta0 + beta1 * x + rnorm(n, 0, sigma)
    data.frame(x = round(x, 2), y = round(y, 2),
               x_label = "X", y_label = "Y")
  }
}

# Generowanie danych do regresji wielorakiej
generate_multi_data <- function(n = 150) {
  set.seed(NULL)
  godziny_nauki <- runif(n, 0, 40)
  frekwencja <- runif(n, 30, 100)
  stres <- runif(n, 1, 10)
  sen_h <- rnorm(n, 7, 1.5)

  ocena <- 1.5 + 0.04 * godziny_nauki + 0.015 * frekwencja -
    0.08 * stres + 0.1 * sen_h + rnorm(n, 0, 0.4)
  ocena <- pmin(pmax(round(ocena, 2), 2.0), 5.0)

  data.frame(
    ocena = ocena,
    godziny_nauki = round(godziny_nauki, 1),
    frekwencja = round(frekwencja, 1),
    stres = round(stres, 1),
    sen_h = round(sen_h, 1)
  )
}

# Generowanie danych do regresji logistycznej
generate_logistic_data <- function(n = 200) {
  set.seed(NULL)
  godziny_nauki <- runif(n, 0, 40)
  srednia_ocen <- rnorm(n, 3.5, 0.6)
  srednia_ocen <- pmin(pmax(srednia_ocen, 2), 5)

  log_odds <- -4 + 0.08 * godziny_nauki + 1.2 * srednia_ocen
  prob <- 1 / (1 + exp(-log_odds))
  zdal <- rbinom(n, 1, prob)

  data.frame(
    zdal = factor(zdal, levels = c(0, 1), labels = c("Nie", "Tak")),
    zdal_num = zdal,
    godziny_nauki = round(godziny_nauki, 1),
    srednia_ocen = round(srednia_ocen, 2),
    prob = round(prob, 3)
  )
}

# Formatowanie wynikow modelu
format_model_summary <- function(model) {
  s <- summary(model)
  tidy_df <- broom::tidy(model)
  glance_df <- broom::glance(model)
  list(tidy = tidy_df, glance = glance_df, summary = s)
}

# Obliczanie metryk porownawczych
compute_model_metrics <- function(model, data = NULL, y_name = NULL) {
  g <- broom::glance(model)

  metrics <- list()

  if ("r.squared" %in% names(g)) {
    metrics$r_squared <- g$r.squared
    metrics$adj_r_squared <- g$adj.r.squared
  }

  metrics$aic <- AIC(model)
  metrics$bic <- BIC(model)

  # RMSE
  resid <- residuals(model)
  metrics$rmse <- sqrt(mean(resid^2))

  # Liczba parametrow
  metrics$n_params <- length(coef(model))

  metrics
}

# Theme dla wykresow regresji
theme_reg <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "#7f8c8d"),
      panel.grid.minor = element_blank()
    )
}
