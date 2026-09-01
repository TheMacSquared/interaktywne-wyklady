# ============================================================================
# FUNKCJE POMOCNICZE - Regresja
# ============================================================================

# Wspólne dane CASchools: ~420 okręgów szkolnych w Kalifornii, lata 90.
# Używane w ch1 (regresja prosta) i ch2 (jakość modelu).
.cas_data <- read.csv(
  file.path(app_dir, "dane", "caschools.csv"),
  stringsAsFactors = FALSE
)

.cas_labels <- c(
  students = "Liczba uczniów",
  grades = "Zakres klas",
  income = "Dochód okręgu (tys. USD)",
  student_teacher_ratio = "Uczniowie / nauczyciel",
  expenditure = "Wydatki na ucznia",
  english = "Angielski jako drugi język (%)",
  lunch = "Lunch subsydiowany (%)",
  computer = "Komputery",
  read = "Wynik: czytanie",
  math = "Wynik: matematyka"
)

# Drugi przypadek dydaktyczny: pingwiny są używane tam, gdzie trzy naturalne
# grupy pozwalają dobrze zobaczyć pominiętą zmienną, predyktor jakościowy oraz
# interakcję. Nie zastępują CASchools w pozostałych rozdziałach.
if (!requireNamespace("palmerpenguins", quietly = TRUE)) {
  stop(
    "Pakiet 'palmerpenguins' jest wymagany przez rozdział o kontekście i interakcjach.",
    call. = FALSE
  )
}

.penguins_data <- as.data.frame(stats::na.omit(palmerpenguins::penguins))
.penguins_data$species <- factor(
  .penguins_data$species,
  levels = c("Adelie", "Chinstrap", "Gentoo")
)

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
               x_label = "Godziny nauki/tydz.", y_label = "Średnia ocen")
  } else if (scenario == "temp_icecream") {
    x <- rnorm(n, mean = 20, sd = 7)
    y <- 50 + 15 * x + rnorm(n, 0, 40)
    y <- pmax(y, 0)
    data.frame(x = round(x, 1), y = round(y, 0),
               x_label = "Temperatura (°C)", y_label = "Sprzedaż lodów")
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

generate_assumption_data <- function(n = 120, scenario = "good") {
  x <- runif(n, 0, 10)

  if (scenario == "nonlinear") {
    y <- 3 + 0.7 * x + 0.22 * (x - 5)^2 + rnorm(n, 0, 1.1)
  } else if (scenario == "hetero") {
    y <- 3 + 0.9 * x + rnorm(n, 0, 0.25 + 0.28 * x)
  } else if (scenario == "outlier") {
    y <- 3 + 0.9 * x + rnorm(n, 0, 1)
    x[1] <- 9.8
    y[1] <- min(y) - 8
  } else if (scenario == "nonnormal") {
    y <- 3 + 0.9 * x + rt(n, df = 2) * 1.1
  } else {
    y <- 3 + 0.9 * x + rnorm(n, 0, 1)
  }

  data.frame(x = round(x, 2), y = round(y, 2))
}

generate_confounding_data <- function(n = 160) {
  przygotowanie <- rnorm(n, 0, 1)
  godziny_nauki <- pmax(0, round(18 + 7 * przygotowanie + rnorm(n, 0, 4), 1))
  frekwencja <- pmin(100, pmax(30, round(68 + 14 * przygotowanie + rnorm(n, 0, 8), 1)))
  ocena <- 2.7 + 0.035 * godziny_nauki + 0.004 * frekwencja +
    0.45 * przygotowanie + rnorm(n, 0, 0.35)
  ocena <- pmin(5, pmax(2, round(ocena, 2)))

  data.frame(
    ocena = ocena,
    godziny_nauki = godziny_nauki,
    frekwencja = frekwencja,
    przygotowanie = round(przygotowanie, 2)
  )
}

generate_collinearity_data <- function(n = 140, rho = 0.8) {
  x1 <- rnorm(n)
  x2 <- rho * x1 + sqrt(max(0.001, 1 - rho^2)) * rnorm(n)
  y <- 2 + 1.1 * x1 + 1.1 * x2 + rnorm(n, 0, 1.1)

  data.frame(
    y = round(y, 2),
    x1 = round(x1, 2),
    x2 = round(x2, 2)
  )
}

compute_vif_simple <- function(df, predictors) {
  sapply(predictors, function(pred) {
    others <- setdiff(predictors, pred)
    if (length(others) == 0) return(1)
    form <- as.formula(paste(pred, "~", paste(others, collapse = " + ")))
    r2 <- summary(lm(form, data = df))$r.squared
    1 / (1 - r2)
  })
}

generate_train_test_poly <- function(n_train = 35, n_test = 180) {
  f <- function(x) sin(x) * 3
  train_x <- sort(runif(n_train, 0, 10))
  test_x <- sort(runif(n_test, 0, 10))

  train <- data.frame(
    set = "Trening",
    x = train_x,
    y = f(train_x) + rnorm(n_train, 0, 1)
  )
  test <- data.frame(
    set = "Test",
    x = test_x,
    y = f(test_x) + rnorm(n_test, 0, 1)
  )

  list(train = train, test = test)
}
