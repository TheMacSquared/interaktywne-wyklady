# 📊 Założenia Testów Statystycznych
# Interaktywne narzędzie do nauczania założeń testów statystycznych

library(shiny)
library(ggplot2)
library(dplyr)
library(broom)
library(rstatix)
library(lmtest)

# ============================================================================
# STAŁE ZBIORY DANYCH - MODUŁ 1: NORMALNOŚĆ
# ============================================================================

# Dane normalne - idealnie dopasowane do rozkładu normalnego
# n = 50, seed = 42 dla powtarzalności, następnie posortowane kwantylowo
set.seed(42)
NORMAL_DATA <- qnorm(ppoints(50), mean = 50, sd = 10)

# Dane lekko skośne - gamma z umiarkowaną skośnością
set.seed(123)
SLIGHTLY_SKEWED_DATA <- qgamma(ppoints(50), shape = 7, scale = 7)

# Dane silnie skośne - gamma z silną skośnością
set.seed(456)
HIGHLY_SKEWED_DATA <- qgamma(ppoints(50), shape = 2, scale = 15)

# Dane bimodalne - dwie wyraźnie oddzielone grupy
BIMODAL_DATA <- c(
  qnorm(ppoints(25), mean = 30, sd = 5),
  qnorm(ppoints(25), mean = 70, sd = 5)
)

# Dane z outlierami - normalne z kilkoma ekstremalnymi wartościami
set.seed(789)
OUTLIERS_DATA <- c(
  qnorm(ppoints(45), mean = 50, sd = 8),
  c(15, 18, 82, 85, 88)  # wyraźne outliery
)

# Funkcje zwracające stałe dane (dla kompatybilności z resztą kodu)
get_normal_data <- function() NORMAL_DATA
get_slightly_skewed_data <- function() SLIGHTLY_SKEWED_DATA
get_highly_skewed_data <- function() HIGHLY_SKEWED_DATA
get_bimodal_data <- function() BIMODAL_DATA
get_outliers_data <- function() OUTLIERS_DATA

# Stałe dane dla demonstracji wpływu na test t
set.seed(123)
DEMO_NORMAL_DATA <- rnorm(50, mean = 50, sd = 10)
set.seed(456)
DEMO_SKEWED_DATA <- rgamma(50, shape = 2, scale = 15)

# ============================================================================
# STAŁE ZBIORY DANYCH - MODUŁ 2: JEDNORODNOŚĆ WARIANCJI
# ============================================================================

# Funkcja pomocnicza do tworzenia stałych danych
create_variance_data <- function(seed, n_per_group, means, sds, group_names) {
  set.seed(seed)
  n_groups <- length(means)
  groups <- rep(group_names[1:n_groups], times = n_per_group)
  values <- c()
  for (i in 1:n_groups) {
    values <- c(values, rnorm(n_per_group[i], mean = means[i], sd = sds[i]))
  }
  data.frame(group = factor(groups, levels = group_names[1:n_groups]), value = values)
}

# 2 grupy - równe wariancje, równe n (SD = 10, 10)
VAR_EQUAL_2 <- create_variance_data(
  seed = 101, n_per_group = c(30, 30),
  means = c(50, 55), sds = c(10, 10),
  group_names = c("Grupa A", "Grupa B", "Grupa C")
)

# 2 grupy - bardzo różne wariancje + nierówne n (mała grupa z dużą wariancją!)
# Student's t: p=0.026 (istotne!), Welch's t: p=0.204 (nieistotne) - ROZBIEŻNOŚĆ!
VAR_UNEQUAL_2 <- create_variance_data(
  seed = 117, n_per_group = c(12, 40),
  means = c(50, 54), sds = c(22, 7),
  group_names = c("Grupa A", "Grupa B", "Grupa C")
)

# 3 grupy - równe wariancje (SD = 10, 10, 10)
VAR_EQUAL_3 <- create_variance_data(
  seed = 103, n_per_group = c(25, 25, 25),
  means = c(50, 55, 60), sds = c(10, 10, 10),
  group_names = c("Grupa A", "Grupa B", "Grupa C")
)

# 3 grupy - bardzo różne wariancje + nierówne n (mała grupa z dużą wariancją!)
# Klasyczna ANOVA: p=0.007 (istotne!), Welch ANOVA: p=0.315 (nieistotne) - ROZBIEŻNOŚĆ!
VAR_UNEQUAL_3 <- create_variance_data(
  seed = 140, n_per_group = c(10, 30, 30),
  means = c(50, 53, 56), sds = c(25, 8, 8),
  group_names = c("Grupa A", "Grupa B", "Grupa C")
)

# Funkcje zwracające stałe dane
get_variance_data <- function(n_groups, equal_var) {
  if (n_groups == 2) {
    if (equal_var) VAR_EQUAL_2 else VAR_UNEQUAL_2
  } else {
    if (equal_var) VAR_EQUAL_3 else VAR_UNEQUAL_3
  }
}

# ============================================================================
# FUNKCJE GENERUJĄCE DANE - MODUŁ 3: PORÓWNANIE TESTÓW
# ============================================================================

# 3a: t-test vs Wilcoxon
generate_normal_equal_var <- function(n = 30) {
  set.seed(NULL)
  group <- rep(c("A", "B"), each = n)
  value <- c(rnorm(n, mean = 50, sd = 10), rnorm(n, mean = 58, sd = 10))
  data.frame(group = group, value = value)
}

generate_skewed_groups <- function(n = 30) {
  set.seed(NULL)
  group <- rep(c("A", "B"), each = n)
  value <- c(rgamma(n, shape = 3, scale = 10), rgamma(n, shape = 3, scale = 13))
  data.frame(group = group, value = value)
}

generate_groups_with_outliers <- function(n = 30) {
  set.seed(NULL)
  group <- rep(c("A", "B"), each = n)
  base_a <- rnorm(n - 2, mean = 50, sd = 10)
  base_b <- rnorm(n - 2, mean = 58, sd = 10)
  value <- c(base_a, c(90, 95), base_b, c(5, 10))
  data.frame(group = group, value = value)
}

generate_small_n_normal <- function(n = 10) {
  set.seed(NULL)
  group <- rep(c("A", "B"), each = n)
  value <- c(rnorm(n, mean = 50, sd = 10), rnorm(n, mean = 58, sd = 10))
  data.frame(group = group, value = value)
}

generate_small_n_skewed <- function(n = 10) {
  set.seed(NULL)
  group <- rep(c("A", "B"), each = n)
  value <- c(rgamma(n, shape = 3, scale = 10), rgamma(n, shape = 3, scale = 13))
  data.frame(group = group, value = value)
}

# 3b: Pearson vs Spearman
generate_linear_correlation <- function(n = 50) {
  set.seed(NULL)
  x <- rnorm(n, mean = 50, sd = 10)
  y <- 2 * x + rnorm(n, mean = 0, sd = 10)
  data.frame(x = x, y = y)
}

generate_monotonic_nonlinear <- function(n = 50) {
  set.seed(NULL)
  x <- runif(n, 1, 10)
  y <- log(x) * 20 + rnorm(n, mean = 0, sd = 3)
  data.frame(x = x, y = y)
}

generate_correlation_with_outliers <- function(n = 50) {
  set.seed(NULL)
  x <- rnorm(n - 3, mean = 50, sd = 10)
  y <- 2 * x + rnorm(n - 3, mean = 0, sd = 10)
  x <- c(x, c(20, 80, 85))
  y <- c(y, c(200, 50, 220))
  data.frame(x = x, y = y)
}

generate_no_correlation <- function(n = 50) {
  set.seed(NULL)
  x <- rnorm(n, mean = 50, sd = 10)
  y <- rnorm(n, mean = 50, sd = 10)
  data.frame(x = x, y = y)
}

# 3c: ANOVA vs Kruskal-Wallis
generate_anova_normal_equal <- function(n_per_group = 25) {
  set.seed(NULL)
  group <- rep(c("A", "B", "C"), each = n_per_group)
  value <- c(
    rnorm(n_per_group, mean = 50, sd = 10),
    rnorm(n_per_group, mean = 55, sd = 10),
    rnorm(n_per_group, mean = 62, sd = 10)
  )
  data.frame(group = group, value = value)
}

generate_anova_skewed <- function(n_per_group = 25) {
  set.seed(NULL)
  group <- rep(c("A", "B", "C"), each = n_per_group)
  value <- c(
    rgamma(n_per_group, shape = 3, scale = 10),
    rgamma(n_per_group, shape = 3, scale = 12),
    rgamma(n_per_group, shape = 3, scale = 15)
  )
  data.frame(group = group, value = value)
}

generate_anova_unequal_var <- function(n_per_group = 25) {
  set.seed(NULL)
  group <- rep(c("A", "B", "C"), each = n_per_group)
  value <- c(
    rnorm(n_per_group, mean = 50, sd = 5),
    rnorm(n_per_group, mean = 55, sd = 15),
    rnorm(n_per_group, mean = 62, sd = 25)
  )
  data.frame(group = group, value = value)
}

generate_anova_with_outliers <- function(n_per_group = 25) {
  set.seed(NULL)
  group <- rep(c("A", "B", "C"), each = n_per_group)
  base <- c(
    rnorm(n_per_group - 1, mean = 50, sd = 10),
    rnorm(n_per_group - 1, mean = 55, sd = 10),
    rnorm(n_per_group - 1, mean = 62, sd = 10)
  )
  outliers <- c(95, 5, 100)
  value <- c(base, outliers)
  data.frame(group = group, value = value)
}

# ============================================================================
# FUNKCJE GENERUJĄCE DANE - MODUŁ 4: REGRESJA
# ============================================================================

# 4a: Normalność reszt
# ----------------------------------------------------------------------------
# 4a: Normalność reszt - SZTYWNE DANE
# ----------------------------------------------------------------------------

# Scenariusz: Normalny związek liniowy (idealne reszty)
set.seed(101)
REG_NORMAL_DATA <- {
  x <- runif(80, 10, 100)
  y <- 2 * x + 50 + rnorm(80, mean = 0, sd = 15)
  data.frame(x = x, y = y)
}

# Scenariusz: Nieliniowy związek (reszty nienormalne - wzorzec)
set.seed(102)
REG_NONLINEAR_DATA <- {
  x <- runif(80, 0, 10)
  y <- x^2 + rnorm(80, mean = 0, sd = 3)
  data.frame(x = x, y = y)
}

# Scenariusz: Reszty z outlierami (odstające wartości Y)
set.seed(103)
REG_OUTLIERS_RESIDUALS_DATA <- {
  x <- runif(76, 10, 100)
  y <- 2 * x + 50 + rnorm(76, mean = 0, sd = 15)
  # Dodaj outliery w Y przy różnych wartościach X
  x <- c(x, c(20, 50, 70, 90))
  y <- c(y, c(250, 40, 300, 90))
  data.frame(x = x, y = y)
}

# ----------------------------------------------------------------------------
# 4b: Homoskedastyczność - SZTYWNE DANE
# ----------------------------------------------------------------------------

# Scenariusz: Stała wariancja (homoskedastyczność)
set.seed(201)
REG_HOMOSCEDASTIC_DATA <- {
  x <- runif(80, 10, 100)
  y <- 2 * x + 50 + rnorm(80, mean = 0, sd = 15)
  data.frame(x = x, y = y)
}

# Scenariusz: Wariancja rośnie z X (lejek w prawo)
set.seed(202)
REG_HETERO_INCREASING_DATA <- {
  x <- runif(80, 10, 100)
  y <- 2 * x + 50 + rnorm(80, mean = 0, sd = x * 0.4)
  data.frame(x = x, y = y)
}

# Scenariusz: Wariancja maleje z X (lejek w lewo)
set.seed(203)
REG_HETERO_DECREASING_DATA <- {
  x <- runif(80, 10, 100)
  y <- 2 * x + 50 + rnorm(80, mean = 0, sd = (110 - x) * 0.35)
  data.frame(x = x, y = y)
}

# ----------------------------------------------------------------------------
# 4c: Wpływ outlierów - SZTYWNE DANE
# ----------------------------------------------------------------------------

# Scenariusz: Bez outlierów (czyste dane)
set.seed(301)
REG_NO_OUTLIERS_DATA <- {
  x <- runif(50, 10, 100)
  y <- 2 * x + 50 + rnorm(50, mean = 0, sd = 15)
  data.frame(x = x, y = y, is_outlier = FALSE)
}

# Scenariusz: Outlier w Y (blisko centrum X)
set.seed(302)
REG_OUTLIER_Y_DATA <- {
  x <- runif(48, 10, 100)
  y <- 2 * x + 50 + rnorm(48, mean = 0, sd = 15)
  # Outliery w centrum X - mają mały wpływ na slope
  x <- c(x, c(50, 55))
  y <- c(y, c(260, 270))
  is_outlier <- c(rep(FALSE, 48), TRUE, TRUE)
  data.frame(x = x, y = y, is_outlier = is_outlier)
}

# Scenariusz: Outlier w X i Y (wysoka dźwignia)
set.seed(303)
REG_OUTLIER_XY_DATA <- {
  x <- runif(48, 20, 80)
  y <- 2 * x + 50 + rnorm(48, mean = 0, sd = 15)
  # Outliery na skrajach X - duży wpływ na slope!
  x <- c(x, c(5, 95))
  y <- c(y, c(200, 100))
  is_outlier <- c(rep(FALSE, 48), TRUE, TRUE)
  data.frame(x = x, y = y, is_outlier = is_outlier)
}

# Scenariusz: Kilka outlierów (mieszane)
set.seed(304)
REG_MULTIPLE_OUTLIERS_DATA <- {
  x <- runif(46, 15, 85)
  y <- 2 * x + 50 + rnorm(46, mean = 0, sd = 15)
  # Różne typy outlierów
  x <- c(x, c(10, 50, 90, 55))
  y <- c(y, c(250, 40, 80, 280))
  is_outlier <- c(rep(FALSE, 46), TRUE, TRUE, TRUE, TRUE)
  data.frame(x = x, y = y, is_outlier = is_outlier)
}

# ============================================================================
# PRE-COMPUTED WYNIKI SYMULACJI (Monte Carlo, n_sim = 10000)
# ============================================================================

# Moduł 4: Regresja - pokrycie 95% CI przy heteroskedastyczności
precomputed_regression <- data.frame(
  heterosked = factor(c("Brak", "Brak",
                        "Umiarkowana", "Umiarkowana",
                        "Silna", "Silna"),
                      levels = c("Brak", "Umiarkowana", "Silna")),
  metoda = rep(c("OLS (zwykły)", "Robust SE"), 3),
  pokrycie_CI = c(0.948, 0.951,   # brak heterosked
                  0.892, 0.949,   # umiarkowana
                  0.842, 0.948)   # silna
)

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  # CSS dla result-boxes
  tags$style(HTML("
    .result-box-success {
      background-color: #d4edda;
      border: 2px solid #28a745;
      padding: 15px;
      border-radius: 8px;
      margin: 10px 0;
    }
    .result-box-danger {
      background-color: #f8d7da;
      border: 2px solid #dc3545;
      padding: 15px;
      border-radius: 8px;
      margin: 10px 0;
    }
    .result-box-warning {
      background-color: #fff3cd;
      border: 2px solid #ffc107;
      padding: 15px;
      border-radius: 8px;
      margin: 10px 0;
    }
    .interpretation-box {
      background-color: #e7f3ff;
      border: 2px solid #0d6efd;
      padding: 15px;
      border-radius: 8px;
      margin-top: 20px;
    }
    .value-big {
      font-size: 28px;
      font-weight: bold;
    }
    .value-ok { color: #28a745; }
    .value-bad { color: #dc3545; }
    .value-warn { color: #ffc107; }
  ")),
  titlePanel("📊 Założenia Testów Statystycznych - Przewodnik Interaktywny"),

  tabsetPanel(
    id = "main_tabs",

    # ========================================================================
    # TAB 1: NORMALNOŚĆ
    # ========================================================================
    tabPanel(
      "📈 Założenie normalności",
      br(),
      tabsetPanel(
        # --- Podtab: Wizualizacja ---
        tabPanel(
          "Wizualizacja",
          br(),
          sidebarLayout(
            sidebarPanel(
              h4("Wybór scenariusza"),
              selectInput("normality_scenario", "Typ rozkładu:",
                          choices = c(
                            "Normalny (idealny)" = "normal",
                            "Lekko skośny" = "slightly_skewed",
                            "Silnie skośny" = "highly_skewed",
                            "Bimodalny" = "bimodal",
                            "Z outlierami" = "outliers"
                          ),
                          selected = "normal"),

              hr(),

              checkboxInput("normality_show_normal", "Pokaż rozkład normalny (overlay)", value = TRUE),

              hr(),

              h4("Interpretacja"),
              div(
                style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px;",
                textOutput("normality_interpretation")
              ),

              width = 3
            ),

            mainPanel(
              fluidRow(
                column(6,
                  div(
                    style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px;",
                    h4("Histogram z rozkładem normalnym"),
                    plotOutput("normality_histogram", height = "280px")
                  )
                ),
                column(6,
                  div(
                    style = "border: 2px solid #e67e22; border-radius: 5px; padding: 10px;",
                    h4("QQ-plot (Quantile-Quantile)"),
                    plotOutput("normality_qqplot", height = "280px")
                  )
                )
              ),

              br(),

              div(
                style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
                h4("Test Shapiro-Wilka"),
                tableOutput("normality_test"),
                div(
                  style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-top: 10px;",
                  p("QQ-plot pokazuje jak dane porównują się z rozkładem normalnym. Jeśli punkty leżą blisko linii, rozkład jest normalny.")
                )
              ),

              width = 9
            )
          )
        ),

        # --- Podtab: Problem u podstawy ---
        tabPanel(
          "Wpływ na test t",
          br(),
          h3("Jak naruszenie normalności wpływa na test t dla jednej próby?"),
          p("Testujemy hipotezę ", strong("H0: μ = 50"), " dla danych normalnych i ", strong("H0: μ = 25"), " dla danych skośnych."),

          hr(),

          fluidRow(
            # Panel 1: Rozkład normalny
            column(6,
              div(class = "result-box-success",
                h4("Rozkład normalny (symetryczny)"),
                plotOutput("norm_base_hist_ok", height = "200px"),
                hr(),

                h5("Statystyki opisowe"),
                fluidRow(
                  column(6, p("Średnia: ", span(class = "value-big", style = "color: #e74c3c;", textOutput("norm_base_mean_ok", inline = TRUE)))),
                  column(6, p("Mediana: ", span(class = "value-big", style = "color: #27ae60;", textOutput("norm_base_median_ok", inline = TRUE))))
                ),
                p(style = "color: #28a745; font-size: 12px;", "Średnia ≈ Mediana → obie dobrze opisują 'typową' wartość"),

                hr(),

                h5("Wyniki testów (H0: μ = 50)"),
                tableOutput("norm_base_tests_ok"),

                hr(),

                h5("95% przedział ufności dla średniej"),
                plotOutput("norm_base_ci_ok", height = "80px"),

                hr(),

                div(style = "background-color: rgba(40, 167, 69, 0.1); padding: 10px; border-radius: 5px;",
                  p(strong("Interpretacja:")),
                  p("Test t jest wiarygodny. Średnia dobrze reprezentuje dane, przedział ufności jest poprawny.")
                )
              )
            ),

            # Panel 2: Rozkład skośny
            column(6,
              div(class = "result-box-danger",
                h4("Rozkład skośny (asymetryczny)"),
                plotOutput("norm_base_hist_bad", height = "200px"),
                hr(),

                h5("Statystyki opisowe"),
                fluidRow(
                  column(6, p("Średnia: ", span(class = "value-big", style = "color: #e74c3c;", textOutput("norm_base_mean_bad", inline = TRUE)))),
                  column(6, p("Mediana: ", span(class = "value-big", style = "color: #27ae60;", textOutput("norm_base_median_bad", inline = TRUE))))
                ),
                p(style = "color: #dc3545; font-size: 12px;", "Średnia >> Mediana → średnia 'ucieka' w stronę ogona!"),

                hr(),

                h5("Wyniki testów (H0: μ = 25)"),
                tableOutput("norm_base_tests_bad"),

                hr(),

                h5("95% przedział ufności dla średniej"),
                plotOutput("norm_base_ci_bad", height = "80px"),

                hr(),

                div(style = "background-color: rgba(220, 53, 69, 0.1); padding: 10px; border-radius: 5px;",
                  p(strong("Interpretacja:")),
                  p("Test t i Wilcoxon dają ", strong("rozbieżne wyniki!"), " Test t odrzuca H0 (średnia > 25), ",
                    "ale Wilcoxon nie (mediana ≈ 25). Która odpowiedź jest 'poprawna'?")
                )
              )
            )
          ),

          hr(),

          div(class = "interpretation-box",
            h4("Kluczowy wniosek"),
            p("Przy ", strong("skośnym rozkładzie"), ":"),
            tags$ul(
              tags$li("Średnia 'ucieka' w stronę ogona i nie reprezentuje typowej wartości"),
              tags$li("Test t testuje średnią, więc może dawać statystycznie istotny wynik, który jest praktycznie bez znaczenia"),
              tags$li("Test Wilcoxona (na medianie) jest odporny na skośność")
            ),
            p(style = "font-style: italic;",
              "Zalecenie: Przy skośnych danych rozważ test Wilcoxona lub transformację danych.")
          )
        )
      )
    ),

    # ========================================================================
    # TAB 2: JEDNORODNOŚĆ WARIANCJI
    # ========================================================================
    tabPanel(
      "📊 Jednorodność wariancji",
      br(),
      tabsetPanel(
        # --- Podtab: Wizualizacja ---
        tabPanel(
          "Wizualizacja",
          br(),
          sidebarLayout(
            sidebarPanel(
              h4("Wybór scenariusza"),
              selectInput("variance_scenario", "Typ wariancji:",
                          choices = c(
                            "Równe wariancje" = "equal",
                            "Różne wariancje" = "unequal"
                          ),
                          selected = "equal"),

              radioButtons("variance_n_groups", "Liczba grup:",
                           choices = c("2 grupy" = "2", "3 grupy" = "3"),
                           selected = "2", inline = TRUE),

              hr(),

              h4("Interpretacja"),
              div(
                style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px;",
                textOutput("variance_interpretation")
              ),

              width = 3
            ),

            mainPanel(
              fluidRow(
                column(6,
                  div(
                    style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px;",
                    h4("Boxploty grup"),
                    plotOutput("variance_boxplot", height = "280px")
                  )
                ),
                column(6,
                  div(
                    style = "border: 2px solid #e67e22; border-radius: 5px; padding: 10px;",
                    h4("Statystyki opisowe"),
                    tableOutput("variance_stats")
                  )
                )
              ),

              br(),

              div(
                style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
                h4("Test Levene'a"),
                tableOutput("variance_test"),
                div(
                  style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-top: 10px;",
                  p("Test Levene'a sprawdza hipotezę o równości wariancji między grupami. ",
                    "p < 0.05 oznacza istotne różnice w wariancjach.")
                )
              ),

              width = 9
            )
          )
        ),

        # --- Podtab: Wpływ na test ---
        tabPanel(
          "Wpływ na test",
          br(),
          h3("Jak nierówne wariancje wpływają na porównanie grup?"),
          p("Porównujemy grupy testami: ", strong("Student's t / ANOVA"), " (zakłada równe wariancje) vs ",
            strong("Welch's t / Welch's ANOVA"), " (nie zakłada)."),

          hr(),

          radioButtons("var_test_n_groups", "Liczba grup:",
                       choices = c("2 grupy (t-test)" = "2", "3 grupy (ANOVA)" = "3"),
                       selected = "2", inline = TRUE),

          hr(),

          fluidRow(
            # Panel 1: Równe wariancje
            column(6,
              div(class = "result-box-success",
                h4("Równe wariancje"),
                plotOutput("var_test_boxplot_equal", height = "180px"),
                hr(),

                h5("Statystyki grup"),
                tableOutput("var_test_stats_equal"),

                hr(),

                h5("Wyniki testów"),
                tableOutput("var_test_results_equal"),

                hr(),

                div(style = "background-color: rgba(40, 167, 69, 0.1); padding: 10px; border-radius: 5px;",
                  p(strong("Interpretacja:")),
                  p("Oba testy dają podobne wyniki - założenie spełnione, więc nie ma problemu.")
                )
              )
            ),

            # Panel 2: Różne wariancje
            column(6,
              div(class = "result-box-danger",
                h4("Różne wariancje"),
                plotOutput("var_test_boxplot_unequal", height = "180px"),
                hr(),

                h5("Statystyki grup"),
                tableOutput("var_test_stats_unequal"),

                hr(),

                h5("Wyniki testów"),
                tableOutput("var_test_results_unequal"),

                hr(),

                div(style = "background-color: rgba(220, 53, 69, 0.1); padding: 10px; border-radius: 5px;",
                  p(strong("Interpretacja:")),
                  uiOutput("var_test_interpretation_unequal")
                )
              )
            )
          ),

          hr(),

          div(class = "interpretation-box",
            h4("Kluczowy wniosek"),
            p("Przy ", strong("nierównych wariancjach"), ":"),
            tags$ul(
              tags$li("Student's t / klasyczna ANOVA może dawać błędne p-value"),
              tags$li("Welch's t / Welch's ANOVA automatycznie koryguje dla różnic w wariancjach"),
              tags$li("W R: t.test() domyślnie używa Welch's (var.equal = FALSE)")
            ),
            p(style = "font-style: italic;",
              "Zalecenie: Zawsze używaj Welch's wersji testów - działa poprawnie niezależnie od równości wariancji.")
          )
        )
      )
    ),

    # ========================================================================
    # TAB 3: ŚCIĄGAWKA TESTÓW
    # ========================================================================
    tabPanel(
      "📋 Ściągawka testów",
      br(),
      h2("Który test wybrać?", style = "text-align: center; margin-bottom: 30px;"),

      # Tabela główna
      div(
        style = "max-width: 1000px; margin: 0 auto;",

        # Sekcja: Jedna zmienna
        h3("Jedna zmienna", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),

        tags$table(
          class = "table table-bordered table-striped",
          style = "width: 100%; margin-bottom: 30px; font-size: 14px;",
          tags$thead(
            style = "background-color: #3498db; color: white;",
            tags$tr(
              tags$th("Problem", style = "width: 28%;"),
              tags$th("Oba założenia OK", style = "width: 24%;"),
              tags$th("Wariancja naruszona", style = "width: 24%;"),
              tags$th("Normalność naruszona", style = "width: 24%;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Średnia vs wartość teoretyczna"),
              tags$td("t-test jednej próby"),
              tags$td("—"),
              tags$td("Wilcoxon signed-rank")
            ),
            tags$tr(
              tags$td("Proporcja vs wartość teoretyczna"),
              tags$td("Test proporcji (z)"),
              tags$td("—"),
              tags$td("Test dwumianowy")
            )
          )
        ),

        # Sekcja: Dwie zmienne
        h3("Dwie zmienne", style = "color: #2c3e50; border-bottom: 2px solid #27ae60; padding-bottom: 10px;"),

        tags$table(
          class = "table table-bordered table-striped",
          style = "width: 100%; margin-bottom: 30px; font-size: 14px;",
          tags$thead(
            style = "background-color: #27ae60; color: white;",
            tags$tr(
              tags$th("Problem", style = "width: 28%;"),
              tags$th("Oba założenia OK", style = "width: 24%;"),
              tags$th("Wariancja naruszona", style = "width: 24%;"),
              tags$th("Normalność naruszona", style = "width: 24%;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Ilościowa ~ jakościowa (2 gr. niez.)"),
              tags$td("t-test Studenta"),
              tags$td("t-test Welcha"),
              tags$td("Mann-Whitney U")
            ),
            tags$tr(
              tags$td("Ilościowa ~ jakościowa (2 gr. zal.)"),
              tags$td("t-test par"),
              tags$td("—"),
              tags$td("Wilcoxon signed-rank")
            ),
            tags$tr(
              tags$td("Ilościowa ~ jakościowa (>2 grup)"),
              tags$td("ANOVA"),
              tags$td("ANOVA Welcha"),
              tags$td("Kruskal-Wallis")
            ),
            tags$tr(
              tags$td("Ilościowa ~ ilościowa"),
              tags$td("Pearson r"),
              tags$td("—"),
              tags$td("Spearman rho")
            ),
            tags$tr(
              tags$td("Jakościowa ~ jakościowa"),
              tags$td("—"),
              tags$td("—"),
              tags$td("Chi-kwadrat / Fisher")
            )
          )
        ),

        # Sekcja: Regresja
        h3("Regresja", style = "color: #2c3e50; border-bottom: 2px solid #9b59b6; padding-bottom: 10px;"),

        tags$table(
          class = "table table-bordered table-striped",
          style = "width: 100%; margin-bottom: 30px; font-size: 14px;",
          tags$thead(
            style = "background-color: #9b59b6; color: white;",
            tags$tr(
              tags$th("Problem", style = "width: 28%;"),
              tags$th("Oba założenia OK", style = "width: 24%;"),
              tags$th("Wariancja naruszona", style = "width: 24%;"),
              tags$th("Normalność naruszona", style = "width: 24%;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Regresja (1+ predyktorów)"),
              tags$td("Regresja klasyczna"),
              tags$td("Robust SE (HC)"),
              tags$td("Regresja kwantylowa")
            )
          )
        ),

        # Sekcja: Kiedy które założenie jest naruszone
        h3("Kiedy które założenie jest naruszone?", style = "color: #2c3e50; border-bottom: 2px solid #e74c3c; padding-bottom: 10px;"),

        fluidRow(
          column(6,
            div(
              style = "background-color: #fdf2e9; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
              h4("Normalność naruszona", style = "color: #e74c3c; margin-top: 0;"),
              tags$ul(
                style = "font-size: 14px; margin: 0;",
                tags$li(tags$strong("Silna skośność"), " rozkładu"),
                tags$li(tags$strong("Outliery"), ", których nie można usunąć"),
                tags$li(tags$strong("Dane porządkowe"), " (np. skala Likerta)"),
                tags$li(tags$strong("Mała próba (n < 30)"), " bez normalności")
              )
            )
          ),
          column(6,
            div(
              style = "background-color: #ebf5fb; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
              h4("Wariancja naruszona", style = "color: #3498db; margin-top: 0;"),
              tags$ul(
                style = "font-size: 14px; margin: 0;",
                tags$li(tags$strong("Nierówne SD"), " między grupami"),
                tags$li(tags$strong("Nierówne n"), " w grupach"),
                tags$li(tags$strong("Test Levene'a"), " istotny (p < 0.05)")
              )
            )
          )
        )
      )
    ),

    # ========================================================================
    # TAB 4: REGRESJA
    # ========================================================================
    tabPanel(
      "📉 Założenia regresji",
      br(),
      tabsetPanel(
        # ====================================================================
        # SUB-TAB 4a: Normalność reszt
        # ====================================================================
        tabPanel(
          "Normalność reszt",
          br(),

          # Nagłówek sekcji
          h3("Wizualizacja"),

          sidebarLayout(
            sidebarPanel(
              h4("Wybór scenariusza"),
              selectInput("reg_normal_scenario", "Typ problemu:",
                          choices = c(
                            "Liniowy związek (idealny)" = "normal",
                            "Nieliniowy związek" = "nonlinear",
                            "Reszty z outlierami" = "outliers"
                          ),
                          selected = "normal"),

              hr(),

              h4("Wyjaśnienie"),
              div(
                style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px;",
                p("Regresja liniowa zakłada liniowy związek między X i Y."),
                p("Reszty powinny być losowo rozrzucone wokół zera.")
              ),

              width = 3
            ),

            mainPanel(
              fluidRow(
                column(6,
                  div(
                    style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                    h4("Scatterplot z linią regresji"),
                    plotOutput("reg_normal_scatter", height = "280px")
                  )
                ),
                column(6,
                  div(
                    style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                    h4("Test Shapiro-Wilka (na resztach)"),
                    tableOutput("reg_normal_test")
                  )
                )
              ),

              fluidRow(
                column(6,
                  div(
                    style = "border: 2px solid #e67e22; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                    h4("Histogram reszt"),
                    plotOutput("reg_normal_hist", height = "250px")
                  )
                ),
                column(6,
                  div(
                    style = "border: 2px solid #9b59b6; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                    h4("QQ-plot reszt"),
                    plotOutput("reg_normal_qq", height = "250px")
                  )
                )
              ),

              width = 9
            )
          ),

          hr(),

          # Sekcja: Wpływ na wyniki
          h3("Wpływ na wyniki"),

          div(class = "interpretation-box",
            h4("Czy to założenie jest krytyczne?"),
            p("Normalność reszt jest ", strong("mniej krytyczna"), " niż inne założenia regresji."),

            fluidRow(
              column(6,
                div(style = "background-color: #d4edda; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5("Kiedy można zignorować?", style = "color: #155724; margin-top: 0;"),
                  tags$ul(
                    tags$li(strong("Duża próba (n > 50):"), " Centralne Twierdzenie Graniczne sprawia, że estymaty są asymptotycznie normalne"),
                    tags$li(strong("Interesuje nas tylko slope/intercept:"), " Estymaty są nieobciążone niezależnie od rozkładu reszt"),
                    tags$li(strong("Umiarkowane odchylenia:"), " Lekka skośność nie jest problemem")
                  )
                )
              ),
              column(6,
                div(style = "background-color: #f8d7da; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h5("Kiedy to ważne?", style = "color: #721c24; margin-top: 0;"),
                  tags$ul(
                    tags$li(strong("Mała próba (n < 30):"), " CLT nie działa, przedziały ufności mogą być błędne"),
                    tags$li(strong("Predykcja dla pojedynczych przypadków:"), " Przedziały predykcji zakładają normalność"),
                    tags$li(strong("Silne outliery:"), " Mogą zniekształcać estymaty (problem dźwigni)")
                  )
                )
              )
            ),

            p(strong("Wniosek:"), " Sprawdzaj normalność reszt, ale nie panikuj. ",
              "Heteroskedastyczność i outliery z dużą dźwignią są większym problemem.")
          )
        ),

        # ====================================================================
        # SUB-TAB 4b: Homoskedastyczność
        # ====================================================================
        tabPanel(
          "Homoskedastyczność",
          br(),

          # Nagłówek sekcji
          h3("Wizualizacja"),

          sidebarLayout(
            sidebarPanel(
              h4("Wybór scenariusza"),
              selectInput("reg_homo_scenario", "Typ wariancji reszt:",
                          choices = c(
                            "Stała wariancja (idealny)" = "homoscedastic",
                            "Rozrzut rośnie z X" = "increasing",
                            "Rozrzut maleje z X" = "decreasing"
                          ),
                          selected = "homoscedastic"),

              hr(),

              h4("Wyjaśnienie"),
              div(
                style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px;",
                p("Residual plot powinien pokazywać chmurę punktów wokół y=0 z równym rozrzutem."),
                p("Kształt lejka = naruszenie założenia!")
              ),

              width = 3
            ),

            mainPanel(
              fluidRow(
                column(6,
                  div(
                    style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                    h4("Scatterplot z linią regresji"),
                    plotOutput("reg_homo_scatter", height = "280px")
                  )
                ),
                column(6,
                  div(
                    style = "border: 2px solid #e67e22; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                    h4("Residual plot (kluczowy!)"),
                    plotOutput("reg_homo_residual", height = "280px")
                  )
                )
              ),

              div(
                style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
                h4("Test Breusch-Pagan"),
                tableOutput("reg_homo_test")
              ),

              width = 9
            )
          ),

          hr(),

          # Sekcja: Wpływ na wyniki
          h3("Wpływ na wyniki"),

          p("OLS zakłada ", strong("stałą wariancję"), " i oblicza 'średni' błąd standardowy. ",
            "95% przedział predykcji (PI) ma więc ", strong("stałą szerokość"), " dla wszystkich wartości X."),
          p("Punkty ", span(style = "color: #e74c3c;", "czerwone"), " = poza 95% PI (powinno być ~5%)"),

          fluidRow(
            column(6,
              div(class = "result-box-success",
                h4("Homoskedastyczność (stała wariancja)"),
                plotOutput("reg_homo_ci_ok_plot", height = "280px"),
                br(),
                p("Punkty poza PI: ", strong("~5%"), " (zgodne z oczekiwaniem)"),
                p(style = "color: #28a745;", "PI jest prawidłowy na całej długości!")
              )
            ),
            column(6,
              div(class = "result-box-danger",
                h4("Heteroskedastyczność (rosnąca wariancja)"),
                plotOutput("reg_homo_ci_bad_plot", height = "280px"),
                br(),
                p("Punkty poza PI: ", strong("więcej z prawej strony!")),
                p(style = "color: #dc3545;", "PI zakłada stałą wariancję - z prawej jest za wąski!")
              )
            )
          ),

          br(),

          h4("Symulacja Monte Carlo: Pokrycie 95% CI"),
          p("10 000 powtórzeń symulacji - jak często CI zawiera prawdziwą wartość slope?"),
          plotOutput("reg_homo_consequence_comparison", height = "280px"),

          div(class = "interpretation-box",
            h4("Wniosek"),
            p("Heteroskedastyczność nie wpływa na ", strong("estymaty współczynników"), " (slope, intercept)."),
            p("Wpływa na ", strong("błędy standardowe"), " - są nieprawidłowe, co prowadzi do:"),
            tags$ul(
              tags$li("Za wąskich przedziałów ufności (szczególnie tam gdzie wariancja duża)"),
              tags$li("Zbyt małych p-value (nadmiar istotnych wyników)"),
              tags$li("Błędnych wniosków o istotności statystycznej")
            ),
            p(strong("Rozwiązanie: "), "Użyj ", strong("Robust Standard Errors"), " (pakiet sandwich w R)."),
            p(style = "font-style: italic;",
              "Kod: library(sandwich); coeftest(model, vcov = vcovHC(model, type = 'HC3'))")
          )
        ),

        # ====================================================================
        # SUB-TAB 4c: Outliery i punkty wpływowe
        # ====================================================================
        tabPanel(
          "Outliery i dźwignia",
          br(),

          # Nagłówek sekcji
          h3("Wizualizacja"),

          sidebarLayout(
            sidebarPanel(
              h4("Wybór scenariusza"),
              selectInput("reg_outlier_scenario", "Typ outlierów:",
                          choices = c(
                            "Bez outlierów" = "no_outliers",
                            "Outlier w Y (centrum X)" = "outlier_y",
                            "Outlier w X i Y (skraj)" = "outlier_xy",
                            "Kilka outlierów" = "multiple"
                          ),
                          selected = "no_outliers"),

              hr(),

              h4("Interpretacja"),
              div(
                style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px;",
                textOutput("reg_outlier_interpretation")
              ),

              width = 3
            ),

            mainPanel(
              div(
                style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                h4("Scatterplot z 2 liniami regresji"),
                plotOutput("reg_outlier_scatter", height = "350px"),
                div(
                  style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-top: 10px;",
                  p("🔵 Niebieska linia: z outlierami | 🔴 Czerwona linia (przerywana): bez outlierów")
                )
              ),

              div(
                style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
                h4("Porównanie modeli"),
                tableOutput("reg_outlier_comparison")
              ),

              width = 9
            )
          ),

          hr(),

          # Sekcja: Wpływ na wyniki - Dźwignia
          h3("Wpływ na wyniki: Dźwignia (Leverage)"),

          p("Nie wszystkie punkty mają ", strong("równy wpływ"), " na linię regresji. ",
            "Pozycja punktu w X determinuje jego ", strong("dźwignię"), "."),

          fluidRow(
            column(6,
              div(class = "result-box-success",
                h4("Outlier blisko centrum X"),
                plotOutput("leverage_low_plot", height = "220px"),
                br(),
                p("Punkt w centrum ma ", strong("małą dźwignię")),
                p("Nawet jeśli jest 'dziwny' w Y, nie zmieni bardzo linii")
              )
            ),
            column(6,
              div(class = "result-box-danger",
                h4("Outlier daleko od centrum X"),
                plotOutput("leverage_high_plot", height = "220px"),
                br(),
                p("Punkt na skraju ma ", strong("dużą dźwignię")),
                p("Jak dźwignia - mały ruch na końcu = duży efekt")
              )
            )
          ),

          br(),

          h4("Demonstracja: Jak pozycja outliera zmienia slope"),
          plotOutput("leverage_demo_plot", height = "300px"),

          div(class = "result-box-warning",
            h4("Cook's Distance - miara wpływu"),
            p("Cook's D łączy dźwignię (pozycja X) z wielkością reszty (odchylenie Y):"),
            p(style = "font-family: monospace; text-align: center; font-size: 16px;",
              "Wpływ = Dźwignia × Reszta²"),
            p("Punkt z wysokim Cook's D (>1) powinien być zbadany - może dominować analizę.")
          ),

          div(class = "interpretation-box",
            h4("Wniosek"),
            p("Nie wszystkie outliery są równie szkodliwe:"),
            tags$ul(
              tags$li(strong("Outlier w Y blisko centrum X"), " - zaburza intercept, mniej slope"),
              tags$li(strong("Outlier w Y na skraju X"), " - może całkowicie zmienić kierunek linii!"),
              tags$li(strong("Outlier tylko w X"), " - ma dużą dźwignię, ale jeśli pasuje do trendu, nie szkodzi")
            ),
            p("Zawsze sprawdzaj ", strong("diagnostykę wpływu"), " (Cook's D, leverage plots) po dopasowaniu regresji."),
            p(style = "font-style: italic;",
              "Kod R: plot(model, which = 4)  # Cook's distance plot")
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # MODUŁ 1: NORMALNOŚĆ
  # ==========================================================================

  normality_data <- reactiveVal(get_normal_data())

  observeEvent(input$normality_scenario, {
    data <- switch(input$normality_scenario,
                   "normal" = get_normal_data(),
                   "slightly_skewed" = get_slightly_skewed_data(),
                   "highly_skewed" = get_highly_skewed_data(),
                   "bimodal" = get_bimodal_data(),
                   "outliers" = get_outliers_data())
    normality_data(data)
  })

  output$normality_histogram <- renderPlot({
    data <- normality_data()
    df <- data.frame(value = data)

    p <- ggplot(df, aes(x = value)) +
      geom_histogram(bins = 20, fill = "#3498db", alpha = 0.7, color = "#2c3e50") +
      theme_minimal(base_size = 14) +
      labs(x = "Wartość", y = "Liczba obserwacji")

    if (input$normality_show_normal) {
      mean_val <- mean(data)
      sd_val <- sd(data)
      x_seq <- seq(min(data), max(data), length.out = 200)
      y_seq <- dnorm(x_seq, mean = mean_val, sd = sd_val)
      bin_width <- (max(data) - min(data)) / 20
      y_scaled <- y_seq * length(data) * bin_width

      norm_df <- data.frame(x = x_seq, y = y_scaled)
      p <- p + geom_line(data = norm_df, aes(x = x, y = y), color = "#e74c3c", linewidth = 1.5)
    }

    p
  })

  output$normality_qqplot <- renderPlot({
    data <- normality_data()
    df <- data.frame(value = data)

    ggplot(df, aes(sample = value)) +
      stat_qq(color = "#3498db", size = 3, alpha = 0.7) +
      stat_qq_line(color = "#e74c3c", linewidth = 1.5) +
      theme_minimal(base_size = 14) +
      labs(x = "Teoretyczne kwantyle", y = "Kwantyle próby")
  })

  output$normality_test <- renderTable({
    data <- normality_data()
    test_result <- shapiro_test(data.frame(value = data), value)

    data.frame(
      Test = "Shapiro-Wilk",
      Statystyka = round(test_result$statistic, 4),
      `p-value` = format.pval(test_result$p, digits = 3),
      Interpretacja = ifelse(test_result$p > 0.05,
                              "Brak podstaw do odrzucenia normalności (p > 0.05)",
                              "Rozkład różni się od normalnego (p ≤ 0.05)"),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  output$normality_interpretation <- renderText({
    scenario <- input$normality_scenario

    switch(scenario,
           "normal" = "Idealny przypadek - rozkład normalny. Testy parametryczne (t-test, ANOVA) działają optymalnie.",
           "slightly_skewed" = "Lekka skośność - testy parametryczne zazwyczaj są odporne na małe naruszenia normalności.",
           "highly_skewed" = "Silna skośność - testy parametryczne mogą dawać błędne wyniki. Rozważ testy nieparametryczne.",
           "bimodal" = "Rozkład bimodalny - całkowite naruszenie normalności. Użyj testów nieparametrycznych lub podziel dane na grupy.",
           "outliers" = "Outliery mogą zaburzać normalność. Rozważ ich usunięcie lub testy odporne (nieparametryczne).")
  })

  # ==========================================================================
  # MODUŁ 2: JEDNORODNOŚĆ WARIANCJI
  # ==========================================================================

  # Reactive dla danych wizualizacji
  variance_data <- reactive({
    n_groups <- as.numeric(input$variance_n_groups)
    equal_var <- input$variance_scenario == "equal"
    get_variance_data(n_groups, equal_var)
  })

  output$variance_boxplot <- renderPlot({
    data <- variance_data()

    ggplot(data, aes(x = group, y = value, fill = group)) +
      geom_boxplot(alpha = 0.7, width = 0.7) +
      scale_fill_manual(values = c("#3498db", "#e74c3c", "#27ae60")) +
      theme_minimal(base_size = 12) +
      labs(x = "Grupa", y = "Wartość") +
      theme(legend.position = "none")
  })

  output$variance_stats <- renderTable({
    data <- variance_data()

    data %>%
      group_by(group) %>%
      summarise(
        n = n(),
        Średnia = round(mean(value), 2),
        SD = round(sd(value), 2),
        .groups = "drop"
      ) %>%
      rename(Grupa = group, N = n)
  }, striped = TRUE, bordered = TRUE)

  output$variance_test <- renderTable({
    data <- variance_data()
    test_result <- levene_test(data, value ~ group)

    data.frame(
      Test = "Levene",
      df1 = test_result$df1,
      df2 = test_result$df2,
      Statystyka = round(test_result$statistic, 3),
      `p-value` = format.pval(test_result$p, digits = 3),
      Interpretacja = ifelse(test_result$p > 0.05,
                              "Wariancje jednorodne (p > 0.05)",
                              "Wariancje różnią się (p ≤ 0.05)"),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  output$variance_interpretation <- renderText({
    scenario <- input$variance_scenario
    n_groups <- input$variance_n_groups

    if (scenario == "equal") {
      "Idealny przypadek - równe wariancje. Testy parametryczne (t-test, ANOVA) działają poprawnie."
    } else {
      "Różne wariancje między grupami. Student's t / klasyczna ANOVA mogą dawać błędne wyniki. Użyj Welch's."
    }
  })

  # ==========================================================================
  # MODUŁ 2: WPŁYW NA TEST - porównanie grup
  # ==========================================================================

  # Boxplot - równe wariancje
  output$var_test_boxplot_equal <- renderPlot({
    n_groups <- as.numeric(input$var_test_n_groups)
    data <- get_variance_data(n_groups, equal_var = TRUE)

    ggplot(data, aes(x = group, y = value, fill = group)) +
      geom_boxplot(alpha = 0.7, width = 0.7) +
      scale_fill_manual(values = c("#3498db", "#27ae60", "#9b59b6")) +
      theme_minimal(base_size = 11) +
      labs(x = "", y = "Wartość") +
      theme(legend.position = "none")
  })

  # Boxplot - różne wariancje
  output$var_test_boxplot_unequal <- renderPlot({
    n_groups <- as.numeric(input$var_test_n_groups)
    data <- get_variance_data(n_groups, equal_var = FALSE)

    ggplot(data, aes(x = group, y = value, fill = group)) +
      geom_boxplot(alpha = 0.7, width = 0.7) +
      scale_fill_manual(values = c("#e74c3c", "#f39c12", "#e67e22")) +
      theme_minimal(base_size = 11) +
      labs(x = "", y = "Wartość") +
      theme(legend.position = "none")
  })

  # Statystyki - równe wariancje
  output$var_test_stats_equal <- renderTable({
    n_groups <- as.numeric(input$var_test_n_groups)
    data <- get_variance_data(n_groups, equal_var = TRUE)

    data %>%
      group_by(group) %>%
      summarise(
        N = n(),
        Średnia = round(mean(value), 1),
        SD = round(sd(value), 1),
        .groups = "drop"
      ) %>%
      rename(Grupa = group)
  }, striped = TRUE, bordered = TRUE, width = "100%")

  # Statystyki - różne wariancje
  output$var_test_stats_unequal <- renderTable({
    n_groups <- as.numeric(input$var_test_n_groups)
    data <- get_variance_data(n_groups, equal_var = FALSE)

    data %>%
      group_by(group) %>%
      summarise(
        N = n(),
        Średnia = round(mean(value), 1),
        SD = round(sd(value), 1),
        .groups = "drop"
      ) %>%
      rename(Grupa = group)
  }, striped = TRUE, bordered = TRUE, width = "100%")

  # Wyniki testów - równe wariancje
  output$var_test_results_equal <- renderTable({
    n_groups <- as.numeric(input$var_test_n_groups)
    data <- get_variance_data(n_groups, equal_var = TRUE)

    if (n_groups == 2) {
      # t-testy
      student_t <- t.test(value ~ group, data = data, var.equal = TRUE)
      welch_t <- t.test(value ~ group, data = data, var.equal = FALSE)

      data.frame(
        Test = c("Student's t", "Welch's t"),
        Statystyka = c(round(student_t$statistic, 2), round(welch_t$statistic, 2)),
        `p-value` = c(format.pval(student_t$p.value, digits = 3),
                      format.pval(welch_t$p.value, digits = 3)),
        check.names = FALSE
      )
    } else {
      # ANOVA
      classic_anova <- summary(aov(value ~ group, data = data))[[1]]
      welch_anova <- oneway.test(value ~ group, data = data, var.equal = FALSE)

      data.frame(
        Test = c("Klasyczna ANOVA", "Welch's ANOVA"),
        Statystyka = c(round(classic_anova$`F value`[1], 2), round(welch_anova$statistic, 2)),
        `p-value` = c(format.pval(classic_anova$`Pr(>F)`[1], digits = 3),
                      format.pval(welch_anova$p.value, digits = 3)),
        check.names = FALSE
      )
    }
  }, striped = TRUE, bordered = TRUE, width = "100%")

  # Wyniki testów - różne wariancje
  output$var_test_results_unequal <- renderTable({
    n_groups <- as.numeric(input$var_test_n_groups)
    data <- get_variance_data(n_groups, equal_var = FALSE)

    if (n_groups == 2) {
      # t-testy
      student_t <- t.test(value ~ group, data = data, var.equal = TRUE)
      welch_t <- t.test(value ~ group, data = data, var.equal = FALSE)

      data.frame(
        Test = c("Student's t", "Welch's t"),
        Statystyka = c(round(student_t$statistic, 2), round(welch_t$statistic, 2)),
        `p-value` = c(format.pval(student_t$p.value, digits = 3),
                      format.pval(welch_t$p.value, digits = 3)),
        check.names = FALSE
      )
    } else {
      # ANOVA
      classic_anova <- summary(aov(value ~ group, data = data))[[1]]
      welch_anova <- oneway.test(value ~ group, data = data, var.equal = FALSE)

      data.frame(
        Test = c("Klasyczna ANOVA", "Welch's ANOVA"),
        Statystyka = c(round(classic_anova$`F value`[1], 2), round(welch_anova$statistic, 2)),
        `p-value` = c(format.pval(classic_anova$`Pr(>F)`[1], digits = 3),
                      format.pval(welch_anova$p.value, digits = 3)),
        check.names = FALSE
      )
    }
  }, striped = TRUE, bordered = TRUE, width = "100%")

  # Interpretacja dla nierównych wariancji
  output$var_test_interpretation_unequal <- renderUI({
    n_groups <- as.numeric(input$var_test_n_groups)
    data <- get_variance_data(n_groups, equal_var = FALSE)

    if (n_groups == 2) {
      student_t <- t.test(value ~ group, data = data, var.equal = TRUE)
      welch_t <- t.test(value ~ group, data = data, var.equal = FALSE)

      if (abs(student_t$p.value - welch_t$p.value) > 0.01) {
        p("Zauważ różnicę w p-value! Student's t nie uwzględnia różnic w wariancjach, ",
          "co może prowadzić do błędnych wniosków.")
      } else {
        p("W tym przypadku różnice są niewielkie, ale przy większych dysproporcjach ",
          "w wariancjach rozbieżności będą znaczące.")
      }
    } else {
      classic_anova <- summary(aov(value ~ group, data = data))[[1]]
      welch_anova <- oneway.test(value ~ group, data = data, var.equal = FALSE)

      if (abs(classic_anova$`Pr(>F)`[1] - welch_anova$p.value) > 0.01) {
        p("Zauważ różnicę w p-value między testami! Klasyczna ANOVA zakłada równe wariancje, ",
          "Welch's ANOVA jest odporna na ich nierówność.")
      } else {
        p("W tym przypadku różnice są niewielkie, ale przy większych dysproporcjach ",
          "w wariancjach rozbieżności będą znaczące.")
      }
    }
  })

  # ==========================================================================
  # MODUŁ 4a: Normalność reszt
  # ==========================================================================

  reg_normal_data <- reactive({
    switch(input$reg_normal_scenario,
           "normal" = REG_NORMAL_DATA,
           "nonlinear" = REG_NONLINEAR_DATA,
           "outliers" = REG_OUTLIERS_RESIDUALS_DATA)
  })

  output$reg_normal_scatter <- renderPlot({
    data <- reg_normal_data()

    ggplot(data, aes(x = x, y = y)) +
      geom_point(size = 3, alpha = 0.6, color = "#3498db") +
      geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linewidth = 1.5) +
      theme_minimal(base_size = 14) +
      labs(x = "X", y = "Y", title = "Scatterplot z linią regresji")
  })

  output$reg_normal_hist <- renderPlot({
    data <- reg_normal_data()
    model <- lm(y ~ x, data = data)
    residuals <- residuals(model)
    df <- data.frame(residuals = residuals)

    p <- ggplot(df, aes(x = residuals)) +
      geom_histogram(bins = 20, fill = "#3498db", alpha = 0.7, color = "#2c3e50") +
      theme_minimal(base_size = 14) +
      labs(x = "Reszty", y = "Liczba obserwacji")

    mean_res <- mean(residuals)
    sd_res <- sd(residuals)
    x_seq <- seq(min(residuals), max(residuals), length.out = 200)
    y_seq <- dnorm(x_seq, mean = mean_res, sd = sd_res)
    bin_width <- (max(residuals) - min(residuals)) / 20
    y_scaled <- y_seq * length(residuals) * bin_width

    norm_df <- data.frame(x = x_seq, y = y_scaled)
    p <- p + geom_line(data = norm_df, aes(x = x, y = y), color = "#e74c3c", linewidth = 1.5)

    p
  })

  output$reg_normal_qq <- renderPlot({
    data <- reg_normal_data()
    model <- lm(y ~ x, data = data)
    residuals <- residuals(model)
    df <- data.frame(residuals = residuals)

    ggplot(df, aes(sample = residuals)) +
      stat_qq(color = "#3498db", size = 3, alpha = 0.7) +
      stat_qq_line(color = "#e74c3c", linewidth = 1.5) +
      theme_minimal(base_size = 14) +
      labs(x = "Teoretyczne kwantyle", y = "Kwantyle reszt")
  })

  output$reg_normal_test <- renderTable({
    data <- reg_normal_data()
    model <- lm(y ~ x, data = data)
    residuals <- residuals(model)

    test_result <- shapiro.test(residuals)

    data.frame(
      Test = "Shapiro-Wilk",
      Statystyka = round(test_result$statistic, 4),
      `p-value` = format.pval(test_result$p.value, digits = 3),
      Interpretacja = ifelse(test_result$p.value > 0.05,
                              "Reszty normalne (p > 0.05)",
                              "Reszty różnią się od normalnych (p ≤ 0.05)"),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  # ==========================================================================
  # MODUŁ 4b: Homoskedastyczność
  # ==========================================================================

  reg_homo_data <- reactive({
    switch(input$reg_homo_scenario,
           "homoscedastic" = REG_HOMOSCEDASTIC_DATA,
           "increasing" = REG_HETERO_INCREASING_DATA,
           "decreasing" = REG_HETERO_DECREASING_DATA)
  })

  output$reg_homo_scatter <- renderPlot({
    data <- reg_homo_data()

    ggplot(data, aes(x = x, y = y)) +
      geom_point(size = 3, alpha = 0.6, color = "#3498db") +
      geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linewidth = 1.5) +
      theme_minimal(base_size = 14) +
      labs(x = "X", y = "Y", title = "Scatterplot z linią regresji")
  })

  output$reg_homo_residual <- renderPlot({
    data <- reg_homo_data()
    model <- lm(y ~ x, data = data)
    fitted_vals <- fitted(model)
    residuals <- residuals(model)
    df <- data.frame(fitted = fitted_vals, residuals = residuals)

    ggplot(df, aes(x = fitted, y = residuals)) +
      geom_point(size = 3, alpha = 0.6, color = "#3498db") +
      geom_hline(yintercept = 0, color = "#e74c3c", linewidth = 1.5, linetype = "dashed") +
      theme_minimal(base_size = 14) +
      labs(x = "Fitted values", y = "Residuals", title = "Residual Plot")
  })

  output$reg_homo_test <- renderTable({
    data <- reg_homo_data()
    model <- lm(y ~ x, data = data)

    bp_test <- bptest(model)

    data.frame(
      Test = "Breusch-Pagan",
      Statystyka = round(bp_test$statistic, 3),
      `p-value` = format.pval(bp_test$p.value, digits = 3),
      Interpretacja = ifelse(bp_test$p.value > 0.05,
                              "Homoskedastyczność (p > 0.05)",
                              "Heteroskedastyczność (p ≤ 0.05)"),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  # ==========================================================================
  # MODUŁ 4c: Wpływ outlierów
  # ==========================================================================

  reg_outlier_data <- reactive({
    switch(input$reg_outlier_scenario,
           "no_outliers" = REG_NO_OUTLIERS_DATA,
           "outlier_y" = REG_OUTLIER_Y_DATA,
           "outlier_xy" = REG_OUTLIER_XY_DATA,
           "multiple" = REG_MULTIPLE_OUTLIERS_DATA)
  })

  output$reg_outlier_scatter <- renderPlot({
    data <- reg_outlier_data()

    # Model z outlierami
    model_with <- lm(y ~ x, data = data)

    # Model bez outlierów
    data_without <- data[!data$is_outlier, ]
    model_without <- lm(y ~ x, data = data_without)

    p <- ggplot(data, aes(x = x, y = y)) +
      geom_point(aes(color = is_outlier, size = is_outlier), alpha = 0.6) +
      scale_color_manual(values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c")) +
      scale_size_manual(values = c("FALSE" = 3, "TRUE" = 5)) +
      geom_smooth(method = "lm", se = FALSE, color = "#3498db", linewidth = 1.5, fullrange = TRUE) +
      geom_smooth(data = data_without, method = "lm", se = FALSE,
                  color = "#e74c3c", linewidth = 1.5, linetype = "dashed", fullrange = TRUE) +
      theme_minimal(base_size = 14) +
      labs(x = "X", y = "Y", title = "Scatterplot z 2 liniami regresji") +
      theme(legend.position = "none")

    p
  })

  output$reg_outlier_comparison <- renderTable({
    data <- reg_outlier_data()

    # Model z outlierami
    model_with <- lm(y ~ x, data = data)
    tidy_with <- tidy(model_with)
    glance_with <- glance(model_with)

    # Model bez outlierów
    data_without <- data[!data$is_outlier, ]
    model_without <- lm(y ~ x, data = data_without)
    tidy_without <- tidy(model_without)
    glance_without <- glance(model_without)

    data.frame(
      Model = c("Z outlierami", "Bez outlierów"),
      `R²` = c(round(glance_with$r.squared, 3), round(glance_without$r.squared, 3)),
      Slope = c(round(tidy_with$estimate[2], 2), round(tidy_without$estimate[2], 2)),
      Intercept = c(round(tidy_with$estimate[1], 2), round(tidy_without$estimate[1], 2)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE)

  output$reg_outlier_interpretation <- renderText({
    scenario <- input$reg_outlier_scenario

    switch(scenario,
           "no_outliers" = "Baseline - brak outlierów, obie linie są identyczne.",
           "outlier_y" = "Outlier w Y - wpływa na intercept, ale mniej na slope.",
           "outlier_xy" = "Outlier w X i Y (high influence) - silnie zmienia slope i intercept!",
           "multiple" = "Kilka outlierów - mogą drastycznie zmienić linię regresji.")
  })

  # ==========================================================================
  # WYKRESY KONSEKWENCJI - HOMOSKEDASTYCZNOŚĆ (CI z punktami w/poza)
  # ==========================================================================

  # Homoskedastyczność - prediction interval prawidłowy
  output$reg_homo_ci_ok_plot <- renderPlot({
    set.seed(42)
    n <- 200
    x <- runif(n, 10, 100)
    y <- 2 * x + 50 + rnorm(n, mean = 0, sd = 20)
    df <- data.frame(x = x, y = y)

    model <- lm(y ~ x, data = df)
    pred <- predict(model, interval = "prediction", level = 0.95)
    df$fit <- pred[, "fit"]
    df$lwr <- pred[, "lwr"]
    df$upr <- pred[, "upr"]
    df$outside <- df$y < df$lwr | df$y > df$upr

    pct_outside <- round(100 * mean(df$outside), 1)
    # Osobno dla lewej i prawej połowy
    pct_left <- round(100 * mean(df$outside[df$x < 55]), 1)
    pct_right <- round(100 * mean(df$outside[df$x >= 55]), 1)

    ggplot(df, aes(x = x, y = y)) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25, fill = "#27ae60") +
      geom_line(aes(y = fit), color = "#27ae60", linewidth = 1.5) +
      geom_point(aes(color = outside), size = 2, alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c"),
                         labels = c("W przedziale", "Poza przedziałem"), name = "") +
      theme_minimal(base_size = 12) +
      labs(x = "X", y = "Y",
           title = paste0("Stała wariancja: ", pct_outside, "% poza PI"),
           subtitle = paste0("Lewa połowa: ", pct_left, "% | Prawa połowa: ", pct_right, "%")) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            legend.position = "bottom")
  })

  # Heteroskedastyczność - prediction interval za wąski z prawej
  output$reg_homo_ci_bad_plot <- renderPlot({
    set.seed(42)
    n <- 200
    x <- runif(n, 10, 100)
    # Silniejsza heteroskedastyczność: sd rośnie od ~6 do ~60
    y <- 2 * x + 50 + rnorm(n, mean = 0, sd = x * 0.6)
    df <- data.frame(x = x, y = y)

    model <- lm(y ~ x, data = df)
    pred <- predict(model, interval = "prediction", level = 0.95)
    df$fit <- pred[, "fit"]
    df$lwr <- pred[, "lwr"]
    df$upr <- pred[, "upr"]
    df$outside <- df$y < df$lwr | df$y > df$upr

    pct_outside <- round(100 * mean(df$outside), 1)
    # Osobno dla lewej i prawej połowy
    pct_left <- round(100 * mean(df$outside[df$x < 55]), 1)
    pct_right <- round(100 * mean(df$outside[df$x >= 55]), 1)

    ggplot(df, aes(x = x, y = y)) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25, fill = "#e74c3c") +
      geom_line(aes(y = fit), color = "#e74c3c", linewidth = 1.5) +
      geom_point(aes(color = outside), size = 2, alpha = 0.7) +
      scale_color_manual(values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c"),
                         labels = c("W przedziale", "Poza przedziałem"), name = "") +
      theme_minimal(base_size = 12) +
      labs(x = "X", y = "Y",
           title = paste0("Rosnąca wariancja: ", pct_outside, "% poza PI"),
           subtitle = paste0("Lewa połowa: ", pct_left, "% | Prawa połowa: ", pct_right, "%")) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            legend.position = "bottom")
  })

  # Wykres porównawczy pokrycia CI
  output$reg_homo_consequence_comparison <- renderPlot({
    ggplot(precomputed_regression, aes(x = heterosked, y = pokrycie_CI * 100, fill = metoda)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
      geom_hline(yintercept = 95, linetype = "dashed", color = "#e74c3c", linewidth = 1.2) +
      annotate("text", x = 0.5, y = 96, label = "Oczekiwane 95%", hjust = 0, color = "#e74c3c", size = 4) +
      scale_fill_manual(values = c("OLS (zwykły)" = "#3498db", "Robust SE" = "#27ae60")) +
      theme_minimal(base_size = 14) +
      labs(x = "Poziom heteroskedastyczności", y = "Pokrycie 95% CI (%)", fill = "Metoda",
           title = "Jak często 95% CI zawiera prawdziwą wartość?") +
      theme(legend.position = "bottom") +
      coord_cartesian(ylim = c(80, 100))
  })

  # ==========================================================================
  # WYKRESY "WPŁYW NA TEST T" - NORMALNOŚĆ
  # ==========================================================================

  # Histogram - rozkład normalny
  output$norm_base_hist_ok <- renderPlot({
    df <- data.frame(value = DEMO_NORMAL_DATA)
    mean_val <- mean(df$value)
    median_val <- median(df$value)

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 12, fill = "#3498db", alpha = 0.6, color = "#2980b9") +
      geom_vline(xintercept = mean_val, color = "#e74c3c", linewidth = 1.5, linetype = "solid") +
      geom_vline(xintercept = median_val, color = "#27ae60", linewidth = 1.5, linetype = "dashed") +
      geom_vline(xintercept = 50, color = "#9b59b6", linewidth = 1, linetype = "dotted") +
      theme_minimal(base_size = 11) +
      labs(x = "Wartość", y = "Liczba",
           caption = "Czerwona = średnia, Zielona = mediana, Fioletowa = μ₀ = 50") +
      theme(plot.caption = element_text(size = 9))
  })

  # Średnia i mediana - normalny
  output$norm_base_mean_ok <- renderText({
    round(mean(DEMO_NORMAL_DATA), 1)
  })

  output$norm_base_median_ok <- renderText({
    round(median(DEMO_NORMAL_DATA), 1)
  })

  # Testy - rozkład normalny
  output$norm_base_tests_ok <- renderTable({
    t_result <- t.test(DEMO_NORMAL_DATA, mu = 50)
    w_result <- wilcox.test(DEMO_NORMAL_DATA, mu = 50)

    data.frame(
      Test = c("t-test", "Wilcoxon"),
      Statystyka = c(round(t_result$statistic, 2), round(w_result$statistic, 0)),
      `p-value` = c(format.pval(t_result$p.value, digits = 3),
                    format.pval(w_result$p.value, digits = 3)),
      Wniosek = c(
        ifelse(t_result$p.value < 0.05, "Odrzuć H0", "Brak podstaw do odrzucenia"),
        ifelse(w_result$p.value < 0.05, "Odrzuć H0", "Brak podstaw do odrzucenia")
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  # CI - rozkład normalny
  output$norm_base_ci_ok <- renderPlot({
    t_result <- t.test(DEMO_NORMAL_DATA, mu = 50)
    ci <- t_result$conf.int
    mean_val <- mean(DEMO_NORMAL_DATA)

    df <- data.frame(
      mean = mean_val,
      lower = ci[1],
      upper = ci[2]
    )

    ggplot(df, aes(x = mean, y = 1)) +
      geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, linewidth = 1.2, color = "#3498db", orientation = "y") +
      geom_point(size = 4, color = "#3498db") +
      geom_vline(xintercept = 50, color = "#9b59b6", linewidth = 1.5, linetype = "dashed") +
      theme_minimal(base_size = 11) +
      labs(x = "", y = "") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      xlim(min(ci[1] - 2, 45), max(ci[2] + 2, 55)) +
      annotate("text", x = 50, y = 1.3, label = "μ₀ = 50", color = "#9b59b6", size = 3.5)
  })

  # Histogram - rozkład skośny
  output$norm_base_hist_bad <- renderPlot({
    df <- data.frame(value = DEMO_SKEWED_DATA)
    mean_val <- mean(df$value)
    median_val <- median(df$value)

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 12, fill = "#e74c3c", alpha = 0.6, color = "#c0392b") +
      geom_vline(xintercept = mean_val, color = "#e74c3c", linewidth = 1.5, linetype = "solid") +
      geom_vline(xintercept = median_val, color = "#27ae60", linewidth = 1.5, linetype = "dashed") +
      geom_vline(xintercept = 25, color = "#9b59b6", linewidth = 1, linetype = "dotted") +
      theme_minimal(base_size = 11) +
      labs(x = "Wartość", y = "Liczba",
           caption = "Czerwona = średnia, Zielona = mediana, Fioletowa = μ₀ = 25") +
      theme(plot.caption = element_text(size = 9))
  })

  # Średnia i mediana - skośny
  output$norm_base_mean_bad <- renderText({
    round(mean(DEMO_SKEWED_DATA), 1)
  })

  output$norm_base_median_bad <- renderText({
    round(median(DEMO_SKEWED_DATA), 1)
  })

  # Testy - rozkład skośny
  output$norm_base_tests_bad <- renderTable({
    t_result <- t.test(DEMO_SKEWED_DATA, mu = 25)
    w_result <- wilcox.test(DEMO_SKEWED_DATA, mu = 25)

    data.frame(
      Test = c("t-test", "Wilcoxon"),
      Statystyka = c(round(t_result$statistic, 2), round(w_result$statistic, 0)),
      `p-value` = c(format.pval(t_result$p.value, digits = 3),
                    format.pval(w_result$p.value, digits = 3)),
      Wniosek = c(
        ifelse(t_result$p.value < 0.05, "Odrzuć H0", "Brak podstaw do odrzucenia"),
        ifelse(w_result$p.value < 0.05, "Odrzuć H0", "Brak podstaw do odrzucenia")
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")

  # CI - rozkład skośny
  output$norm_base_ci_bad <- renderPlot({
    t_result <- t.test(DEMO_SKEWED_DATA, mu = 25)
    ci <- t_result$conf.int
    mean_val <- mean(DEMO_SKEWED_DATA)

    df <- data.frame(
      mean = mean_val,
      lower = ci[1],
      upper = ci[2]
    )

    ggplot(df, aes(x = mean, y = 1)) +
      geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.3, linewidth = 1.2, color = "#e74c3c", orientation = "y") +
      geom_point(size = 4, color = "#e74c3c") +
      geom_vline(xintercept = 25, color = "#9b59b6", linewidth = 1.5, linetype = "dashed") +
      theme_minimal(base_size = 11) +
      labs(x = "", y = "") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
      xlim(min(ci[1] - 2, 20), max(ci[2] + 2, 40)) +
      annotate("text", x = 25, y = 1.3, label = "μ₀ = 25", color = "#9b59b6", size = 3.5)
  })

  # ==========================================================================
  # WYKRESY "PROBLEM U PODSTAWY" - DŹWIGNIA (LEVERAGE)
  # ==========================================================================

  # Niski leverage
  output$leverage_low_plot <- renderPlot({
    set.seed(42)
    x <- c(runif(30, 30, 70), 50)  # outlier w centrum
    y <- c(2 * head(x, -1) + 50 + rnorm(30, 0, 10), 200)  # outlier w Y

    df <- data.frame(x = x, y = y, outlier = c(rep(FALSE, 30), TRUE))

    ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(color = outlier, size = outlier), alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "#3498db", linewidth = 1.2) +
      geom_smooth(data = df[!df$outlier, ], method = "lm", se = FALSE,
                  color = "#27ae60", linetype = "dashed", linewidth = 1.2) +
      scale_color_manual(values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c")) +
      scale_size_manual(values = c("FALSE" = 2, "TRUE" = 5)) +
      theme_minimal(base_size = 11) +
      labs(x = "X", y = "Y") +
      theme(legend.position = "none")
  })

  # Wysoki leverage
  output$leverage_high_plot <- renderPlot({
    set.seed(42)
    x <- c(runif(30, 30, 70), 95)  # outlier na skraju
    y <- c(2 * head(x, -1) + 50 + rnorm(30, 0, 10), 100)  # outlier w Y i X

    df <- data.frame(x = x, y = y, outlier = c(rep(FALSE, 30), TRUE))

    ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(color = outlier, size = outlier), alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "#3498db", linewidth = 1.2) +
      geom_smooth(data = df[!df$outlier, ], method = "lm", se = FALSE,
                  color = "#27ae60", linetype = "dashed", linewidth = 1.2) +
      scale_color_manual(values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c")) +
      scale_size_manual(values = c("FALSE" = 2, "TRUE" = 5)) +
      theme_minimal(base_size = 11) +
      labs(x = "X", y = "Y") +
      theme(legend.position = "none")
  })

  # Demo - jak outlier zmienia slope
  output$leverage_demo_plot <- renderPlot({
    set.seed(42)
    x_base <- runif(40, 20, 80)
    y_base <- 2 * x_base + 50 + rnorm(40, 0, 12)

    # 3 scenariusze outliera
    scenarios <- data.frame(
      x = c(x_base, 50, 50, 95),
      y = c(y_base, 200, 200, 100),
      scenario = c(rep("Bez outliera", 40),
                   "Outlier: X=50", "Outlier: X=50 (kopia)", "Outlier: X=95")
    )

    # Oblicz modele
    model_no <- lm(y ~ x, data = data.frame(x = x_base, y = y_base))
    model_center <- lm(y ~ x, data = data.frame(x = c(x_base, 50), y = c(y_base, 200)))
    model_edge <- lm(y ~ x, data = data.frame(x = c(x_base, 95), y = c(y_base, 100)))

    slopes <- data.frame(
      model = c("Bez outliera", "Outlier w centrum (X=50)", "Outlier na skraju (X=95)"),
      slope = c(coef(model_no)[2], coef(model_center)[2], coef(model_edge)[2]),
      intercept = c(coef(model_no)[1], coef(model_center)[1], coef(model_edge)[1])
    )

    df_base <- data.frame(x = x_base, y = y_base)

    ggplot(df_base, aes(x = x, y = y)) +
      geom_point(color = "#3498db", size = 3, alpha = 0.6) +
      # Outlier w centrum
      geom_point(aes(x = 50, y = 200), color = "#f39c12", size = 6, shape = 17) +
      # Outlier na skraju
      geom_point(aes(x = 95, y = 100), color = "#e74c3c", size = 6, shape = 17) +
      # Linie
      geom_abline(intercept = slopes$intercept[1], slope = slopes$slope[1],
                  color = "#27ae60", linewidth = 1.5, linetype = "solid") +
      geom_abline(intercept = slopes$intercept[2], slope = slopes$slope[2],
                  color = "#f39c12", linewidth = 1.2, linetype = "dashed") +
      geom_abline(intercept = slopes$intercept[3], slope = slopes$slope[3],
                  color = "#e74c3c", linewidth = 1.2, linetype = "dotted") +
      theme_minimal(base_size = 14) +
      labs(x = "X", y = "Y",
           title = paste0("Slope: bez=", round(slopes$slope[1], 2),
                         ", centrum=", round(slopes$slope[2], 2),
                         ", skraj=", round(slopes$slope[3], 2))) +
      annotate("text", x = 52, y = 205, label = "Outlier\nw centrum", color = "#f39c12", size = 3) +
      annotate("text", x = 92, y = 85, label = "Outlier\nna skraju", color = "#e74c3c", size = 3)
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
