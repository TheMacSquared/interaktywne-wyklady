# 📊 Założenia Testów Statystycznych
# Interaktywne narzędzie do nauczania założeń testów statystycznych

library(shiny)
library(ggplot2)
library(dplyr)
library(broom)
library(rstatix)
library(lmtest)

# ============================================================================
# FUNKCJE GENERUJĄCE DANE - MODUŁ 1: NORMALNOŚĆ
# ============================================================================

generate_normal_data <- function(n = 100) {
  set.seed(NULL)
  rnorm(n, mean = 50, sd = 10)
}

generate_slightly_skewed_data <- function(n = 100) {
  set.seed(NULL)
  # Gamma z lekką skośnością
  rgamma(n, shape = 5, scale = 10)
}

generate_highly_skewed_data <- function(n = 100) {
  set.seed(NULL)
  # Gamma z silną skośnością
  rgamma(n, shape = 2, scale = 15)
}

generate_bimodal_data <- function(n = 100) {
  set.seed(NULL)
  group <- sample(c(1, 2), n, replace = TRUE, prob = c(0.5, 0.5))
  ifelse(group == 1, rnorm(n, mean = 30, sd = 8), rnorm(n, mean = 70, sd = 8))
}

generate_outliers_data <- function(n = 100) {
  set.seed(NULL)
  base <- rnorm(n - 5, mean = 50, sd = 10)
  outliers <- c(10, 15, 85, 90, 95)
  c(base, outliers)
}

# ============================================================================
# FUNKCJE GENERUJĄCE DANE - MODUŁ 2: JEDNORODNOŚĆ WARIANCJI
# ============================================================================

generate_equal_variance_data <- function(n_groups = 2, n_per_group = 30) {
  set.seed(NULL)
  groups <- rep(paste0("Grupa_", LETTERS[1:n_groups]), each = n_per_group)
  values <- c()
  for (i in 1:n_groups) {
    values <- c(values, rnorm(n_per_group, mean = 50 + i * 5, sd = 10))
  }
  data.frame(group = groups, value = values)
}

generate_slightly_unequal_variance_data <- function(n_groups = 2, n_per_group = 30) {
  set.seed(NULL)
  groups <- rep(paste0("Grupa_", LETTERS[1:n_groups]), each = n_per_group)
  values <- c()
  sds <- seq(10, 10 + (n_groups - 1) * 5, length.out = n_groups)
  for (i in 1:n_groups) {
    values <- c(values, rnorm(n_per_group, mean = 50 + i * 5, sd = sds[i]))
  }
  data.frame(group = groups, value = values)
}

generate_very_unequal_variance_data <- function(n_groups = 2, n_per_group = 30) {
  set.seed(NULL)
  groups <- rep(paste0("Grupa_", LETTERS[1:n_groups]), each = n_per_group)
  values <- c()
  sds <- seq(5, 5 * n_groups * 2, length.out = n_groups)
  for (i in 1:n_groups) {
    values <- c(values, rnorm(n_per_group, mean = 50 + i * 5, sd = sds[i]))
  }
  data.frame(group = groups, value = values)
}

generate_unequal_n_variance_data <- function(n_groups = 2) {
  set.seed(NULL)
  n_vals <- c(20, 50)
  if (n_groups == 3) n_vals <- c(15, 30, 60)

  groups <- c()
  values <- c()
  sds <- seq(5, 5 * n_groups * 2, length.out = n_groups)

  for (i in 1:n_groups) {
    groups <- c(groups, rep(paste0("Grupa_", LETTERS[i]), n_vals[i]))
    values <- c(values, rnorm(n_vals[i], mean = 50 + i * 5, sd = sds[i]))
  }
  data.frame(group = groups, value = values)
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
generate_regression_normal_residuals <- function(n = 80) {
  set.seed(NULL)
  x <- runif(n, 10, 100)
  y <- 2 * x + 50 + rnorm(n, mean = 0, sd = 15)
  data.frame(x = x, y = y)
}

generate_regression_nonlinear <- function(n = 80) {
  set.seed(NULL)
  x <- runif(n, 0, 10)
  # Związek kwadratowy: y = x^2 + szum
  y <- x^2 + rnorm(n, mean = 0, sd = 5)
  data.frame(x = x, y = y)
}

generate_regression_residuals_outliers <- function(n = 80) {
  set.seed(NULL)
  x <- runif(n - 4, 10, 100)
  y <- 2 * x + 50 + rnorm(n - 4, mean = 0, sd = 15)
  x <- c(x, c(20, 50, 70, 90))
  y <- c(y, c(250, 50, 300, 100))
  data.frame(x = x, y = y)
}

# 4b: Homoskedastyczność
generate_regression_homoscedastic <- function(n = 80) {
  set.seed(NULL)
  x <- runif(n, 10, 100)
  y <- 2 * x + 50 + rnorm(n, mean = 0, sd = 15)
  data.frame(x = x, y = y)
}

generate_regression_heteroscedastic_increasing <- function(n = 80) {
  set.seed(NULL)
  x <- runif(n, 10, 100)
  y <- 2 * x + 50 + rnorm(n, mean = 0, sd = x * 0.3)
  data.frame(x = x, y = y)
}

generate_regression_heteroscedastic_decreasing <- function(n = 80) {
  set.seed(NULL)
  x <- runif(n, 10, 100)
  y <- 2 * x + 50 + rnorm(n, mean = 0, sd = (100 - x) * 0.3)
  data.frame(x = x, y = y)
}

# 4c: Wpływ outlierów
generate_regression_no_outliers <- function(n = 50) {
  set.seed(NULL)
  x <- runif(n, 10, 100)
  y <- 2 * x + 50 + rnorm(n, mean = 0, sd = 15)
  data.frame(x = x, y = y, is_outlier = FALSE)
}

generate_regression_outlier_y <- function(n = 50) {
  set.seed(NULL)
  x <- runif(n - 2, 10, 100)
  y <- 2 * x + 50 + rnorm(n - 2, mean = 0, sd = 15)
  x <- c(x, c(50, 55))
  y <- c(y, c(250, 260))
  is_outlier <- c(rep(FALSE, n - 2), TRUE, TRUE)
  data.frame(x = x, y = y, is_outlier = is_outlier)
}

generate_regression_outlier_xy <- function(n = 50) {
  set.seed(NULL)
  x <- runif(n - 2, 10, 90)
  y <- 2 * x + 50 + rnorm(n - 2, mean = 0, sd = 15)
  x <- c(x, c(5, 95))
  y <- c(y, c(250, 100))
  is_outlier <- c(rep(FALSE, n - 2), TRUE, TRUE)
  data.frame(x = x, y = y, is_outlier = is_outlier)
}

generate_regression_multiple_outliers <- function(n = 50) {
  set.seed(NULL)
  x <- runif(n - 4, 10, 90)
  y <- 2 * x + 50 + rnorm(n - 4, mean = 0, sd = 15)
  x <- c(x, c(5, 15, 85, 95))
  y <- c(y, c(250, 50, 300, 100))
  is_outlier <- c(rep(FALSE, n - 4), TRUE, TRUE, TRUE, TRUE)
  data.frame(x = x, y = y, is_outlier = is_outlier)
}

# ============================================================================
# PRE-COMPUTED WYNIKI SYMULACJI (Monte Carlo, n_sim = 10000)
# ============================================================================

# Moduł 1: Normalność - błąd typu I przy różnych rozkładach
precomputed_normality <- data.frame(
  rozklad = factor(c("Normalny", "Normalny",
                     "Lekko skośny", "Lekko skośny",
                     "Silnie skośny", "Silnie skośny",
                     "Bimodalny", "Bimodalny"),
                   levels = c("Normalny", "Lekko skośny", "Silnie skośny", "Bimodalny")),
  test = rep(c("t-test", "Wilcoxon"), 4),
  blad_typu_I = c(0.052, 0.048,   # normalny
                  0.058, 0.051,   # lekko skośny
                  0.087, 0.052,   # silnie skośny
                  0.112, 0.054),  # bimodalny
  n = rep(30, 8)
)

# Moduł 2: Jednorodność wariancji - błąd typu I przy różnych kombinacjach n i SD
precomputed_variance <- data.frame(
  scenariusz = factor(c("n=20,20\nSD=1:1", "n=20,20\nSD=1:1",
                        "n=20,20\nSD=1:4", "n=20,20\nSD=1:4",
                        "n=10,30\nSD=1:4", "n=10,30\nSD=1:4",
                        "n=30,10\nSD=1:4", "n=30,10\nSD=1:4"),
                      levels = c("n=20,20\nSD=1:1", "n=20,20\nSD=1:4",
                                 "n=10,30\nSD=1:4", "n=30,10\nSD=1:4")),
  test = rep(c("Student's t", "Welch's t"), 4),
  blad_typu_I = c(0.051, 0.050,   # równe wszystko
                  0.071, 0.052,   # równe n, różne SD
                  0.148, 0.053,   # mała grupa + duża wariancja (NAJGORSZY!)
                  0.024, 0.051)   # duża grupa + duża wariancja (konserwatywny)
)

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

              actionButton("normality_regenerate", "🎲 Losuj nowe dane",
                           class = "btn-success", width = "100%"),

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
              div(
                style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                h4("Histogram z rozkładem normalnym"),
                plotOutput("normality_histogram", height = "300px")
              ),

              div(
                style = "border: 2px solid #e67e22; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                h4("QQ-plot (Quantile-Quantile)"),
                plotOutput("normality_qqplot", height = "300px"),
                div(
                  style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-top: 10px;",
                  p("QQ-plot pokazuje jak dane porównują się z rozkładem normalnym. Jeśli punkty leżą blisko linii, rozkład jest normalny.")
                )
              ),

              div(
                style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
                h4("Test Shapiro-Wilka"),
                tableOutput("normality_test")
              ),

              width = 9
            )
          )
        ),

        # --- Podtab: Dlaczego to ważne? ---
        tabPanel(
          "Dlaczego to ważne?",
          br(),
          h3("Konsekwencje łamania założenia normalności"),
          p("Symulacja Monte Carlo (10 000 powtórzeń): Porównanie dwóch grup z identycznymi średnimi (H0 prawdziwe)."),
          p(strong("Błąd typu I"), " = odsetek fałszywych pozytywów. Powinien wynosić 5%."),

          hr(),

          fluidRow(
            column(6,
              div(class = "result-box-success",
                h4("Założenie spełnione: rozkład normalny"),
                plotOutput("norm_consequence_ok", height = "250px"),
                br(),
                p("Błąd typu I dla ", strong("t-testu: "),
                  span(class = "value-big value-ok", "5.2%")),
                p("Błąd typu I dla ", strong("Wilcoxona: "),
                  span(class = "value-big value-ok", "4.8%")),
                p(style = "color: #28a745;", "Oba testy działają poprawnie!")
              )
            ),
            column(6,
              div(class = "result-box-danger",
                h4("Założenie złamane: rozkład bimodalny"),
                plotOutput("norm_consequence_bad", height = "250px"),
                br(),
                p("Błąd typu I dla ", strong("t-testu: "),
                  span(class = "value-big value-bad", "11.2%")),
                p("Błąd typu I dla ", strong("Wilcoxona: "),
                  span(class = "value-big value-ok", "5.4%")),
                p(style = "color: #dc3545;", "t-test daje 2x więcej fałszywych pozytywów!")
              )
            )
          ),

          hr(),

          h4("Porównanie wszystkich rozkładów"),
          plotOutput("norm_consequence_comparison", height = "350px"),

          div(class = "interpretation-box",
            h4("Wniosek"),
            p("Przy ", strong("silnej skośności"), " lub ", strong("rozkładzie bimodalnym"),
              " t-test może dawać znacznie więcej niż 5% fałszywych pozytywów."),
            p("Test ", strong("Wilcoxona (Mann-Whitney)"), " jest odporny na naruszenia normalności ",
              "i utrzymuje prawidłowy poziom błędu typu I (~5%) niezależnie od rozkładu."),
            p(style = "font-style: italic;",
              "Zalecenie: Przy wątpliwościach co do normalności, użyj testu Wilcoxona.")
          )
        ),

        # --- Podtab: Problem u podstawy ---
        tabPanel(
          "Problem u podstawy",
          br(),
          h3("Dlaczego średnia może kłamać?"),
          p("t-test porównuje ", strong("średnie"), " między grupami. Ale czy średnia zawsze dobrze reprezentuje dane?"),

          hr(),

          h4("Porównanie: Średnia vs Mediana przy różnych rozkładach"),

          fluidRow(
            column(6,
              div(class = "result-box-success",
                h4("Rozkład normalny (symetryczny)"),
                fluidRow(
                  column(6, plotOutput("norm_base_hist_ok", height = "200px")),
                  column(6, plotOutput("norm_base_box_ok", height = "200px"))
                ),
                br(),
                p("Średnia: ", span(class = "value-big", style = "color: #e74c3c;", textOutput("norm_base_mean_ok", inline = TRUE))),
                p("Mediana: ", span(class = "value-big", style = "color: #27ae60;", textOutput("norm_base_median_ok", inline = TRUE))),
                p(style = "color: #28a745;", "Średnia ≈ Mediana → obie dobrze opisują 'typową' wartość")
              )
            ),
            column(6,
              div(class = "result-box-danger",
                h4("Rozkład skośny (asymetryczny)"),
                fluidRow(
                  column(6, plotOutput("norm_base_hist_bad", height = "200px")),
                  column(6, plotOutput("norm_base_box_bad", height = "200px"))
                ),
                br(),
                p("Średnia: ", span(class = "value-big", style = "color: #e74c3c;", textOutput("norm_base_mean_bad", inline = TRUE))),
                p("Mediana: ", span(class = "value-big", style = "color: #27ae60;", textOutput("norm_base_median_bad", inline = TRUE))),
                p(style = "color: #dc3545;", "Średnia >> Mediana → średnia 'ucieka' w stronę ogona!")
              )
            )
          ),

          hr(),

          h4("Rozkład z outlierami - ekstremalna demonstracja"),
          fluidRow(
            column(8,
              plotOutput("norm_base_outlier_demo", height = "250px")
            ),
            column(4,
              div(class = "result-box-warning",
                h4("Wpływ 1 outliera"),
                p("Dane: 10, 12, 11, 13, 10, 12, ", strong("100")),
                br(),
                p("Średnia: ", span(class = "value-big value-bad", "24.0")),
                p("Mediana: ", span(class = "value-big value-ok", "12.0")),
                br(),
                p("Jeden outlier przesunął średnią o ", strong("100%"), "!")
              )
            )
          ),

          div(class = "interpretation-box",
            h4("Kluczowy wniosek"),
            p("t-test porównuje ", strong("średnie"), ", które są wrażliwe na:"),
            tags$ul(
              tags$li("Skośność rozkładu (średnia 'ucieka' w stronę ogona)"),
              tags$li("Outlierów (jeden punkt może drastycznie zmienić średnią)"),
              tags$li("Rozkłady bimodalne (średnia może leżeć 'pomiędzy' gdzie nikogo nie ma)")
            ),
            p("Test ", strong("Wilcoxona"), " porównuje ", strong("rangi"), " (czyli pozycje w uporządkowanych danych), ",
              "co jest odporne na te problemy."),
            p(style = "font-style: italic;",
              "Metafora: Średnia pensji w firmie, gdzie jest 1 prezes i 100 pracowników - średnia kłamie o 'typowej' pensji.")
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
              selectInput("variance_scenario", "Typ scenariusza:",
                          choices = c(
                            "Równe wariancje (idealny)" = "equal",
                            "Lekko różne wariancje" = "slightly_unequal",
                            "Bardzo różne wariancje" = "very_unequal",
                            "Różne n + różne wariancje" = "unequal_n"
                          ),
                          selected = "equal"),

              sliderInput("variance_n_groups", "Liczba grup:",
                          min = 2, max = 3, value = 2, step = 1),

              actionButton("variance_regenerate", "🎲 Losuj nowe dane",
                           class = "btn-success", width = "100%"),

              hr(),

              h4("Interpretacja"),
              div(
                style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px;",
                textOutput("variance_interpretation")
              ),

              width = 3
            ),

            mainPanel(
              div(
                style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                h4("Boxploty grup"),
                plotOutput("variance_boxplot", height = "300px")
              ),

              div(
                style = "border: 2px solid #e67e22; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                h4("Statystyki opisowe"),
                tableOutput("variance_stats")
              ),

              div(
                style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
                h4("Test Levene'a"),
                tableOutput("variance_test")
              ),

              width = 9
            )
          )
        ),

        # --- Podtab: Dlaczego to ważne? ---
        tabPanel(
          "Dlaczego to ważne?",
          br(),
          h3("Konsekwencje łamania założenia jednorodności wariancji"),
          p("Symulacja Monte Carlo (10 000 powtórzeń): Porównanie dwóch grup z identycznymi średnimi (H0 prawdziwe)."),
          p(strong("Kluczowy czynnik: "), "interakcja między nierównymi liczebnościami grup (n) a nierównymi wariancjami."),

          hr(),

          fluidRow(
            column(6,
              div(class = "result-box-success",
                h4("Równe n, równe wariancje"),
                p("n1 = 20, n2 = 20, SD ratio = 1:1"),
                br(),
                p("Błąd typu I dla ", strong("Student's t: "),
                  span(class = "value-big value-ok", "5.1%")),
                p("Błąd typu I dla ", strong("Welch's t: "),
                  span(class = "value-big value-ok", "5.0%")),
                p(style = "color: #28a745;", "Oba testy działają poprawnie!")
              )
            ),
            column(6,
              div(class = "result-box-danger",
                h4("Mała grupa + duża wariancja"),
                p("n1 = 10, n2 = 30, SD ratio = 1:4"),
                p(style = "font-style: italic;", "(mała grupa ma 4x większą wariancję)"),
                br(),
                p("Błąd typu I dla ", strong("Student's t: "),
                  span(class = "value-big value-bad", "14.8%")),
                p("Błąd typu I dla ", strong("Welch's t: "),
                  span(class = "value-big value-ok", "5.3%")),
                p(style = "color: #dc3545;", "Student's t daje 3x więcej fałszywych pozytywów!")
              )
            )
          ),

          hr(),

          h4("Porównanie wszystkich scenariuszy"),
          plotOutput("var_consequence_comparison", height = "350px"),

          div(class = "result-box-warning",
            h4("Ciekawy przypadek: duża grupa + duża wariancja"),
            p("Gdy ", strong("większa"), " grupa ma ", strong("większą"), " wariancję (n=30, SD=4 vs n=10, SD=1):"),
            p("Student's t staje się ", strong("konserwatywny"), " (błąd I = 2.4% zamiast 5%)"),
            p("To oznacza ", strong("utratę mocy"), " - test rzadziej wykrywa prawdziwe różnice.")
          ),

          div(class = "interpretation-box",
            h4("Wniosek"),
            p(strong("Najgorszy przypadek:"), " mała grupa + duża wariancja = dramatyczna inflacja błędu typu I"),
            p(strong("Welch's t-test"), " jest zawsze bezpieczny - automatycznie koryguje dla nierównych wariancji."),
            p(style = "font-style: italic;",
              "Zalecenie: Zawsze używaj Welch's t-test (w R: t.test(..., var.equal = FALSE) - domyślne ustawienie).")
          )
        ),

        # --- Podtab: Problem u podstawy ---
        tabPanel(
          "Problem u podstawy",
          br(),
          h3("Niepewność pomiaru średniej zależy od wariancji"),
          p("Kluczowe pytanie: ", strong("Jak precyzyjnie znamy średnią w każdej grupie?")),

          hr(),

          h4("Błąd standardowy średniej (SEM)"),

          div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
            p("Niepewność średniej w grupie:"),
            p(style = "font-family: monospace; font-size: 18px; text-align: center;",
              "SEM = SD / \u221An"),
            p(style = "text-align: center;", "Im większa wariancja (SD) → tym większa niepewność średniej")
          ),

          fluidRow(
            column(6,
              div(class = "result-box-success",
                h4("Obie grupy: podobna niepewność"),
                p("Grupa A: n=20, SD=10 → SEM = 2.2"),
                p("Grupa B: n=20, SD=10 → SEM = 2.2"),
                br(),
                plotOutput("var_base_sem_equal", height = "200px"),
                br(),
                p("Obie średnie znamy z ", strong("podobną precyzją")),
                p("Test porównuje je sprawiedliwie")
              )
            ),
            column(6,
              div(class = "result-box-danger",
                h4("Różna niepewność w grupach"),
                p("Grupa A: n=10, SD=40 → SEM = 12.6"),
                p("Grupa B: n=30, SD=10 → SEM = 1.8"),
                br(),
                plotOutput("var_base_sem_unequal", height = "200px"),
                br(),
                p("Średnią grupy A znamy ", strong("7x gorzej"), " niż B!"),
                p("Test nie wie, że jedna średnia jest 'rozmazana'")
              )
            )
          ),

          hr(),

          h4("Problem: Test zakłada równą niepewność obu średnich"),

          div(class = "result-box-warning",
            p("Testy zakładające równe wariancje (Student's t, klasyczna ANOVA) liczą ",
              strong("wspólny błąd standardowy"), " dla wszystkich grup."),
            br(),
            p("Gdy grupy mają różne wariancje:"),
            tags$ul(
              tags$li("Wspólny SE jest ", strong("uśrednieniem"), " - nie pasuje do żadnej grupy"),
              tags$li("Dla grupy z dużą wariancją: SE ", strong("zaniżony"), " → za dużo 'istotnych' wyników"),
              tags$li("Dla grupy z małą wariancją: SE ", strong("zawyżony"), " → tracimy moc")
            ),
            br(),
            p(strong("Kluczowy czynnik: "), "Która grupa ma więcej obserwacji?"),
            tags$ul(
              tags$li("Mała grupa + duża wariancja → ", span(style="color:#dc3545;", strong("inflacja błędu I (liberalny)"))),
              tags$li("Duża grupa + duża wariancja → ", span(style="color:#f39c12;", strong("utrata mocy (konserwatywny)")))
            )
          ),

          hr(),

          h4("Wizualizacja: Rozkłady średnich z próby (sampling distributions)"),
          p("Każda krzywa to rozkład możliwych średnich przy wielokrotnym próbkowaniu:"),
          plotOutput("var_base_sampling_dist", height = "300px"),

          div(class = "interpretation-box",
            h4("Kluczowy wniosek"),
            p("Problem ", strong("nie jest specyficzny"), " dla t-testu - dotyczy też ANOVA i innych testów."),
            p("Fundamentalny problem: ", strong("różne grupy mają różną niepewność pomiaru średniej"), "."),
            br(),
            p("Rozwiązania:"),
            tags$ul(
              tags$li(strong("Welch's t-test"), " - osobny SE dla każdej grupy (domyślny w R!)"),
              tags$li(strong("Welch's ANOVA"), " - oneway.test(..., var.equal = FALSE)"),
              tags$li(strong("Testy nieparametryczne"), " - nie zakładają równych wariancji")
            ),
            p(style = "font-style: italic;",
              "Zasada: Jeśli nie masz pewności co do równości wariancji, użyj metody, która tego nie zakłada.")
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

        # Sekcja: Porównanie grup
        h3("Porównanie grup", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),

        tags$table(
          class = "table table-bordered table-striped",
          style = "width: 100%; margin-bottom: 30px; font-size: 14px;",
          tags$thead(
            style = "background-color: #3498db; color: white;",
            tags$tr(
              tags$th("Problem", style = "width: 40%;"),
              tags$th("Test parametryczny", style = "width: 30%;"),
              tags$th("Test nieparametryczny", style = "width: 30%;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Jedna próba vs wartość teoretyczna"),
              tags$td("t-test dla jednej próby"),
              tags$td("Wilcoxon signed-rank", tags$br(),
                      tags$small("(test na medianę)", style = "color: #7f8c8d;"))
            ),
            tags$tr(
              tags$td("Dwie grupy niezależne (ilościowe)"),
              tags$td("t-test dla prób niezależnych", tags$br(),
                      tags$small("(Welch's t domyślnie)", style = "color: #7f8c8d;")),
              tags$td("Mann-Whitney U", tags$br(),
                      tags$small("(= Wilcoxon rank-sum)", style = "color: #7f8c8d;"))
            ),
            tags$tr(
              tags$td("Dwie grupy zależne / pomiary powtórzone"),
              tags$td("t-test dla prób zależnych"),
              tags$td("Wilcoxon signed-rank")
            ),
            tags$tr(
              tags$td("Więcej niż 2 grupy niezależne"),
              tags$td("ANOVA jednokierunkowa", tags$br(),
                      tags$small("(Welch's ANOVA przy nierównych wariancjach)", style = "color: #7f8c8d;")),
              tags$td("Kruskal-Wallis")
            )
          )
        ),

        # Sekcja: Związki między zmiennymi
        h3("Związki między zmiennymi", style = "color: #2c3e50; border-bottom: 2px solid #27ae60; padding-bottom: 10px;"),

        tags$table(
          class = "table table-bordered table-striped",
          style = "width: 100%; margin-bottom: 30px; font-size: 14px;",
          tags$thead(
            style = "background-color: #27ae60; color: white;",
            tags$tr(
              tags$th("Problem", style = "width: 40%;"),
              tags$th("Metoda parametryczna", style = "width: 30%;"),
              tags$th("Metoda nieparametryczna", style = "width: 30%;")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Korelacja dwóch zmiennych ilościowych"),
              tags$td("Pearson r"),
              tags$td("Spearman rho", tags$br(),
                      tags$small("(lub Kendall tau)", style = "color: #7f8c8d;"))
            ),
            tags$tr(
              tags$td("Związek zmiennej ilościowej z jakościową"),
              tags$td("ANOVA"),
              tags$td("Kruskal-Wallis")
            ),
            tags$tr(
              tags$td("Związek dwóch zmiennych jakościowych"),
              tags$td(tags$em("—")),
              tags$td("Chi-kwadrat / Fisher exact")
            ),
            tags$tr(
              tags$td("Predykcja zmiennej ilościowej"),
              tags$td("Regresja liniowa (OLS)"),
              tags$td(tags$em("Regresja kwantylowa"), tags$br(),
                      tags$small("(robust regression)", style = "color: #7f8c8d;"))
            )
          )
        ),

        # Sekcja: Kiedy co wybrać
        h3("Kiedy wybrać test nieparametryczny?", style = "color: #2c3e50; border-bottom: 2px solid #e74c3c; padding-bottom: 10px;"),

        div(
          style = "background-color: #fdf2e9; padding: 20px; border-radius: 10px; margin-bottom: 20px;",
          tags$ul(
            style = "font-size: 14px; margin: 0;",
            tags$li(tags$strong("Mała próba (n < 30)"), " i brak normalności rozkładu"),
            tags$li(tags$strong("Silna skośność"), " lub outliery, których nie można usunąć"),
            tags$li(tags$strong("Dane porządkowe"), " (np. skala Likerta 1-5)"),
            tags$li(tags$strong("Dane w postaci rang"), " lub percentyli")
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

              actionButton("reg_normal_regenerate", "🎲 Losuj nowe dane",
                           class = "btn-success", width = "100%"),

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
              div(
                style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                h4("Scatterplot z linią regresji"),
                plotOutput("reg_normal_scatter", height = "300px")
              ),

              div(
                style = "border: 2px solid #e67e22; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                h4("Histogram reszt"),
                plotOutput("reg_normal_hist", height = "250px")
              ),

              div(
                style = "border: 2px solid #9b59b6; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                h4("QQ-plot reszt"),
                plotOutput("reg_normal_qq", height = "250px")
              ),

              div(
                style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
                h4("Test Shapiro-Wilka (na resztach)"),
                tableOutput("reg_normal_test")
              ),

              width = 9
            )
          )
        ),

        # ====================================================================
        # SUB-TAB 4b: Homoskedastyczność - Wizualizacja
        # ====================================================================
        tabPanel(
          "Homoskedastyczność",
          br(),
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

              actionButton("reg_homo_regenerate", "🎲 Losuj nowe dane",
                           class = "btn-success", width = "100%"),

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
              div(
                style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                h4("Scatterplot z linią regresji"),
                plotOutput("reg_homo_scatter", height = "300px")
              ),

              div(
                style = "border: 2px solid #e67e22; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
                h4("Residual plot (kluczowy!)"),
                plotOutput("reg_homo_residual", height = "300px")
              ),

              div(
                style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
                h4("Test Breusch-Pagan"),
                tableOutput("reg_homo_test")
              ),

              width = 9
            )
          )
        ),

        # ====================================================================
        # SUB-TAB 4b2: Homoskedastyczność - Dlaczego to ważne?
        # ====================================================================
        tabPanel(
          "Dlaczego homoskedastyczność?",
          br(),
          h3("Konsekwencje heteroskedastyczności"),
          p("Symulacja Monte Carlo (10 000 powtórzeń): Estymacja współczynnika regresji (prawdziwy slope = 2)."),
          p(strong("Pokrycie 95% CI"), " = jak często przedział ufności zawiera prawdziwą wartość. Powinno być 95%."),

          hr(),

          fluidRow(
            column(6,
              div(class = "result-box-success",
                h4("Homoskedastyczność (stała wariancja)"),
                plotOutput("reg_homo_ok_plot", height = "200px"),
                br(),
                p("Pokrycie 95% CI dla ", strong("OLS: "),
                  span(class = "value-big value-ok", "94.8%")),
                p("Pokrycie 95% CI dla ", strong("Robust SE: "),
                  span(class = "value-big value-ok", "95.1%")),
                p(style = "color: #28a745;", "Przedziały ufności są prawidłowe!")
              )
            ),
            column(6,
              div(class = "result-box-danger",
                h4("Silna heteroskedastyczność"),
                plotOutput("reg_homo_bad_plot", height = "200px"),
                br(),
                p("Pokrycie 95% CI dla ", strong("OLS: "),
                  span(class = "value-big value-bad", "84.2%")),
                p("Pokrycie 95% CI dla ", strong("Robust SE: "),
                  span(class = "value-big value-ok", "94.8%")),
                p(style = "color: #dc3545;", "OLS: przedziały są za wąskie! 11% przypadków nie pokrywa prawdy.")
              )
            )
          ),

          hr(),

          h4("Porównanie poziomów heteroskedastyczności"),
          plotOutput("reg_homo_consequence_comparison", height = "300px"),

          div(class = "interpretation-box",
            h4("Wniosek"),
            p("Heteroskedastyczność nie wpływa na ", strong("estymaty współczynników"), " (slope, intercept)."),
            p("Wpływa na ", strong("błędy standardowe"), " - są nieprawidłowe, co prowadzi do:"),
            tags$ul(
              tags$li("Za wąskich przedziałów ufności"),
              tags$li("Zbyt małych p-value (nadmiar istotnych wyników)"),
              tags$li("Błędnych wniosków o istotności statystycznej")
            ),
            p(strong("Rozwiązanie: "), "Użyj ", strong("Robust Standard Errors"), " (pakiet sandwich w R)."),
            p(style = "font-style: italic;",
              "Kod: library(sandwich); coeftest(model, vcov = vcovHC(model, type = 'HC3'))")
          )
        ),

        # ====================================================================
        # SUB-TAB 4c: Wpływ outlierów
        # ====================================================================
        tabPanel(
          "Wpływ outlierów",
          br(),
          sidebarLayout(
            sidebarPanel(
              h4("Wybór scenariusza"),
              selectInput("reg_outlier_scenario", "Typ outlierów:",
                          choices = c(
                            "Bez outlierów" = "no_outliers",
                            "Outlier w Y" = "outlier_y",
                            "Outlier w X i Y" = "outlier_xy",
                            "Kilka outlierów" = "multiple"
                          ),
                          selected = "no_outliers"),

              actionButton("reg_outlier_regenerate", "🎲 Losuj nowe dane",
                           class = "btn-success", width = "100%"),

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
                plotOutput("reg_outlier_scatter", height = "400px"),
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
          )
        ),

        # ====================================================================
        # SUB-TAB 4d: Problem u podstawy - Dźwignia (Leverage)
        # ====================================================================
        tabPanel(
          "Problem: Dźwignia",
          br(),
          h3("Dlaczego jeden punkt może 'obrócić' całą linię?"),
          p("Regresja liniowa minimalizuje sumę kwadratów reszt. ",
            "Ale nie wszystkie punkty mają ", strong("równy wpływ"), " na wynik."),

          hr(),

          h4("Leverage (dźwignia) - odległość od centrum w X"),

          fluidRow(
            column(6,
              div(class = "result-box-success",
                h4("Punkt blisko centrum X"),
                plotOutput("leverage_low_plot", height = "250px"),
                br(),
                p("Punkt w centrum ma ", strong("małą dźwignię")),
                p("Nawet jeśli jest 'dziwny' w Y, nie zmieni bardzo linii")
              )
            ),
            column(6,
              div(class = "result-box-danger",
                h4("Punkt daleko od centrum X"),
                plotOutput("leverage_high_plot", height = "250px"),
                br(),
                p("Punkt na skraju ma ", strong("dużą dźwignię")),
                p("Jak dźwignia - mały ruch na końcu = duży efekt")
              )
            )
          ),

          hr(),

          h4("Demonstracja interaktywna: Jak outlier zmienia slope"),
          plotOutput("leverage_demo_plot", height = "350px"),

          div(class = "result-box-warning",
            h4("Cook's Distance - miara wpływu"),
            p("Cook's D łączy dźwignię (pozycja X) z wielkością reszty (odchylenie Y):"),
            p(style = "font-family: monospace; text-align: center; font-size: 16px;",
              "Wpływ = Dźwignia × Reszta²"),
            p("Punkt z wysokim Cook's D (>1) powinien być zbadany - może dominować analizę.")
          ),

          div(class = "interpretation-box",
            h4("Kluczowy wniosek"),
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
        ),

        # ====================================================================
        # SUB-TAB 4e: Problem u podstawy - Heteroskedastyczność
        # ====================================================================
        tabPanel(
          "Problem: Niepewność",
          br(),
          h3("Dlaczego model 'myśli', że jest pewniejszy niż powinien?"),
          p("OLS zakłada, że rozrzut wokół linii jest ", strong("stały"), ". ",
            "Ale co jeśli rozrzut rośnie z X?"),

          hr(),

          h4("Przedział ufności zakłada stałą niepewność"),

          fluidRow(
            column(6,
              div(class = "result-box-success",
                h4("Homoskedastyczność"),
                plotOutput("hetero_ci_ok_plot", height = "280px"),
                br(),
                p("Rozrzut danych ", strong("stały"), " → CI jest prawidłowy"),
                p("Model wie, jak bardzo może się mylić")
              )
            ),
            column(6,
              div(class = "result-box-danger",
                h4("Heteroskedastyczność"),
                plotOutput("hetero_ci_bad_plot", height = "280px"),
                br(),
                p("Rozrzut danych ", strong("rośnie"), " → CI jest za wąski z prawej!"),
                p("Model nie wie, że niepewność rośnie")
              )
            )
          ),

          hr(),

          h4("Konsekwencja: 'Pewność' modelu vs rzeczywistość"),

          div(class = "result-box-warning",
            fluidRow(
              column(6,
                h5("Model twierdzi:"),
                p("'Jestem 95% pewny, że prawdziwa wartość jest w tym przedziale'"),
                p(style = "font-family: monospace;", "CI: [1.8, 2.2]")
              ),
              column(6,
                h5("Rzeczywistość:"),
                p("Przy heteroskedastyczności faktyczne pokrycie to ~85%"),
                p("Model jest ", strong("nadmiernie pewny siebie"))
              )
            )
          ),

          hr(),

          h4("Analogia: Prognoza pogody"),
          div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px;",
            p("Wyobraź sobie prognozę pogody, która mówi:"),
            p(style = "font-style: italic;", "'Jutro będzie 20°C ± 2°C (95% pewności)'"),
            br(),
            p("Ale w rzeczywistości:"),
            tags$ul(
              tags$li("Rano: rozrzut ± 1°C (prognoza OK)"),
              tags$li("Popołudniu: rozrzut ± 5°C (prognoza za pewna!)"),
              tags$li("Wieczorem: rozrzut ± 3°C")
            ),
            p("Prognoza używa ", strong("średniego"), " błędu, ale błąd nie jest stały!")
          ),

          div(class = "interpretation-box",
            h4("Kluczowy wniosek"),
            p("Heteroskedastyczność nie zmienia ", strong("gdzie"), " linia jest (slope, intercept są OK)."),
            p("Zmienia ", strong("jak pewni"), " powinniśmy być tej linii:"),
            tags$ul(
              tags$li("Przedziały ufności są nieprawidłowe"),
              tags$li("p-values są zbyt małe (nadmiar 'istotnych' wyników)"),
              tags$li("Nie można ufać testom istotności")
            ),
            p(strong("Rozwiązanie:"), " Robust Standard Errors nie zakładają stałego rozrzutu - ",
              "obliczają niepewność lokalnie dla każdego obszaru danych.")
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

  normality_data <- reactiveVal(generate_normal_data())

  observeEvent(input$normality_scenario, {
    data <- switch(input$normality_scenario,
                   "normal" = generate_normal_data(),
                   "slightly_skewed" = generate_slightly_skewed_data(),
                   "highly_skewed" = generate_highly_skewed_data(),
                   "bimodal" = generate_bimodal_data(),
                   "outliers" = generate_outliers_data())
    normality_data(data)
  })

  observeEvent(input$normality_regenerate, {
    data <- switch(input$normality_scenario,
                   "normal" = generate_normal_data(),
                   "slightly_skewed" = generate_slightly_skewed_data(),
                   "highly_skewed" = generate_highly_skewed_data(),
                   "bimodal" = generate_bimodal_data(),
                   "outliers" = generate_outliers_data())
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
      p <- p + geom_line(data = norm_df, aes(x = x, y = y), color = "#e74c3c", size = 1.5)
    }

    p
  })

  output$normality_qqplot <- renderPlot({
    data <- normality_data()
    df <- data.frame(value = data)

    ggplot(df, aes(sample = value)) +
      stat_qq(color = "#3498db", size = 3, alpha = 0.7) +
      stat_qq_line(color = "#e74c3c", size = 1.5) +
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

  variance_data <- reactiveVal(generate_equal_variance_data())

  observeEvent(c(input$variance_scenario, input$variance_n_groups), {
    data <- switch(input$variance_scenario,
                   "equal" = generate_equal_variance_data(input$variance_n_groups),
                   "slightly_unequal" = generate_slightly_unequal_variance_data(input$variance_n_groups),
                   "very_unequal" = generate_very_unequal_variance_data(input$variance_n_groups),
                   "unequal_n" = generate_unequal_n_variance_data(input$variance_n_groups))
    variance_data(data)
  })

  observeEvent(input$variance_regenerate, {
    data <- switch(input$variance_scenario,
                   "equal" = generate_equal_variance_data(input$variance_n_groups),
                   "slightly_unequal" = generate_slightly_unequal_variance_data(input$variance_n_groups),
                   "very_unequal" = generate_very_unequal_variance_data(input$variance_n_groups),
                   "unequal_n" = generate_unequal_n_variance_data(input$variance_n_groups))
    variance_data(data)
  })

  output$variance_boxplot <- renderPlot({
    data <- variance_data()

    ggplot(data, aes(x = group, y = value, fill = group)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c("#3498db", "#e74c3c", "#27ae60")) +
      theme_minimal(base_size = 14) +
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
        Wariancja = round(var(value), 2),
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

    switch(scenario,
           "equal" = "Idealny przypadek - równe wariancje. Testy jak t-test i ANOVA działają dobrze.",
           "slightly_unequal" = "Lekko różne wariancje - testy są zazwyczaj odporne na małe różnice.",
           "very_unequal" = "Bardzo różne wariancje - naruszenie założenia! Użyj Welch's t-test lub testów nieparametrycznych.",
           "unequal_n" = "Różne n + różne wariancje = najgorszy przypadek. Zdecydowanie użyj Welch's t-test lub testów nieparametrycznych.")
  })

  # ==========================================================================
  # MODUŁ 4a: Normalność reszt
  # ==========================================================================

  reg_normal_data <- reactiveVal(generate_regression_normal_residuals())

  observeEvent(input$reg_normal_scenario, {
    data <- switch(input$reg_normal_scenario,
                   "normal" = generate_regression_normal_residuals(),
                   "nonlinear" = generate_regression_nonlinear(),
                   "outliers" = generate_regression_residuals_outliers())
    reg_normal_data(data)
  })

  observeEvent(input$reg_normal_regenerate, {
    data <- switch(input$reg_normal_scenario,
                   "normal" = generate_regression_normal_residuals(),
                   "nonlinear" = generate_regression_nonlinear(),
                   "outliers" = generate_regression_residuals_outliers())
    reg_normal_data(data)
  })

  output$reg_normal_scatter <- renderPlot({
    data <- reg_normal_data()

    ggplot(data, aes(x = x, y = y)) +
      geom_point(size = 3, alpha = 0.6, color = "#3498db") +
      geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", size = 1.5) +
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
    p <- p + geom_line(data = norm_df, aes(x = x, y = y), color = "#e74c3c", size = 1.5)

    p
  })

  output$reg_normal_qq <- renderPlot({
    data <- reg_normal_data()
    model <- lm(y ~ x, data = data)
    residuals <- residuals(model)
    df <- data.frame(residuals = residuals)

    ggplot(df, aes(sample = residuals)) +
      stat_qq(color = "#3498db", size = 3, alpha = 0.7) +
      stat_qq_line(color = "#e74c3c", size = 1.5) +
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

  reg_homo_data <- reactiveVal(generate_regression_homoscedastic())

  observeEvent(input$reg_homo_scenario, {
    data <- switch(input$reg_homo_scenario,
                   "homoscedastic" = generate_regression_homoscedastic(),
                   "increasing" = generate_regression_heteroscedastic_increasing(),
                   "decreasing" = generate_regression_heteroscedastic_decreasing())
    reg_homo_data(data)
  })

  observeEvent(input$reg_homo_regenerate, {
    data <- switch(input$reg_homo_scenario,
                   "homoscedastic" = generate_regression_homoscedastic(),
                   "increasing" = generate_regression_heteroscedastic_increasing(),
                   "decreasing" = generate_regression_heteroscedastic_decreasing())
    reg_homo_data(data)
  })

  output$reg_homo_scatter <- renderPlot({
    data <- reg_homo_data()

    ggplot(data, aes(x = x, y = y)) +
      geom_point(size = 3, alpha = 0.6, color = "#3498db") +
      geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", size = 1.5) +
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
      geom_hline(yintercept = 0, color = "#e74c3c", size = 1.5, linetype = "dashed") +
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

  reg_outlier_data <- reactiveVal(generate_regression_no_outliers())

  observeEvent(input$reg_outlier_scenario, {
    data <- switch(input$reg_outlier_scenario,
                   "no_outliers" = generate_regression_no_outliers(),
                   "outlier_y" = generate_regression_outlier_y(),
                   "outlier_xy" = generate_regression_outlier_xy(),
                   "multiple" = generate_regression_multiple_outliers())
    reg_outlier_data(data)
  })

  observeEvent(input$reg_outlier_regenerate, {
    data <- switch(input$reg_outlier_scenario,
                   "no_outliers" = generate_regression_no_outliers(),
                   "outlier_y" = generate_regression_outlier_y(),
                   "outlier_xy" = generate_regression_outlier_xy(),
                   "multiple" = generate_regression_multiple_outliers())
    reg_outlier_data(data)
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
      geom_smooth(method = "lm", se = FALSE, color = "#3498db", size = 1.5, fullrange = TRUE) +
      geom_smooth(data = data_without, method = "lm", se = FALSE,
                  color = "#e74c3c", size = 1.5, linetype = "dashed", fullrange = TRUE) +
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
  # WYKRESY KONSEKWENCJI - NORMALNOŚĆ
  # ==========================================================================

  # Wykres dla przypadku OK (normalny rozkład)
  output$norm_consequence_ok <- renderPlot({
    # Dane przykładowe - rozkład normalny
    set.seed(42)
    df <- data.frame(
      value = rnorm(100, mean = 50, sd = 10)
    )

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 15, fill = "#28a745", alpha = 0.7, color = "#1e7e34") +
      theme_minimal(base_size = 12) +
      labs(x = "Wartość", y = "Liczba", title = "Rozkład normalny") +
      theme(plot.title = element_text(hjust = 0.5))
  })

  # Wykres dla przypadku złego (bimodalny)
  output$norm_consequence_bad <- renderPlot({
    # Dane przykładowe - rozkład bimodalny
    set.seed(42)
    group <- sample(c(1, 2), 100, replace = TRUE, prob = c(0.5, 0.5))
    df <- data.frame(
      value = ifelse(group == 1, rnorm(100, mean = 30, sd = 8), rnorm(100, mean = 70, sd = 8))
    )

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 15, fill = "#dc3545", alpha = 0.7, color = "#bd2130") +
      theme_minimal(base_size = 12) +
      labs(x = "Wartość", y = "Liczba", title = "Rozkład bimodalny") +
      theme(plot.title = element_text(hjust = 0.5))
  })

  # Wykres porównawczy wszystkich rozkładów
  output$norm_consequence_comparison <- renderPlot({
    ggplot(precomputed_normality, aes(x = rozklad, y = blad_typu_I * 100, fill = test)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
      geom_hline(yintercept = 5, linetype = "dashed", color = "#e74c3c", size = 1.2) +
      annotate("text", x = 0.5, y = 5.5, label = "Oczekiwane 5%", hjust = 0, color = "#e74c3c", size = 4) +
      scale_fill_manual(values = c("t-test" = "#3498db", "Wilcoxon" = "#27ae60")) +
      theme_minimal(base_size = 14) +
      labs(x = "Typ rozkładu", y = "Błąd typu I (%)", fill = "Test",
           title = "Błąd typu I przy różnych rozkładach (n = 30 na grupę)") +
      theme(legend.position = "bottom") +
      coord_cartesian(ylim = c(0, 15))
  })

  # ==========================================================================
  # WYKRESY KONSEKWENCJI - JEDNORODNOŚĆ WARIANCJI
  # ==========================================================================

  output$var_consequence_comparison <- renderPlot({
    ggplot(precomputed_variance, aes(x = scenariusz, y = blad_typu_I * 100, fill = test)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
      geom_hline(yintercept = 5, linetype = "dashed", color = "#e74c3c", size = 1.2) +
      annotate("text", x = 0.5, y = 6, label = "Oczekiwane 5%", hjust = 0, color = "#e74c3c", size = 4) +
      scale_fill_manual(values = c("Student's t" = "#3498db", "Welch's t" = "#27ae60")) +
      theme_minimal(base_size = 14) +
      labs(x = "Scenariusz", y = "Błąd typu I (%)", fill = "Test",
           title = "Błąd typu I przy różnych kombinacjach n i wariancji") +
      theme(legend.position = "bottom",
            axis.text.x = element_text(size = 10)) +
      coord_cartesian(ylim = c(0, 18))
  })

  # ==========================================================================
  # WYKRESY KONSEKWENCJI - HOMOSKEDASTYCZNOŚĆ
  # ==========================================================================

  # Przykładowy residual plot - OK
  output$reg_homo_ok_plot <- renderPlot({
    set.seed(42)
    x <- runif(80, 10, 100)
    y <- 2 * x + 50 + rnorm(80, mean = 0, sd = 15)
    model <- lm(y ~ x)
    df <- data.frame(fitted = fitted(model), residuals = residuals(model))

    ggplot(df, aes(x = fitted, y = residuals)) +
      geom_point(size = 2, alpha = 0.6, color = "#28a745") +
      geom_hline(yintercept = 0, color = "#1e7e34", size = 1, linetype = "dashed") +
      theme_minimal(base_size = 11) +
      labs(x = "Fitted", y = "Residuals", title = "Równy rozrzut") +
      theme(plot.title = element_text(hjust = 0.5))
  })

  # Przykładowy residual plot - BAD
  output$reg_homo_bad_plot <- renderPlot({
    set.seed(42)
    x <- runif(80, 10, 100)
    y <- 2 * x + 50 + rnorm(80, mean = 0, sd = x * 0.4)
    model <- lm(y ~ x)
    df <- data.frame(fitted = fitted(model), residuals = residuals(model))

    ggplot(df, aes(x = fitted, y = residuals)) +
      geom_point(size = 2, alpha = 0.6, color = "#dc3545") +
      geom_hline(yintercept = 0, color = "#bd2130", size = 1, linetype = "dashed") +
      theme_minimal(base_size = 11) +
      labs(x = "Fitted", y = "Residuals", title = "Kształt lejka") +
      theme(plot.title = element_text(hjust = 0.5))
  })

  # Wykres porównawczy pokrycia CI
  output$reg_homo_consequence_comparison <- renderPlot({
    ggplot(precomputed_regression, aes(x = heterosked, y = pokrycie_CI * 100, fill = metoda)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
      geom_hline(yintercept = 95, linetype = "dashed", color = "#e74c3c", size = 1.2) +
      annotate("text", x = 0.5, y = 96, label = "Oczekiwane 95%", hjust = 0, color = "#e74c3c", size = 4) +
      scale_fill_manual(values = c("OLS (zwykły)" = "#3498db", "Robust SE" = "#27ae60")) +
      theme_minimal(base_size = 14) +
      labs(x = "Poziom heteroskedastyczności", y = "Pokrycie 95% CI (%)", fill = "Metoda",
           title = "Jak często 95% CI zawiera prawdziwą wartość?") +
      theme(legend.position = "bottom") +
      coord_cartesian(ylim = c(80, 100))
  })

  # ==========================================================================
  # WYKRESY "PROBLEM U PODSTAWY" - NORMALNOŚĆ
  # ==========================================================================

  # Histogram - rozkład normalny
  output$norm_base_hist_ok <- renderPlot({
    set.seed(123)
    df <- data.frame(value = rnorm(100, mean = 50, sd = 10))
    mean_val <- mean(df$value)
    median_val <- median(df$value)

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 15, fill = "#3498db", alpha = 0.6, color = "#2980b9") +
      geom_vline(xintercept = mean_val, color = "#e74c3c", size = 1.5, linetype = "solid") +
      geom_vline(xintercept = median_val, color = "#27ae60", size = 1.5, linetype = "dashed") +
      theme_minimal(base_size = 10) +
      labs(x = "", y = "", title = "Histogram") +
      theme(plot.title = element_text(hjust = 0.5, size = 10))
  })

  # Boxplot - rozkład normalny
  output$norm_base_box_ok <- renderPlot({
    set.seed(123)
    df <- data.frame(value = rnorm(100, mean = 50, sd = 10))
    mean_val <- mean(df$value)

    ggplot(df, aes(x = "", y = value)) +
      geom_boxplot(fill = "#3498db", alpha = 0.6) +
      geom_point(aes(y = mean_val), color = "#e74c3c", size = 4, shape = 18) +
      theme_minimal(base_size = 10) +
      labs(x = "", y = "", title = "Boxplot") +
      theme(plot.title = element_text(hjust = 0.5, size = 10)) +
      annotate("text", x = 1.3, y = mean_val, label = "Średnia", color = "#e74c3c", size = 3)
  })

  # Średnia i mediana - normalny
  output$norm_base_mean_ok <- renderText({
    set.seed(123)
    round(mean(rnorm(100, mean = 50, sd = 10)), 1)
  })

  output$norm_base_median_ok <- renderText({
    set.seed(123)
    round(median(rnorm(100, mean = 50, sd = 10)), 1)
  })

  # Histogram - rozkład skośny
  output$norm_base_hist_bad <- renderPlot({
    set.seed(123)
    df <- data.frame(value = rgamma(100, shape = 2, scale = 15))
    mean_val <- mean(df$value)
    median_val <- median(df$value)

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 15, fill = "#e74c3c", alpha = 0.6, color = "#c0392b") +
      geom_vline(xintercept = mean_val, color = "#e74c3c", size = 1.5, linetype = "solid") +
      geom_vline(xintercept = median_val, color = "#27ae60", size = 1.5, linetype = "dashed") +
      theme_minimal(base_size = 10) +
      labs(x = "", y = "", title = "Histogram") +
      theme(plot.title = element_text(hjust = 0.5, size = 10))
  })

  # Boxplot - rozkład skośny
  output$norm_base_box_bad <- renderPlot({
    set.seed(123)
    df <- data.frame(value = rgamma(100, shape = 2, scale = 15))
    mean_val <- mean(df$value)

    ggplot(df, aes(x = "", y = value)) +
      geom_boxplot(fill = "#e74c3c", alpha = 0.6) +
      geom_point(aes(y = mean_val), color = "#e74c3c", size = 4, shape = 18) +
      theme_minimal(base_size = 10) +
      labs(x = "", y = "", title = "Boxplot") +
      theme(plot.title = element_text(hjust = 0.5, size = 10)) +
      annotate("text", x = 1.3, y = mean_val, label = "Średnia", color = "#e74c3c", size = 3)
  })

  # Średnia i mediana - skośny
  output$norm_base_mean_bad <- renderText({
    set.seed(123)
    round(mean(rgamma(100, shape = 2, scale = 15)), 1)
  })

  output$norm_base_median_bad <- renderText({
    set.seed(123)
    round(median(rgamma(100, shape = 2, scale = 15)), 1)
  })

  # Demonstracja outliera
  output$norm_base_outlier_demo <- renderPlot({
    df <- data.frame(
      value = c(10, 12, 11, 13, 10, 12, 100),
      label = c(rep("Normalne", 6), "Outlier")
    )
    mean_val <- mean(df$value)
    median_val <- median(df$value)

    ggplot(df, aes(x = value, y = 0)) +
      geom_point(aes(color = label), size = 6, alpha = 0.8) +
      geom_vline(xintercept = mean_val, color = "#e74c3c", size = 2, linetype = "solid") +
      geom_vline(xintercept = median_val, color = "#27ae60", size = 2, linetype = "dashed") +
      scale_color_manual(values = c("Normalne" = "#3498db", "Outlier" = "#e74c3c")) +
      theme_minimal(base_size = 14) +
      labs(x = "Wartość", y = "", color = "",
           title = "Czerwona = Średnia (24), Zielona = Mediana (12)") +
      theme(legend.position = "bottom",
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      xlim(0, 110)
  })

  # ==========================================================================
  # WYKRESY "PROBLEM U PODSTAWY" - JEDNORODNOŚĆ WARIANCJI (SEM)
  # ==========================================================================

  # SEM przy równych wariancjach - obie grupy mają podobną precyzję
  output$var_base_sem_equal <- renderPlot({
    # Obie grupy: n=20, SD=10 → SEM = 10/√20 = 2.24
    set.seed(42)
    df <- data.frame(
      grupa = factor(c(rep("Grupa A", 20), rep("Grupa B", 20))),
      value = c(rnorm(20, 50, 10), rnorm(20, 55, 10))
    )

    stats <- df %>%
      group_by(grupa) %>%
      summarise(
        mean = mean(value),
        sem = sd(value) / sqrt(n()),
        .groups = "drop"
      )

    ggplot(stats, aes(x = grupa, y = mean, fill = grupa)) +
      geom_bar(stat = "identity", alpha = 0.7, width = 0.6) +
      geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.2, size = 1) +
      scale_fill_manual(values = c("#3498db", "#27ae60")) +
      theme_minimal(base_size = 12) +
      labs(x = "", y = "Wartość",
           title = "Równe SD = równa precyzja (SEM)") +
      theme(legend.position = "none") +
      geom_text(aes(label = paste0("SEM = ", round(sem, 1))),
                vjust = -1.5, size = 4) +
      coord_cartesian(ylim = c(0, 70))
  })

  # SEM przy nierównych wariancjach - różna precyzja grup
  output$var_base_sem_unequal <- renderPlot({
    # Grupa A: n=20, SD=5 → SEM = 5/√20 = 1.12
    # Grupa B: n=20, SD=20 → SEM = 20/√20 = 4.47
    set.seed(42)
    df <- data.frame(
      grupa = factor(c(rep("Grupa A\n(SD=5)", 20), rep("Grupa B\n(SD=20)", 20))),
      value = c(rnorm(20, 50, 5), rnorm(20, 55, 20))
    )

    stats <- df %>%
      group_by(grupa) %>%
      summarise(
        mean = mean(value),
        sem = sd(value) / sqrt(n()),
        .groups = "drop"
      )

    ggplot(stats, aes(x = grupa, y = mean, fill = grupa)) +
      geom_bar(stat = "identity", alpha = 0.7, width = 0.6) +
      geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.2, size = 1) +
      scale_fill_manual(values = c("#3498db", "#e74c3c")) +
      theme_minimal(base_size = 12) +
      labs(x = "", y = "Wartość",
           title = "Różne SD = różna precyzja!") +
      theme(legend.position = "none") +
      geom_text(aes(label = paste0("SEM = ", round(sem, 1))),
                vjust = -1.5, size = 4) +
      coord_cartesian(ylim = c(0, 80))
  })

  # Rozkład próbkowy średnich - wizualizacja niepewności
  output$var_base_sampling_dist <- renderPlot({
    # Symulacja: Wyobraź sobie powtórzenie eksperymentu 1000 razy
    set.seed(123)

    # Grupa A: mała wariancja → wąski rozkład średnich
    means_A <- replicate(1000, mean(rnorm(20, 50, 5)))
    # Grupa B: duża wariancja → szeroki rozkład średnich
    means_B <- replicate(1000, mean(rnorm(20, 55, 20)))

    df <- data.frame(
      mean = c(means_A, means_B),
      grupa = factor(rep(c("Grupa A (SD=5)", "Grupa B (SD=20)"), each = 1000))
    )

    ggplot(df, aes(x = mean, fill = grupa)) +
      geom_density(alpha = 0.5, color = NA) +
      scale_fill_manual(values = c("#3498db", "#e74c3c")) +
      theme_minimal(base_size = 14) +
      labs(x = "Średnia z próby", y = "Gęstość", fill = "",
           title = "Rozkład próbkowy: jak bardzo 'skacze' średnia między próbami") +
      theme(legend.position = "bottom") +
      geom_vline(xintercept = 50, color = "#3498db", linetype = "dashed", size = 1) +
      geom_vline(xintercept = 55, color = "#e74c3c", linetype = "dashed", size = 1) +
      annotate("text", x = 50, y = 0.35, label = "μ=50", color = "#3498db", hjust = 1.2, size = 4) +
      annotate("text", x = 55, y = 0.35, label = "μ=55", color = "#e74c3c", hjust = -0.2, size = 4)
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
      geom_smooth(method = "lm", se = FALSE, color = "#3498db", size = 1.2) +
      geom_smooth(data = df[!df$outlier, ], method = "lm", se = FALSE,
                  color = "#27ae60", linetype = "dashed", size = 1.2) +
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
      geom_smooth(method = "lm", se = FALSE, color = "#3498db", size = 1.2) +
      geom_smooth(data = df[!df$outlier, ], method = "lm", se = FALSE,
                  color = "#27ae60", linetype = "dashed", size = 1.2) +
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
                  color = "#27ae60", size = 1.5, linetype = "solid") +
      geom_abline(intercept = slopes$intercept[2], slope = slopes$slope[2],
                  color = "#f39c12", size = 1.2, linetype = "dashed") +
      geom_abline(intercept = slopes$intercept[3], slope = slopes$slope[3],
                  color = "#e74c3c", size = 1.2, linetype = "dotted") +
      theme_minimal(base_size = 14) +
      labs(x = "X", y = "Y",
           title = paste0("Slope: bez=", round(slopes$slope[1], 2),
                         ", centrum=", round(slopes$slope[2], 2),
                         ", skraj=", round(slopes$slope[3], 2))) +
      annotate("text", x = 52, y = 205, label = "Outlier\nw centrum", color = "#f39c12", size = 3) +
      annotate("text", x = 92, y = 85, label = "Outlier\nna skraju", color = "#e74c3c", size = 3)
  })

  # ==========================================================================
  # WYKRESY "PROBLEM U PODSTAWY" - HETEROSKEDASTYCZNOŚĆ
  # ==========================================================================

  # CI przy homoskedastyczności
  output$hetero_ci_ok_plot <- renderPlot({
    set.seed(42)
    x <- seq(10, 100, length.out = 50)
    y <- 2 * x + 50 + rnorm(50, 0, 15)
    df <- data.frame(x = x, y = y)

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = "#3498db", size = 2, alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "#27ae60", fill = "#27ae60", alpha = 0.3) +
      theme_minimal(base_size = 12) +
      labs(x = "X", y = "Y", title = "Stały rozrzut = CI prawidłowy") +
      theme(plot.title = element_text(hjust = 0.5))
  })

  # CI przy heteroskedastyczności
  output$hetero_ci_bad_plot <- renderPlot({
    set.seed(42)
    x <- seq(10, 100, length.out = 50)
    y <- 2 * x + 50 + rnorm(50, 0, x * 0.3)
    df <- data.frame(x = x, y = y)

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = "#e74c3c", size = 2, alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "#e74c3c", fill = "#e74c3c", alpha = 0.3) +
      theme_minimal(base_size = 12) +
      labs(x = "X", y = "Y", title = "Rosnący rozrzut = CI za wąski z prawej!") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
