# Interaktywna aplikacja Shiny - Sampling Explorer
# Aplikacja do samplowania rozkładów i analizy prawdopodobieństw empirycznych

library(shiny)
library(ggplot2)
library(dplyr)

# UI aplikacji
ui <- fluidPage(
  titlePanel("Sampling Explorer - Próbkowanie rozkładów"),

  sidebarLayout(
    sidebarPanel(
      selectInput("rozklad", "Wybierz rozkład:",
                  choices = c("--- DYSKRETNE ---" = "",
                              "Dwumianowy (dyskretny)" = "binom",
                              "Jednostajny (dyskretny)" = "unif_discrete",
                              "Poissona (dyskretny)" = "pois",
                              "--- CIĄGŁE ---" = "",
                              "Beta (ciągły)" = "beta",
                              "Chi-kwadrat (ciągły)" = "chisq",
                              "Gamma (ciągły)" = "gamma",
                              "Jednostajny (ciągły)" = "unif",
                              "Normalny (ciągły)" = "norm",
                              "t-Studenta (ciągły)" = "t",
                              "Wykładniczy (ciągły)" = "exp"),
                  selected = "binom"),

      hr(),

      # Parametry dla rozkładu normalnego
      conditionalPanel(
        condition = "input.rozklad == 'norm'",
        numericInput("norm_mean", "Średnia (μ):", value = 0, step = 0.5),
        numericInput("norm_sd", "Odchylenie standardowe (σ):",
                    value = 1, min = 0.1, step = 0.1)
      ),

      # Parametry dla rozkładu wykładniczego
      conditionalPanel(
        condition = "input.rozklad == 'exp'",
        numericInput("exp_rate", "Współczynnik λ (rate):",
                    value = 1, min = 0.01, step = 0.1)
      ),

      # Parametry dla rozkładu Gamma
      conditionalPanel(
        condition = "input.rozklad == 'gamma'",
        numericInput("gamma_shape", "Kształt (shape):",
                    value = 2, min = 0.1, step = 0.1),
        numericInput("gamma_rate", "Skala (rate):",
                    value = 1, min = 0.1, step = 0.1)
      ),

      # Parametry dla rozkładu Beta
      conditionalPanel(
        condition = "input.rozklad == 'beta'",
        numericInput("beta_shape1", "α (shape1):",
                    value = 2, min = 0.1, step = 0.1),
        numericInput("beta_shape2", "β (shape2):",
                    value = 2, min = 0.1, step = 0.1)
      ),

      # Parametry dla rozkładu t-Studenta
      conditionalPanel(
        condition = "input.rozklad == 't'",
        numericInput("t_df", "Stopnie swobody (df):",
                    value = 5, min = 1, step = 1)
      ),

      # Parametry dla rozkładu Chi-kwadrat
      conditionalPanel(
        condition = "input.rozklad == 'chisq'",
        numericInput("chisq_df", "Stopnie swobody (df):",
                    value = 5, min = 1, step = 1)
      ),

      # Parametry dla rozkładu jednostajnego ciągłego
      conditionalPanel(
        condition = "input.rozklad == 'unif'",
        numericInput("unif_min", "Minimum:", value = 0, step = 0.5),
        numericInput("unif_max", "Maximum:", value = 1, step = 0.5)
      ),

      # Parametry dla rozkładu jednostajnego dyskretnego
      conditionalPanel(
        condition = "input.rozklad == 'unif_discrete'",
        numericInput("unif_discrete_min", "Minimum (całkowite):", value = 1, step = 1),
        numericInput("unif_discrete_max", "Maximum (całkowite):", value = 6, step = 1)
      ),

      # Parametry dla rozkładu Poissona
      conditionalPanel(
        condition = "input.rozklad == 'pois'",
        numericInput("pois_lambda", "λ (lambda):",
                    value = 5, min = 0.1, step = 0.5)
      ),

      # Parametry dla rozkładu dwumianowego
      conditionalPanel(
        condition = "input.rozklad == 'binom'",
        numericInput("binom_n", "Liczba prób w eksperymencie (n):",
                    value = 20, min = 1, step = 1),
        numericInput("binom_p", "Prawdopodobieństwo sukcesu (p):",
                    value = 0.5, min = 0, max = 1, step = 0.05)
      ),

      hr(),

      numericInput("sample_n", "Rozmiar próby (liczba losowań):",
                  value = 1000, min = 10, max = 100000, step = 100),

      actionButton("generate", "Generuj próbkę",
                  class = "btn-success btn-lg", width = "100%"),

      hr(),

      h4("Oblicz prawdopodobieństwo"),
      radioButtons("prob_type", "Typ prawdopodobieństwa:",
                  choices = c("P(X ≤ k)" = "leq",
                              "P(X > k) = 1 - P(X ≤ k)" = "greater",
                              "P(k1 < X ≤ k2)" = "between"),
                  selected = "leq"),

      conditionalPanel(
        condition = "input.prob_type != 'between'",
        numericInput("k_value", "Wartość k:", value = 0, step = 0.1)
      ),

      conditionalPanel(
        condition = "input.prob_type == 'between'",
        numericInput("k1_value", "Wartość k1 (dolna):", value = -1, step = 0.1),
        numericInput("k2_value", "Wartość k2 (górna):", value = 1, step = 0.1)
      ),

      hr(),

      checkboxInput("show_theoretical", "Pokaż rozkład teoretyczny (overlay)",
                   value = TRUE)
    ),

    mainPanel(
      plotOutput("samplePlot", height = "500px"),

      hr(),

      h4("Statystyki empiryczne z próby:"),
      tableOutput("sample_stats"),

      hr(),

      div(
        style = "background-color: #e8f4f8; padding: 15px; border-radius: 5px; border: 2px solid #3498db;",
        h4("Obliczone prawdopodobieństwo:", style = "color: #2c3e50;"),
        verbatimTextOutput("probability_result")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive values dla próbki
  sample_data <- reactiveVal(NULL)

  # Generowanie próbki
  observeEvent(input$generate, {
    n <- input$sample_n

    sample <- switch(input$rozklad,
      "norm" = rnorm(n, mean = input$norm_mean, sd = input$norm_sd),
      "exp" = rexp(n, rate = input$exp_rate),
      "gamma" = rgamma(n, shape = input$gamma_shape, rate = input$gamma_rate),
      "beta" = rbeta(n, shape1 = input$beta_shape1, shape2 = input$beta_shape2),
      "t" = rt(n, df = input$t_df),
      "chisq" = rchisq(n, df = input$chisq_df),
      "unif" = runif(n, min = input$unif_min, max = input$unif_max),
      "unif_discrete" = sample(input$unif_discrete_min:input$unif_discrete_max,
                               size = n, replace = TRUE),
      "pois" = rpois(n, lambda = input$pois_lambda),
      "binom" = rbinom(n, size = input$binom_n, prob = input$binom_p)
    )

    sample_data(sample)
  })

  # Funkcja określająca czy rozkład jest dyskretny
  is_discrete <- reactive({
    input$rozklad %in% c("pois", "binom", "unif_discrete")
  })

  # Dane teoretyczne dla overlay
  theoretical_data <- reactive({
    req(sample_data())

    if (is_discrete()) {
      # Rozkłady dyskretne
      if (input$rozklad == "pois") {
        x <- 0:max(50, max(sample_data()) + 5)
        y <- dpois(x, lambda = input$pois_lambda)
        # Skalujemy do liczby obserwacji
        y_scaled <- y * input$sample_n
        data.frame(x = x, y = y_scaled)
      } else if (input$rozklad == "binom") {
        x <- 0:input$binom_n
        y <- dbinom(x, size = input$binom_n, prob = input$binom_p)
        y_scaled <- y * input$sample_n
        data.frame(x = x, y = y_scaled)
      } else if (input$rozklad == "unif_discrete") {
        x <- input$unif_discrete_min:input$unif_discrete_max
        # Dla rozkładu jednostajnego dyskretnego wszystkie wartości równie prawdopodobne
        n_values <- length(x)
        y <- rep(1/n_values, n_values)
        y_scaled <- y * input$sample_n
        data.frame(x = x, y = y_scaled)
      }
    } else {
      # Rozkłady ciągłe
      sample_range <- range(sample_data())
      margin <- diff(sample_range) * 0.2
      x <- seq(sample_range[1] - margin, sample_range[2] + margin, length.out = 500)

      y <- switch(input$rozklad,
        "norm" = dnorm(x, mean = input$norm_mean, sd = input$norm_sd),
        "exp" = dexp(x, rate = input$exp_rate),
        "gamma" = dgamma(x, shape = input$gamma_shape, rate = input$gamma_rate),
        "beta" = dbeta(x, shape1 = input$beta_shape1, shape2 = input$beta_shape2),
        "t" = dt(x, df = input$t_df),
        "chisq" = dchisq(x, df = input$chisq_df),
        "unif" = dunif(x, min = input$unif_min, max = input$unif_max)
      )

      # Skalujemy do histogramu
      # Używamy 30 bins
      bin_width <- diff(sample_range) / 30
      y_scaled <- y * input$sample_n * bin_width

      data.frame(x = x, y = y_scaled)
    }
  })

  # Wykres
  output$samplePlot <- renderPlot({
    req(sample_data())

    df <- data.frame(value = sample_data())

    dist_name <- switch(input$rozklad,
      "binom" = "Dwumianowego (dyskretny)",
      "unif_discrete" = "Jednostajnego (dyskretny)",
      "pois" = "Poissona (dyskretny)",
      "beta" = "Beta (ciągły)",
      "chisq" = "Chi-kwadrat (ciągły)",
      "gamma" = "Gamma (ciągły)",
      "unif" = "Jednostajnego (ciągły)",
      "norm" = "Normalnego (ciągły)",
      "t" = "t-Studenta (ciągły)",
      "exp" = "Wykładniczego (ciągły)"
    )

    if (is_discrete()) {
      # Histogram dla rozkładów dyskretnych
      p <- ggplot(df, aes(x = value)) +
        geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7,
                      color = "#2c3e50", center = 0) +
        labs(title = paste("Rozkład empiryczny -", dist_name,
                          "(n =", input$sample_n, ")"),
             x = "Wartość", y = "Liczba obserwacji") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))

      # Overlay teoretyczny
      if (input$show_theoretical) {
        theo_df <- theoretical_data()
        p <- p +
          geom_point(data = theo_df, aes(x = x, y = y),
                    color = "#e74c3c", size = 3, alpha = 0.7) +
          geom_line(data = theo_df, aes(x = x, y = y),
                   color = "#e74c3c", size = 1, alpha = 0.5)
      }

    } else {
      # Histogram dla rozkładów ciągłych
      p <- ggplot(df, aes(x = value)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7,
                      color = "#2c3e50") +
        labs(title = paste("Rozkład empiryczny -", dist_name,
                          "(n =", input$sample_n, ")"),
             x = "Wartość", y = "Liczba obserwacji") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))

      # Overlay teoretyczny
      if (input$show_theoretical) {
        theo_df <- theoretical_data()
        p <- p +
          geom_area(data = theo_df, aes(x = x, y = y),
                   fill = "#e74c3c", alpha = 0.3, color = "#c0392b", size = 1)
      }
    }

    # Dodaj linie dla k lub k1/k2
    if (input$prob_type == "between") {
      k1 <- input$k1_value
      k2 <- input$k2_value

      p <- p +
        geom_vline(xintercept = k1, color = "#27ae60", size = 1.5,
                  linetype = "dashed") +
        geom_vline(xintercept = k2, color = "#27ae60", size = 1.5,
                  linetype = "dashed") +
        annotate("text", x = k1, y = Inf, label = paste("k1 =", k1),
                vjust = 1.5, hjust = -0.1, color = "#27ae60", size = 5,
                fontface = "bold") +
        annotate("text", x = k2, y = Inf, label = paste("k2 =", k2),
                vjust = 3, hjust = -0.1, color = "#27ae60", size = 5,
                fontface = "bold")
    } else {
      k <- input$k_value
      p <- p +
        geom_vline(xintercept = k, color = "#27ae60", size = 1.5,
                  linetype = "dashed") +
        annotate("text", x = k, y = Inf, label = paste("k =", k),
                  vjust = 1.5, hjust = -0.1, color = "#27ae60", size = 5,
                  fontface = "bold")
    }

    p
  })

  # Statystyki empiryczne
  output$sample_stats <- renderTable({
    req(sample_data())

    data.frame(
      Statystyka = c("Średnia empiryczna", "Wariancja empiryczna",
                     "Odch. std empiryczne", "Minimum", "Maksimum",
                     "Mediana", "Q1 (25%)", "Q3 (75%)"),
      Wartość = c(
        round(mean(sample_data()), 4),
        round(var(sample_data()), 4),
        round(sd(sample_data()), 4),
        round(min(sample_data()), 4),
        round(max(sample_data()), 4),
        round(median(sample_data()), 4),
        round(quantile(sample_data(), 0.25), 4),
        round(quantile(sample_data(), 0.75), 4)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Obliczanie prawdopodobieństwa
  output$probability_result <- renderText({
    req(sample_data())

    sample <- sample_data()
    n <- length(sample)

    if (input$prob_type == "between") {
      k1 <- input$k1_value
      k2 <- input$k2_value

      # Sprawdź czy k2 > k1
      if (k2 <= k1) {
        return("BŁĄD: k2 musi być większe od k1!")
      }

      # Prawdopodobieństwo empiryczne - P(k1 < X <= k2)
      empirical_prob <- sum(sample > k1 & sample <= k2) / n

      # Prawdopodobieństwo teoretyczne
      theoretical_prob <- switch(input$rozklad,
        "norm" = pnorm(k2, mean = input$norm_mean, sd = input$norm_sd) -
                 pnorm(k1, mean = input$norm_mean, sd = input$norm_sd),
        "exp" = pexp(k2, rate = input$exp_rate) -
                pexp(k1, rate = input$exp_rate),
        "gamma" = pgamma(k2, shape = input$gamma_shape, rate = input$gamma_rate) -
                  pgamma(k1, shape = input$gamma_shape, rate = input$gamma_rate),
        "beta" = pbeta(k2, shape1 = input$beta_shape1, shape2 = input$beta_shape2) -
                 pbeta(k1, shape1 = input$beta_shape1, shape2 = input$beta_shape2),
        "t" = pt(k2, df = input$t_df) - pt(k1, df = input$t_df),
        "chisq" = pchisq(k2, df = input$chisq_df) - pchisq(k1, df = input$chisq_df),
        "unif" = punif(k2, min = input$unif_min, max = input$unif_max) -
                 punif(k1, min = input$unif_min, max = input$unif_max),
        "unif_discrete" = {
          min_val <- input$unif_discrete_min
          max_val <- input$unif_discrete_max
          n_values <- max_val - min_val + 1
          # Liczba wartości w przedziale (k1, k2]
          count <- sum((min_val:max_val) > k1 & (min_val:max_val) <= k2)
          count / n_values
        },
        "pois" = ppois(k2, lambda = input$pois_lambda) -
                 ppois(k1, lambda = input$pois_lambda),
        "binom" = pbinom(k2, size = input$binom_n, prob = input$binom_p) -
                  pbinom(k1, size = input$binom_n, prob = input$binom_p)
      )

      prob_label <- paste0("P(", k1, " < X ≤ ", k2, ")")

      paste0(
        prob_label, "\n\n",
        "Empiryczne (z próby):     ", round(empirical_prob, 6),
        "  (", round(empirical_prob * 100, 2), "%)\n",
        "Teoretyczne (z rozkładu): ", round(theoretical_prob, 6),
        "  (", round(theoretical_prob * 100, 2), "%)\n",
        "Różnica:                  ", round(abs(empirical_prob - theoretical_prob), 6)
      )

    } else {
      k <- input$k_value

      # Prawdopodobieństwo empiryczne
      empirical_prob <- switch(input$prob_type,
        "leq" = sum(sample <= k) / n,
        "greater" = sum(sample > k) / n
      )

      # Prawdopodobieństwo teoretyczne
      theoretical_prob <- switch(input$rozklad,
        "norm" = switch(input$prob_type,
          "leq" = pnorm(k, mean = input$norm_mean, sd = input$norm_sd),
          "greater" = 1 - pnorm(k, mean = input$norm_mean, sd = input$norm_sd)
        ),
        "exp" = switch(input$prob_type,
          "leq" = pexp(k, rate = input$exp_rate),
          "greater" = 1 - pexp(k, rate = input$exp_rate)
        ),
        "gamma" = switch(input$prob_type,
          "leq" = pgamma(k, shape = input$gamma_shape, rate = input$gamma_rate),
          "greater" = 1 - pgamma(k, shape = input$gamma_shape, rate = input$gamma_rate)
        ),
        "beta" = switch(input$prob_type,
          "leq" = pbeta(k, shape1 = input$beta_shape1, shape2 = input$beta_shape2),
          "greater" = 1 - pbeta(k, shape1 = input$beta_shape1, shape2 = input$beta_shape2)
        ),
        "t" = switch(input$prob_type,
          "leq" = pt(k, df = input$t_df),
          "greater" = 1 - pt(k, df = input$t_df)
        ),
        "chisq" = switch(input$prob_type,
          "leq" = pchisq(k, df = input$chisq_df),
          "greater" = 1 - pchisq(k, df = input$chisq_df)
        ),
        "unif" = switch(input$prob_type,
          "leq" = punif(k, min = input$unif_min, max = input$unif_max),
          "greater" = 1 - punif(k, min = input$unif_min, max = input$unif_max)
        ),
        "unif_discrete" = {
          min_val <- input$unif_discrete_min
          max_val <- input$unif_discrete_max
          n_values <- max_val - min_val + 1
          switch(input$prob_type,
            "leq" = sum((min_val:max_val) <= k) / n_values,
            "greater" = sum((min_val:max_val) > k) / n_values
          )
        },
        "pois" = switch(input$prob_type,
          "leq" = ppois(k, lambda = input$pois_lambda),
          "greater" = 1 - ppois(k, lambda = input$pois_lambda)
        ),
        "binom" = switch(input$prob_type,
          "leq" = pbinom(k, size = input$binom_n, prob = input$binom_p),
          "greater" = 1 - pbinom(k, size = input$binom_n, prob = input$binom_p)
        )
      )

      prob_label <- switch(input$prob_type,
        "leq" = paste0("P(X ≤ ", k, ")"),
        "greater" = paste0("P(X > ", k, ") = 1 - P(X ≤ ", k, ")")
      )

      paste0(
        prob_label, "\n\n",
        "Empiryczne (z próby):     ", round(empirical_prob, 6),
        "  (", round(empirical_prob * 100, 2), "%)\n",
        "Teoretyczne (z rozkładu): ", round(theoretical_prob, 6),
        "  (", round(theoretical_prob * 100, 2), "%)\n",
        "Różnica:                  ", round(abs(empirical_prob - theoretical_prob), 6)
      )
    }
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
