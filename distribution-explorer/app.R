# Interaktywna aplikacja Shiny - Rozkłady prawdopodobieństwa
# Aplikacja do wizualizacji i analizy różnych rozkładów prawdopodobieństwa

library(shiny)
library(ggplot2)

# UI aplikacji
ui <- fluidPage(
  titlePanel("Interaktywne rozkłady prawdopodobieństwa"),

  sidebarLayout(
    sidebarPanel(
      selectInput("rozklad", "Wybierz rozkład:",
                  choices = c("Normalny" = "norm",
                              "Wykładniczy" = "exp",
                              "Gamma" = "gamma",
                              "Beta" = "beta",
                              "t-Studenta" = "t",
                              "Chi-kwadrat" = "chisq",
                              "Jednostajny" = "unif",
                              "Poissona" = "pois",
                              "Dwumianowy" = "binom"),
                  selected = "norm"),

      hr(),

      # Parametry dla rozkładu normalnego
      conditionalPanel(
        condition = "input.rozklad == 'norm'",
        sliderInput("norm_mean", "Średnia (μ):",
                    min = -10, max = 10, value = 0, step = 0.5),
        sliderInput("norm_sd", "Odchylenie standardowe (σ):",
                    min = 0.1, max = 5, value = 1, step = 0.1)
      ),

      # Parametry dla rozkładu wykładniczego
      conditionalPanel(
        condition = "input.rozklad == 'exp'",
        sliderInput("exp_rate", "Współczynnik λ (rate):",
                    min = 0.1, max = 5, value = 1, step = 0.1)
      ),

      # Parametry dla rozkładu Gamma
      conditionalPanel(
        condition = "input.rozklad == 'gamma'",
        sliderInput("gamma_shape", "Kształt (shape):",
                    min = 0.1, max = 10, value = 2, step = 0.1),
        sliderInput("gamma_rate", "Skala (rate):",
                    min = 0.1, max = 5, value = 1, step = 0.1)
      ),

      # Parametry dla rozkładu Beta
      conditionalPanel(
        condition = "input.rozklad == 'beta'",
        sliderInput("beta_shape1", "α (shape1):",
                    min = 0.1, max = 10, value = 2, step = 0.1),
        sliderInput("beta_shape2", "β (shape2):",
                    min = 0.1, max = 10, value = 2, step = 0.1)
      ),

      # Parametry dla rozkładu t-Studenta
      conditionalPanel(
        condition = "input.rozklad == 't'",
        sliderInput("t_df", "Stopnie swobody (df):",
                    min = 1, max = 30, value = 5, step = 1)
      ),

      # Parametry dla rozkładu Chi-kwadrat
      conditionalPanel(
        condition = "input.rozklad == 'chisq'",
        sliderInput("chisq_df", "Stopnie swobody (df):",
                    min = 1, max = 30, value = 5, step = 1)
      ),

      # Parametry dla rozkładu jednostajnego
      conditionalPanel(
        condition = "input.rozklad == 'unif'",
        sliderInput("unif_min", "Minimum:",
                    min = -10, max = 10, value = 0, step = 0.5),
        sliderInput("unif_max", "Maximum:",
                    min = -10, max = 10, value = 1, step = 0.5)
      ),

      # Parametry dla rozkładu Poissona
      conditionalPanel(
        condition = "input.rozklad == 'pois'",
        sliderInput("pois_lambda", "λ (lambda):",
                    min = 0.1, max = 20, value = 5, step = 0.5)
      ),

      # Parametry dla rozkładu dwumianowego
      conditionalPanel(
        condition = "input.rozklad == 'binom'",
        sliderInput("binom_n", "Liczba prób (n):",
                    min = 1, max = 100, value = 20, step = 1),
        sliderInput("binom_p", "Prawdopodobieństwo sukcesu (p):",
                    min = 0, max = 1, value = 0.5, step = 0.05)
      ),

      hr(),
      checkboxInput("show_stats", "Pokaż statystyki", value = TRUE)
    ),

    mainPanel(
      plotOutput("distPlot", height = "500px"),
      hr(),
      conditionalPanel(
        condition = "input.show_stats",
        h4("Statystyki rozkładu:"),
        verbatimTextOutput("stats")
      )
    )
  )
)

# Server
server <- function(input, output) {

  # Funkcja generująca dane dla wykresu
  dist_data <- reactive({

    if (input$rozklad %in% c("pois", "binom")) {
      # Rozkłady dyskretne
      if (input$rozklad == "pois") {
        x <- 0:50
        y <- dpois(x, lambda = input$pois_lambda)
        list(x = x, y = y, type = "discrete", xlim = c(-1, 51), ylim = c(0, 0.25))
      } else if (input$rozklad == "binom") {
        x <- 0:100
        y <- dbinom(x, size = input$binom_n, prob = input$binom_p)
        list(x = x, y = y, type = "discrete", xlim = c(-2, 102), ylim = c(0, 0.25))
      }
    } else {
      # Rozkłady ciągłe
      if (input$rozklad == "norm") {
        x <- seq(-15, 15, length.out = 500)
        y <- dnorm(x, mean = input$norm_mean, sd = input$norm_sd)
        list(x = x, y = y, type = "continuous", xlim = c(-15, 15), ylim = c(0, 0.8))
      } else if (input$rozklad == "exp") {
        x <- seq(0, 15, length.out = 500)
        y <- dexp(x, rate = input$exp_rate)
        list(x = x, y = y, type = "continuous", xlim = c(0, 15), ylim = c(0, 5))
      } else if (input$rozklad == "gamma") {
        x <- seq(0, 20, length.out = 500)
        y <- dgamma(x, shape = input$gamma_shape, rate = input$gamma_rate)
        list(x = x, y = y, type = "continuous", xlim = c(0, 20), ylim = c(0, 1))
      } else if (input$rozklad == "beta") {
        x <- seq(0, 1, length.out = 500)
        y <- dbeta(x, shape1 = input$beta_shape1, shape2 = input$beta_shape2)
        list(x = x, y = y, type = "continuous", xlim = c(0, 1), ylim = c(0, 4))
      } else if (input$rozklad == "t") {
        x <- seq(-6, 6, length.out = 500)
        y <- dt(x, df = input$t_df)
        list(x = x, y = y, type = "continuous", xlim = c(-6, 6), ylim = c(0, 0.5))
      } else if (input$rozklad == "chisq") {
        x <- seq(0, 50, length.out = 500)
        y <- dchisq(x, df = input$chisq_df)
        list(x = x, y = y, type = "continuous", xlim = c(0, 50), ylim = c(0, 0.5))
      } else if (input$rozklad == "unif") {
        x <- seq(-12, 12, length.out = 500)
        y <- dunif(x, min = input$unif_min, max = input$unif_max)
        list(x = x, y = y, type = "continuous", xlim = c(-12, 12), ylim = c(0, 1.2))
      }
    }
  })

  # Generowanie wykresu
  output$distPlot <- renderPlot({
    data <- dist_data()

    if (data$type == "discrete") {
      # Wykres słupkowy dla rozkładów dyskretnych
      df <- data.frame(x = data$x, y = data$y)
      ggplot(df, aes(x = x, y = y)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
        labs(title = paste("Rozkład",
                          switch(input$rozklad,
                                 "pois" = "Poissona",
                                 "binom" = "Dwumianowy")),
             x = "Wartość", y = "Prawdopodobieństwo") +
        coord_cartesian(xlim = data$xlim, ylim = data$ylim) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    } else {
      # Wykres liniowy dla rozkładów ciągłych
      df <- data.frame(x = data$x, y = data$y)
      ggplot(df, aes(x = x, y = y)) +
        geom_line(color = "steelblue", size = 1.2) +
        geom_area(fill = "steelblue", alpha = 0.3) +
        labs(title = paste("Rozkład",
                          switch(input$rozklad,
                                 "norm" = "Normalny",
                                 "exp" = "Wykładniczy",
                                 "gamma" = "Gamma",
                                 "beta" = "Beta",
                                 "t" = "t-Studenta",
                                 "chisq" = "Chi-kwadrat",
                                 "unif" = "Jednostajny")),
             x = "Wartość", y = "Gęstość prawdopodobieństwa") +
        coord_cartesian(xlim = data$xlim, ylim = data$ylim) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })

  # Obliczanie i wyświetlanie statystyk
  output$stats <- renderText({
    stats_text <- ""

    if (input$rozklad == "norm") {
      mean_val <- input$norm_mean
      var_val <- input$norm_sd^2
      sd_val <- input$norm_sd
      stats_text <- paste0(
        "Średnia (E[X]): ", round(mean_val, 4), "\n",
        "Wariancja (Var[X]): ", round(var_val, 4), "\n",
        "Odchylenie standardowe: ", round(sd_val, 4)
      )
    } else if (input$rozklad == "exp") {
      mean_val <- 1 / input$exp_rate
      var_val <- 1 / input$exp_rate^2
      stats_text <- paste0(
        "Średnia (E[X]): ", round(mean_val, 4), "\n",
        "Wariancja (Var[X]): ", round(var_val, 4), "\n",
        "Odchylenie standardowe: ", round(sqrt(var_val), 4)
      )
    } else if (input$rozklad == "gamma") {
      mean_val <- input$gamma_shape / input$gamma_rate
      var_val <- input$gamma_shape / input$gamma_rate^2
      stats_text <- paste0(
        "Średnia (E[X]): ", round(mean_val, 4), "\n",
        "Wariancja (Var[X]): ", round(var_val, 4), "\n",
        "Odchylenie standardowe: ", round(sqrt(var_val), 4)
      )
    } else if (input$rozklad == "beta") {
      a <- input$beta_shape1
      b <- input$beta_shape2
      mean_val <- a / (a + b)
      var_val <- (a * b) / ((a + b)^2 * (a + b + 1))
      stats_text <- paste0(
        "Średnia (E[X]): ", round(mean_val, 4), "\n",
        "Wariancja (Var[X]): ", round(var_val, 4), "\n",
        "Odchylenie standardowe: ", round(sqrt(var_val), 4)
      )
    } else if (input$rozklad == "t") {
      df <- input$t_df
      mean_val <- if(df > 1) 0 else "Nieokreślona"
      var_val <- if(df > 2) df / (df - 2) else if(df > 1) "Nieskończona" else "Nieokreślona"
      stats_text <- paste0(
        "Średnia (E[X]): ", mean_val, "\n",
        "Wariancja (Var[X]): ", var_val
      )
    } else if (input$rozklad == "chisq") {
      df <- input$chisq_df
      mean_val <- df
      var_val <- 2 * df
      stats_text <- paste0(
        "Średnia (E[X]): ", round(mean_val, 4), "\n",
        "Wariancja (Var[X]): ", round(var_val, 4), "\n",
        "Odchylenie standardowe: ", round(sqrt(var_val), 4)
      )
    } else if (input$rozklad == "unif") {
      a <- input$unif_min
      b <- input$unif_max
      mean_val <- (a + b) / 2
      var_val <- (b - a)^2 / 12
      stats_text <- paste0(
        "Średnia (E[X]): ", round(mean_val, 4), "\n",
        "Wariancja (Var[X]): ", round(var_val, 4), "\n",
        "Odchylenie standardowe: ", round(sqrt(var_val), 4)
      )
    } else if (input$rozklad == "pois") {
      lambda <- input$pois_lambda
      stats_text <- paste0(
        "Średnia (E[X]): ", round(lambda, 4), "\n",
        "Wariancja (Var[X]): ", round(lambda, 4), "\n",
        "Odchylenie standardowe: ", round(sqrt(lambda), 4)
      )
    } else if (input$rozklad == "binom") {
      n <- input$binom_n
      p <- input$binom_p
      mean_val <- n * p
      var_val <- n * p * (1 - p)
      stats_text <- paste0(
        "Średnia (E[X]): ", round(mean_val, 4), "\n",
        "Wariancja (Var[X]): ", round(var_val, 4), "\n",
        "Odchylenie standardowe: ", round(sqrt(var_val), 4)
      )
    }

    stats_text
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
