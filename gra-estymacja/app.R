# 🎮 Gra w Estymację Statystyk
# Interaktywna gra do ćwiczenia rozpoznawania statystyk z wykresów

library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("🎮 Gra w Estymację Statystyk"),

  sidebarLayout(
    sidebarPanel(
      # Panel startowy
      conditionalPanel(
        condition = "output.game_state == 'start'",
        h3("Witaj w grze!"),
        p("Zgadniesz statystyki z histogramu?"),
        hr(),
        h4("Ustawienia gry"),
        selectInput("num_rounds", "Liczba rund:",
                    choices = c("3 rundy" = 3, "5 rund" = 5, "10 rund" = 10),
                    selected = 5),
        selectInput("difficulty", "Poziom trudności:",
                    choices = c("Łatwy" = "easy", "Średni" = "medium", "Trudny" = "hard"),
                    selected = "easy"),
        checkboxGroupInput("stats_to_guess", "Co chcesz zgadywać?",
                          choices = c("Średnia" = "mean",
                                     "Mediana" = "median",
                                     "Odchylenie standardowe" = "sd"),
                          selected = c("mean", "median")),
        hr(),
        actionButton("start_game", "🎮 START GRY", class = "btn-success btn-lg", width = "100%")
      ),

      # Panel gry
      conditionalPanel(
        condition = "output.game_state == 'playing'",
        h3(textOutput("round_info")),
        hr(),
        h4("Twoje estymaty:"),
        uiOutput("sliders"),
        br(),
        actionButton("check_answer", "✓ SPRAWDŹ", class = "btn-primary btn-lg", width = "100%")
      ),

      # Panel wyniku rundy
      conditionalPanel(
        condition = "output.game_state == 'round_result'",
        h3("Wynik rundy"),
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          uiOutput("round_results")
        ),
        hr(),
        h4(textOutput("points_text"), style = "color: #27ae60; font-weight: bold;"),
        br(),
        actionButton("next_round", "➡ NASTĘPNA RUNDA", class = "btn-info btn-lg", width = "100%")
      ),

      # Panel podsumowania
      conditionalPanel(
        condition = "output.game_state == 'summary'",
        h3("🏆 Podsumowanie gry"),
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
          h4(textOutput("final_score"), style = "color: #e74c3c;"),
          h4(textOutput("rating"), style = "color: #3498db;")
        ),
        hr(),
        verbatimTextOutput("all_rounds_summary"),
        br(),
        actionButton("play_again", "🔄 ZAGRAJ PONOWNIE", class = "btn-success btn-lg", width = "100%")
      ),

      width = 4
    ),

    mainPanel(
      # Histogram
      conditionalPanel(
        condition = "output.game_state != 'start'",
        plotOutput("histogram", height = "500px")
      ),

      # Ekran powitalny
      conditionalPanel(
        condition = "output.game_state == 'start'",
        div(
          style = "text-align: center; padding-top: 100px;",
          h1("🎮 Gra w Estymację", style = "color: #2c3e50;"),
          h3("Zgadnij statystyki z histogramu!", style = "color: #7f8c8d;"),
          br(),
          p("Ustaw opcje gry po lewej i kliknij START GRY", style = "font-size: 18px;")
        )
      ),

      width = 8
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive values
  game <- reactiveValues(
    state = "start",  # start, playing, round_result, summary
    current_round = 0,
    num_rounds = 5,
    difficulty = "easy",
    stats_to_guess = c("mean", "median"),
    current_data = NULL,
    estimates = list(),
    true_values = list(),
    points_per_round = numeric(),
    all_round_results = list()
  )

  # Funkcja generująca dane dla rundy
  generate_round_data <- function(difficulty) {
    if (difficulty == "easy") {
      # Bardzo różne rozkłady, łatwe do rozpoznania
      types <- c("normal_low", "normal_high", "uniform", "skewed_right", "bimodal")
      chosen <- sample(types, 1)

      if (chosen == "normal_low") {
        data <- rnorm(100, mean = 30, sd = 5)
      } else if (chosen == "normal_high") {
        data <- rnorm(100, mean = 70, sd = 8)
      } else if (chosen == "uniform") {
        data <- runif(100, min = 20, max = 80)
      } else if (chosen == "skewed_right") {
        data <- rgamma(100, shape = 2, scale = 10) + 20
      } else {  # bimodal
        data <- c(rnorm(50, mean = 30, sd = 5), rnorm(50, mean = 70, sd = 5))
      }

    } else if (difficulty == "medium") {
      # Subtelne różnice
      mean_val <- sample(40:60, 1)
      sd_val <- sample(c(5, 10, 15), 1)
      data <- rnorm(100, mean = mean_val, sd = sd_val)

    } else {  # hard
      # Bardzo podobne rozkłady
      mean_val <- sample(48:52, 1)
      sd_val <- sample(8:12, 1)
      data <- rnorm(100, mean = mean_val, sd = sd_val)
    }

    # Ogranicz zakres do 0-100
    data <- pmax(pmin(data, 100), 0)
    return(data)
  }

  # Stan gry (output)
  output$game_state <- reactive({
    game$state
  })
  outputOptions(output, "game_state", suspendWhenHidden = FALSE)

  # START GRY
  observeEvent(input$start_game, {
    req(length(input$stats_to_guess) > 0)  # Co najmniej jedna statystyka wybrana

    game$num_rounds <- as.numeric(input$num_rounds)
    game$difficulty <- input$difficulty
    game$stats_to_guess <- input$stats_to_guess
    game$current_round <- 1
    game$points_per_round <- numeric()
    game$all_round_results <- list()

    # Generuj dane dla pierwszej rundy
    game$current_data <- generate_round_data(game$difficulty)

    game$state <- "playing"
  })

  # Info o rundzie
  output$round_info <- renderText({
    paste0("Runda ", game$current_round, " / ", game$num_rounds)
  })

  # Dynamiczne slidery
  output$sliders <- renderUI({
    stats <- game$stats_to_guess
    sliders <- list()

    if ("mean" %in% stats) {
      sliders <- c(sliders, list(
        sliderInput("estimate_mean", "Średnia:",
                    min = 0, max = 100, value = 50, step = 1)
      ))
    }

    if ("median" %in% stats) {
      sliders <- c(sliders, list(
        sliderInput("estimate_median", "Mediana:",
                    min = 0, max = 100, value = 50, step = 1)
      ))
    }

    if ("sd" %in% stats) {
      sliders <- c(sliders, list(
        sliderInput("estimate_sd", "Odchylenie standardowe:",
                    min = 0, max = 30, value = 10, step = 1)
      ))
    }

    do.call(tagList, sliders)
  })

  # Histogram
  output$histogram <- renderPlot({
    req(game$current_data)

    df <- data.frame(value = game$current_data)

    p <- ggplot(df, aes(x = value)) +
      geom_histogram(bins = 20, fill = "#3498db", color = "white", alpha = 0.7) +
      labs(
        title = paste0("Runda ", game$current_round, " - Zgadnij statystyki!"),
        x = "Wartość",
        y = "Liczba obserwacji"
      ) +
      theme_minimal(base_size = 16) +
      theme(plot.title = element_text(face = "bold", size = 20)) +
      xlim(0, 100)

    # Jeśli stan = round_result, pokaż linie
    if (game$state == "round_result") {
      if ("mean" %in% game$stats_to_guess) {
        true_mean <- game$true_values$mean
        est_mean <- game$estimates$mean
        p <- p +
          geom_vline(xintercept = true_mean, color = "#e74c3c", size = 2, linetype = "solid") +
          geom_vline(xintercept = est_mean, color = "#f39c12", size = 2, linetype = "dashed") +
          annotate("text", x = true_mean, y = Inf, label = paste0("Prawdziwa średnia: ", round(true_mean, 1)),
                   vjust = 2, color = "#e74c3c", size = 5, fontface = "bold") +
          annotate("text", x = est_mean, y = Inf, label = paste0("Twoja estymata: ", round(est_mean, 1)),
                   vjust = 4, color = "#f39c12", size = 5, fontface = "bold")
      }

      if ("median" %in% game$stats_to_guess) {
        true_median <- game$true_values$median
        est_median <- game$estimates$median
        p <- p +
          geom_vline(xintercept = true_median, color = "#9b59b6", size = 2, linetype = "solid") +
          geom_vline(xintercept = est_median, color = "#3498db", size = 2, linetype = "dashed")
      }
    }

    p
  })

  # SPRAWDŹ odpowiedź
  observeEvent(input$check_answer, {
    # Zbierz estymaty
    estimates <- list()
    if ("mean" %in% game$stats_to_guess) {
      estimates$mean <- input$estimate_mean
    }
    if ("median" %in% game$stats_to_guess) {
      estimates$median <- input$estimate_median
    }
    if ("sd" %in% game$stats_to_guess) {
      estimates$sd <- input$estimate_sd
    }

    game$estimates <- estimates

    # Oblicz prawdziwe wartości
    true_values <- list()
    if ("mean" %in% game$stats_to_guess) {
      true_values$mean <- mean(game$current_data)
    }
    if ("median" %in% game$stats_to_guess) {
      true_values$median <- median(game$current_data)
    }
    if ("sd" %in% game$stats_to_guess) {
      true_values$sd <- sd(game$current_data)
    }

    game$true_values <- true_values

    # Oblicz błędy i punkty
    errors <- list()
    points <- 0
    max_points <- length(game$stats_to_guess) * 100

    for (stat in game$stats_to_guess) {
      error <- abs(estimates[[stat]] - true_values[[stat]])
      errors[[stat]] <- error

      # Punkty: max 100 za statystykę, odejmowanie za błąd
      stat_points <- max(0, 100 - error * 5)
      points <- points + stat_points
    }

    game$points_per_round[game$current_round] <- points

    # Zapisz wyniki rundy
    game$all_round_results[[game$current_round]] <- list(
      round = game$current_round,
      estimates = estimates,
      true_values = true_values,
      errors = errors,
      points = points
    )

    game$state <- "round_result"
  })

  # Wyniki rundy
  output$round_results <- renderUI({
    req(game$state == "round_result")

    results <- game$all_round_results[[game$current_round]]
    lines <- list()

    for (stat in game$stats_to_guess) {
      stat_name <- switch(stat,
                         "mean" = "Średnia",
                         "median" = "Mediana",
                         "sd" = "Odchylenie std.")

      true_val <- round(results$true_values[[stat]], 1)
      est_val <- round(results$estimates[[stat]], 1)
      error <- round(results$errors[[stat]], 1)

      lines <- c(lines, list(
        p(strong(stat_name)),
        p(paste0("  Prawdziwa wartość: ", true_val)),
        p(paste0("  Twoja estymata: ", est_val)),
        p(paste0("  Błąd: ", error), style = "color: #e74c3c;"),
        br()
      ))
    }

    do.call(tagList, lines)
  })

  output$points_text <- renderText({
    req(game$state == "round_result")
    points <- game$points_per_round[game$current_round]
    max_points <- length(game$stats_to_guess) * 100
    paste0("Punkty w tej rundzie: ", round(points, 0), " / ", max_points)
  })

  # NASTĘPNA RUNDA
  observeEvent(input$next_round, {
    if (game$current_round < game$num_rounds) {
      # Kolejna runda
      game$current_round <- game$current_round + 1
      game$current_data <- generate_round_data(game$difficulty)
      game$state <- "playing"
    } else {
      # Koniec gry
      game$state <- "summary"
    }
  })

  # Podsumowanie gry
  output$final_score <- renderText({
    req(game$state == "summary")
    total <- sum(game$points_per_round)
    max_total <- game$num_rounds * length(game$stats_to_guess) * 100
    paste0("Suma punktów: ", round(total, 0), " / ", max_total)
  })

  output$rating <- renderText({
    req(game$state == "summary")
    total <- sum(game$points_per_round)
    max_total <- game$num_rounds * length(game$stats_to_guess) * 100
    percent <- (total / max_total) * 100

    rating <- if (percent < 40) {
      "❌ Spróbuj ponownie!"
    } else if (percent < 60) {
      "⭐ Nieźle!"
    } else if (percent < 80) {
      "⭐⭐ Dobrze!"
    } else if (percent < 90) {
      "⭐⭐⭐ Bardzo dobrze!"
    } else {
      "⭐⭐⭐⭐ EKSPERT STATYSTYKI!"
    }

    paste0("Ocena: ", rating, " (", round(percent, 1), "%)")
  })

  output$all_rounds_summary <- renderText({
    req(game$state == "summary")

    lines <- c("═══════════════════════════════", "Szczegóły wszystkich rund:", "")

    for (i in 1:game$num_rounds) {
      results <- game$all_round_results[[i]]
      lines <- c(lines, paste0("Runda ", i, ": ", round(results$points, 0), " pkt"))

      for (stat in game$stats_to_guess) {
        stat_name <- switch(stat,
                           "mean" = "Średnia",
                           "median" = "Mediana",
                           "sd" = "SD")
        error <- round(results$errors[[stat]], 1)
        lines <- c(lines, paste0("  ", stat_name, " - błąd: ", error))
      }
      lines <- c(lines, "")
    }

    lines <- c(lines, "═══════════════════════════════")
    paste(lines, collapse = "\n")
  })

  # ZAGRAJ PONOWNIE
  observeEvent(input$play_again, {
    game$state <- "start"
    game$current_round <- 0
    game$points_per_round <- numeric()
    game$all_round_results <- list()
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)