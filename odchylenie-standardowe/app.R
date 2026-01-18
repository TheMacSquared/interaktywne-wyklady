# 📏 Odchylenie Standardowe - Intuicja
# Interaktywne narzędzie do zrozumienia odchylenia standardowego

library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("📏 Odchylenie Standardowe - Krok po kroku"),

  sidebarLayout(
    sidebarPanel(
      h4("Wybór danych"),
      selectInput("scenario",
                  "Wybierz typ danych:",
                  choices = c(
                    "Bardzo skupione (SD ≈ 2)" = "skupione",
                    "Umiarkowanie rozproszone (SD ≈ 5)" = "umiarkowane",
                    "Bardzo rozproszone (SD ≈ 10)" = "rozproszone"
                  )),

      actionButton("regenerate", "🎲 Losuj nowy zestaw danych",
                   class = "btn-success", width = "100%"),

      hr(),

      h4("Kroki obliczania SD"),
      p("Kliknij kolejne kroki, aby zobaczyć jak liczymy odchylenie standardowe:"),

      actionButton("step1", "1. Pokaż surowe dane", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step2", "2. Oblicz średnią", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step3", "3. Odległości i kwadraty", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step4", "4. Odchylenie standardowe", class = "btn-outline-primary", width = "100%"),
      br(), br(),

      hr(),

      actionButton("reset", "🔄 Reset do początku", class = "btn-danger", width = "100%"),

      hr(),

      h4("Statystyki"),
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
        h5(textOutput("n_text"), style = "margin: 5px 0;"),
        h5(textOutput("mean_text"), style = "margin: 5px 0; color: #e74c3c;"),
        h5(textOutput("sd_text"), style = "margin: 5px 0; color: #3498db; font-weight: bold;")
      ),

      width = 3
    ),

    mainPanel(
      # Górny panel: Dynamiczny wykres
      div(
        style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
        h4(textOutput("step_title"), style = "color: #2c3e50;"),
        plotOutput("main_plot", height = "400px"),
        div(
          style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-top: 10px;",
          textOutput("step_explanation")
        )
      ),

      # Dolny panel: Tabela obliczeń
      div(
        style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
        h4("Obliczenia", style = "color: #7f8c8d;"),
        tableOutput("calculations_table")
      ),

      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {

  # Scenariusze
  scenarios <- list(
    skupione = list(
      name = "Bardzo skupione",
      mean = 50,
      sd = 2,
      color = "#27ae60"
    ),
    umiarkowane = list(
      name = "Umiarkowanie rozproszone",
      mean = 50,
      sd = 5,
      color = "#f39c12"
    ),
    rozproszone = list(
      name = "Bardzo rozproszone",
      mean = 50,
      sd = 10,
      color = "#e74c3c"
    )
  )

  # Reactive values
  data <- reactiveVal(rnorm(20, mean = 50, sd = 2))
  current_step <- reactiveVal(0)

  # Regeneruj dane przy zmianie scenariusza lub kliknięciu przycisku
  observeEvent(input$scenario, {
    scenario <- scenarios[[input$scenario]]
    data(rnorm(20, mean = scenario$mean, sd = scenario$sd))
    current_step(0)
  })

  observeEvent(input$regenerate, {
    scenario <- scenarios[[input$scenario]]
    data(rnorm(20, mean = scenario$mean, sd = scenario$sd))
    current_step(0)
  })

  # Reset
  observeEvent(input$reset, {
    current_step(0)
  })

  # Kroki
  observeEvent(input$step1, { current_step(1) })
  observeEvent(input$step2, { current_step(2) })
  observeEvent(input$step3, { current_step(3) })
  observeEvent(input$step4, { current_step(4) })

  # Obliczenia
  mean_val <- reactive({
    mean(data())
  })

  sd_val <- reactive({
    sd(data())
  })

  # Teksty statystyk
  output$n_text <- renderText({
    paste0("n = ", length(data()), " obserwacji")
  })

  output$mean_text <- renderText({
    paste0("Średnia: ", round(mean_val(), 2))
  })

  output$sd_text <- renderText({
    paste0("Odchylenie standardowe: ", round(sd_val(), 2))
  })

  # Tytuł kroku
  output$step_title <- renderText({
    step <- current_step()
    if (step == 0) return("Wylosuj dane i kliknij 'Krok 1', aby rozpocząć")
    if (step == 1) return("Krok 1: Surowe dane")
    if (step == 2) return("Krok 2: Obliczanie średniej")
    if (step == 3) return("Krok 3: Odległości od średniej i ich kwadraty")
    if (step == 4) return("Krok 4: Odchylenie standardowe")
  })

  # Wyjaśnienie kroku
  output$step_explanation <- renderText({
    step <- current_step()
    n <- length(data())

    if (step == 0) return(paste0("Wylosowano ", n, " obserwacji. Kliknij kolejne kroki, aby zobaczyć jak obliczamy odchylenie standardowe."))
    if (step == 1) return(paste0("To są nasze surowe dane: ", n, " wylosowanych wartości. Każdy punkt to jedna obserwacja."))
    if (step == 2) return(paste0("Średnia to 'środek ciężkości' danych. Obliczamy ją jako sumę wszystkich wartości podzieloną przez ich liczbę. Średnia = ", round(mean_val(), 2)))
    if (step == 3) return("Dla każdego punktu obliczamy odległość od średniej (x - x̄), a potem podnosimy ją do kwadratu. Zobacz tabelę poniżej!")
    if (step == 4) return(paste0("Suma kwadratów / (n-1) = Wariancja. Pierwiastek z wariancji = SD = ", round(sd_val(), 2), ". To 'typowa odległość' punktu od średniej. Zobacz obliczenia w tabeli!"))
  })

  # Główny wykres (dynamiczny)
  output$main_plot <- renderPlot({
    scenario <- scenarios[[input$scenario]]
    step <- current_step()

    df <- data.frame(
      value = data(),
      index = seq_along(data())
    )

    # Obliczenia pomocnicze
    df$deviation <- df$value - mean_val()
    df$squared_dev <- df$deviation^2
    df$distance <- abs(df$deviation)
    df$distance_normalized <- df$distance / max(df$distance + 0.001)

    if (step == 0) {
      # Pusty wykres
      ggplot() +
        annotate("text", x = 50, y = 0,
                 label = "Kliknij 'Krok 1', aby rozpocząć",
                 size = 6, color = "gray50") +
        theme_void() +
        xlim(20, 80) +
        ylim(-0.5, 0.5)

    } else if (step == 1) {
      # Krok 1: Same dane
      ggplot(df, aes(x = value, y = 0)) +
        geom_jitter(height = 0.3, size = 4, alpha = 0.7, color = scenario$color) +
        labs(x = "Wartość", y = "") +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        xlim(20, 80) +
        ylim(-0.5, 0.5)

    } else if (step == 2) {
      # Krok 2: Dane + średnia
      ggplot(df, aes(x = value, y = 0)) +
        geom_jitter(height = 0.3, size = 4, alpha = 0.7, color = scenario$color) +
        geom_vline(xintercept = mean_val(), color = "#e74c3c", size = 2, linetype = "solid") +
        annotate("text", x = mean_val(), y = 0.45,
                 label = paste0("x̄ = ", round(mean_val(), 2)),
                 color = "#e74c3c", size = 6, fontface = "bold") +
        labs(x = "Wartość", y = "") +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        xlim(20, 80) +
        ylim(-0.5, 0.5)

    } else if (step == 3) {
      # Krok 3: Odległości od średniej - punkty posortowane wertykalnie
      df_sorted <- df %>% arrange(value)
      n <- nrow(df_sorted)
      df_sorted$y_pos <- seq(from = -0.4, to = 0.4, length.out = n)

      ggplot(df_sorted, aes(x = value, y = y_pos)) +
        geom_segment(aes(x = value, xend = mean_val(), y = y_pos, yend = y_pos),
                     color = "#9b59b6", size = 1, alpha = 0.7,
                     arrow = arrow(length = unit(0.2, "cm"), type = "closed")) +
        geom_point(size = 4, alpha = 0.8, color = scenario$color) +
        geom_vline(xintercept = mean_val(), color = "#e74c3c", size = 2, linetype = "solid") +
        annotate("text", x = mean_val(), y = 0.45,
                 label = paste0("x̄ = ", round(mean_val(), 2)),
                 color = "#e74c3c", size = 5, fontface = "bold") +
        labs(x = "Wartość", y = "") +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        xlim(20, 80) +
        ylim(-0.5, 0.5)

    } else if (step == 4) {
      # Krok 4: Pełne odchylenie standardowe z przedziałami
      ggplot(df, aes(x = value, y = 0)) +
        # Przedziały ±1SD, ±2SD
        annotate("rect", xmin = mean_val() - 2*sd_val(), xmax = mean_val() + 2*sd_val(),
                 ymin = -0.5, ymax = 0.5, alpha = 0.1, fill = "#3498db") +
        annotate("rect", xmin = mean_val() - sd_val(), xmax = mean_val() + sd_val(),
                 ymin = -0.5, ymax = 0.5, alpha = 0.2, fill = "#3498db") +
        geom_jitter(height = 0.3, size = 4, alpha = 0.7, color = scenario$color) +
        geom_vline(xintercept = mean_val(), color = "#e74c3c", size = 2, linetype = "solid") +
        geom_vline(xintercept = mean_val() - sd_val(), color = "#3498db", size = 1.5, linetype = "dashed") +
        geom_vline(xintercept = mean_val() + sd_val(), color = "#3498db", size = 1.5, linetype = "dashed") +
        annotate("text", x = mean_val(), y = 0.45,
                 label = paste0("SD = ", round(sd_val(), 2)),
                 color = "#3498db", size = 6, fontface = "bold") +
        annotate("text", x = mean_val(), y = -0.45,
                 label = "±1SD (68% danych)",
                 color = "#3498db", size = 4, fontface = "bold") +
        labs(x = "Wartość", y = "") +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        xlim(20, 80) +
        ylim(-0.5, 0.5)
    }
  })

  # Tabela obliczeń
  output$calculations_table <- renderTable({
    step <- current_step()

    if (step < 1) {
      return(data.frame(Info = "Obliczenia pojawią się od kroku 1"))
    }

    vals <- data()
    mean_v <- mean_val()
    n <- length(vals)

    # Przygotuj dane dla tabeli (max 10 pierwszych punktów)
    show_n <- min(10, n)

    df <- data.frame(
      Nr = 1:show_n,
      Wartość = round(vals[1:show_n], 2)
    )

    if (step >= 2) {
      df$`Średnia (x̄)` = round(mean_v, 2)
    }

    if (step >= 3) {
      df$`Odległość (x - x̄)` = round(vals[1:show_n] - mean_v, 2)
      df$`Kwadrat (x - x̄)²` = round((vals[1:show_n] - mean_v)^2, 2)
    }

    if (step >= 4) {
      # Dodaj wiersz podsumowania
      sum_squared <- sum((vals - mean_v)^2)
      variance <- sum_squared / (n - 1)

      # Zwróć tabelę z informacją podsumowującą
      summary_text <- data.frame(
        Info = c(
          paste0("Suma kwadratów = ", round(sum_squared, 2)),
          paste0("Wariancja = ", round(sum_squared, 2), " / ", (n-1), " = ", round(variance, 2)),
          paste0("Odchylenie standardowe = √", round(variance, 2), " = ", round(sd_val(), 2))
        )
      )

      return(summary_text)
    }

    df
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)