# 📊 Średnia vs Mediana - Kiedy co?
# Interaktywne narzędzie do zrozumienia różnicy między średnią a medianą

library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("📊 Średnia vs Mediana - Kiedy co?"),

  sidebarLayout(
    sidebarPanel(
      h4("Wybierz scenariusz"),
      selectInput("scenario",
                  "Przykład z życia:",
                  choices = c(
                    "Zarobki w firmie" = "zarobki",
                    "Wyniki egzaminu" = "egzamin",
                    "Czas dojazdu do pracy" = "dojazd",
                    "Ceny mieszkań" = "mieszkania",
                    "Czas odpowiedzi email" = "email"
                  )),

      hr(),

      h4("Dodaj nową obserwację"),
      uiOutput("value_slider"),

      actionButton("add_value", "Dodaj wartość", class = "btn-primary", width = "100%"),
      br(), br(),
      actionButton("add_outlier", "Dodaj outlier", class = "btn-warning", width = "100%"),
      br(), br(),
      actionButton("reset", "Reset do początku", class = "btn-danger", width = "100%"),

      hr(),

      h4("Statystyki"),
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
        h5(textOutput("mean_text"), style = "color: #e74c3c; margin: 5px 0;"),
        h5(textOutput("median_text"), style = "color: #3498db; margin: 5px 0;"),
        h5(textOutput("diff_text"), style = "color: #2c3e50; margin: 5px 0; font-weight: bold;")
      ),

      width = 3
    ),

    mainPanel(
      plotOutput("histogram", height = "400px"),
      br(),
      plotOutput("stripplot", height = "150px"),
      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {

  # Funkcja generująca dane dla scenariusza
  generate_data <- function(scenario_name) {
    set.seed(NULL)  # Każde wywołanie generuje nowe dane

    if (scenario_name == "zarobki") {
      # Gamma distribution (skośny prawo)
      data <- rgamma(80, shape = 2, scale = 2000) + 3000

    } else if (scenario_name == "egzamin") {
      # Normalny rozkład, ograniczony do 0-100
      data <- rnorm(80, mean = 70, sd = 12)
      data <- pmax(pmin(data, 100), 0)

    } else if (scenario_name == "dojazd") {
      # Gamma (skośny prawo, lekki) - czas dojazdu w minutach
      data <- rgamma(80, shape = 3, scale = 8) + 5

    } else if (scenario_name == "mieszkania") {
      # Gamma + outliery (skośny prawo z wartościami odstającymi)
      base <- rgamma(76, shape = 3, scale = 80) + 200
      outliers <- runif(4, 1200, 1800)
      data <- c(base, outliers)

    } else if (scenario_name == "email") {
      # Rozkład wykładniczy (bardzo skośny, długi ogon)
      data <- rexp(80, rate = 0.2) + 1
    }

    return(data)
  }

  # Scenariusze danych
  scenarios <- list(
    zarobki = list(
      name = "Zarobki w firmie (PLN)",
      slider_min = 3000,
      slider_max = 25000,
      slider_value = 7000,
      outlier_factor = 3
    ),
    egzamin = list(
      name = "Wyniki egzaminu (punkty)",
      slider_min = 0,
      slider_max = 100,
      slider_value = 70,
      outlier_factor = 0.3
    ),
    dojazd = list(
      name = "Czas dojazdu do pracy (minuty)",
      slider_min = 5,
      slider_max = 120,
      slider_value = 30,
      outlier_factor = 2
    ),
    mieszkania = list(
      name = "Ceny mieszkań (tys. PLN)",
      slider_min = 200,
      slider_max = 2000,
      slider_value = 500,
      outlier_factor = 2.5
    ),
    email = list(
      name = "Czas odpowiedzi email (godziny)",
      slider_min = 1,
      slider_max = 48,
      slider_value = 8,
      outlier_factor = 3
    )
  )

  # Reactive values
  data <- reactiveVal(generate_data("zarobki"))

  # Reset danych przy zmianie scenariusza
  observeEvent(input$scenario, {
    data(generate_data(input$scenario))
  })

  # Reset do początku (losuje nowe dane)
  observeEvent(input$reset, {
    data(generate_data(input$scenario))
  })

  # Dynamiczny slider
  output$value_slider <- renderUI({
    scenario <- scenarios[[input$scenario]]
    sliderInput("new_value",
                "Wartość do dodania:",
                min = scenario$slider_min,
                max = scenario$slider_max,
                value = scenario$slider_value,
                step = (scenario$slider_max - scenario$slider_min) / 100)
  })

  # Dodaj wartość
  observeEvent(input$add_value, {
    req(input$new_value)
    data(c(data(), input$new_value))
  })

  # Dodaj outlier
  observeEvent(input$add_outlier, {
    current_max <- max(data())
    outlier <- current_max * scenarios[[input$scenario]]$outlier_factor
    # Ogranicz do max slidera
    outlier <- min(outlier, scenarios[[input$scenario]]$slider_max)
    data(c(data(), outlier))
  })

  # Obliczenia statystyk
  mean_val <- reactive({
    mean(data())
  })

  median_val <- reactive({
    median(data())
  })

  diff_val <- reactive({
    abs(mean_val() - median_val())
  })

  # Teksty statystyk
  output$mean_text <- renderText({
    paste0("🔴 Średnia: ", round(mean_val(), 2))
  })

  output$median_text <- renderText({
    paste0("🔵 Mediana: ", round(median_val(), 2))
  })

  output$diff_text <- renderText({
    paste0("Różnica: ", round(diff_val(), 2))
  })

  # Histogram
  output$histogram <- renderPlot({
    df <- data.frame(value = data())
    scenario <- scenarios[[input$scenario]]

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 15, fill = "#95a5a6", color = "white", alpha = 0.7) +
      geom_vline(xintercept = mean_val(), color = "#e74c3c", size = 1.5, linetype = "solid") +
      geom_vline(xintercept = median_val(), color = "#3498db", size = 1.5, linetype = "dashed") +
      annotate("text", x = mean_val(), y = Inf,
               label = paste0("Średnia: ", round(mean_val(), 2)),
               vjust = 2, hjust = ifelse(mean_val() > median_val(), -0.1, 1.1),
               color = "#e74c3c", size = 5, fontface = "bold") +
      annotate("text", x = median_val(), y = Inf,
               label = paste0("Mediana: ", round(median_val(), 2)),
               vjust = 3.5, hjust = ifelse(median_val() > mean_val(), -0.1, 1.1),
               color = "#3498db", size = 5, fontface = "bold") +
      labs(
        title = scenario$name,
        subtitle = paste0("n = ", length(data()), " obserwacji"),
        x = "Wartość",
        y = "Liczba obserwacji"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(size = 18, face = "bold"))
  })

  # Strip plot
  output$stripplot <- renderPlot({
    df <- data.frame(
      value = data(),
      index = seq_along(data())
    )

    ggplot(df, aes(x = value, y = 0)) +
      geom_jitter(height = 0.15, alpha = 0.6, size = 3, color = "#95a5a6") +
      geom_vline(xintercept = mean_val(), color = "#e74c3c", size = 1.5, linetype = "solid") +
      geom_vline(xintercept = median_val(), color = "#3498db", size = 1.5, linetype = "dashed") +
      labs(
        title = "Wszystkie obserwacje (każdy punkt = jedna wartość)",
        x = "Wartość"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      ylim(-0.5, 0.5)
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)