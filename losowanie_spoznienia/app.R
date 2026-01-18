# 📊 Uniwersalny Symulator Rozkładów
# Interaktywne narzędzie do nauczania statystyki

library(shiny)
library(ggplot2)
library(dplyr)

# Funkcje generujące dane dla każdego scenariusza
generate_autobusy_data <- function(n) {
  set.seed(NULL)  # Losowe seed za każdym razem
  # Losujemy która grupa (rano/popołudnie/wieczór) dla każdej obserwacji
  grupa <- sample(c("rano", "popoludnie", "wieczor"), n, replace = TRUE,
                  prob = c(0.4, 0.3, 0.3))

  opoznienie_min <- ifelse(grupa == "rano", rnorm(n, mean = 2, sd = 3),
                    ifelse(grupa == "popoludnie", rnorm(n, mean = 5, sd = 5),
                           rnorm(n, mean = 1, sd = 2)))

  round(pmax(opoznienie_min, -2), 1)  # Minimum -2 min (może przyjechać wcześniej)
}

generate_kac_data <- function(n) {
  set.seed(NULL)
  jednostki_alkoholu <- rpois(n, lambda = 5) + 1
  woda_wypita <- sample(c("malo","srednio","duzo"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))
  jedzenie_przed <- sample(c("tak","nie"), n, replace = TRUE, prob = c(0.6, 0.4))
  mieszanie_alkoholi <- sample(c("tak","nie"), n, replace = TRUE, prob = c(0.3, 0.7))
  godziny_snu <- pmax(2, pmin(12, round(rnorm(n, mean = 6, sd = 2), 1)))
  plec <- sample(c("K","M"), n, replace = TRUE)

  intensywnosc_kaca <- (
    jednostki_alkoholu * 1.2 +
    ifelse(woda_wypita == "malo", 3, ifelse(woda_wypita == "srednio", 1, -1)) +
    ifelse(jedzenie_przed == "nie", 2, -1) +
    ifelse(mieszanie_alkoholi == "tak", 2, 0) +
    (8 - godziny_snu) * 0.5 +
    ifelse(plec == "K", 1, 0) +
    rnorm(n, 0, 1)
  )
  round(pmax(0, pmin(10, intensywnosc_kaca)), 1)
}

generate_sklep_data <- function(n) {
  set.seed(NULL)
  # Losujemy która grupa (poranni/dzienni/wieczorni) dla każdej obserwacji
  grupa <- sample(c("poranni", "dzienni", "wieczorni"), n, replace = TRUE,
                  prob = c(0.17, 0.50, 0.33))

  godzina_zakupow <- ifelse(grupa == "poranni", rnorm(n, 8, 1),
                      ifelse(grupa == "dzienni", rnorm(n, 14, 3),
                             rnorm(n, 19, 1.5)))
  godzina_zakupow <- pmax(6, pmin(22, round(godzina_zakupow, 1)))

  dzien_tygodnia <- sample(c("1_pon","2_wt","3_sr","4_czw","5_pt","6_sob","7_nd"), n,
                           replace = TRUE, prob = c(0.12,0.12,0.12,0.12,0.18,0.2,0.14))

  liczba_produktow <- case_when(
    dzien_tygodnia == "6_sob" ~ rpois(n, lambda = 25) + 5,
    godzina_zakupow < 10 ~ rpois(n, lambda = 5) + 1,
    TRUE ~ rpois(n, lambda = 12) + 3
  )

  kwota <- liczba_produktow * rnorm(n, mean = 8, sd = 3)
  round(pmax(10, kwota), 2)
}

# Konfiguracja scenariuszy
scenarios <- list(
  autobusy = list(
    title = "🚌 Spóźnienia Autobusu",
    generator = generate_autobusy_data,
    unit = "min",
    x_label = "Spóźnienie (minuty)",
    decision_label = "O ile minut wcześniej wychodzę?",
    decision_min = -5, decision_max = 15, decision_default = 5, decision_step = 0.5,
    prob_label = "Prawdopodobieństwo zdążenia",
    success_condition = ">=",
    color_fail = "#e74c3c", color_success = "#27ae60",
    label_fail = "Spóźnisz się", label_success = "Zdążysz",
    x_min = -2, x_max = 20
  ),
  kac = list(
    title = "🍺 Intensywność Kaca",
    generator = generate_kac_data,
    unit = "pkt",
    x_label = "Intensywność kaca (0-10)",
    decision_label = "Akceptowalny poziom kaca (próg)",
    decision_min = 0, decision_max = 10, decision_default = 5, decision_step = 0.5,
    prob_label = "Prawdopodobieństwo uniknięcia silnego kaca",
    success_condition = "<=",
    color_fail = "#e74c3c", color_success = "#27ae60",
    label_fail = "Silny kac", label_success = "OK",
    x_min = 0, x_max = 10
  ),
  sklep = list(
    title = "🛒 Kwota Zakupów",
    generator = generate_sklep_data,
    unit = "zł",
    x_label = "Kwota zakupów (zł)",
    decision_label = "Budżet na zakupy (zł)",
    decision_min = 0, decision_max = 500, decision_default = 100, decision_step = 10,
    prob_label = "Prawdopodobieństwo zmieszczenia w budżecie",
    success_condition = "<=",
    color_fail = "#e74c3c", color_success = "#27ae60",
    label_fail = "Przekroczenie", label_success = "W budżecie",
    x_min = 0, x_max = 500
  )
)

# UI
ui <- fluidPage(
  titlePanel("📊 Uniwersalny Symulator Rozkładów"),

  sidebarLayout(
    sidebarPanel(
      h4("Wybór scenariusza"),
      selectInput("scenario", "Scenariusz:",
                  choices = c("Autobusy" = "autobusy",
                              "Kac po alkoholu" = "kac",
                              "Zakupy w sklepie" = "sklep"),
                  selected = "autobusy"),

      hr(),

      h4("Zbieranie danych"),
      h3(textOutput("counter"), style = "color: #2c3e50;"),

      fluidRow(
        column(6, actionButton("add1", "+1 dzień", class = "btn-primary", width = "100%")),
        column(6, actionButton("add10", "+10 dni", class = "btn-primary", width = "100%"))
      ),
      br(),
      fluidRow(
        column(6, actionButton("add100", "+100 dni", class = "btn-info", width = "100%")),
        column(6, actionButton("reset", "Reset", class = "btn-danger", width = "100%"))
      ),

      hr(),

      h4("Decyzja"),
      uiOutput("decision_slider"),

      h4(textOutput("prob_text"), style = "color: #27ae60; font-weight: bold;"),

      hr(),

      checkboxInput("show_stats", "Pokaż statystyki opisowe", value = FALSE),

      conditionalPanel(
        condition = "input.show_stats == true",
        h4("Statystyki opisowe"),
        verbatimTextOutput("stats")
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

  # Reactive value przechowujący zebrane dane
  collected_data <- reactiveVal(numeric(0))

  # Funkcja generująca nowe wartości dla aktualnego scenariusza
  generate_values <- function(n) {
    scenario <- scenarios[[input$scenario]]
    scenario$generator(n)
  }

  # Dynamiczny slider decyzyjny
  output$decision_slider <- renderUI({
    scenario <- scenarios[[input$scenario]]
    sliderInput("decision_value",
                scenario$decision_label,
                min = scenario$decision_min,
                max = scenario$decision_max,
                value = scenario$decision_default,
                step = scenario$decision_step,
                post = paste0(" ", scenario$unit))
  })

  # Reset danych przy zmianie scenariusza
  observeEvent(input$scenario, {
    collected_data(numeric(0))
  })

  # Dodawanie pomiarów
  observeEvent(input$add1, {
    new_values <- generate_values(1)
    collected_data(c(collected_data(), new_values))
  })

  observeEvent(input$add10, {
    new_values <- generate_values(10)
    collected_data(c(collected_data(), new_values))
  })

  observeEvent(input$add100, {
    new_values <- generate_values(100)
    collected_data(c(collected_data(), new_values))
  })

  observeEvent(input$reset, {
    collected_data(numeric(0))
  })

  # Licznik pomiarów
  output$counter <- renderText({
    n <- length(collected_data())
    if (n == 0) {
      "Zebrane pomiary: 0 dni"
    } else if (n == 1) {
      "Zebrane pomiary: 1 dzień"
    } else {
      paste0("Zebrane pomiary: ", n, " dni")
    }
  })

  # Obliczanie prawdopodobieństwa (uniwersalne dla wszystkich scenariuszy)
  output$prob_text <- renderText({
    if (length(collected_data()) == 0 || is.null(input$decision_value)) {
      return("Zbierz dane, aby obliczyć prawdopodobieństwo")
    }

    scenario <- scenarios[[input$scenario]]

    # Logika zależna od warunku sukcesu
    if (scenario$success_condition == ">=") {
      # Autobusy: opóźnienie >= decyzja (zdążę)
      success_count <- sum(collected_data() >= input$decision_value)
    } else {
      # Kac, Sklep: wartość <= decyzja (OK)
      success_count <- sum(collected_data() <= input$decision_value)
    }

    prob_success <- success_count / length(collected_data()) * 100

    paste0(scenario$prob_label, ": ", round(prob_success, 1), "%")
  })

  # Statystyki opisowe
  output$stats <- renderText({
    if (length(collected_data()) == 0) {
      return("Brak danych")
    }

    scenario <- scenarios[[input$scenario]]

    paste0(
      "n = ", length(collected_data()), "\n",
      "Średnia: ", round(mean(collected_data()), 2), " ", scenario$unit, "\n",
      "Mediana: ", round(median(collected_data()), 2), " ", scenario$unit, "\n",
      "Odch. std.: ", round(sd(collected_data()), 2), " ", scenario$unit, "\n",
      "Min: ", round(min(collected_data()), 2), " ", scenario$unit, "\n",
      "Max: ", round(max(collected_data()), 2), " ", scenario$unit
    )
  })

  # Histogram
  output$histogram <- renderPlot({
    scenario <- scenarios[[input$scenario]]

    if (length(collected_data()) == 0) {
      # Pusty wykres z instrukcją
      ggplot() +
        annotate("text", x = 0, y = 0,
                 label = "Kliknij '+1 dzień', aby rozpocząć zbieranie danych",
                 size = 6, color = "gray50") +
        theme_void() +
        xlim(scenario$x_min, scenario$x_max) +
        ylim(-1, 1)
    } else {
      # Wymagane jest decision_value
      req(input$decision_value)

      # Przygotowanie danych
      df <- data.frame(value = collected_data())

      # Kolorowanie słupków względem decyzji (zależnie od warunku)
      if (scenario$success_condition == ">=") {
        # Autobusy: wartość >= decyzja = sukces
        df$color_group <- ifelse(df$value >= input$decision_value,
                                 scenario$label_success,
                                 scenario$label_fail)
      } else {
        # Kac, Sklep: wartość <= decyzja = sukces
        df$color_group <- ifelse(df$value <= input$decision_value,
                                 scenario$label_success,
                                 scenario$label_fail)
      }

      # Dynamiczna binwidth
      binwidth <- (scenario$x_max - scenario$x_min) / 30

      ggplot(df, aes(x = value, fill = color_group)) +
        geom_histogram(binwidth = binwidth, color = "white", alpha = 0.8) +
        geom_vline(xintercept = input$decision_value,
                   linetype = "dashed", color = "blue", size = 1.2) +
        annotate("text", x = input$decision_value, y = Inf,
                 label = paste0("Próg: ", input$decision_value, " ", scenario$unit),
                 vjust = 1.5, hjust = -0.1, color = "blue", size = 4) +
        scale_fill_manual(values = setNames(
          c(scenario$color_fail, scenario$color_success),
          c(scenario$label_fail, scenario$label_success)
        )) +
        scale_x_continuous(
          limits = c(scenario$x_min, scenario$x_max),
          breaks = seq(scenario$x_min, scenario$x_max,
                      length.out = min(11, scenario$x_max - scenario$x_min + 1))
        ) +
        labs(
          title = paste("Rozkład:", scenario$title),
          x = scenario$x_label,
          y = "Liczba obserwacji",
          fill = "Wynik"
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")
    }
  })

  # Strip plot (surowe dane)
  output$stripplot <- renderPlot({
    scenario <- scenarios[[input$scenario]]

    if (length(collected_data()) == 0) {
      # Pusty wykres
      ggplot() +
        theme_void() +
        xlim(scenario$x_min, scenario$x_max)
    } else {
      req(input$decision_value)

      df <- data.frame(
        value = collected_data(),
        index = seq_along(collected_data())
      )

      # Kolorowanie punktów względem decyzji
      if (scenario$success_condition == ">=") {
        df$color_group <- ifelse(df$value >= input$decision_value,
                                 scenario$label_success,
                                 scenario$label_fail)
      } else {
        df$color_group <- ifelse(df$value <= input$decision_value,
                                 scenario$label_success,
                                 scenario$label_fail)
      }

      ggplot(df, aes(x = value, y = 0, color = color_group)) +
        geom_jitter(height = 0.15, alpha = 0.6, size = 2) +
        geom_vline(xintercept = input$decision_value,
                   linetype = "dashed", color = "blue", size = 1.2) +
        scale_color_manual(values = setNames(
          c(scenario$color_fail, scenario$color_success),
          c(scenario$label_fail, scenario$label_success)
        )) +
        scale_x_continuous(
          limits = c(scenario$x_min, scenario$x_max),
          breaks = seq(scenario$x_min, scenario$x_max,
                      length.out = min(11, scenario$x_max - scenario$x_min + 1))
        ) +
        labs(
          title = "Surowe dane (każdy punkt = jedna obserwacja)",
          x = scenario$x_label,
          color = "Wynik"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none"
        ) +
        ylim(-0.5, 0.5)
    }
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)