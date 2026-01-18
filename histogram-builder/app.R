# 📊 Histogram Builder - Krok po kroku
# Interaktywne narzędzie do zrozumienia budowy histogramu

library(shiny)
library(ggplot2)
library(dplyr)

# Funkcje generujące dane
generate_autobusy_data <- function(n) {
  set.seed(NULL)
  grupa <- sample(c("rano", "popoludnie", "wieczor"), n, replace = TRUE,
                  prob = c(0.4, 0.3, 0.3))
  opoznienie_min <- ifelse(grupa == "rano", rnorm(n, mean = 2, sd = 3),
                    ifelse(grupa == "popoludnie", rnorm(n, mean = 5, sd = 5),
                           rnorm(n, mean = 1, sd = 2)))
  round(pmax(opoznienie_min, -2), 1)
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
    x_min = -2, x_max = 20,
    bin_start_default = 0,
    bin_width_default = 2
  ),
  kac = list(
    title = "🍺 Intensywność Kaca",
    generator = generate_kac_data,
    unit = "pkt",
    x_label = "Intensywność kaca (0-10)",
    x_min = 0, x_max = 10,
    bin_start_default = 0,
    bin_width_default = 1
  ),
  sklep = list(
    title = "🛒 Kwota Zakupów",
    generator = generate_sklep_data,
    unit = "zł",
    x_label = "Kwota zakupów (zł)",
    x_min = 0, x_max = 500,
    bin_start_default = 0,
    bin_width_default = 50
  )
)

# UI
ui <- fluidPage(
  titlePanel("📊 Histogram Builder - Krok po kroku"),

  sidebarLayout(
    sidebarPanel(
      h4("Wybór danych"),
      selectInput("scenario", "Scenariusz:",
                  choices = c("Autobusy" = "autobusy",
                              "Kac po alkoholu" = "kac",
                              "Zakupy w sklepie" = "sklep"),
                  selected = "autobusy"),

      sliderInput("n_obs", "Liczba obserwacji do wylosowania:",
                  min = 20, max = 100, value = 50, step = 10),

      actionButton("draw_new", "🎲 Losuj nowe dane", class = "btn-success", width = "100%"),

      hr(),

      h4("Parametry binów"),
      uiOutput("bin_start_slider"),
      uiOutput("bin_width_slider"),

      hr(),

      h4("Kroki budowy histogramu"),
      p("Kliknij kolejne kroki, aby zobaczyć jak powstaje histogram:"),

      actionButton("step1", "1. Pokaż surowe dane", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step2", "2. Zakres danych (min/max)", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step4", "3. Przypisz dane do binów", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step8", "4. Pełny histogram", class = "btn-outline-primary", width = "100%"),
      br(), br(),

      hr(),

      actionButton("reset", "🔄 Reset do początku", class = "btn-danger", width = "100%"),

      hr(),

      h4("Legenda kroków"),
      div(
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; font-size: 12px;",
        p(strong("Bin (przedział):"), "Zakres wartości (np. 0-2, 2-4)"),
        p(strong("Początek binów:"), "Wartość pierwszego binu"),
        p(strong("Długość binu:"), "Szerokość każdego przedziału"),
        p(strong("Wysokość słupka:"), "Liczba obserwacji w binie"),
        p(strong("Częstość:"), "Ile razy wartość wystąpiła")
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

      # Dolny panel: Informacja o binach
      div(
        style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
        h4("Informacje o binach", style = "color: #7f8c8d;"),
        tableOutput("bin_info")
      ),

      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive: wygeneruj dane dla wybranej liczby obserwacji
  sampled_data <- reactive({
    scenario <- scenarios[[input$scenario]]
    scenario$generator(input$n_obs)
  })

  # Reactive values
  current_step <- reactiveVal(0)
  data <- reactiveVal(numeric(0))

  # Dynamiczne slidery dla binów
  output$bin_start_slider <- renderUI({
    scenario <- scenarios[[input$scenario]]
    sliderInput("bin_start", "Początek binów:",
                min = scenario$x_min,
                max = scenario$x_max - 1,
                value = scenario$bin_start_default,
                step = 0.5)
  })

  output$bin_width_slider <- renderUI({
    scenario <- scenarios[[input$scenario]]
    max_width <- (scenario$x_max - scenario$x_min) / 2
    sliderInput("bin_width", "Długość binu (szerokość):",
                min = 0.5,
                max = max_width,
                value = scenario$bin_width_default,
                step = 0.5)
  })

  # Inicjalne losowanie przy starcie lub zmianie scenariusza
  observeEvent(c(input$scenario, input$n_obs), {
    data(sampled_data())
    current_step(0)
  }, ignoreNULL = FALSE)

  # Losowanie nowych danych na żądanie
  observeEvent(input$draw_new, {
    data(sampled_data())
    current_step(0)
  })

  # Reset (tylko krok, dane zostają)
  observeEvent(input$reset, {
    current_step(0)
  })

  # Kroki
  observeEvent(input$step1, { current_step(1) })
  observeEvent(input$step2, { current_step(2) })
  observeEvent(input$step4, { current_step(4) })
  observeEvent(input$step8, { current_step(8) })

  # Obliczenia dla binów
  bin_breaks <- reactive({
    req(input$bin_start, input$bin_width)
    scenario <- scenarios[[input$scenario]]
    start <- input$bin_start
    width <- input$bin_width
    max_val <- scenario$x_max

    breaks <- seq(start, max_val + width, by = width)
    breaks
  })

  # Przypisanie danych do binów
  data_with_bins <- reactive({
    req(input$bin_start, input$bin_width)

    df <- data.frame(value = data())
    breaks <- bin_breaks()

    df$bin <- cut(df$value, breaks = breaks, include.lowest = TRUE, right = FALSE)
    df$bin_num <- as.numeric(df$bin)
    df
  })

  # Statystyki binów
  bin_stats <- reactive({
    df <- data_with_bins()
    breaks <- bin_breaks()

    # Policz obserwacje w każdym binie
    bin_counts <- df %>%
      filter(!is.na(bin)) %>%
      group_by(bin) %>%
      summarise(count = n(), .groups = 'drop')

    # Dodaj środki binów
    bin_counts$bin_start <- breaks[-length(breaks)][1:nrow(bin_counts)]
    bin_counts$bin_end <- breaks[2:(nrow(bin_counts) + 1)]
    bin_counts$bin_mid <- (bin_counts$bin_start + bin_counts$bin_end) / 2

    bin_counts
  })

  # Tytuł kroku
  output$step_title <- renderText({
    scenario <- scenarios[[input$scenario]]
    step <- current_step()
    if (step == 0) return(paste("Dane:", scenario$title, "- Kliknij 'Krok 1', aby rozpocząć"))
    if (step == 1) return("Krok 1: Surowe dane")
    if (step == 2) return("Krok 2: Zakres danych (min/max)")
    if (step == 4) return("Krok 3: Przypisanie danych do binów")
    if (step == 8) return("Krok 4: Pełny histogram")
  })

  # Wyjaśnienie kroku
  output$step_explanation <- renderText({
    scenario <- scenarios[[input$scenario]]
    step <- current_step()
    n <- length(data())
    unit <- scenario$unit

    if (step == 0) return(paste0("Wylosowano ", n, " obserwacji. Ustaw parametry binów i rozpocznij budowę histogramu."))
    if (step == 1) return(paste0("To są nasze surowe dane: ", n, " wylosowanych obserwacji. Każdy punkt to jedna wartość."))
    if (step == 2) return(paste0("Zakres danych: Min = ", round(min(data()), 1), " ", unit, ", Max = ", round(max(data()), 1), " ", unit, ". To określa skalę osi X."))
    if (step == 4) return("Każda obserwacja trafia do odpowiedniego binu. Kolor punktu = przynależność do binu.")
    if (step == 8) return("Pełny histogram pokazuje rozkład danych. Wysokość słupka = liczba obserwacji w binie. Zmień parametry binów, aby zobaczyć różne perspektywy!")
  })

  # Główny wykres (dynamiczny)
  output$main_plot <- renderPlot({
    scenario <- scenarios[[input$scenario]]
    step <- current_step()
    x_min <- scenario$x_min
    x_max <- scenario$x_max
    x_label <- scenario$x_label

    if (step == 0) {
      # Pusty wykres
      ggplot() +
        annotate("text", x = (x_min + x_max) / 2, y = 0.5,
                 label = "Wylosuj dane i kliknij 'Krok 1'",
                 size = 6, color = "gray50") +
        theme_void() +
        xlim(x_min, x_max) +
        ylim(0, 1)

    } else if (step == 1) {
      # Krok 1: Surowe dane
      df <- data.frame(value = data())
      ggplot(df, aes(x = value, y = 0)) +
        geom_jitter(height = 0.3, size = 3, alpha = 0.6, color = "#3498db") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 2) {
      # Krok 2: Zakres danych
      df <- data.frame(value = data())
      min_val <- min(data())
      max_val <- max(data())

      ggplot(df, aes(x = value, y = 0)) +
        geom_jitter(height = 0.3, size = 3, alpha = 0.6, color = "#3498db") +
        geom_vline(xintercept = min_val, color = "#e74c3c", size = 1.5, linetype = "dashed") +
        geom_vline(xintercept = max_val, color = "#e74c3c", size = 1.5, linetype = "dashed") +
        annotate("text", x = min_val, y = 0.4,
                 label = paste0("Min\n", round(min_val, 1)),
                 color = "#e74c3c", size = 4, fontface = "bold") +
        annotate("text", x = max_val, y = 0.4,
                 label = paste0("Max\n", round(max_val, 1)),
                 color = "#e74c3c", size = 4, fontface = "bold") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 3) {
      # Krok 3: Definicja binów
      req(input$bin_start, input$bin_width)
      df <- data.frame(value = data())
      breaks <- bin_breaks()

      # Przygotuj dane dla prostokątów binów
      bin_rects <- data.frame(
        xmin = breaks[-length(breaks)],
        xmax = breaks[-1]
      )

      ggplot() +
        geom_rect(data = bin_rects,
                  aes(xmin = xmin, xmax = xmax, ymin = -0.4, ymax = 0.4),
                  fill = NA, color = "#2c3e50", size = 1) +
        geom_jitter(data = df, aes(x = value, y = 0),
                    height = 0.2, size = 2, alpha = 0.4, color = "#95a5a6") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 4) {
      # Krok 4: Przypisanie do binów (kolorowanie)
      df <- data_with_bins()
      breaks <- bin_breaks()

      bin_rects <- data.frame(
        xmin = breaks[-length(breaks)],
        xmax = breaks[-1],
        bin_num = 1:(length(breaks) - 1)
      )

      ggplot() +
        geom_rect(data = bin_rects,
                  aes(xmin = xmin, xmax = xmax, ymin = -0.4, ymax = 0.4, fill = factor(bin_num)),
                  alpha = 0.2, color = "#2c3e50", size = 1) +
        geom_jitter(data = df %>% filter(!is.na(bin)),
                    aes(x = value, y = 0, color = factor(bin_num)),
                    height = 0.2, size = 3, alpha = 0.7) +
        scale_fill_viridis_d(guide = "none") +
        scale_color_viridis_d(guide = "none") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 5) {
      # Krok 5: Liczenie obserwacji
      df <- data_with_bins()
      stats <- bin_stats()
      breaks <- bin_breaks()

      bin_rects <- data.frame(
        xmin = breaks[-length(breaks)],
        xmax = breaks[-1],
        bin_num = 1:(length(breaks) - 1)
      )

      # Połącz z licznikami
      bin_rects <- bin_rects %>%
        left_join(stats %>% select(bin_start, count),
                  by = c("xmin" = "bin_start"))
      bin_rects$count[is.na(bin_rects$count)] <- 0
      bin_rects$bin_mid <- (bin_rects$xmin + bin_rects$xmax) / 2

      ggplot() +
        geom_rect(data = bin_rects,
                  aes(xmin = xmin, xmax = xmax, ymin = -0.4, ymax = 0.4, fill = factor(bin_num)),
                  alpha = 0.2, color = "#2c3e50", size = 1) +
        geom_jitter(data = df %>% filter(!is.na(bin)),
                    aes(x = value, y = 0, color = factor(bin_num)),
                    height = 0.2, size = 2, alpha = 0.5) +
        geom_text(data = bin_rects,
                  aes(x = bin_mid, y = 0.45, label = paste0("n=", count)),
                  size = 4, fontface = "bold", color = "#2c3e50") +
        scale_fill_viridis_d(guide = "none") +
        scale_color_viridis_d(guide = "none") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.6)

    } else if (step == 6) {
      # Krok 6: Słupki
      stats <- bin_stats()

      ggplot(stats, aes(x = bin_mid, y = count)) +
        geom_col(width = input$bin_width * 0.9, fill = "#3498db", alpha = 0.7, color = "#2c3e50") +
        labs(x = x_label, y = "Liczba obserwacji") +
        theme_minimal(base_size = 14) +
        xlim(x_min, x_max)

    } else if (step == 7) {
      # Krok 7: Z etykietami
      stats <- bin_stats()

      ggplot(stats, aes(x = bin_mid, y = count)) +
        geom_col(width = input$bin_width * 0.9, fill = "#3498db", alpha = 0.7, color = "#2c3e50") +
        geom_text(aes(label = count), vjust = -0.5, size = 4, fontface = "bold") +
        labs(
          title = paste("Histogram:", scenario$title),
          subtitle = paste0("n = ", length(data()), " | Szerokość binu = ", input$bin_width, " ", scenario$unit),
          x = x_label,
          y = "Liczba obserwacji (częstość)"
        ) +
        theme_minimal(base_size = 14) +
        xlim(x_min, x_max)

    } else if (step == 8) {
      # Krok 8: Pełny histogram
      df <- data.frame(value = data())

      ggplot(df, aes(x = value)) +
        geom_histogram(binwidth = input$bin_width,
                       boundary = input$bin_start,
                       fill = "#3498db", alpha = 0.7, color = "#2c3e50") +
        labs(
          title = paste("Histogram:", scenario$title),
          subtitle = paste0("n = ", length(data()), " | Szerokość binu = ", input$bin_width, " ", scenario$unit),
          x = x_label,
          y = "Liczba obserwacji (częstość)"
        ) +
        theme_minimal(base_size = 14) +
        xlim(x_min, x_max)
    }
  })

  # Tabela informacji o binach
  output$bin_info <- renderTable({
    if (current_step() < 4) {
      return(data.frame(Info = "Biny pojawią się od kroku 3"))
    }

    req(input$bin_start, input$bin_width)
    stats <- bin_stats()
    scenario <- scenarios[[input$scenario]]

    if (nrow(stats) == 0) {
      return(data.frame(Info = "Brak danych w binach"))
    }

    stats %>%
      select(
        `Przedział` = bin,
        `Początek` = bin_start,
        `Koniec` = bin_end,
        `Liczba obs.` = count
      ) %>%
      head(10)  # Pokaż max 10 binów
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
