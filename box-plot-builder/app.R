# 📦 Box Plot Builder - Krok po kroku
# Interaktywne narzędzie do zrozumienia budowy wykresu pudełkowego

library(shiny)
library(ggplot2)
library(dplyr)

# Funkcje generujące dane dla każdego scenariusza
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
    x_min = -2, x_max = 20
  ),
  kac = list(
    title = "🍺 Intensywność Kaca",
    generator = generate_kac_data,
    unit = "pkt",
    x_label = "Intensywność kaca (0-10)",
    x_min = 0, x_max = 10
  ),
  sklep = list(
    title = "🛒 Kwota Zakupów",
    generator = generate_sklep_data,
    unit = "zł",
    x_label = "Kwota zakupów (zł)",
    x_min = 0, x_max = 500
  )
)

# UI
ui <- fluidPage(
  titlePanel("📦 Box Plot Builder - Krok po kroku"),

  sidebarLayout(
    sidebarPanel(
      h4("Wybór danych"),
      selectInput("scenario", "Scenariusz:",
                  choices = c("Autobusy" = "autobusy",
                              "Kac po alkoholu" = "kac",
                              "Zakupy w sklepie" = "sklep"),
                  selected = "autobusy"),

      sliderInput("n_obs", "Liczba obserwacji do wylosowania:",
                  min = 10, max = 100, value = 30, step = 5),

      actionButton("draw_new", "🎲 Losuj nowe dane", class = "btn-success", width = "100%"),

      hr(),

      h4("Kroki budowy box plotu"),
      p("Kliknij kolejne kroki, aby zobaczyć jak powstaje wykres pudełkowy:"),

      actionButton("step1", "1. Pokaż surowe dane", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step2", "2. Sortuj dane", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step3", "3. Znajdź medianę (Q2)", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step4", "4. Znajdź Q1 i Q3", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step5", "5. Oblicz IQR", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step6", "6. Dodaj wąsy", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step7", "7. Zaznacz outliery", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step8", "8. Pokaż box plot", class = "btn-outline-primary", width = "100%"),
      br(), br(),

      hr(),

      actionButton("reset", "🔄 Reset do początku", class = "btn-danger", width = "100%"),

      hr(),

      h4("Legenda kroków"),
      div(
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; font-size: 12px;",
        p(strong("Q1:"), "25. percentyl (1/4 danych poniżej)"),
        p(strong("Q2 (mediana):"), "50. percentyl (1/2 danych poniżej)"),
        p(strong("Q3:"), "75. percentyl (3/4 danych poniżej)"),
        p(strong("IQR:"), "Rozstęp międzykwartylowy = Q3 - Q1"),
        p(strong("Wąsy:"), "Min/Max w granicach 1.5 × IQR"),
        p(strong("Outliery:"), "Punkty poza wąsami")
      ),

      width = 3
    ),

    mainPanel(
      # Górny panel: Dynamiczny wykres
      div(
        style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
        h4(textOutput("step_title"), style = "color: #2c3e50;"),
        plotOutput("main_plot", height = "300px"),
        div(
          style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-top: 10px;",
          textOutput("step_explanation")
        )
      ),

      # Dolny panel: Statyczny histogram dla porównania
      div(
        style = "border: 2px solid #95a5a6; border-radius: 5px; padding: 10px;",
        h4("Dla porównania: Histogram tych samych danych", style = "color: #7f8c8d;"),
        plotOutput("histogram", height = "250px")
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

  # Inicjalne losowanie przy starcie lub zmianie scenariusza
  observeEvent(c(input$scenario, input$n_obs), {
    data(sampled_data())
    current_step(0)  # Reset kroku
  }, ignoreNULL = FALSE)

  # Losowanie nowych danych na żądanie
  observeEvent(input$draw_new, {
    data(sampled_data())
    current_step(0)  # Reset kroku
  })

  # Reset (tylko krok, dane zostają)
  observeEvent(input$reset, {
    current_step(0)
  })

  # Kroki
  observeEvent(input$step1, { current_step(1) })
  observeEvent(input$step2, { current_step(2) })
  observeEvent(input$step3, { current_step(3) })
  observeEvent(input$step4, { current_step(4) })
  observeEvent(input$step5, { current_step(5) })
  observeEvent(input$step6, { current_step(6) })
  observeEvent(input$step7, { current_step(7) })
  observeEvent(input$step8, { current_step(8) })

  # Obliczenia statystyk
  sorted_data <- reactive({
    sort(data())
  })

  q1 <- reactive({
    quantile(data(), 0.25)
  })

  q2 <- reactive({
    median(data())
  })

  q3 <- reactive({
    quantile(data(), 0.75)
  })

  iqr <- reactive({
    q3() - q1()
  })

  lower_whisker <- reactive({
    max(min(data()), q1() - 1.5 * iqr())
  })

  upper_whisker <- reactive({
    min(max(data()), q3() + 1.5 * iqr())
  })

  outliers <- reactive({
    data()[data() < lower_whisker() | data() > upper_whisker()]
  })

  # Tytuł kroku
  output$step_title <- renderText({
    scenario <- scenarios[[input$scenario]]
    step <- current_step()
    if (step == 0) return(paste("Dane:", scenario$title, "- Kliknij 'Krok 1', aby rozpocząć"))
    if (step == 1) return("Krok 1: Surowe dane")
    if (step == 2) return("Krok 2: Posortowane dane")
    if (step == 3) return("Krok 3: Mediana (Q2)")
    if (step == 4) return("Krok 4: Kwartyle (Q1 i Q3)")
    if (step == 5) return("Krok 5: Rozstęp międzykwartylowy (IQR)")
    if (step == 6) return("Krok 6: Wąsy")
    if (step == 7) return("Krok 7: Outliery")
    if (step == 8) return("Krok 8: Pełny Box Plot")
  })

  # Wyjaśnienie kroku
  output$step_explanation <- renderText({
    scenario <- scenarios[[input$scenario]]
    step <- current_step()
    n <- length(data())
    unit <- scenario$unit

    if (step == 0) return(paste0("Wylosowano ", n, " obserwacji. Rozpocznij budowę wykresu pudełkowego krok po kroku."))
    if (step == 1) return(paste0("To są nasze surowe dane: ", n, " wylosowanych obserwacji. Każdy punkt to jedna obserwacja."))
    if (step == 2) return(paste0("Sortujemy dane od najmniejszej do największej wartości. To pozwala znaleźć percentyle. Min = ", round(min(data()), 1), " ", unit, ", Max = ", round(max(data()), 1), " ", unit))
    if (step == 3) return(paste0("Mediana (Q2) to wartość środkowa - połowa danych jest poniżej, połowa powyżej. Mediana = ", round(q2(), 1), " ", unit))
    if (step == 4) return(paste0("Q1 (25. percentyl) = ", round(q1(), 1), " ", unit, " | Q3 (75. percentyl) = ", round(q3(), 1), " ", unit, ". Środkowe 50% danych znajduje się między Q1 a Q3."))
    if (step == 5) return(paste0("IQR (Inter-Quartile Range) = Q3 - Q1 = ", round(q3(), 1), " - ", round(q1(), 1), " = ", round(iqr(), 1), " ", unit, ". To miara rozproszenia środkowych 50% danych."))
    if (step == 6) return(paste0("Wąsy pokazują zakres 'typowych' wartości. Dolny wąs = ", round(lower_whisker(), 1), " ", unit, ", Górny wąs = ", round(upper_whisker(), 1), " ", unit, ". Obliczamy je jako Q1 - 1.5×IQR i Q3 + 1.5×IQR (ale nie dalej niż min/max)."))
    if (step == 7) {
      outlier_text <- if (length(outliers()) > 0) paste(round(outliers(), 1), collapse = ", ") else "brak"
      return(paste0("Outliery to wartości poza wąsami. Znaleziono ", length(outliers()), " outlier(ów): ", outlier_text, ". To nietypowe obserwacje."))
    }
    if (step == 8) return("Pełny box plot (horizontal) - łatwo porównać z histogramem poniżej. Pudełko = Q1 do Q3, linia = mediana, wąsy = zakres typowych wartości.")
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
      # Krok 1: Surowe dane (nieposortowane)
      df <- data.frame(value = data(), index = seq_along(data()))
      ggplot(df, aes(x = value, y = 0)) +
        geom_jitter(height = 0.3, size = 4, alpha = 0.6, color = "#3498db") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 2) {
      # Krok 2: Posortowane dane
      df <- data.frame(value = sorted_data(), index = seq_along(sorted_data()))
      ggplot(df, aes(x = value, y = 0)) +
        geom_point(size = 4, alpha = 0.7, color = "#27ae60") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 3) {
      # Krok 3: Mediana
      df <- data.frame(value = sorted_data(), index = seq_along(sorted_data()))
      ggplot(df, aes(x = value, y = 0)) +
        geom_point(size = 4, alpha = 0.5, color = "#95a5a6") +
        geom_vline(xintercept = q2(), color = "#e74c3c", size = 2, linetype = "solid") +
        annotate("text", x = q2(), y = 0.4, label = paste0("Mediana\n", round(q2(), 1)),
                 color = "#e74c3c", size = 5, fontface = "bold") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 4) {
      # Krok 4: Q1, Q2, Q3
      df <- data.frame(value = sorted_data(), index = seq_along(sorted_data()))
      ggplot(df, aes(x = value, y = 0)) +
        geom_point(size = 4, alpha = 0.5, color = "#95a5a6") +
        geom_vline(xintercept = q1(), color = "#3498db", size = 1.5, linetype = "dashed") +
        geom_vline(xintercept = q2(), color = "#e74c3c", size = 2, linetype = "solid") +
        geom_vline(xintercept = q3(), color = "#3498db", size = 1.5, linetype = "dashed") +
        annotate("text", x = q1(), y = 0.4, label = paste0("Q1\n", round(q1(), 1)),
                 color = "#3498db", size = 4, fontface = "bold") +
        annotate("text", x = q2(), y = -0.4, label = paste0("Q2\n", round(q2(), 1)),
                 color = "#e74c3c", size = 4, fontface = "bold") +
        annotate("text", x = q3(), y = 0.4, label = paste0("Q3\n", round(q3(), 1)),
                 color = "#3498db", size = 4, fontface = "bold") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 5) {
      # Krok 5: IQR (pudełko)
      df <- data.frame(value = sorted_data(), index = seq_along(sorted_data()))
      ggplot(df, aes(x = value, y = 0)) +
        annotate("rect", xmin = q1(), xmax = q3(), ymin = -0.2, ymax = 0.2,
                 fill = "#3498db", alpha = 0.3) +
        geom_point(size = 4, alpha = 0.5, color = "#95a5a6") +
        geom_vline(xintercept = q2(), color = "#e74c3c", size = 2) +
        annotate("text", x = (q1() + q3()) / 2, y = 0.35,
                 label = paste0("IQR = ", round(iqr(), 1)),
                 color = "#2c3e50", size = 5, fontface = "bold") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 6) {
      # Krok 6: Wąsy
      df <- data.frame(value = sorted_data(), index = seq_along(sorted_data()))
      ggplot(df, aes(x = value, y = 0)) +
        # Wąsy
        geom_segment(aes(x = lower_whisker(), xend = q1(), y = 0, yend = 0),
                     size = 1.5, color = "#2c3e50") +
        geom_segment(aes(x = q3(), xend = upper_whisker(), y = 0, yend = 0),
                     size = 1.5, color = "#2c3e50") +
        # Pudełko
        annotate("rect", xmin = q1(), xmax = q3(), ymin = -0.2, ymax = 0.2,
                 fill = "#3498db", alpha = 0.3, color = "#2c3e50", size = 1) +
        geom_vline(xintercept = q2(), color = "#e74c3c", size = 2) +
        # Punkty
        geom_point(size = 4, alpha = 0.5, color = "#95a5a6") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 7) {
      # Krok 7: Outliery
      df <- data.frame(value = sorted_data(), index = seq_along(sorted_data()))
      df$is_outlier <- df$value < lower_whisker() | df$value > upper_whisker()

      ggplot(df, aes(x = value, y = 0)) +
        # Wąsy
        geom_segment(aes(x = lower_whisker(), xend = q1(), y = 0, yend = 0),
                     size = 1.5, color = "#2c3e50") +
        geom_segment(aes(x = q3(), xend = upper_whisker(), y = 0, yend = 0),
                     size = 1.5, color = "#2c3e50") +
        # Pudełko
        annotate("rect", xmin = q1(), xmax = q3(), ymin = -0.2, ymax = 0.2,
                 fill = "#3498db", alpha = 0.3, color = "#2c3e50", size = 1) +
        geom_vline(xintercept = q2(), color = "#e74c3c", size = 2) +
        # Punkty (outliery czerwone)
        geom_point(aes(color = is_outlier), size = 4, alpha = 0.7) +
        scale_color_manual(values = c("FALSE" = "#95a5a6", "TRUE" = "#e74c3c"), guide = "none") +
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max) +
        ylim(-0.5, 0.5)

    } else if (step == 8) {
      # Krok 8: Pełny box plot (horizontal)
      df <- data.frame(value = data(), group = "Dane")

      p <- ggplot(df, aes(x = value, y = group)) +
        geom_boxplot(fill = "#3498db", alpha = 0.5, color = "#2c3e50", size = 1,
                     outlier.shape = NA) +  # Nie pokazuj outlierów automatycznie
        labs(x = x_label, y = "") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        xlim(x_min, x_max)

      # Dodaj outliery tylko jeśli istnieją
      if (length(outliers()) > 0) {
        df_outliers <- data.frame(value = outliers(), group = "Dane")
        p <- p + geom_jitter(data = df_outliers, aes(x = value, y = group),
                            height = 0.1, size = 4, color = "#e74c3c", alpha = 0.7)
      }

      p
    }
  })

  # Histogram (statyczny, dla porównania)
  output$histogram <- renderPlot({
    scenario <- scenarios[[input$scenario]]
    df <- data.frame(value = data())

    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 15, fill = "#95a5a6", color = "white", alpha = 0.7) +
      labs(
        x = scenario$x_label,
        y = "Liczba obserwacji"
      ) +
      theme_minimal(base_size = 14) +
      xlim(scenario$x_min, scenario$x_max)
  })
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)