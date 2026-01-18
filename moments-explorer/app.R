# 📈 Moments Explorer - Wizualizacja momentów rozkładu
# Interaktywne narzędzie do zrozumienia 4 momentów statystycznych

library(shiny)
library(ggplot2)
library(dplyr)
library(e1071)  # dla skewness() i kurtosis()

# Predefiniowane rozkłady statystyczne
distributions <- list(
  normal = list(
    name = "Normalny (Gaussa)",
    description = "Symetryczny, dzwonowaty - najczęstszy w naturze",
    generator = function(n) rnorm(n, mean = 50, sd = 15),
    x_min = 0, x_max = 100
  ),
  uniform = list(
    name = "Jednostajny",
    description = "Wszystkie wartości równie prawdopodobne",
    generator = function(n) runif(n, min = 20, max = 80),
    x_min = 0, x_max = 100
  ),
  exponential = list(
    name = "Wykładniczy",
    description = "Prawostronna skośność - czasy oczekiwania",
    generator = function(n) pmin(rexp(n, rate = 0.05), 100),
    x_min = 0, x_max = 100
  ),
  gamma_right = list(
    name = "Gamma (prawostronna)",
    description = "Umiarkowana prawostronna skośność",
    generator = function(n) rgamma(n, shape = 3, scale = 10),
    x_min = 0, x_max = 100
  ),
  gamma_left = list(
    name = "Gamma (lewostronna)",
    description = "Lewostronna skośność (odwrócona gamma)",
    generator = function(n) 100 - rgamma(n, shape = 3, scale = 10),
    x_min = 0, x_max = 100
  ),
  beta_platykurtic = list(
    name = "Beta (platykurtyczna)",
    description = "Spłaszczony rozkład - niższa kurtoza",
    generator = function(n) rbeta(n, shape1 = 2, shape2 = 2) * 80 + 10,
    x_min = 0, x_max = 100
  ),
  t_student = list(
    name = "t-Studenta (leptokurtyczna)",
    description = "Spiczasty rozkład - wyższa kurtoza, grubsze ogony",
    generator = function(n) rt(n, df = 5) * 15 + 50,
    x_min = 0, x_max = 100
  ),
  bimodal = list(
    name = "Bimodalny",
    description = "Dwa szczyty - mieszanka dwóch rozkładów",
    generator = function(n) {
      group <- sample(c(1, 2), n, replace = TRUE, prob = c(0.5, 0.5))
      ifelse(group == 1, rnorm(n, mean = 30, sd = 8), rnorm(n, mean = 70, sd = 8))
    },
    x_min = 0, x_max = 100
  ),
  chisq = list(
    name = "Chi-kwadrat",
    description = "Silna prawostronna skośność i wysoka kurtoza",
    generator = function(n) pmin(rchisq(n, df = 3) * 8, 100),
    x_min = 0, x_max = 100
  )
)

# UI
ui <- fluidPage(
  titlePanel("📈 Moments Explorer - Wizualizacja momentów rozkładu"),

  sidebarLayout(
    sidebarPanel(
      h4("Wybór rozkładu"),

      selectInput("distribution", "Typ rozkładu:",
                  choices = c(
                    "Normalny (Gaussa)" = "normal",
                    "Jednostajny" = "uniform",
                    "Wykładniczy" = "exponential",
                    "Gamma (prawostronna)" = "gamma_right",
                    "Gamma (lewostronna)" = "gamma_left",
                    "Beta (platykurtyczna)" = "beta_platykurtic",
                    "t-Studenta (leptokurtyczna)" = "t_student",
                    "Bimodalny" = "bimodal",
                    "Chi-kwadrat" = "chisq"
                  ),
                  selected = "normal"),

      div(
        style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
        textOutput("distribution_description")
      ),

      div(style = "font-size: 12px; color: #95a5a6; margin-bottom: 10px;",
          "Liczba obserwacji: 500 (stała)"),

      actionButton("generate", "🎲 Wygeneruj rozkład",
                   class = "btn-success btn-lg", width = "100%"),

      hr(),

      h4("Wizualizacja"),
      radioButtons("sd_level", "Przedziały odch. std:",
                   choices = c("±1σ (68%)" = 1,
                               "±2σ (95%)" = 2,
                               "±3σ (99.7%)" = 3),
                   selected = 1,
                   inline = TRUE),

      checkboxInput("show_normal", "Pokaż rozkład normalny (overlay)", value = TRUE),

      hr(),

      h4("Kroki budowy"),
      actionButton("step1", "1. Surowe dane", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step2", "2. + Średnia (μ)", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step3", "3. + Odchylenie std (σ)", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step4", "4. + Rozkład normalny", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step5", "5. + Skośność", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step6", "6. + Kurtoza", class = "btn-outline-primary", width = "100%"),
      br(), br(),
      actionButton("step7", "7. Pełna wizualizacja", class = "btn-outline-primary", width = "100%"),
      br(), br(),

      hr(),

      actionButton("reset", "🔄 Reset", class = "btn-danger", width = "100%"),

      width = 3
    ),

    mainPanel(
      # Górny panel: Wizualizacja
      div(
        style = "border: 2px solid #3498db; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
        h4(textOutput("step_title"), style = "color: #2c3e50;"),
        plotOutput("main_plot", height = "400px"),
        div(
          style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-top: 10px;",
          textOutput("step_explanation")
        )
      ),


      # Dolny panel: Tabela momentów
      div(
        style = "border: 2px solid #e67e22; border-radius: 5px; padding: 10px;",
        h4("Momenty rozkładu - statystyki", style = "color: #d35400;"),
        tableOutput("moments_table")
      ),

      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive values
  current_step <- reactiveVal(0)
  data <- reactiveVal(numeric(0))

  # Opis wybranego rozkładu
  output$distribution_description <- renderText({
    req(input$distribution)
    distributions[[input$distribution]]$description
  })

  # Generowanie danych z wybranego rozkładu
  observeEvent(input$generate, {
    req(input$distribution)
    dist <- distributions[[input$distribution]]
    new_data <- dist$generator(500)
    data(new_data)
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
  observeEvent(input$step5, { current_step(5) })
  observeEvent(input$step6, { current_step(6) })
  observeEvent(input$step7, { current_step(7) })

  # Obliczenia momentów
  moments <- reactive({
    req(length(data()) > 0)
    list(
      mean = mean(data()),
      sd = sd(data()),
      variance = var(data()),
      skewness = skewness(data()),
      kurtosis = kurtosis(data()),  # e1071 zwraca excess kurtosis (kurtoza - 3)
      min = min(data()),
      max = max(data()),
      n = length(data())
    )
  })

  # Funkcja interpretacji skośności
  interpret_skewness <- function(skew) {
    if (abs(skew) < 0.5) {
      return("Rozkład symetryczny (brak skośności)")
    } else if (skew > 0) {
      return(paste0("Rozkład prawostronnie skośny (długi prawy ogon)"))
    } else {
      return(paste0("Rozkład lewostronnie skośny (długi lewy ogon)"))
    }
  }

  # Funkcja interpretacji kurtozy
  interpret_kurtosis <- function(kurt) {
    # e1071::kurtosis zwraca excess kurtosis (kurtoza - 3)
    if (abs(kurt) < 0.5) {
      return("Mezokurtyczna (jak rozkład normalny)")
    } else if (kurt > 0) {
      return(paste0("Leptokurtyczna (wyższy szczyt, grubsze ogony)"))
    } else {
      return(paste0("Platykurtyczna (niższy szczyt, cieńsze ogony)"))
    }
  }

  # Funkcja generowania rozkładu normalnego dla overlay
  normal_density <- reactive({
    req(moments(), input$distribution)
    m <- moments()
    dist <- distributions[[input$distribution]]

    x_seq <- seq(dist$x_min, dist$x_max, length.out = 200)
    y_seq <- dnorm(x_seq, mean = m$mean, sd = m$sd)

    # Skalujemy do tego samego zakresu co histogram
    # Musimy przeskalować gęstość do częstości
    # Liczba bins musi być zgodna z histogramem (20)
    bin_width <- (dist$x_max - dist$x_min) / 20
    y_scaled <- y_seq * m$n * bin_width

    data.frame(x = x_seq, y = y_scaled)
  })

  # Tytuł kroku
  output$step_title <- renderText({
    req(input$distribution)
    dist <- distributions[[input$distribution]]
    step <- current_step()

    if (step == 0) return(paste("Rozkład:", dist$name, "- Kliknij 'Krok 1'"))
    if (step == 1) return("Krok 1: Surowe dane (histogram)")
    if (step == 2) return("Krok 2: Pierwszy moment - Średnia (μ)")
    if (step == 3) return("Krok 3: Drugi moment - Odchylenie standardowe (σ)")
    if (step == 4) return("Krok 4: Rozkład normalny (overlay)")
    if (step == 5) return("Krok 5: Trzeci moment - Skośność")
    if (step == 6) return("Krok 6: Czwarty moment - Kurtoza")
    if (step == 7) return("Krok 7: Pełna wizualizacja wszystkich momentów")
  })

  # Wyjaśnienie kroku
  output$step_explanation <- renderText({
    req(moments())
    m <- moments()
    step <- current_step()

    if (step == 0) return(paste0("Wybierz rozkład i kliknij 'Wygeneruj'. Liczba obserwacji: ", m$n))
    if (step == 1) return(paste0("Histogram pokazuje rozkład ", m$n, " obserwacji. Każdy słupek = liczba wartości w danym przedziale."))
    if (step == 2) return(paste0("ŚREDNIA (μ) = ", round(m$mean, 2), " - centrum rozkładu, pierwszy moment. To 'punkt równowagi' danych."))
    if (step == 3) {
      sd_mult <- as.numeric(input$sd_level)
      pct <- c(68, 95, 99.7)[sd_mult]
      return(paste0("ODCHYLENIE STD (σ) = ", round(m$sd, 2), " - miara rozrzutu, drugi moment. Przedział μ ± ", sd_mult, "σ zawiera ~", pct, "% danych."))
    }
    if (step == 4) return(paste0("ROZKŁAD NORMALNY (czerwony overlay) o tych samych μ i σ. Pokazuje jak wyglądałby 'idealny' rozkład normalny dla porównania."))
    if (step == 5) {
      skew_txt <- interpret_skewness(m$skewness)
      return(paste0("SKOŚNOŚĆ = ", round(m$skewness, 2), " (trzeci moment). ", skew_txt, ". Porównaj z rozkładem normalnym (symetrycznym)."))
    }
    if (step == 6) {
      kurt_txt <- interpret_kurtosis(m$kurtosis)
      return(paste0("KURTOZA = ", round(m$kurtosis, 2), " (czwarty moment, nadmiar/excess). ", kurt_txt, ". Porównaj wysokość szczytu z rozkładem normalnym."))
    }
    if (step == 7) return("Wszystkie 4 momenty razem: μ (położenie), σ (rozrzut), skośność (asymetria), kurtoza (spiczastość). Czerwony overlay = punkt odniesienia.")
  })

  # Główny wykres
  output$main_plot <- renderPlot({
    req(length(data()) > 0, moments(), input$distribution)

    dist <- distributions[[input$distribution]]
    m <- moments()
    step <- current_step()
    x_min <- dist$x_min
    x_max <- dist$x_max
    x_label <- "Wartość"

    # Przygotowanie danych
    df <- data.frame(value = data())

    # Oblicz max liczność dla stałej osi Y
    n_bins <- 20  # Kompromis między szczegółowością a ciągłością
    hist_data <- hist(data(), breaks = n_bins, plot = FALSE)
    y_max <- max(hist_data$counts) * 1.2  # Dodaj 20% marginesu na górze

    # Bazowy histogram ze stałą osią Y
    p <- ggplot(df, aes(x = value)) +
      geom_histogram(bins = n_bins, fill = "#3498db", alpha = 0.7, color = "#2c3e50") +
      theme_minimal(base_size = 14) +
      xlim(x_min, x_max) +
      ylim(0, y_max)  # Stała oś Y - lepiej widać różnice w kurtozie

    if (step >= 1) {
      p <- p + labs(x = x_label, y = "Liczba obserwacji")
    }

    # Krok 2: Średnia
    if (step >= 2) {
      p <- p +
        geom_vline(xintercept = m$mean, color = "#2c3e50", size = 1.5, linetype = "solid") +
        annotate("text", x = m$mean, y = Inf,
                 label = paste0("μ = ", round(m$mean, 2)),
                 vjust = 1.5, color = "#2c3e50", size = 5, fontface = "bold")
    }

    # Krok 3: Odchylenie standardowe (żółty kolor)
    if (step >= 3) {
      sd_mult <- as.numeric(input$sd_level)

      # Przytnij granice SD do zakresu osi X
      sd_xmin <- max(x_min, m$mean - sd_mult * m$sd)
      sd_xmax <- min(x_max, m$mean + sd_mult * m$sd)

      p <- p +
        annotate("rect",
                 xmin = sd_xmin,
                 xmax = sd_xmax,
                 ymin = 0, ymax = Inf,
                 fill = "#f39c12", alpha = 0.25) +  # Żółty, półprzezroczysty
        annotate("text",
                 x = m$mean - sd_mult * m$sd, y = Inf,
                 label = paste0("-", sd_mult, "σ"),
                 vjust = 3, hjust = 1.2, color = "#d68910", size = 4) +
        annotate("text",
                 x = m$mean + sd_mult * m$sd, y = Inf,
                 label = paste0("+", sd_mult, "σ"),
                 vjust = 3, hjust = -0.2, color = "#d68910", size = 4)
    }

    # Krok 4+: Rozkład normalny (overlay)
    if (step >= 4 && input$show_normal) {
      norm_df <- normal_density()
      p <- p +
        geom_area(data = norm_df, aes(x = x, y = y),
                  fill = "#e74c3c", alpha = 0.3, color = "#c0392b", size = 1)
    }

    # Krok 5: Skośność - adnotacje
    if (step >= 5) {
      skew <- m$skewness
      if (abs(skew) > 0.1) {
        arrow_x <- if (skew > 0) m$mean + 1.5 * m$sd else m$mean - 1.5 * m$sd
        arrow_dir <- if (skew > 0) "Ogon →" else "← Ogon"

        p <- p +
          annotate("text", x = arrow_x, y = Inf,
                   label = arrow_dir,
                   vjust = 5, color = "#e74c3c", size = 5, fontface = "bold")
      }
    }

    # Krok 6: Kurtoza - adnotacje
    if (step >= 6) {
      kurt <- m$kurtosis
      if (abs(kurt) > 0.2) {
        arrow_symbol <- if (kurt > 0) "↑ Wyższy szczyt" else "↓ Niższy szczyt"

        p <- p +
          annotate("text", x = m$mean, y = Inf,
                   label = arrow_symbol,
                   vjust = 7, color = "#9b59b6", size = 5, fontface = "bold")
      }
    }

    p
  })


  # Tabela momentów
  output$moments_table <- renderTable({
    req(moments())
    m <- moments()

    data.frame(
      Moment = c("1. Średnia (μ)",
                 "2. Wariancja (σ²)",
                 "2. Odch. std (σ)",
                 "3. Skośność",
                 "4. Kurtoza (nadmiar)"),
      Wartość = c(round(m$mean, 2),
                  round(m$variance, 2),
                  round(m$sd, 2),
                  round(m$skewness, 2),
                  round(m$kurtosis, 2)),
      Wzór = c("Σx/n",
               "Σ(x-μ)²/n",
               "√σ²",
               "E[(x-μ)³]/σ³",
               "E[(x-μ)⁴]/σ⁴ - 3"),
      Interpretacja = c("Centrum rozkładu",
                        "Rozrzut danych (kwadrat)",
                        "Rozrzut danych",
                        interpret_skewness(m$skewness),
                        interpret_kurtosis(m$kurtosis))
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)
