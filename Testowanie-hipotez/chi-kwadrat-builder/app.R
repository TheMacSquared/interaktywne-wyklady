# Test Chi-kwadrat niezależności - Builder (krok po kroku)
# Interaktywne narzędzie do nauki testu chi-kwadrat

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# ==============================================================================
# SCENARIUSZE DANYCH
# ==============================================================================

scenarios <- list(
  siedlisko_gatunek = list(
    name = "Typ siedliska vs obecność gatunku wskaźnikowego",
    row_var = "Typ siedliska",
    col_var = "Obecność gatunku",
    row_levels = c("Las", "Łąka", "Mokradło"),
    col_levels = c("Obecny", "Nieobecny"),
    # Prawdopodobieństwa przy zależności (gatunek preferuje mokradła)
    probs_dependent = matrix(c(
      0.10, 0.20,  # Las: mało obecnych
      0.12, 0.18,  # Łąka: trochę więcej
      0.25, 0.15   # Mokradło: dużo obecnych
    ), nrow = 3, byrow = TRUE),
    description = "Czy występowanie gatunku wskaźnikowego zależy od typu siedliska?"
  ),

  jakosc_sezon = list(
    name = "Klasa jakości wody vs sezon",
    row_var = "Klasa jakości",
    col_var = "Sezon",
    row_levels = c("I (dobra)", "II (umiark.)", "III (zła)"),
    col_levels = c("Wiosna", "Lato", "Jesień", "Zima"),
    probs_dependent = matrix(c(
      0.08, 0.05, 0.07, 0.10,  # I: najlepsza zimą
      0.07, 0.10, 0.08, 0.07,  # II: równomiernie
      0.05, 0.15, 0.10, 0.08   # III: najgorsza latem
    ), nrow = 3, byrow = TRUE),
    description = "Czy jakość wody w jeziorze zależy od pory roku?"
  ),

  zbiornik_stan = list(
    name = "Typ zbiornika vs stan ekologiczny",
    row_var = "Typ zbiornika",
    col_var = "Stan ekologiczny",
    row_levels = c("Naturalny", "Sztuczny"),
    col_levels = c("Dobry", "Umiarkowany", "Zły"),
    probs_dependent = matrix(c(
      0.25, 0.15, 0.10,  # Naturalny: częściej dobry
      0.10, 0.20, 0.20   # Sztuczny: częściej zły
    ), nrow = 2, byrow = TRUE),
    description = "Czy stan ekologiczny zależy od pochodzenia zbiornika wodnego?"
  ),

  teren_zanieczyszczenie = list(
    name = "Użytkowanie terenu vs przekroczenie normy",
    row_var = "Użytkowanie terenu",
    col_var = "Norma zanieczyszczeń",
    row_levels = c("Las", "Pole uprawne", "Miasto"),
    col_levels = c("W normie", "Przekroczona"),
    probs_dependent = matrix(c(
      0.18, 0.02,  # Las: rzadko przekroczona
      0.12, 0.18,  # Pole: często przekroczona
      0.08, 0.22   # Miasto: bardzo często przekroczona
    ), nrow = 3, byrow = TRUE),
    description = "Czy przekroczenie norm zanieczyszczeń zależy od użytkowania terenu w zlewni?"
  ),

  metoda_wynik_brak = list(
    name = "Metoda poboru vs jakość próbki (brak zależności)",
    row_var = "Metoda poboru",
    col_var = "Jakość próbki",
    row_levels = c("Automatyczna", "Ręczna", "Kompozytowa"),
    col_levels = c("Dobra", "Akceptowalna", "Zła"),
    # Równomierne prawdopodobieństwa - BRAK zależności
    probs_dependent = matrix(c(
      0.11, 0.11, 0.11,
      0.11, 0.11, 0.11,
      0.11, 0.12, 0.11
    ), nrow = 3, byrow = TRUE),
    description = "Czy jakość próbki zależy od metody poboru? UWAGA: To przykład, gdzie NIE oczekujemy zależności między zmiennymi."
  )
)

# ==============================================================================
# FUNKCJE POMOCNICZE
# ==============================================================================

generate_data <- function(scenario, n) {
  set.seed(NULL)
  s <- scenarios[[scenario]]

  # Normalizuj prawdopodobieństwa
  probs <- s$probs_dependent / sum(s$probs_dependent)

  # Generuj dane jako wektor
  cells <- length(probs)
  counts <- as.vector(rmultinom(1, n, as.vector(probs)))

  # Utwórz macierz
  observed <- matrix(counts, nrow = length(s$row_levels), byrow = FALSE)
  rownames(observed) <- s$row_levels
  colnames(observed) <- s$col_levels

  observed
}

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  titlePanel("Test Chi-kwadrat niezależności - Krok po kroku"),

  sidebarLayout(
    sidebarPanel(
      h4("Wybierz scenariusz"),
      selectInput("scenario", "Dane:",
                  choices = c(
                    "Siedlisko vs gatunek" = "siedlisko_gatunek",
                    "Jakość wody vs sezon" = "jakosc_sezon",
                    "Typ zbiornika vs stan" = "zbiornik_stan",
                    "Teren vs zanieczyszczenie" = "teren_zanieczyszczenie",
                    "⚠️ Metoda vs jakość (brak zależności)" = "metoda_wynik_brak"
                  )),

      hr(),

      sliderInput("n", "Łączna liczba obserwacji:",
                  min = 50, max = 300, value = 150, step = 10),

      hr(),

      h4("Kroki analizy"),

      actionButton("step1", "1. Hipotezy i dane", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step2", "2. Sumy brzegowe", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step3", "3. Liczebności oczekiwane", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step4", "4. Różnice O - E", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step5", "5. Statystyka χ²", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step6", "6. Decyzja", width = "100%", class = "btn-outline-primary"),

      hr(),

      actionButton("reset", "Losuj nowe dane", class = "btn-warning", width = "100%"),

      hr(),

      h4("Statystyki"),
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; font-size: 13px;",
        uiOutput("stats_panel")
      ),

      width = 3
    ),

    mainPanel(
      # Opis scenariusza
      div(
        style = "background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
        h4(textOutput("scenario_title")),
        p(textOutput("scenario_desc"))
      ),

      # Wizualizacja główna
      uiOutput("main_display"),

      # Wyjaśnienie kroku
      div(
        style = "background-color: #fff3cd; padding: 15px; border-radius: 5px; margin-top: 20px;",
        h4("Co widzimy na tym kroku?"),
        uiOutput("step_explanation")
      ),

      width = 9
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # Reactive values
  observed <- reactiveVal(NULL)
  current_step <- reactiveVal(1)

  # Inicjalizacja danych
  observe({
    observed(generate_data(input$scenario, input$n))
  })

  # Reset danych
  observeEvent(input$reset, {
    observed(generate_data(input$scenario, input$n))
  })

  # Zmiana scenariusza lub n
  observeEvent(c(input$scenario, input$n), {
    observed(generate_data(input$scenario, input$n))
    current_step(1)
  })

  # Obsługa kroków
  observeEvent(input$step1, { current_step(1) })
  observeEvent(input$step2, { current_step(2) })
  observeEvent(input$step3, { current_step(3) })
  observeEvent(input$step4, { current_step(4) })
  observeEvent(input$step5, { current_step(5) })
  observeEvent(input$step6, { current_step(6) })

  # Obliczenia
  stats <- reactive({
    req(observed())
    obs <- observed()

    # Sumy brzegowe
    row_sums <- rowSums(obs)
    col_sums <- colSums(obs)
    total <- sum(obs)

    # Oczekiwane
    expected <- outer(row_sums, col_sums) / total

    # Test chi-kwadrat
    chi_test <- chisq.test(obs, correct = FALSE)

    # V Cramera
    k <- min(nrow(obs), ncol(obs))
    cramers_v <- sqrt(chi_test$statistic / (total * (k - 1)))

    list(
      observed = obs,
      expected = expected,
      row_sums = row_sums,
      col_sums = col_sums,
      total = total,
      chi_squared = chi_test$statistic,
      df = chi_test$parameter,
      p_value = chi_test$p.value,
      cramers_v = cramers_v,
      residuals = obs - expected,
      std_residuals = chi_test$residuals
    )
  })

  # Tytuł i opis scenariusza
  output$scenario_title <- renderText({
    scenarios[[input$scenario]]$name
  })

  output$scenario_desc <- renderText({
    scenarios[[input$scenario]]$description
  })

  # Panel statystyk
  output$stats_panel <- renderUI({
    req(stats())
    st <- stats()

    HTML(paste0(
      "<b>n = ", st$total, "</b><br><br>",
      "<b>χ² = ", round(st$chi_squared, 2), "</b><br>",
      "<b>df = ", st$df, "</b><br>",
      "<b>p = ", format(st$p_value, digits = 4), "</b><br><br>",
      "<b>V Cramera = ", round(st$cramers_v, 3), "</b><br>",
      "<small>(",
      if(st$cramers_v < 0.1) "pomijalny" else
      if(st$cramers_v < 0.3) "słaby" else
      if(st$cramers_v < 0.5) "umiarkowany" else "silny",
      " związek)</small>"
    ))
  })

  # Główna wizualizacja
  output$main_display <- renderUI({
    step <- current_step()

    if(step %in% c(1, 2, 3, 4)) {
      # Tabele
      tableOutput("main_table")
    } else if(step == 5) {
      # Wykres składników chi-kwadrat
      tagList(
        tableOutput("chi_table"),
        plotOutput("chi_plot", height = "300px")
      )
    } else {
      # Wykres mozaikowy + wynik
      tagList(
        plotOutput("mosaic_plot", height = "350px"),
        tableOutput("summary_table")
      )
    }
  })

  # Tabela główna
  output$main_table <- renderTable({
    req(stats())
    st <- stats()
    s <- scenarios[[input$scenario]]
    step <- current_step()

    if(step == 1) {
      # Tylko obserwowane
      df <- as.data.frame(st$observed)
      df <- cbind(data.frame(` ` = rownames(st$observed)), df)
      df
    } else if(step == 2) {
      # Z sumami brzegowymi
      df <- as.data.frame(st$observed)
      df$`Suma` <- st$row_sums
      df <- cbind(data.frame(` ` = rownames(st$observed)), df)
      df <- rbind(df, c("Suma", st$col_sums, st$total))
      df
    } else if(step == 3) {
      # Oczekiwane
      df <- as.data.frame(round(st$expected, 1))
      df <- cbind(data.frame(` ` = rownames(st$observed)), df)
      df
    } else if(step == 4) {
      # Różnice O - E
      df <- as.data.frame(round(st$residuals, 1))
      df <- cbind(data.frame(` ` = rownames(st$observed)), df)
      df
    }
  }, rownames = FALSE, digits = 1)

  # Tabela składników chi-kwadrat
  output$chi_table <- renderTable({
    req(stats())
    st <- stats()

    # (O-E)²/E dla każdej komórki
    chi_components <- (st$observed - st$expected)^2 / st$expected
    df <- as.data.frame(round(chi_components, 2))
    df <- cbind(data.frame(` ` = rownames(st$observed)), df)
    df <- rbind(df, c("Suma", round(colSums(chi_components), 2)))
    df
  }, rownames = FALSE, digits = 2)

  # Wykres składników chi-kwadrat
  output$chi_plot <- renderPlot({
    req(stats())
    st <- stats()
    s <- scenarios[[input$scenario]]

    # Przygotuj dane do wykresu
    chi_components <- (st$observed - st$expected)^2 / st$expected

    df_plot <- as.data.frame(as.table(chi_components))
    names(df_plot) <- c("row", "col", "value")
    df_plot$contribution <- df_plot$value / st$chi_squared * 100

    ggplot(df_plot, aes(x = col, y = row, fill = value)) +
      geom_tile(color = "white", size = 1) +
      geom_text(aes(label = paste0(round(value, 1), "\n(",
                                   round(contribution, 0), "%)")),
                color = "white", size = 4, fontface = "bold") +
      scale_fill_gradient(low = "#3498db", high = "#e74c3c",
                         name = "(O-E)²/E") +
      labs(
        title = "Wkład każdej komórki do χ²",
        subtitle = paste0("χ² = ", round(st$chi_squared, 2), " (suma wszystkich komórek)"),
        x = s$col_var,
        y = s$row_var
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "right"
      )
  })

  # Wykres mozaikowy
  output$mosaic_plot <- renderPlot({
    req(stats())
    st <- stats()
    s <- scenarios[[input$scenario]]

    # Przygotuj dane
    df <- as.data.frame(as.table(st$observed))
    names(df) <- c("row", "col", "count")

    # Proporcje
    df <- df %>%
      group_by(row) %>%
      mutate(prop = count / sum(count)) %>%
      ungroup()

    is_sig <- st$p_value < 0.05
    title_color <- if(is_sig) "#27ae60" else "#e74c3c"

    ggplot(df, aes(x = row, y = prop, fill = col)) +
      geom_bar(stat = "identity", position = "fill", width = 0.8) +
      scale_fill_brewer(palette = "Set2", name = s$col_var) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = if(is_sig) "ZWIĄZEK ISTOTNY" else "BRAK ISTOTNEGO ZWIĄZKU",
        subtitle = paste0("χ²(", st$df, ") = ", round(st$chi_squared, 2),
                         ", p = ", format(st$p_value, digits = 4),
                         ", V = ", round(st$cramers_v, 3)),
        x = s$row_var,
        y = "Proporcja"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 18, face = "bold", color = title_color),
        legend.position = "bottom"
      )
  })

  # Tabela podsumowująca
  output$summary_table <- renderTable({
    req(stats())
    st <- stats()
    s <- scenarios[[input$scenario]]

    # Proporcje w wierszach
    props <- prop.table(st$observed, margin = 1) * 100
    df <- as.data.frame(round(props, 1))
    df <- cbind(data.frame(` ` = rownames(st$observed)), df)
    df
  }, rownames = FALSE, digits = 1)

  # Wyjaśnienia kroków
  output$step_explanation <- renderUI({
    step <- current_step()
    st <- stats()
    s <- scenarios[[input$scenario]]

    explanations <- list(
      step1 = HTML(paste0(
        "<p><b>Hipotezy statystyczne</b> dla testu chi-kwadrat niezależności:</p>",
        "<ul>",
        "<li><b>H₀ (hipoteza zerowa):</b> Zmienne są niezależne (brak związku między ",
        s$row_var, " a ", s$col_var, ")</li>",
        "<li><b>Hₐ (hipoteza alternatywna):</b> Zmienne są zależne (istnieje związek)</li>",
        "</ul>",
        "<p><b>Tabela obserwowanych liczebności</b> pokazuje, ile obserwacji przypada ",
        "na każdą kombinację kategorii.</p>",
        "<p>Mamy ", length(s$row_levels), " kategorie zmiennej \"", s$row_var,
        "\" i ", length(s$col_levels), " kategorie zmiennej \"", s$col_var, "\".</p>"
      )),

      step2 = HTML(paste0(
        "<p><b>Sumy brzegowe (marginesy)</b> pokazują łączne liczebności dla każdej kategorii.</p>",
        "<p>Suma wiersza = ile obserwacji w danej kategorii ", s$row_var, "</p>",
        "<p>Suma kolumny = ile obserwacji w danej kategorii ", s$col_var, "</p>",
        "<p>Łączna suma = ", st$total, " obserwacji</p>",
        "<p>Te sumy są potrzebne do obliczenia <b>oczekiwanych liczebności</b>.</p>"
      )),

      step3 = HTML(paste0(
        "<p><b>Liczebności oczekiwane</b> to wartości, które byśmy obserwowali, ",
        "gdyby zmienne były <b>niezależne</b>.</p>",
        "<p><b>Wzór:</b> E = (suma wiersza × suma kolumny) / suma całkowita</p>",
        "<p><b>Interpretacja:</b> Przy niezależności, proporcje w każdym wierszu powinny ",
        "być takie same jak proporcje ogólne (w sumach kolumn).</p>",
        "<p>Porównaj te wartości z obserwowanymi - duże różnice sugerują zależność.</p>"
      )),

      step4 = HTML(paste0(
        "<p><b>Różnice O - E</b> (residua) pokazują, gdzie obserwowane wartości ",
        "odbiegają od oczekiwanych.</p>",
        "<p><b>Wartości dodatnie:</b> więcej obserwacji niż oczekiwano przy niezależności</p>",
        "<p><b>Wartości ujemne:</b> mniej obserwacji niż oczekiwano</p>",
        "<p>Duże wartości bezwzględne wskazują komórki, które najbardziej ",
        "przyczyniają się do odchylenia od niezależności.</p>"
      )),

      step5 = HTML(paste0(
        "<p><b>Statystyka χ²</b> podsumowuje wszystkie różnice w jednej liczbie.</p>",
        "<p><b>Wzór:</b> χ² = Σ (O - E)² / E</p>",
        "<p>Dla każdej komórki obliczamy (O-E)²/E, a potem sumujemy.</p>",
        "<p><b>χ² = ", round(st$chi_squared, 2), "</b></p>",
        "<p>Im większe χ², tym większe odchylenie od niezależności.</p>",
        "<p>Wykres pokazuje wkład każdej komórki - ciemniejsze = większy wkład.</p>"
      )),

      step6 = HTML(paste0(
        "<p><b>Decyzja:</b> ",
        if(st$p_value < 0.05)
          paste0("p = ", format(st$p_value, digits = 4), " < 0.05 → <b>Odrzucamy H₀</b>")
        else
          paste0("p = ", format(st$p_value, digits = 4), " ≥ 0.05 → <b>Nie odrzucamy H₀</b>"),
        "</p>",
        "<p><b>Siła związku (V Cramera):</b> ", round(st$cramers_v, 3), " - ",
        if(st$cramers_v < 0.1) "pomijalny" else
        if(st$cramers_v < 0.3) "słaby" else
        if(st$cramers_v < 0.5) "umiarkowany" else "silny",
        " związek</p>",
        "<p><b>Interpretacja:</b> ",
        if(st$p_value < 0.05)
          paste0("Istnieje istotny związek między ", s$row_var, " a ", s$col_var,
                ". Wykres pokazuje, jak proporcje ", s$col_var, " różnią się między kategoriami ", s$row_var, ".")
        else
          paste0("Brak dowodów na związek między ", s$row_var, " a ", s$col_var,
                ". Różnice w proporcjach mogą być przypadkowe."),
        "</p>"
      ))
    )

    explanations[[paste0("step", step)]]
  })
}

# ==============================================================================
# URUCHOMIENIE
# ==============================================================================

shinyApp(ui = ui, server = server)
