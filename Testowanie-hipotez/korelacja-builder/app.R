# Korelacja Pearsona - Builder (krok po kroku)
# Interaktywne narzędzie do nauki korelacji

library(shiny)
library(ggplot2)
library(dplyr)
library(MASS)

# ==============================================================================
# SCENARIUSZE DANYCH
# ==============================================================================

scenarios <- list(
  temperatura = list(
    name = "Temperatura powietrza vs temperatura wody",
    x_name = "Temperatura powietrza",
    y_name = "Temperatura wody",
    x_unit = "°C",
    y_unit = "°C",
    x_mean = 18,
    y_mean = 14,
    x_sd = 6,
    y_sd = 4,
    r_true = 0.85,
    description = "Związek między temperaturą powietrza a temperaturą wody w jeziorze."
  ),
  opady_przeplyw = list(
    name = "Opady vs przepływ rzeki",
    x_name = "Suma opadów miesięcznych",
    y_name = "Średni przepływ",
    x_unit = "mm",
    y_unit = "m³/s",
    x_mean = 60,
    y_mean = 12,
    x_sd = 25,
    y_sd = 5,
    r_true = 0.72,
    description = "Związek między sumą opadów a przepływem rzeki w skali miesięcznej."
  ),
  glebokosc_przezroczystosc = list(
    name = "Głębokość vs przezroczystość",
    x_name = "Głębokość jeziora",
    y_name = "Przezroczystość (Secchi)",
    x_unit = "m",
    y_unit = "m",
    x_mean = 8,
    y_mean = 3,
    x_sd = 4,
    y_sd = 1.5,
    r_true = 0.55,
    description = "Związek między głębokością jeziora a przezroczystością wody (głębokość krążka Secchiego)."
  ),
  zlewnia_przeplyw = list(
    name = "Powierzchnia zlewni vs przepływ",
    x_name = "Powierzchnia zlewni",
    y_name = "Średni roczny przepływ",
    x_unit = "km²",
    y_unit = "m³/s",
    x_mean = 500,
    y_mean = 8,
    x_sd = 300,
    y_sd = 5,
    r_true = 0.90,
    description = "Związek między powierzchnią zlewni a średnim rocznym przepływem rzeki."
  ),
  ph_chlorofil_brak = list(
    name = "pH vs chlorofil (brak korelacji)",
    x_name = "pH wody",
    y_name = "Chlorofil a",
    x_unit = "pH",
    y_unit = "μg/L",
    x_mean = 7.5,
    y_mean = 15,
    x_sd = 0.8,
    y_sd = 8,
    r_true = 0.05,
    description = "Związek między pH wody a stężeniem chlorofilu a w jeziorach. UWAGA: To przykład, gdzie NIE oczekujemy istotnej korelacji."
  )
)

# ==============================================================================
# FUNKCJE POMOCNICZE
# ==============================================================================

generate_data <- function(scenario, n) {
  set.seed(NULL)
  s <- scenarios[[scenario]]

  # Generuj skorelowane dane
  Sigma <- matrix(c(1, s$r_true, s$r_true, 1), 2)
  raw_data <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)

  # Skaluj do realistycznych wartości
  x <- raw_data[,1] * s$x_sd + s$x_mean
  y <- raw_data[,2] * s$y_sd + s$y_mean

  data.frame(x = x, y = y)
}

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  titlePanel("Korelacja Pearsona - Krok po kroku"),

  sidebarLayout(
    sidebarPanel(
      h4("Wybierz scenariusz"),
      selectInput("scenario", "Dane:",
                  choices = c(
                    "Temperatura powietrza vs wody" = "temperatura",
                    "Opady vs przepływ rzeki" = "opady_przeplyw",
                    "Głębokość vs przezroczystość" = "glebokosc_przezroczystosc",
                    "Powierzchnia zlewni vs przepływ" = "zlewnia_przeplyw",
                    "⚠️ pH vs chlorofil (brak korelacji)" = "ph_chlorofil_brak"
                  )),

      hr(),

      sliderInput("n", "Liczba obserwacji:",
                  min = 10, max = 50, value = 25, step = 5),

      hr(),

      h4("Kroki analizy"),

      actionButton("step1", "1. Hipotezy i dane", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step2", "2. Średnie (linie krzyżowe)", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step3", "3. Ćwiartki i odchylenia", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step4", "4. Współczynnik r", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step5", "5. Linia regresji", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step6", "6. r² i interpretacja", width = "100%", class = "btn-outline-primary"),

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

      # Wykres główny
      plotOutput("main_plot", height = "450px"),

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
  dane <- reactiveVal(NULL)
  current_step <- reactiveVal(1)

  # Inicjalizacja danych
  observe({
    dane(generate_data(input$scenario, input$n))
  })

  # Reset danych
  observeEvent(input$reset, {
    dane(generate_data(input$scenario, input$n))
  })

  # Zmiana scenariusza lub n
  observeEvent(c(input$scenario, input$n), {
    dane(generate_data(input$scenario, input$n))
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
    req(dane())
    d <- dane()

    cor_test <- cor.test(d$x, d$y)
    lm_fit <- lm(y ~ x, data = d)

    list(
      mean_x = mean(d$x),
      mean_y = mean(d$y),
      sd_x = sd(d$x),
      sd_y = sd(d$y),
      r = cor(d$x, d$y),
      r_squared = cor(d$x, d$y)^2,
      p_value = cor_test$p.value,
      ci_low = cor_test$conf.int[1],
      ci_high = cor_test$conf.int[2],
      intercept = coef(lm_fit)[1],
      slope = coef(lm_fit)[2],
      n = nrow(d)
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
    s <- scenarios[[input$scenario]]

    HTML(paste0(
      "<b>", s$x_name, ":</b><br>",
      "x̄ = ", round(st$mean_x, 2), " ", s$x_unit, "<br>",
      "SD = ", round(st$sd_x, 2), "<br><br>",
      "<b>", s$y_name, ":</b><br>",
      "ȳ = ", round(st$mean_y, 2), " ", s$y_unit, "<br>",
      "SD = ", round(st$sd_y, 2), "<br><br>",
      "<b>Korelacja:</b><br>",
      "r = ", round(st$r, 3), "<br>",
      "r² = ", round(st$r_squared, 3), "<br>",
      "p = ", format(st$p_value, digits = 4)
    ))
  })

  # Wykres główny
  output$main_plot <- renderPlot({
    req(dane(), stats())
    d <- dane()
    st <- stats()
    s <- scenarios[[input$scenario]]
    step <- current_step()

    # Kolory
    col_point <- "#3498db"
    col_mean <- "#e74c3c"
    col_line <- "#2c3e50"
    col_pos <- "#27ae60"  # zielony dla ćwiartek dodatnich
    col_neg <- "#e74c3c"  # czerwony dla ćwiartek ujemnych

    # Bazowy wykres
    base_plot <- ggplot(d, aes(x = x, y = y)) +
      labs(
        x = paste0(s$x_name, " (", s$x_unit, ")"),
        y = paste0(s$y_name, " (", s$y_unit, ")")
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(size = 18, face = "bold"))

    if (step == 1) {
      # KROK 1: Hipotezy i wykres rozrzutu
      base_plot +
        geom_point(size = 4, alpha = 0.7, color = col_point) +
        # Dodaj panel z hipotezami
        annotate("label", x = min(d$x) + diff(range(d$x)) * 0.02,
                 y = max(d$y) - diff(range(d$y)) * 0.02,
                 label = paste0("H₀: ρ = 0 (brak korelacji)\n",
                               "Hₐ: ρ ≠ 0 (jest korelacja)"),
                 size = 4.5, fill = "#e8f4f8", fontface = "bold",
                 hjust = 0, vjust = 1,
                 label.padding = unit(0.5, "lines")) +
        labs(
          title = "Krok 1: Hipotezy i dane",
          subtitle = paste0("n = ", st$n, " obserwacji | Czy zmienne są skorelowane?")
        )

    } else if (step == 2) {
      # KROK 2: Średnie (linie krzyżowe)
      base_plot +
        geom_vline(xintercept = st$mean_x, color = col_mean, size = 1, linetype = "dashed") +
        geom_hline(yintercept = st$mean_y, color = col_mean, size = 1, linetype = "dashed") +
        geom_point(size = 4, alpha = 0.7, color = col_point) +
        annotate("point", x = st$mean_x, y = st$mean_y, size = 6, color = col_mean, shape = 18) +
        annotate("label", x = st$mean_x, y = max(d$y),
                 label = paste0("x̄ = ", round(st$mean_x, 1)),
                 fill = "#fff3cd", size = 4) +
        annotate("label", x = max(d$x), y = st$mean_y,
                 label = paste0("ȳ = ", round(st$mean_y, 1)),
                 fill = "#fff3cd", size = 4, hjust = 1) +
        labs(
          title = "Krok 2: Średnie obu zmiennych",
          subtitle = "Linie krzyżowe dzielą wykres na 4 ćwiartki"
        )

    } else if (step == 3) {
      # KROK 3: Ćwiartki i odchylenia
      # Dodaj kolory ćwiartek (używając >= i <= aby uniknąć NA)
      d_plot <- d
      d_plot$contribution <- with(d_plot, case_when(
        x >= st$mean_x & y >= st$mean_y ~ "dodatni",
        x <= st$mean_x & y <= st$mean_y ~ "dodatni",
        TRUE ~ "ujemny"
      ))

      # Prostokąty ćwiartek - nowy wykres z zaktualizowanymi danymi
      ggplot(d_plot, aes(x = x, y = y)) +
        annotate("rect",
                 xmin = st$mean_x, xmax = Inf, ymin = st$mean_y, ymax = Inf,
                 fill = col_pos, alpha = 0.1) +
        annotate("rect",
                 xmin = -Inf, xmax = st$mean_x, ymin = -Inf, ymax = st$mean_y,
                 fill = col_pos, alpha = 0.1) +
        annotate("rect",
                 xmin = st$mean_x, xmax = Inf, ymin = -Inf, ymax = st$mean_y,
                 fill = col_neg, alpha = 0.1) +
        annotate("rect",
                 xmin = -Inf, xmax = st$mean_x, ymin = st$mean_y, ymax = Inf,
                 fill = col_neg, alpha = 0.1) +
        geom_vline(xintercept = st$mean_x, color = col_mean, size = 1, linetype = "dashed") +
        geom_hline(yintercept = st$mean_y, color = col_mean, size = 1, linetype = "dashed") +
        geom_segment(aes(xend = st$mean_x, yend = y), alpha = 0.3, color = "gray50") +
        geom_segment(aes(xend = x, yend = st$mean_y), alpha = 0.3, color = "gray50") +
        geom_point(aes(color = contribution), size = 4, alpha = 0.8) +
        scale_color_manual(values = c("dodatni" = col_pos, "ujemny" = col_neg),
                          name = "Wkład do r") +
        annotate("text", x = max(d$x), y = max(d$y), label = "++", size = 8, color = col_pos, hjust = 1) +
        annotate("text", x = min(d$x), y = min(d$y), label = "--", size = 8, color = col_pos, hjust = 0) +
        annotate("text", x = max(d$x), y = min(d$y), label = "+-", size = 8, color = col_neg, hjust = 1) +
        annotate("text", x = min(d$x), y = max(d$y), label = "-+", size = 8, color = col_neg, hjust = 0) +
        labs(
          title = "Krok 3: Ćwiartki i wkład do korelacji",
          subtitle = "Zielone ćwiartki zwiększają r, czerwone zmniejszają",
          x = paste0(s$x_name, " (", s$x_unit, ")"),
          y = paste0(s$y_name, " (", s$y_unit, ")")
        ) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(size = 18, face = "bold"))

    } else if (step == 4) {
      # KROK 4: Współczynnik r
      r_text <- round(st$r, 3)
      r_interpretation <- if(abs(st$r) < 0.3) "słaba" else
                          if(abs(st$r) < 0.6) "umiarkowana" else
                          if(abs(st$r) < 0.9) "silna" else "bardzo silna"
      r_direction <- if(st$r > 0) "dodatnia" else "ujemna"

      base_plot +
        geom_point(size = 4, alpha = 0.7, color = col_point) +
        geom_vline(xintercept = st$mean_x, color = col_mean, size = 0.5, linetype = "dashed", alpha = 0.5) +
        geom_hline(yintercept = st$mean_y, color = col_mean, size = 0.5, linetype = "dashed", alpha = 0.5) +
        # Etykieta z wartością r (prawy górny róg)
        annotate("label", x = max(d$x) - diff(range(d$x)) * 0.02,
                 y = max(d$y) - diff(range(d$y)) * 0.02,
                 label = paste0("r = ", r_text, "\n(", r_interpretation, " ", r_direction, ")"),
                 size = 6, fill = "#d4edda", fontface = "bold", hjust = 1, vjust = 1) +
        labs(
          title = "Krok 4: Współczynnik korelacji Pearsona",
          subtitle = paste0("r = ", r_text, " (", r_interpretation, " ", r_direction, ")")
        )

    } else if (step == 5) {
      # KROK 5: Linia regresji
      base_plot +
        geom_smooth(method = "lm", se = TRUE, color = col_line, fill = col_line, alpha = 0.2) +
        geom_segment(aes(xend = x, yend = predict(lm(y ~ x, data = d))),
                     alpha = 0.4, color = col_neg, linetype = "dotted") +
        geom_point(size = 4, alpha = 0.7, color = col_point) +
        annotate("label", x = min(d$x), y = max(d$y),
                 label = paste0("y = ", round(st$intercept, 2), " + ",
                               round(st$slope, 3), " × x"),
                 size = 5, fill = "#fff3cd", hjust = 0) +
        labs(
          title = "Krok 5: Linia regresji",
          subtitle = "Linia najlepszego dopasowania + odległości punktów (residua)"
        )

    } else if (step == 6) {
      # KROK 6: r² i interpretacja
      r2_pct <- round(st$r_squared * 100, 1)

      # Wykres z kolorowym paskiem pokazującym r²
      base_plot +
        geom_smooth(method = "lm", se = FALSE, color = col_line, size = 1.2) +
        geom_point(size = 4, alpha = 0.7, color = col_point) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = max(d$y) * 1.02, ymax = max(d$y) * 1.12,
                 fill = col_pos, alpha = st$r_squared) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = max(d$y) * 1.02, ymax = max(d$y) * 1.12,
                 fill = NA, color = "black", size = 0.5) +
        annotate("text", x = mean(range(d$x)), y = max(d$y) * 1.07,
                 label = paste0("r² = ", round(st$r_squared, 3), " (", r2_pct, "% zmienności wyjaśnione)"),
                 size = 5, fontface = "bold") +
        labs(
          title = "Krok 6: Współczynnik determinacji r²",
          subtitle = paste0(r2_pct, "% zmienności ", s$y_name, " jest wyjaśnione przez ", s$x_name)
        )
    }
  })

  # Wyjaśnienia kroków
  output$step_explanation <- renderUI({
    step <- current_step()
    st <- stats()
    s <- scenarios[[input$scenario]]

    r_interpretation <- if(abs(st$r) < 0.3) "słaba" else
                        if(abs(st$r) < 0.6) "umiarkowana" else
                        if(abs(st$r) < 0.9) "silna" else "bardzo silna"

    explanations <- list(
      step1 = HTML(paste0(
        "<p><b>Hipotezy statystyczne</b> dla testu korelacji:</p>",
        "<ul>",
        "<li><b>H₀ (hipoteza zerowa):</b> ρ = 0 — brak korelacji w populacji</li>",
        "<li><b>Hₐ (hipoteza alternatywna):</b> ρ ≠ 0 — istnieje korelacja</li>",
        "</ul>",
        "<p><b>Wykres rozrzutu (scatter plot)</b> to podstawowe narzędzie do wizualizacji związku ",
        "między dwiema zmiennymi ilościowymi. Każdy punkt to jedna obserwacja.</p>",
        "<p><b>Pytanie:</b> Czy widać wzorzec? Czy gdy ", s$x_name,
        " rośnie, to ", s$y_name, " też rośnie (lub maleje)?</p>"
      )),

      step2 = HTML(paste0(
        "<p><b>Średnie</b> obu zmiennych (x̄ i ȳ) wyznaczają \"środek ciężkości\" danych.</p>",
        "<p>Linie krzyżowe dzielą wykres na <b>4 ćwiartki</b>:</p>",
        "<ul>",
        "<li><b>++ (prawy górny):</b> powyżej średniej w obu zmiennych</li>",
        "<li><b>-- (lewy dolny):</b> poniżej średniej w obu zmiennych</li>",
        "<li><b>+- (prawy dolny):</b> powyżej x̄, poniżej ȳ</li>",
        "<li><b>-+ (lewy górny):</b> poniżej x̄, powyżej ȳ</li>",
        "</ul>"
      )),

      step3 = HTML(paste0(
        "<p><b>Wkład punktów do korelacji:</b></p>",
        "<p><b>Ćwiartki ++ i --</b> (zielone): Punkty tu zwiększają wartość r ",
        "(obie zmienne odchylają się w tym samym kierunku od średniej)</p>",
        "<p><b>Ćwiartki +- i -+</b> (czerwone): Punkty tu zmniejszają wartość r ",
        "(zmienne odchylają się w przeciwnych kierunkach)</p>",
        "<p>Jeśli większość punktów jest w zielonych ćwiartkach → r > 0 (korelacja dodatnia)</p>",
        "<p>Jeśli większość punktów jest w czerwonych ćwiartkach → r < 0 (korelacja ujemna)</p>"
      )),

      step4 = HTML(paste0(
        "<p><b>Współczynnik korelacji Pearsona (r)</b> mierzy siłę i kierunek liniowego związku.</p>",
        "<p><b>r = ", round(st$r, 3), "</b> — ",
        if(st$r > 0) "dodatnia" else "ujemna", " ", r_interpretation, " korelacja</p>",
        "<hr>",
        "<p><b>Test istotności korelacji:</b></p>",
        "<ul>",
        "<li><b>H₀:</b> ρ = 0 (brak korelacji w populacji)</li>",
        "<li><b>Hₐ:</b> ρ ≠ 0 (korelacja istnieje)</li>",
        "</ul>",
        "<p><b>p-value = ", format(st$p_value, digits = 4), "</b></p>",
        if(st$p_value < 0.05)
          "<p style='color: green;'><b>p < 0.05 → Odrzucamy H₀</b><br>Korelacja jest istotna statystycznie.</p>"
        else
          "<p style='color: red;'><b>p ≥ 0.05 → Nie odrzucamy H₀</b><br>Brak wystarczających dowodów na istotną korelację.</p>",
        "<hr>",
        "<p><b>Interpretacja siły |r|:</b> < 0.3 słaba | 0.3-0.6 umiarkowana | 0.6-0.9 silna | > 0.9 bardzo silna</p>"
      )),

      step5 = HTML(paste0(
        "<p><b>Linia regresji</b> to prosta najlepiej dopasowana do danych.</p>",
        "<p><b>Równanie:</b> ", s$y_name, " = ", round(st$intercept, 2), " + ",
        round(st$slope, 3), " × ", s$x_name, "</p>",
        "<p><b>Nachylenie (", round(st$slope, 3), "):</b> Gdy ", s$x_name,
        " wzrasta o 1 ", s$x_unit, ", ", s$y_name,
        if(st$slope > 0) " wzrasta" else " maleje",
        " średnio o ", round(abs(st$slope), 3), " ", s$y_unit, "</p>",
        "<p><b>Residua</b> (pionowe linie przerywane) pokazują odległość punktów od linii - ",
        "im mniejsze residua, tym lepsze dopasowanie.</p>"
      )),

      step6 = HTML(paste0(
        "<p><b>Współczynnik determinacji r²</b> mówi, jaki procent zmienności Y jest wyjaśniony przez X.</p>",
        "<p><b>r² = ", round(st$r_squared, 3), " (", round(st$r_squared * 100, 1), "%)</b></p>",
        "<p><b>Interpretacja:</b> ", round(st$r_squared * 100, 1), "% zmienności w ", s$y_name,
        " można wyjaśnić znajomością ", s$x_name, ". Pozostałe ",
        round((1 - st$r_squared) * 100, 1), "% to zmienność niewyjaśniona (inne czynniki, losowość).</p>",
        "<p><b>UWAGA:</b> Korelacja nie oznacza przyczynowości!</p>"
      ))
    )

    explanations[[paste0("step", step)]]
  })
}

# ==============================================================================
# URUCHOMIENIE
# ==============================================================================

shinyApp(ui = ui, server = server)
