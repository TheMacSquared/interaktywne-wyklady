# Test t dla dwóch prób - Builder (krok po kroku)
# Interaktywne narzędzie do nauki testu t

library(shiny)
library(ggplot2)
library(dplyr)

# ==============================================================================
# SCENARIUSZE DANYCH
# ==============================================================================

scenarios <- list(
  azotany = list(
    name = "Stężenie azotanów w rzece",
    group1_name = "Powyżej oczyszczalni",
    group2_name = "Poniżej oczyszczalni",
    unit = "mg/L",
    mu1 = 2.5,
    mu2 = 4.8,
    sigma = 1.2,
    description = "Porównanie stężenia azotanów w rzece powyżej i poniżej punktu zrzutu ścieków z oczyszczalni."
  ),
  temperatura = list(
    name = "Temperatura wody w strumieniach",
    group1_name = "Zlewnia leśna",
    group2_name = "Zlewnia rolnicza",
    unit = "°C",
    mu1 = 12.5,
    mu2 = 15.2,
    sigma = 2.0,
    description = "Porównanie temperatury wody w strumieniach płynących przez tereny leśne i rolnicze."
  ),
  przeplyw = list(
    name = "Przepływ rzeki",
    group1_name = "Sezon suchy",
    group2_name = "Sezon mokry",
    unit = "m³/s",
    mu1 = 8.5,
    mu2 = 24.3,
    sigma = 5.0,
    description = "Porównanie przepływu rzeki w sezonie suchym (lato) i mokrym (wiosna)."
  ),
  ph = list(
    name = "pH jezior",
    group1_name = "Region północny",
    group2_name = "Region południowy",
    unit = "pH",
    mu1 = 7.2,
    mu2 = 6.8,
    sigma = 0.4,
    description = "Porównanie odczynu pH jezior w dwóch regionach o różnej geologii podłoża."
  ),
  tlen_brak = list(
    name = "Tlen rozpuszczony (brak różnicy)",
    group1_name = "Punkt A",
    group2_name = "Punkt B",
    unit = "mg/L",
    mu1 = 8.5,
    mu2 = 8.6,
    sigma = 1.5,
    description = "Porównanie stężenia tlenu rozpuszczonego w dwóch punktach pomiarowych na tej samej rzece. UWAGA: To przykład, gdzie NIE oczekujemy istotnej różnicy."
  )
)

# ==============================================================================
# FUNKCJE POMOCNICZE
# ==============================================================================

generate_data <- function(scenario, n) {
  set.seed(NULL)
  s <- scenarios[[scenario]]

  grupa1 <- rnorm(n, mean = s$mu1, sd = s$sigma)
  grupa2 <- rnorm(n, mean = s$mu2, sd = s$sigma)

  data.frame(
    wartosc = c(grupa1, grupa2),
    grupa = factor(c(rep(s$group1_name, n), rep(s$group2_name, n)),
                   levels = c(s$group1_name, s$group2_name))
  )
}

# ==============================================================================
# UI
# ==============================================================================

ui <- fluidPage(
  titlePanel("Test t dla dwóch prób - Krok po kroku"),

  sidebarLayout(
    sidebarPanel(
      h4("Wybierz scenariusz"),
      selectInput("scenario", "Dane:",
                  choices = c(
                    "Stężenie azotanów" = "azotany",
                    "Temperatura wody" = "temperatura",
                    "Przepływ rzeki" = "przeplyw",
                    "pH jezior" = "ph",
                    "⚠️ Tlen (brak różnicy)" = "tlen_brak"
                  )),

      hr(),

      sliderInput("n", "Liczba pomiarów w każdej grupie:",
                  min = 10, max = 50, value = 25, step = 5),

      hr(),

      h4("Kroki analizy"),

      actionButton("step1", "1. Hipotezy i dane", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step2", "2. Statystyki opisowe", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step3", "3. Różnica średnich", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step4", "4. Statystyka t", width = "100%", class = "btn-outline-primary"),
      br(), br(),
      actionButton("step5", "5. Wartość p", width = "100%", class = "btn-outline-primary"),
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

      # Wykres główny
      plotOutput("main_plot", height = "400px"),

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
    s <- scenarios[[input$scenario]]

    g1 <- d$wartosc[d$grupa == s$group1_name]
    g2 <- d$wartosc[d$grupa == s$group2_name]

    test_result <- t.test(g1, g2, var.equal = FALSE)

    list(
      mean1 = mean(g1),
      mean2 = mean(g2),
      sd1 = sd(g1),
      sd2 = sd(g2),
      n1 = length(g1),
      n2 = length(g2),
      diff = mean(g1) - mean(g2),
      t_stat = test_result$statistic,
      df = test_result$parameter,
      p_value = test_result$p.value,
      ci_low = test_result$conf.int[1],
      ci_high = test_result$conf.int[2],
      se = sqrt(sd(g1)^2/length(g1) + sd(g2)^2/length(g2))
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
      "<b>", s$group1_name, ":</b><br>",
      "Średnia: ", round(st$mean1, 2), " ", s$unit, "<br>",
      "SD: ", round(st$sd1, 2), "<br>",
      "n = ", st$n1, "<br><br>",
      "<b>", s$group2_name, ":</b><br>",
      "Średnia: ", round(st$mean2, 2), " ", s$unit, "<br>",
      "SD: ", round(st$sd2, 2), "<br>",
      "n = ", st$n2, "<br><br>",
      "<b>Różnica:</b> ", round(st$diff, 2), " ", s$unit, "<br>",
      "<b>t =</b> ", round(st$t_stat, 3), "<br>",
      "<b>p =</b> ", format(st$p_value, digits = 4)
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
    col1 <- "#3498db"  # niebieski
    col2 <- "#e74c3c"  # czerwony
    col_mean <- "#2c3e50"  # ciemny

    if (step == 1) {
      # KROK 1: Hipotezy i dane surowe - strip plot
      ggplot(d, aes(x = grupa, y = wartosc, color = grupa)) +
        geom_jitter(width = 0.2, size = 3, alpha = 0.7) +
        scale_color_manual(values = c(col1, col2)) +
        # Dodaj panel z hipotezami (prawy górny róg, wewnątrz wykresu)
        annotate("label", x = 2.4,
                 y = max(d$wartosc) - diff(range(d$wartosc)) * 0.02,
                 label = paste0("H₀: μ₁ = μ₂ (brak różnicy)\n",
                               "Hₐ: μ₁ ≠ μ₂ (jest różnica)"),
                 size = 4.5, fill = "#e8f4f8", fontface = "bold",
                 hjust = 1, vjust = 1,
                 label.padding = unit(0.5, "lines")) +
        labs(
          title = "Krok 1: Hipotezy i dane",
          subtitle = paste0("n = ", input$n, " w każdej grupie | Czy średnie się różnią?"),
          x = "",
          y = paste0("Wartość (", s$unit, ")")
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"))

    } else if (step == 2) {
      # KROK 2: Statystyki opisowe - box plot + średnie
      ggplot(d, aes(x = grupa, y = wartosc, fill = grupa)) +
        geom_boxplot(alpha = 0.5, outlier.shape = NA) +
        geom_jitter(width = 0.15, size = 2, alpha = 0.5) +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = col_mean) +
        stat_summary(fun = mean, geom = "text",
                     aes(label = paste0("x̄ = ", round(after_stat(y), 2))),
                     vjust = -1.5, size = 5, color = col_mean) +
        scale_fill_manual(values = c(col1, col2)) +
        labs(
          title = "Krok 2: Statystyki opisowe",
          subtitle = "Box plot z zaznaczoną średnią (romb)",
          x = "",
          y = paste0("Wartość (", s$unit, ")")
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"))

    } else if (step == 3) {
      # KROK 3: Różnica średnich - density plot
      ggplot(d, aes(x = wartosc, fill = grupa)) +
        geom_density(alpha = 0.5) +
        geom_vline(xintercept = st$mean1, color = col1, size = 1.5, linetype = "dashed") +
        geom_vline(xintercept = st$mean2, color = col2, size = 1.5, linetype = "dashed") +
        annotate("segment", x = st$mean1, xend = st$mean2,
                 y = 0, yend = 0,
                 arrow = arrow(ends = "both", length = unit(0.2, "cm")),
                 size = 1.5, color = col_mean) +
        annotate("text", x = (st$mean1 + st$mean2)/2, y = 0.02,
                 label = paste0("Różnica = ", round(st$diff, 2), " ", s$unit),
                 size = 5, fontface = "bold") +
        scale_fill_manual(values = c(col1, col2)) +
        labs(
          title = "Krok 3: Różnica średnich",
          subtitle = paste0("Obserwowana różnica: ", round(st$diff, 2), " ", s$unit),
          x = paste0("Wartość (", s$unit, ")"),
          y = "Gęstość"
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"))

    } else if (step == 4) {
      # KROK 4: Statystyka t - wizualizacja
      # Górny panel: dane z zaznaczonym SE
      p1 <- ggplot(d, aes(x = grupa, y = wartosc, color = grupa)) +
        geom_jitter(width = 0.2, size = 2, alpha = 0.5) +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = col_mean) +
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
                     color = col_mean, size = 1) +
        scale_color_manual(values = c(col1, col2)) +
        labs(
          title = "Krok 4: Statystyka t",
          subtitle = paste0("t = różnica / błąd standardowy = ",
                           round(st$diff, 2), " / ", round(st$se, 2),
                           " = ", round(st$t_stat, 2)),
          x = "",
          y = paste0("Wartość (", s$unit, ")")
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"))

      # Dodaj annotację z interpretacją
      p1 + annotate("label", x = 1.5, y = max(d$wartosc),
                    label = paste0("Różnica (", round(abs(st$diff), 2), " ", s$unit,
                                  ") to\n", round(abs(st$t_stat), 1),
                                  " błędów standardowych"),
                    size = 4, fill = "#fff3cd")

    } else if (step == 5) {
      # KROK 5: P-value - rozkład t z obszarem krytycznym
      t_val <- st$t_stat
      df_val <- st$df

      # Dane do wykresu rozkładu t
      x_seq <- seq(-4, 4, length.out = 200)
      y_seq <- dt(x_seq, df = df_val)
      t_df <- data.frame(x = x_seq, y = y_seq)

      # Wartość krytyczna dla α = 0.05 (dwustronny)
      t_crit <- qt(0.975, df = df_val)

      # Kolor decyzji
      is_significant <- st$p_value < 0.05
      decision_text <- if(is_significant) "Odrzucamy H₀" else "Nie odrzucamy H₀"
      t_color <- if(is_significant) col2 else "#2ecc71"

      ggplot(t_df, aes(x = x, y = y)) +
        geom_line(size = 1.2) +
        # Obszar akceptacji H₀ (środkowe 95%)
        geom_area(data = subset(t_df, x >= -t_crit & x <= t_crit), aes(x = x, y = y),
                  fill = "#2ecc71", alpha = 0.2) +
        # Obszary odrzucenia H₀ (ogony, po 2.5% z każdej strony)
        geom_area(data = subset(t_df, x <= -t_crit), aes(x = x, y = y),
                  fill = col2, alpha = 0.3) +
        geom_area(data = subset(t_df, x >= t_crit), aes(x = x, y = y),
                  fill = col2, alpha = 0.3) +
        # Linie krytyczne
        geom_vline(xintercept = t_crit, color = "darkgray", size = 1, linetype = "dashed") +
        geom_vline(xintercept = -t_crit, color = "darkgray", size = 1, linetype = "dashed") +
        # Nasza wartość t
        geom_vline(xintercept = t_val, color = t_color, size = 1.5) +
        annotate("text", x = t_val, y = max(y_seq) * 0.95,
                 label = paste0("t = ", round(t_val, 2)),
                 hjust = if(t_val > 0) -0.1 else 1.1, size = 5, color = t_color, fontface = "bold") +
        # Etykiety wartości krytycznych
        annotate("text", x = t_crit, y = max(y_seq) * 0.15,
                 label = paste0("t_kr = ", round(t_crit, 2)),
                 hjust = -0.1, size = 3.5, color = "darkgray") +
        annotate("text", x = -t_crit, y = max(y_seq) * 0.15,
                 label = paste0("-t_kr = ", round(-t_crit, 2)),
                 hjust = 1.1, size = 3.5, color = "darkgray") +
        # Etykiety obszarów
        annotate("label", x = 0, y = max(y_seq) * 0.5,
                 label = "Obszar przyjęcia H₀\n(95%)",
                 size = 4, fill = "#d4edda", alpha = 0.8) +
        annotate("text", x = -3.5, y = max(y_seq) * 0.2,
                 label = "2.5%", size = 3.5, color = col2) +
        annotate("text", x = 3.5, y = max(y_seq) * 0.2,
                 label = "2.5%", size = 3.5, color = col2) +
        labs(
          title = "Krok 5: Obszar krytyczny",
          subtitle = paste0("α = 0.05 | t_krytyczne = ±", round(t_crit, 2), " | Gdzie jest nasze t?"),
          x = "Statystyka t",
          y = "Gęstość"
        ) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(size = 18, face = "bold"))

    } else if (step == 6) {
      # KROK 6: Decyzja - podsumowanie wizualne
      is_significant <- st$p_value < 0.05
      decision_col <- if(is_significant) "#2ecc71" else "#e74c3c"
      decision_text <- if(is_significant) "ISTOTNA RÓŻNICA" else "BRAK ISTOTNEJ RÓŻNICY"

      ggplot(d, aes(x = grupa, y = wartosc, fill = grupa)) +
        geom_boxplot(alpha = 0.6) +
        geom_jitter(width = 0.15, size = 2, alpha = 0.4) +
        stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = col_mean) +
        scale_fill_manual(values = c(col1, col2)) +
        annotate("rect", xmin = 0.4, xmax = 2.6,
                 ymin = max(d$wartosc) * 1.05, ymax = max(d$wartosc) * 1.25,
                 fill = decision_col, alpha = 0.3) +
        annotate("text", x = 1.5, y = max(d$wartosc) * 1.15,
                 label = decision_text,
                 size = 7, fontface = "bold", color = decision_col) +
        annotate("text", x = 1.5, y = min(d$wartosc) * 0.85,
                 label = paste0("p = ", format(st$p_value, digits = 4),
                               "  |  95% CI: [", round(st$ci_low, 2), ", ",
                               round(st$ci_high, 2), "] ", s$unit),
                 size = 4) +
        labs(
          title = "Krok 6: Decyzja",
          subtitle = if(is_significant)
            "p < 0.05 → Odrzucamy H₀ - średnie różnią się istotnie" else
            "p ≥ 0.05 → Nie odrzucamy H₀ - brak dowodu na różnicę",
          x = "",
          y = paste0("Wartość (", s$unit, ")")
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"))
    }
  })

  # Wyjaśnienia kroków
  output$step_explanation <- renderUI({
    step <- current_step()
    st <- stats()
    s <- scenarios[[input$scenario]]

    explanations <- list(
      step1 = HTML(paste0(
        "<p><b>Hipotezy statystyczne</b> to pierwszy krok każdego testu:</p>",
        "<ul>",
        "<li><b>H₀ (hipoteza zerowa):</b> μ₁ = μ₂ — średnie w obu grupach są równe (brak różnicy)</li>",
        "<li><b>Hₐ (hipoteza alternatywna):</b> μ₁ ≠ μ₂ — średnie różnią się (jest różnica)</li>",
        "</ul>",
        "<p><b>Dane:</b> Mamy dwie grupy: <b>", s$group1_name, "</b> (n = ", input$n, ") ",
        "i <b>", s$group2_name, "</b> (n = ", input$n, ").</p>",
        "<p>Celem testu jest sprawdzenie, czy mamy wystarczające dowody, żeby <b>odrzucić H₀</b>.</p>"
      )),

      step2 = HTML(paste0(
        "<p><b>Statystyki opisowe</b> podsumowują dane w każdej grupie.</p>",
        "<p><b>", s$group1_name, ":</b> średnia = ", round(st$mean1, 2), " ", s$unit,
        ", odch. std. = ", round(st$sd1, 2), "</p>",
        "<p><b>", s$group2_name, ":</b> średnia = ", round(st$mean2, 2), " ", s$unit,
        ", odch. std. = ", round(st$sd2, 2), "</p>",
        "<p>Romb (◆) na wykresie pokazuje średnią, box plot pokazuje medianę i kwartyle.</p>"
      )),

      step3 = HTML(paste0(
        "<p><b>Różnica średnich</b> to obserwowany efekt: ", round(st$diff, 2), " ", s$unit, "</p>",
        "<p>Rozkłady gęstości pokazują, jak wartości rozpraszają się w każdej grupie. ",
        "Przerywane linie to średnie.</p>",
        "<p><b>Kluczowe pytanie:</b> Czy ta różnica jest na tyle duża, że prawdopodobnie ",
        "odzwierciedla rzeczywistą różnicę, czy mogła powstać przez przypadek?</p>"
      )),

      step4 = HTML(paste0(
        "<p><b>Statystyka t</b> standaryzuje różnicę - wyraża ją w jednostkach błędu standardowego.</p>",
        "<p><b>Wzór:</b> t = (średnia₁ - średnia₂) / błąd standardowy różnicy</p>",
        "<p><b>Obliczenie:</b> t = ", round(st$diff, 2), " / ", round(st$se, 2),
        " = <b>", round(st$t_stat, 2), "</b></p>",
        "<p><b>Interpretacja:</b> Obserwowana różnica to ", round(abs(st$t_stat), 1),
        " błędów standardowych od zera. Im dalej od zera, tym mniej prawdopodobne, że różnica jest przypadkowa.</p>"
      )),

      step5 = HTML(paste0(
        "<p><b>Obszar krytyczny</b> to wartości statystyki t, przy których odrzucamy H₀.</p>",
        "<p>Dla α = 0.05 (test dwustronny): t_krytyczne = ±", round(qt(0.975, df = st$df), 2), "</p>",
        "<hr>",
        "<p><b>Nasza wartość:</b> t = ", round(st$t_stat, 2), "</p>",
        "<p><b>Wartość p = ", format(st$p_value, digits = 4), "</b></p>",
        "<hr>",
        if(st$p_value < 0.05)
          paste0("<p style='color: green;'><b>|t| > t_krytyczne → Odrzucamy H₀</b><br>",
                 "Nasza statystyka t wpada w czerwony obszar odrzucenia. ",
                 "Różnica jest istotna statystycznie (p < 0.05).</p>")
        else
          paste0("<p style='color: red;'><b>|t| < t_krytyczne → Nie odrzucamy H₀</b><br>",
                 "Nasza statystyka t pozostaje w zielonym obszarze przyjęcia H₀. ",
                 "Brak wystarczających dowodów na istotną różnicę (p ≥ 0.05).</p>")
      )),

      step6 = HTML(paste0(
        "<p><b>Decyzja:</b> ",
        if(st$p_value < 0.05)
          paste0("p = ", format(st$p_value, digits = 4), " < 0.05 → <b>Odrzucamy H₀</b>")
        else
          paste0("p = ", format(st$p_value, digits = 4), " ≥ 0.05 → <b>Nie odrzucamy H₀</b>"),
        "</p>",
        "<p><b>95% przedział ufności dla różnicy:</b> [", round(st$ci_low, 2), ", ",
        round(st$ci_high, 2), "] ", s$unit, "</p>",
        "<p><b>Interpretacja praktyczna:</b> ",
        if(st$p_value < 0.05)
          paste0("Istnieje istotna różnica między grupami. ", s$group1_name,
                 if(st$diff > 0) " ma wyższe " else " ma niższe ",
                 "wartości niż ", s$group2_name, " (różnica: ", round(abs(st$diff), 2), " ", s$unit, ").")
        else
          "Nie ma wystarczających dowodów na istnienie różnicy między grupami.",
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
