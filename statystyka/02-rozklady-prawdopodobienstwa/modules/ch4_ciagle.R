# ============================================================================
# CHAPTER 4: Rozklady ciagle
# ============================================================================

ch4_ui <- list(
  id = "ch-ciagle", num = "04", title = "Rozkłady ciągłe",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Rozkłady prawdopodobieństwa",
      num    = "04",
      title  = "Rozkłady ciągłe.",
      lead   = "Rozkłady dyskretne opisują zmienne o skończonej liczbie wartości.
                Ale co, gdy zmienna może przyjąć dowolną wartość z pewnego przedziału?"
    ),

    lc_h2("ch4-histogram", "Od histogramu do krzywej gęstości"),

    tagList(
      p("Znasz już histogramy ze statystyki opisowej. Teraz zobaczymy,
        jak histogram przechodzi w gładką krzywą gdy zwiększamy próbę i zwężamy przedziały.
        Ta krzywa to ", tags$b(gloss("funkcja gęstości", "funkcja gęstości prawdopodobieństwa"), " (PDF)"),
        " — ciągły odpowiednik PMF.")
    ),

    # ========================================================================
    # WIDGET 1: Od histogramu do krzywej (krok po kroku)
    # ========================================================================
    figure_panel(
      label = "Ryc. 4.1",
      full_width = TRUE,
      lc_step_widget("ch4_step",
        title = "Od histogramu do krzywej gęstości",
        steps = c("Surowe dane (rug)", "Histogram (5 binów)", "Więcej binów (15)",
                  "Jeszcze więcej (30)", "Skala gęstości", "Krzywa gęstości",
                  "Tylko PDF"),
        toolbar = lc_toolbar(
          selectInput("ch4_step_dist", "Rozkład źródłowy",
            choices = c("Normalny" = "normal", "Wykładniczy" = "exp",
                        "Jednostajny" = "unif"),
            selected = "normal"
          ),
          lc_slider("ch4_step_n", "Wielkość próby", 50, 10000, 500, 50)
        ),
        plot_id = "ch4_step_plot"
      )
    ),

    # ========================================================================
    # WIDGET 2: Prawdopodobienstwo = pole — BEZ ZMIAN
    # ========================================================================
    lc_h2("ch4-pole", "Prawdopodobieństwo = pole pod krzywą"),

    tagList(
      p("W rozkładach ciągłych prawdopodobieństwo to ",
        tags$b("pole pod krzywą gęstości"), " w danym przedziale.
        Wysokość krzywej to NIE prawdopodobieństwo!"),
      p("Ważna konsekwencja: ", tags$b("P(X = dokładnie 5.0) = 0"),
        " dla rozkładów ciągłych. Sens ma tylko pytanie o przedziały.")
    ),

    figure_panel(
      label = "Ryc. 4.2",
      title = "Zacieniuj przedział i odczytaj prawdopodobieństwo",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch4_area_dist", "Rozkład:",
            choices = c("Normalny N(0,1)" = "norm",
                        "Wykładniczy Exp(1)" = "exp",
                        "Jednostajny U(0,10)" = "unif"),
            selected = "norm"
          ),
          lc_slider("ch4_area_a", "Dolna granica (a)", -4, 4, -1, 0.1),
          lc_slider("ch4_area_b", "Górna granica (b)", -4, 4, 1, 0.1)
        ),
        column(8,
          zoom_plot_ui("ch4_area_plot", height = "350px"),
          uiOutput("ch4_area_stats")
        )
      )
    ),

    inline_callout(
      label = "Zapamiętaj",
      tagList(
        "W rozkładzie ciągłym gęstość f(x) może być > 1 (np. U(0, 0.5) ma f(x) = 2),
        ale ", tags$b("pole pod całą krzywą zawsze wynosi 1"), "."
      ),
      color = "uwaga"
    ),

    # ========================================================================
    # WIDGET 3: Jednostajny ciagly — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-jednostajny", "Rozkład jednostajny ciągły"),

    tagList(
      p(tags$b(gloss("rozkład jednostajny", "Jednostajny ciągły"), " U(a, b)"), " — każda wartość w przedziale
        [a, b] jest jednakowo prawdopodobna. Przykład: losowa liczba z generatora.")
    ),

    figure_panel(
      label = "Ryc. 4.3",
      title = "Rozkład jednostajny U(a, b)",
      full_width = TRUE,
      fluidRow(
        column(4,
          checkboxGroupInput("ch4_unif_scenarios", "Scenariusze:",
            choices = c(
              "U(0, 10)" = "unif_1",
              "U(2, 8)" = "unif_2",
              "U(0, 2)" = "unif_3",
              "U(4, 6)" = "unif_4"
            ),
            selected = "unif_1"
          )
        ),
        column(8,
          zoom_plot_ui("ch4_unif_plot", height = "350px"),
          uiOutput("ch4_unif_stats")
        )
      ),
      lc_formula_box(
        withMathJax(
          helpText("$$f(x) = \\frac{1}{b-a}, \\quad E(X) = \\frac{a+b}{2}, \\quad Var(X) = \\frac{(b-a)^2}{12}$$")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3b: Wykladniczy — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-wykladniczy", "Rozkład wykładniczy"),

    tagList(
      p(tags$b(gloss("rozkład wykładniczy", "Wykładniczy"), " Exp(λ)"), " — modeluje czas oczekiwania między
        zdarzeniami. Przykład: czas między wiadomościami na WhatsAppie, czas między awariami maszyn.")
    ),

    figure_panel(
      label = "Ryc. 4.4",
      title = "Rozkład wykładniczy Exp(λ)",
      full_width = TRUE,
      fluidRow(
        column(4,
          checkboxGroupInput("ch4_exp_scenarios", "Scenariusze:",
            choices = c(
              "Awarie: λ = 0.3/dzień" = "exp_1",
              "Wiadomości: λ = 1/godz" = "exp_2",
              "Zgłoszenia: λ = 2/godz" = "exp_3",
              "Połączenia: λ = 5/min" = "exp_4"
            ),
            selected = "exp_2"
          )
        ),
        column(8,
          zoom_plot_ui("ch4_exp_plot", height = "350px"),
          uiOutput("ch4_exp_stats")
        )
      ),
      lc_formula_box(
        withMathJax(
          helpText("$$f(x) = \\lambda e^{-\\lambda x}, \\quad E(X) = \\frac{1}{\\lambda}, \\quad Var(X) = \\frac{1}{\\lambda^2}$$")
        )
      )
    ),

    inline_callout(
      label = "Związek z Poissonem",
      tagList("Jeśli liczba zdarzeń w czasie ma ", gloss("rozkład Poissona", "rozkład Poissona(λ)"), ",
       to czas między zdarzeniami ma rozkład Exp(λ) — dwie strony tego samego procesu.")
    ),

    inline_callout(
      label = "Bezpamięciowość",
      tagList(
        "Czekasz na wiadomość już 2 godziny. Czy następna przyjdzie szybciej?",
        tags$b(" Nie."),
        " Czas już spędzony nie wpływa na dalsze oczekiwanie."
      ),
      color = "uwaga"
    ),

    # ========================================================================
    # WIDGET 4: Rozklad t-Studenta — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-t-studenta", "Rozkład t-Studenta"),

    tagList(
      p(gloss("rozkład t-Studenta", "Rozkład t-Studenta"), " wygląda jak normalny, ale ma cięższe ogony —
        wartości ekstremalne są bardziej prawdopodobne. Jest kluczowy we
        wnioskowaniu statystycznym (test t, przedziały ufności)."),
      p("Parametr ", tags$b("df"), " (", gloss("stopnie swobody"), ") kontroluje 'grubość' ogonów.
        Im więcej df, tym bliżej do rozkładu normalnego.")
    ),

    figure_panel(
      label = "Ryc. 4.5",
      title = "Rozkład t-Studenta t(df)",
      full_width = TRUE,
      fluidRow(
        column(4,
          checkboxGroupInput("ch4_t_scenarios", "Scenariusze:",
            choices = c(
              "t(df=1) — Cauchy" = "t_1",
              "t(df=3)" = "t_2",
              "t(df=5)" = "t_3",
              "t(df=30) ≈ normalny" = "t_4"
            ),
            selected = c("t_2", "t_4")
          ),
          hr(),
          checkboxInput("ch4_t_show_normal", "Pokaż N(0,1) jako odniesienie", value = TRUE)
        ),
        column(8,
          zoom_plot_ui("ch4_t_plot", height = "400px"),
          uiOutput("ch4_t_stats")
        )
      ),
      lc_formula_box(
        withMathJax(helpText(
          "$$E(X) = 0 \\; (df > 1), \\quad Var(X) = \\frac{df}{df - 2} \\; (df > 2)$$"
        ))
      )
    ),

    inline_callout(
      label = "Dlaczego t-Studenta?",
      tagList("Gdy nie znamy prawdziwego σ populacji i szacujemy je z próby,
       rozkład ", gloss("statystyka testowa", "statystyki testowej"), " to t-Studenta, nie normalny.
       Przy małych próbach (n < 30) różnica jest znacząca!")
    ),

    inline_callout(
      label = "W praktyce",
      "Przy df=30 krzywa t jest już prawie normalną. Przy df=3 (n=4!)
       ogony są wyraźnie cięższe — wartości ekstremalne bardziej prawdopodobne.",
      color = "uwaga"
    ),

    # ========================================================================
    # WIDGET 5: Rozklad chi-kwadrat — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-chi-kwadrat", "Rozkład chi-kwadrat (χ²)"),

    tagList(
      p(gloss("rozkład chi-kwadrat", "Rozkład chi-kwadrat"), " powstaje jako suma kwadratów niezależnych zmiennych N(0,1).
        Jest zawsze nieujemny i prawoskośny."),
      p("Zastosowania: testy niezależności, testy dopasowania,
        estymacja wariancji.")
    ),

    figure_panel(
      label = "Ryc. 4.6",
      title = "Rozkład χ²(df)",
      full_width = TRUE,
      fluidRow(
        column(4,
          checkboxGroupInput("ch4_chisq_scenarios", "Scenariusze:",
            choices = c(
              "χ²(df=2)" = "chisq_1",
              "χ²(df=5)" = "chisq_2",
              "χ²(df=10)" = "chisq_3",
              "χ²(df=20)" = "chisq_4"
            ),
            selected = "chisq_2"
          )
        ),
        column(8,
          zoom_plot_ui("ch4_chisq_plot", height = "400px"),
          uiOutput("ch4_chisq_stats")
        )
      ),
      lc_formula_box(
        withMathJax(helpText(
          "$$E(X) = df, \\quad Var(X) = 2 \\cdot df$$"
        ))
      )
    ),

    inline_callout(
      label = "Obserwacja",
      "Przy małym df rozkład jest mocno prawoskośny.
       Gdy df rośnie, staje się coraz bardziej symetryczny i zbliża się do normalnego (CTG!)."
    ),

    inline_callout(
      label = "Intuicja",
      tagList("χ² mierzy odleglosc od idealu. ", gloss("test chi-kwadrat", "Test χ²"), " sprawdza, czy obserwowane
       czestosci sa zbyt daleko od oczekiwanych."),
      color = "uwaga"
    ),

    # ========================================================================
    # WIDGET 6: Rozklad log-normalny — scenariusze overlay
    # ========================================================================
    lc_h2("ch4-lognormalny", "Rozkład log-normalny"),

    tagList(
      p("Jeśli ", tags$b("ln(X) ~ N(μ, σ)"), ", to X ma ", gloss("rozkład log-normalny"), ".
        Zmienna jest zawsze dodatnia i prawoskośna."),
      p("Pojawia się wszędzie tam, gdzie dane rosną multiplikatywnie:
        dochody, ceny akcji, czasy reakcji, stężenia substancji.")
    ),

    figure_panel(
      label = "Ryc. 4.7",
      title = "Rozkład LogN(μ, σ)",
      full_width = TRUE,
      fluidRow(
        column(4,
          checkboxGroupInput("ch4_lnorm_scenarios", "Scenariusze:",
            choices = c(
              "Czas reakcji: LogN(0, 0.3)" = "lnorm_1",
              "Ceny akcji: LogN(1, 0.5)" = "lnorm_2",
              "Dochody: LogN(2, 0.8)" = "lnorm_3",
              "Duża zmienność: LogN(1, 1)" = "lnorm_4"
            ),
            selected = "lnorm_2"
          )
        ),
        column(8,
          zoom_plot_ui("ch4_lnorm_plot", height = "400px"),
          uiOutput("ch4_lnorm_stats")
        )
      ),
      lc_formula_box(
        withMathJax(helpText(
          "$$E(X) = e^{\\mu + \\sigma^2/2}, \\quad Var(X) = \\left(e^{\\sigma^2} - 1\\right) \\cdot e^{2\\mu + \\sigma^2}$$"
        ))
      )
    ),

    inline_callout(
      label = "Uwaga na średnią!",
      tagList(
        "Średnia > ", gloss("mediana"), ". Mediana = e^μ, średnia = e^(μ + σ²/2).
         Dlatego ", tags$b("mediana"), " dochodów lepiej opisuje 'typowego' pracownika niż średnia."
      ),
      color = "uwaga"
    ),

    inline_callout(
      label = "Jak rozpoznać?",
      "Dane zawsze dodatnie + długi prawy ogon → myśl log-normalny.
       Prosty test: jeśli po zlogarytmowaniu histogram wygląda normalnie — to log-normalny."
    ),

    lc_chapter_next(
      num       = "05",
      title     = "Rozkład normalny",
      lead      = "królowa rozkładów — dlaczego pojawia się wszędzie.",
      target_id = "ch-normalny"
    )
  )
)

# --------------------------------------------------------------------------
# Definicje scenariuszy
# --------------------------------------------------------------------------

ch4_unif_defs <- list(
  unif_1 = list(label = "U(0, 10)", a = 0, b = 10),
  unif_2 = list(label = "U(2, 8)", a = 2, b = 8),
  unif_3 = list(label = "U(0, 2)", a = 0, b = 2),
  unif_4 = list(label = "U(4, 6)", a = 4, b = 6)
)

ch4_exp_defs <- list(
  exp_1 = list(label = "Awarie: λ = 0.3/dzień", lambda = 0.3),
  exp_2 = list(label = "Wiadomości: λ = 1/godz", lambda = 1),
  exp_3 = list(label = "Zgłoszenia: λ = 2/godz", lambda = 2),
  exp_4 = list(label = "Połączenia: λ = 5/min", lambda = 5)
)

ch4_t_defs <- list(
  t_1 = list(label = "t(df=1) — Cauchy", df = 1),
  t_2 = list(label = "t(df=3)", df = 3),
  t_3 = list(label = "t(df=5)", df = 5),
  t_4 = list(label = "t(df=30) ≈ normalny", df = 30)
)

ch4_chisq_defs <- list(
  chisq_1 = list(label = "χ²(df=2)", df = 2),
  chisq_2 = list(label = "χ²(df=5)", df = 5),
  chisq_3 = list(label = "χ²(df=10)", df = 10),
  chisq_4 = list(label = "χ²(df=20)", df = 20)
)

ch4_lnorm_defs <- list(
  lnorm_1 = list(label = "Czas reakcji: LogN(0, 0.3)", mu = 0, sigma = 0.3),
  lnorm_2 = list(label = "Ceny akcji: LogN(1, 0.5)", mu = 1, sigma = 0.5),
  lnorm_3 = list(label = "Dochody: LogN(2, 0.8)", mu = 2, sigma = 0.8),
  lnorm_4 = list(label = "Duża zmienność: LogN(1, 1)", mu = 1, sigma = 1)
)

# --------------------------------------------------------------------------
# Chapter 4 Server
# --------------------------------------------------------------------------

ch4_server <- function(input, output, session) {

  # --- Widget 1: Krok po kroku ---
  # Krok widgetu (1..7) żyje w przeglądarce; zmiana rozkładu lub próby nie cofa kroku.
  ch4_step <- lc_step_server("ch4_step", input)$step

  ch4_sample_data <- reactive({
    req(input$ch4_step_n, input$ch4_step_dist)
    switch(input$ch4_step_dist,
      "normal" = rnorm(input$ch4_step_n, mean = 5, sd = 1.5),
      "exp"    = rexp(input$ch4_step_n, rate = 0.5),
      "unif"   = runif(input$ch4_step_n, min = 0, max = 10)
    )
  })

  # Stała rama z pełnej próby: oś X wspólna dla kroków, oś Y wspólna dla
  # kroków na skali gęstości (5–7). Zapas 14% mieści skrajne słupki 5 binów.
  ch4_step_frame <- reactive({
    data <- ch4_sample_data()
    df <- data.frame(x = data)
    x_pad <- diff(range(data)) * 0.14
    hist_max <- function(bins, stat) {
      max(layer_data(ggplot(df, aes(x = x)) + geom_histogram(bins = bins))[[stat]])
    }
    dens_max <- max(hist_max(30, "density"), max(density(data)$y))
    list(
      xlim = range(data) + c(-x_pad, x_pad),
      count_max = c(`5` = hist_max(5, "count"), `15` = hist_max(15, "count"),
                    `30` = hist_max(30, "count")),
      dens_max = dens_max
    )
  })

  zoom_plot_server("ch4_step_plot", reactive({
    step <- ch4_step()
    data <- ch4_sample_data()
    fr <- ch4_step_frame()

    df <- data.frame(x = data)
    count_frame <- function(bins) {
      step_frame(xlim = fr$xlim, ylim = c(0, fr$count_max[[as.character(bins)]] * 1.08))
    }
    dens_frame <- step_frame(xlim = fr$xlim, ylim = c(0, fr$dens_max * 1.08))

    if (step == 1) {
      ggplot(df, aes(x = x)) +
        step_layer(geom_rug, "data", alpha = 0.3) +
        scale_y_continuous() +
        labs(x = "Wartość", y = "") +
        step_frame(xlim = fr$xlim, ylim = c(0, 1), y_axis = FALSE)
    } else if (step == 2) {
      ggplot(df, aes(x = x)) +
        step_result(geom_histogram, bins = 5) +
        step_layer(geom_rug, "background") +
        labs(x = "Wartość", y = "Liczebność") +
        count_frame(5)
    } else if (step == 3) {
      ggplot(df, aes(x = x)) +
        step_result(geom_histogram, bins = 15) +
        labs(x = "Wartość", y = "Liczebność") +
        count_frame(15)
    } else if (step == 4) {
      ggplot(df, aes(x = x)) +
        step_result(geom_histogram, bins = 30) +
        labs(x = "Wartość", y = "Liczebność") +
        count_frame(30)
    } else if (step == 5) {
      ggplot(df, aes(x = x)) +
        step_result(geom_histogram, mapping = aes(y = after_stat(density)), bins = 30) +
        labs(x = "Wartość", y = "Gęstość") +
        dens_frame
    } else if (step == 6) {
      ggplot(df, aes(x = x)) +
        step_result(geom_histogram, mapping = aes(y = after_stat(density)), bins = 30,
                    alpha = 0.5) +
        step_layer(geom_density, "new", linewidth = 1.5) +
        labs(x = "Wartość", y = "Gęstość") +
        dens_frame
    } else {
      ggplot(df, aes(x = x)) +
        step_layer(geom_density, "known", fill = STEP_ROLES$data$colour,
                   linewidth = 1.2, alpha = 0.3) +
        labs(x = "Wartość", y = "Gęstość f(x)") +
        dens_frame
    }
  }))

  output$ch4_step_text <- renderUI({
    step <- ch4_step()
    texts <- c(
      "Każda kreska to jedna obserwacja. Trudno coś z tego odczytać.",
      "5 binów — widzimy ogólny zarys, ale mało szczegółów.",
      "15 binów — kształt staje się wyraźniejszy.",
      "30 binów — jeszcze więcej szczegółów, ale słupki są nierówne.",
      "Zmiana osi Y na gęstość — teraz pole słupków = 1.",
      "Nakładamy gładką krzywą, która przybliża kształt danych.",
      "To jest PDF — teoretyczny model opisujący rozkład. Pole pod krzywą = 1."
    )
    texts[step]
  })

  # --- Widget 2: Prawdopodobienstwo = pole (bez zmian) ---
  observeEvent(input$ch4_area_dist, {
    if (input$ch4_area_dist == "norm") {
      updateSliderInput(session, "ch4_area_a", min = -4, max = 4, value = -1, step = 0.1)
      updateSliderInput(session, "ch4_area_b", min = -4, max = 4, value = 1, step = 0.1)
    } else if (input$ch4_area_dist == "exp") {
      updateSliderInput(session, "ch4_area_a", min = 0, max = 8, value = 0, step = 0.1)
      updateSliderInput(session, "ch4_area_b", min = 0, max = 8, value = 2, step = 0.1)
    } else {
      updateSliderInput(session, "ch4_area_a", min = 0, max = 10, value = 2, step = 0.1)
      updateSliderInput(session, "ch4_area_b", min = 0, max = 10, value = 7, step = 0.1)
    }
  })

  zoom_plot_server("ch4_area_plot", reactive({
    dist <- input$ch4_area_dist
    a <- input$ch4_area_a
    b <- input$ch4_area_b

    if (dist == "norm") {
      x_range <- c(-4, 4)
      dfn <- function(x) dnorm(x)
      prob <- pnorm(b) - pnorm(a)
    } else if (dist == "exp") {
      x_range <- c(0, 8)
      dfn <- function(x) dexp(x)
      prob <- pexp(b) - pexp(a)
    } else {
      x_range <- c(0, 10)
      dfn <- function(x) dunif(x, 0, 10)
      prob <- punif(b, 0, 10) - punif(a, 0, 10)
    }

    x_seq <- seq(x_range[1], x_range[2], length.out = 500)
    df_curve <- data.frame(x = x_seq, y = dfn(x_seq))

    shade_x <- seq(max(a, x_range[1]), min(b, x_range[2]), length.out = 300)
    shade_df <- data.frame(x = shade_x, y = dfn(shade_x))

    ggplot() +
      geom_area(data = shade_df, aes(x = x, y = y),
                fill = unname(upwr_cat["niebo"]), alpha = 0.35) +
      geom_line(data = df_curve, aes(x = x, y = y),
                color = upwr_secondary, linewidth = 1.2) +
      geom_vline(xintercept = a, color = unname(upwr_cat["terakota"]), linetype = "dashed") +
      geom_vline(xintercept = b, color = unname(upwr_cat["terakota"]), linetype = "dashed") +
      annotate("text", x = (a + b) / 2, y = max(dfn(x_seq)) * 0.5,
               label = sprintf("P = %.4f", prob),
               size = 6, fontface = "bold", color = upwr_secondary) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr()
  }))

  output$ch4_area_stats <- renderUI({
    dist <- input$ch4_area_dist
    a <- input$ch4_area_a
    b <- input$ch4_area_b

    if (dist == "norm") {
      prob <- pnorm(b) - pnorm(a)
    } else if (dist == "exp") {
      prob <- pexp(b) - pexp(a)
    } else {
      prob <- punif(b, 0, 10) - punif(a, 0, 10)
    }

    lc_center(
      lc_stat_box(paste0("P(", a, " < X < ", b, ")"),
                  sprintf("%.4f", max(0, prob)),
                  color = unname(upwr_cat["niebo"])),
      lc_stat_box("Procent", sprintf("%.1f", max(0, prob) * 100), "%",
                  color = upwr_secondary)
    )
  })

  # --- Widget 3: Jednostajny — scenariusze overlay ---
  zoom_plot_server("ch4_unif_plot", reactive({
    selected <- input$ch4_unif_scenarios
    req(length(selected) > 0)

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch4_unif_defs[[selected[i]]]
      x_seq <- seq(-1, 12, length.out = 1000)
      y_seq <- dunif(x_seq, s$a, s$b)
      data.frame(x = x_seq, y = y_seq, scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch4_unif_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch4_unif_defs[selected], `[[`, "label"))

    ggplot(df, aes(x = x, y = y, color = scenario, fill = scenario)) +
      geom_area(alpha = 0.15, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch4_unif_stats <- renderUI({
    selected <- input$ch4_unif_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch4_unif_defs[[id]]
      mu <- (s$a + s$b) / 2
      sd_val <- sqrt((s$b - s$a)^2 / 12)
      paste0(s$label, ":  E(X) = ", round(mu, 1), ",  SD = ", round(sd_val, 2))
    })
    div(style = "font-size: 13px; margin-top: 10px; line-height: 1.8;",
      lapply(stats, function(s) div(s))
    )
  })

  # --- Widget 3b: Wykladniczy — scenariusze overlay ---
  zoom_plot_server("ch4_exp_plot", reactive({
    selected <- input$ch4_exp_scenarios
    req(length(selected) > 0)

    x_max <- max(sapply(selected, function(id) qexp(0.99, ch4_exp_defs[[id]]$lambda)))

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch4_exp_defs[[selected[i]]]
      x_seq <- seq(0, x_max, length.out = 500)
      y_seq <- dexp(x_seq, rate = s$lambda)
      data.frame(x = x_seq, y = y_seq, scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch4_exp_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch4_exp_defs[selected], `[[`, "label"))

    ggplot(df, aes(x = x, y = y, color = scenario, fill = scenario)) +
      geom_area(alpha = 0.15, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch4_exp_stats <- renderUI({
    selected <- input$ch4_exp_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch4_exp_defs[[id]]
      mu <- 1 / s$lambda
      paste0(s$label, ":  E(X) = 1/λ = ", round(mu, 2), ",  SD = ", round(mu, 2))
    })
    div(style = "font-size: 13px; margin-top: 10px; line-height: 1.8;",
      lapply(stats, function(s) div(s))
    )
  })

  # --- Widget 4: t-Studenta — scenariusze overlay ---
  zoom_plot_server("ch4_t_plot", reactive({
    selected <- input$ch4_t_scenarios
    show_normal <- input$ch4_t_show_normal
    req(length(selected) > 0 || show_normal)

    x_seq <- seq(-5, 5, length.out = 500)

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch4_t_defs[[selected[i]]]
      data.frame(x = x_seq, y = dt(x_seq, df = s$df), scenario = s$label)
    })
    df <- do.call(rbind, dfs)

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch4_t_defs[selected], `[[`, "label"))

    if (show_normal) {
      df_norm <- data.frame(x = x_seq, y = dnorm(x_seq), scenario = "N(0,1)")
      df <- rbind(df, df_norm)
      colors <- c(colors, "N(0,1)" = "#999999")
    }

    df$scenario <- factor(df$scenario, levels = unique(df$scenario))

    ggplot(df, aes(x = x, y = y, color = scenario, fill = scenario)) +
      geom_area(alpha = 0.15, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch4_t_stats <- renderUI({
    selected <- input$ch4_t_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch4_t_defs[[id]]
      mu_text <- if (s$df > 1) "E(X) = 0" else "E(X) = niezdef."
      sd_text <- if (s$df > 2) {
        paste0("SD = ", round(sqrt(s$df / (s$df - 2)), 2))
      } else "SD = ∞"
      paste0(s$label, ":  ", mu_text, ",  ", sd_text)
    })
    div(style = "font-size: 13px; margin-top: 10px; line-height: 1.8;",
      lapply(stats, function(s) div(s))
    )
  })

  # --- Widget 5: Chi-kwadrat — scenariusze overlay ---
  zoom_plot_server("ch4_chisq_plot", reactive({
    selected <- input$ch4_chisq_scenarios
    req(length(selected) > 0)

    x_max <- max(sapply(selected, function(id) qchisq(0.99, ch4_chisq_defs[[id]]$df)))
    x_seq <- seq(0.01, x_max, length.out = 500)

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch4_chisq_defs[[selected[i]]]
      data.frame(x = x_seq, y = dchisq(x_seq, df = s$df), scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch4_chisq_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch4_chisq_defs[selected], `[[`, "label"))

    ggplot(df, aes(x = x, y = y, color = scenario, fill = scenario)) +
      geom_area(alpha = 0.15, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch4_chisq_stats <- renderUI({
    selected <- input$ch4_chisq_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch4_chisq_defs[[id]]
      paste0(s$label, ":  E(X) = ", s$df, ",  SD = ", round(sqrt(2 * s$df), 2))
    })
    div(style = "font-size: 13px; margin-top: 10px; line-height: 1.8;",
      lapply(stats, function(s) div(s))
    )
  })

  # --- Widget 6: Log-normalny — scenariusze overlay ---
  zoom_plot_server("ch4_lnorm_plot", reactive({
    selected <- input$ch4_lnorm_scenarios
    req(length(selected) > 0)

    # Oblicz wspolny zakres x na podstawie wybranych scenariuszy
    x_max <- max(sapply(selected, function(id) {
      s <- ch4_lnorm_defs[[id]]
      qlnorm(0.99, s$mu, s$sigma)
    }))

    x_seq <- seq(0.001, x_max, length.out = 500)

    dfs <- lapply(seq_along(selected), function(i) {
      s <- ch4_lnorm_defs[[selected[i]]]
      data.frame(x = x_seq, y = dlnorm(x_seq, s$mu, s$sigma), scenario = s$label)
    })
    df <- do.call(rbind, dfs)
    df$scenario <- factor(df$scenario, levels = sapply(ch4_lnorm_defs[selected], `[[`, "label"))

    n_sel <- length(selected)
    colors <- setNames(upwr_cat_n(n_sel),
                       sapply(ch4_lnorm_defs[selected], `[[`, "label"))

    ggplot(df, aes(x = x, y = y, color = scenario, fill = scenario)) +
      geom_area(alpha = 0.15, position = "identity") +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = colors, name = NULL) +
      scale_fill_manual(values = colors, guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(
           x = "x", y = "f(x)") +
      theme_upwr() +
      theme(legend.position = "top", legend.text = element_text(size = 11))
  }))

  output$ch4_lnorm_stats <- renderUI({
    selected <- input$ch4_lnorm_scenarios
    req(length(selected) > 0)

    stats <- lapply(selected, function(id) {
      s <- ch4_lnorm_defs[[id]]
      ev <- exp(s$mu + s$sigma^2 / 2)
      med <- exp(s$mu)
      sd_val <- sqrt((exp(s$sigma^2) - 1) * exp(2 * s$mu + s$sigma^2))
      paste0(s$label, ":  E(X) = ", round(ev, 1),
             ",  Me = ", round(med, 1),
             ",  SD = ", round(sd_val, 1))
    })
    div(style = "font-size: 13px; margin-top: 10px; line-height: 1.8;",
      lapply(stats, function(s) div(s))
    )
  })

}
