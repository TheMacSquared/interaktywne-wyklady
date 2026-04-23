# ============================================================================
# CHAPTER 1: Od danych do prawdopodobienstwa
# ============================================================================

ch1_ui <- list(
  id = "ch-most", num = "01", title = "Od danych do prawdopodobieństwa",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Rozkłady prawdopodobieństwa",
      num    = "01",
      title  = "Od danych do prawdopodobieństwa.",
      lead   = "W statystyce opisowej nauczyliśmy się liczyć częstości, obliczać
                średnie i rysować histogramy. Teraz zobaczymy, jak częstości względne
                zbiegają do prawdopodobieństw i czym jest rozkład prawdopodobieństwa."
    ),

    # ========================================================================
    # WIDGET 1: Stabilizacja czestosci (rzut kostka)
    # ========================================================================
    h2(id = "ch1-pwl", class = "section-title", "Prawo wielkich liczb w akcji"),

    div(class = "narrative",
      p("Wyobraź sobie, że rzucasz kostką. Jak często wypada każda ścianka?
        Przy kilku rzutach wyniki są chaotyczne, ale im więcej rzutów,
        tym częstości względne stają się bardziej stabilne.")
    ),

    figure_panel(
      label = "Ryc. 1.1",
      title = "Rzuty kostką — stabilizacja częstości",
      full_width = TRUE,
      fluidRow(
        column(4,
          div(style = "display: flex; flex-direction: column; gap: 8px;",
            actionButton("ch1_roll_1", "Rzuć 1x",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_roll_10", "Rzuć 10x",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_roll_100", "Rzuć 100x",
                         class = "btn-primary", width = "100%"),
            actionButton("ch1_roll_1000", "Rzuć 1000x",
                         class = "btn-primary", width = "100%"),
            hr(),
            actionButton("ch1_roll_reset", "Reset",
                         class = "btn-outline-secondary", width = "100%")
          ),
          br(),
          uiOutput("ch1_roll_count")
        ),
        column(8,
          plotOutput("ch1_freq_bar", height = "250px"),
          plotOutput("ch1_conv_plot", height = "250px")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Prawo wielkich liczb:"),
      " Wraz ze wzrostem liczby obserwacji, częstość względna każdego
        wyniku zbiega do jego prawdopodobieństwa teoretycznego.
        Dla uczciwej kostki każda ścianka ma P = 1/6 ≈ 0.167."
    ),

    # ========================================================================
    # WIDGET 0: Rozklad empiryczny vs teoretyczny
    # ========================================================================
    h2(id = "ch1-rozklad-emp", class = "section-title", "Rozkład empiryczny vs teoretyczny"),

    div(class = "narrative",
      p("Kostka to prosty przykład, ale ten sam mechanizm działa dla każdej
        zmiennej losowej. Znasz już histogram — pokazuje, jak często dane
        przyjmują różne wartości. To jest ", tags$b("rozkład empiryczny"),
        " — oparty na obserwacjach."),
      p("A gdybyśmy znali ", tags$b("regułę generującą dane"),
        "? Wtedy zamiast histogramu mielibyśmy gładką krzywą — ",
        tags$b("rozkład teoretyczny"),
        ". Zobaczmy, jak jedno przechodzi w drugie.")
    ),

    figure_panel(
      label = "Ryc. 1.2",
      title = "Histogram (dane) vs krzywa gęstości (model)",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch1_emp_dist", "Rozkład źródłowy:",
            choices = c(
              "Wzrost studentów (normalny)" = "normal",
              "Czas dojazdu (skośny)"       = "skewed",
              "Ocena losowa (jednostajny)"   = "uniform"
            ),
            selected = "normal"
          ),
          sliderInput("ch1_emp_n", "Wielkość próby:",
                      min = 20, max = 5000, value = 200, step = 20),
          actionButton("ch1_emp_resample", "Losuj nową próbę",
                       class = "btn-primary", width = "100%"),
          hr(),
          checkboxInput("ch1_show_hist", "Histogram (dane empiryczne)", value = TRUE),
          checkboxInput("ch1_show_density", "Krzywa gęstości (model teoretyczny)", value = FALSE)
        ),
        column(8,
          plotOutput("ch1_emp_vs_theo", height = "380px"),
          uiOutput("ch1_emp_text")
        )
      )
    ),

    margin_callout(
      label = "Kluczowa idea",
      tagList(
        "Włącz obie warstwy i zwiększaj próbę. Im więcej danych,
        tym lepiej histogram przybliża krzywą.
        Rozkład teoretyczny to ", tags$b("ideał"), " — dane to jego ",
        tags$b("niedoskonałe odbicie"), "."
      )
    ),

    # ========================================================================
    # WIDGET 2: Czestosci vs prawdopodobienstwo
    # ========================================================================
    h2(id = "ch1-czestosci", class = "section-title", "Częstości vs prawdopodobieństwo"),

    div(class = "narrative",
      p("Prawdopodobieństwo to ", tags$b("teoretyczny model"),
        " opisujący, jak często powinny występować różne wyniki.
        Częstości względne z danych to ", tags$b("empiryczne przybliżenie"),
        " tego modelu.")
    ),

    figure_panel(
      label = "Ryc. 1.3",
      title = "Porównanie: teoria vs obserwacja",
      full_width = TRUE,
      fluidRow(
        column(4,
          radioButtons("ch1_scenario", "Scenariusz:",
            choices = c(
              "Uczciwa kostka"   = "fair",
              "Obciążona kostka" = "loaded",
              "Moneta"           = "coin"
            ),
            selected = "fair"
          ),
          sliderInput("ch1_n_obs", "Liczba obserwacji:",
                      min = 10, max = 5000, value = 100, step = 10),
          actionButton("ch1_resample", "Losuj ponownie",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch1_freq_vs_prob", height = "350px"),
          uiOutput("ch1_freq_vs_prob_text")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Czym jest rozklad?
    # ========================================================================
    h2(id = "ch1-rozklad", class = "section-title", "Czym jest rozkład prawdopodobieństwa?"),

    div(class = "narrative",
      p("Rozkład prawdopodobieństwa to ", tags$b("kompletny opis"),
        " wszystkich możliwych wyników i ich prawdopodobieństw.
        Musi spełniać dwa warunki:"),
      tags$ol(
        tags$li("Każde prawdopodobieństwo jest nieujemne: P(x) ≥ 0"),
        tags$li("Suma wszystkich prawdopodobieństw wynosi 1")
      ),
      p("Spróbuj sam(a) zbudować rozkład — ustaw prawdopodobieństwa
        czterech wyników tak, żeby sumowały się do 1.")
    ),

    figure_panel(
      label = "Ryc. 1.4",
      title = "Zbuduj własny rozkład",
      full_width = TRUE,
      fluidRow(
        column(6,
          sliderInput("ch1_p1", "P(Wynik A):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch1_p2", "P(Wynik B):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch1_p3", "P(Wynik C):", min = 0, max = 1, value = 0.25, step = 0.01),
          sliderInput("ch1_p4", "P(Wynik D):", min = 0, max = 1, value = 0.25, step = 0.01),
          uiOutput("ch1_sum_check")
        ),
        column(6,
          plotOutput("ch1_custom_dist", height = "300px")
        )
      )
    ),

    margin_callout(
      label = "Zapamiętaj",
      tagList(
        "Rozkład prawdopodobieństwa to nie dane — to ", tags$b("model matematyczny"),
        ". Dane to próbka z tego modelu. Im większa próbka, tym lepiej przybliża model."
      ),
      color = "uwaga"
    ),

    lc_chapter_next(
      num       = "02",
      title     = "Wartość oczekiwana i wariancja",
      lead      = "czego się spodziewać i jak mierzyć rozrzut wyników.",
      target_id = "ch-ev-var"
    )
  )
)

# --------------------------------------------------------------------------
# Chapter 1 Server
# --------------------------------------------------------------------------

ch1_server <- function(input, output, session) {

  # --- Widget 0: Rozklad empiryczny vs teoretyczny ---
  ch1_emp_data <- reactiveVal(NULL)

  generate_emp_data <- function() {
    req(input$ch1_emp_n, input$ch1_emp_dist)
    n <- input$ch1_emp_n
    dist <- input$ch1_emp_dist
    data <- switch(dist,
      "normal"  = rnorm(n, mean = 170, sd = 8),
      "skewed"  = rgamma(n, shape = 3, scale = 10) + 5,
      "uniform" = runif(n, min = 1, max = 6)
    )
    ch1_emp_data(list(data = data, dist = dist, n = n))
  }

  observe({
    input$ch1_emp_dist
    input$ch1_emp_n
    generate_emp_data()
  })
  observeEvent(input$ch1_emp_resample, generate_emp_data())

  output$ch1_emp_vs_theo <- renderPlot({
    d <- ch1_emp_data()
    req(d)

    show_hist <- input$ch1_show_hist
    show_dens <- input$ch1_show_density

    if (!show_hist && !show_dens) {
      return(ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Włącz przynajmniej jedną warstwę",
                 size = 6, color = "#7f8c8d") +
        theme_void())
    }

    df <- data.frame(x = d$data)

    # Zakres teoretyczny osi X
    theo_xlim <- switch(d$dist,
      "normal"  = c(170 - 4*8, 170 + 4*8),   # mu +/- 4*sigma
      "skewed"  = c(0, 5 + qgamma(0.999, shape = 3, scale = 10)),
      "uniform" = c(0.5, 6.5)
    )
    # Zakres empiryczny
    emp_xlim <- range(d$data)
    # Wez szerszy z dwoch
    x_lo <- min(theo_xlim[1], emp_xlim[1])
    x_hi <- max(theo_xlim[2], emp_xlim[2])
    x_margin <- (x_hi - x_lo) * 0.05
    fixed_xlim <- c(x_lo - x_margin, x_hi + x_margin)

    x_seq <- seq(fixed_xlim[1], fixed_xlim[2], length.out = 500)

    theo_y <- switch(d$dist,
      "normal"  = dnorm(x_seq, mean = 170, sd = 8),
      "skewed"  = dgamma(x_seq - 5, shape = 3, scale = 10),
      "uniform" = dunif(x_seq, min = 1, max = 6)
    )
    theo_y[is.na(theo_y) | theo_y < 0] <- 0
    df_theo <- data.frame(x = x_seq, y = theo_y)

    # Stale breaks oparte na danych (niezalezne od osi)
    n_bins <- min(50, max(10, d$n / 10))
    bin_breaks <- seq(min(d$data), max(d$data), length.out = n_bins + 1)

    # Zakres Y: max z gestosci teoretycznej i histogramu
    theo_ymax <- max(theo_y)
    hist_obj <- hist(d$data, breaks = bin_breaks, plot = FALSE)
    hist_ymax <- max(hist_obj$density)
    fixed_ymax <- max(theo_ymax, hist_ymax) * 1.08

    dist_label <- switch(d$dist,
      "normal"  = "Rozkład normalny N(170, 8)",
      "skewed"  = "Rozkład gamma (skośny)",
      "uniform" = "Rozkład jednostajny U(1, 6)"
    )

    p <- ggplot()

    if (show_hist) {
      p <- p + geom_histogram(data = df, aes(x = x, y = after_stat(density)),
                               breaks = bin_breaks,
                               fill = col_primary, color = "white", alpha = 0.6)
    }

    if (show_dens) {
      p <- p + geom_line(data = df_theo, aes(x = x, y = y),
                          color = col_secondary, linewidth = 1.8) +
        geom_area(data = df_theo, aes(x = x, y = y),
                  fill = col_secondary, alpha = 0.1)
    }

    p + coord_cartesian(xlim = fixed_xlim, ylim = c(0, fixed_ymax)) +
    labs(
      title = paste0(if (show_hist && show_dens) "Dane + model"
                     else if (show_hist) "Dane empiryczne (histogram)"
                     else "Model teoretyczny (krzywa)"),
      subtitle = paste0("n = ", d$n, " | ", dist_label),
      x = "Wartość", y = "Gęstość"
    ) +
    theme_upwr()
  })

  output$ch1_emp_text <- renderUI({
    show_hist <- input$ch1_show_hist
    show_dens <- input$ch1_show_density

    if (show_hist && show_dens) {
      div(class = "callout-success",
        "Widzisz obie warstwy. Histogram to dane, krzywa to model.
         Zwiększaj próbę — histogram coraz lepiej przybliża krzywą!")
    } else if (show_hist) {
      div(class = "callout-info",
        "To histogram — rozkład empiryczny oparty na danych.
         Włącz krzywą gęstości, żeby zobaczyć model teoretyczny.")
    } else if (show_dens) {
      div(class = "callout-info",
        "To krzywa gęstości — model teoretyczny.
         Włącz histogram, żeby porównać z danymi.")
    }
  })

  # --- Widget 1: Rzuty kostka ---
  dice_rolls <- reactiveVal(integer(0))

  observeEvent(input$ch1_roll_1, {
    dice_rolls(c(dice_rolls(), sample(1:6, 1)))
  })
  observeEvent(input$ch1_roll_10, {
    dice_rolls(c(dice_rolls(), sample(1:6, 10, replace = TRUE)))
  })
  observeEvent(input$ch1_roll_100, {
    dice_rolls(c(dice_rolls(), sample(1:6, 100, replace = TRUE)))
  })
  observeEvent(input$ch1_roll_1000, {
    dice_rolls(c(dice_rolls(), sample(1:6, 1000, replace = TRUE)))
  })
  observeEvent(input$ch1_roll_reset, {
    dice_rolls(integer(0))
  })

  output$ch1_roll_count <- renderUI({
    n <- length(dice_rolls())
    div(class = "stat-box", style = paste0("background: ", col_primary, ";"),
        paste0("Rzutów: ", n))
  })

  output$ch1_freq_bar <- renderPlot({
    rolls <- dice_rolls()
    if (length(rolls) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij przycisk, aby rzucić kostką",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      df <- data.frame(face = factor(rolls, levels = 1:6))
      freq_df <- df %>% count(face, .drop = FALSE) %>%
        mutate(rel_freq = n / sum(n))
      ggplot(freq_df, aes(x = face, y = rel_freq)) +
        geom_col(fill = col_primary, color = "white", alpha = 0.85) +
        geom_hline(yintercept = 1/6, color = col_secondary, linewidth = 1, linetype = "dashed") +
        geom_text(aes(label = sprintf("%.3f", rel_freq)), vjust = -0.5, size = 4) +
        scale_y_continuous(limits = c(0, max(0.35, max(freq_df$rel_freq) * 1.15)),
                           expand = expansion(mult = c(0, 0.05))) +
        labs(title = "Częstości względne", x = "Ścianka", y = "Częstość względna") +
        annotate("text", x = 6.3, y = 1/6, label = "1/6", color = col_secondary,
                 fontface = "bold", size = 4, hjust = 0) +
        theme_upwr()
    }
  })

  output$ch1_conv_plot <- renderPlot({
    rolls <- dice_rolls()
    if (length(rolls) < 2) return(NULL)

    # Linia zbieznosci dla kazdej scianki
    n_total <- length(rolls)
    # Wybierz punkty do wykreslenia (max 200 punktow dla wydajnosci)
    if (n_total <= 200) {
      indices <- seq_len(n_total)
    } else {
      indices <- unique(c(
        seq(1, min(50, n_total)),
        round(seq(51, n_total, length.out = 150))
      ))
    }

    conv_data <- do.call(rbind, lapply(indices, function(i) {
      tab <- table(factor(rolls[1:i], levels = 1:6)) / i
      data.frame(n = i, face = factor(1:6), rel_freq = as.numeric(tab))
    }))

    ggplot(conv_data, aes(x = n, y = rel_freq, color = face)) +
      geom_line(linewidth = 0.8, alpha = 0.7) +
      geom_hline(yintercept = 1/6, color = "gray40", linewidth = 0.8, linetype = "dashed") +
      scale_color_brewer(palette = "Set2", name = "Ścianka") +
      labs(title = "Zbieżność częstości do 1/6",
           x = "Liczba rzutów", y = "Częstość względna") +
      theme_upwr() +
      theme(legend.position = "right")
  })

  # --- Widget 2: Czestosci vs prawdopodobienstwo ---
  freq_data <- reactiveVal(NULL)

  observe({
    input$ch1_resample
    input$ch1_scenario
    req(input$ch1_n_obs, input$ch1_scenario)
    n <- input$ch1_n_obs

    scenario <- input$ch1_scenario
    if (scenario == "fair") {
      obs <- sample(1:6, n, replace = TRUE)
      theo <- rep(1/6, 6)
      labels <- as.character(1:6)
    } else if (scenario == "loaded") {
      probs <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
      obs <- sample(1:6, n, replace = TRUE, prob = probs)
      theo <- probs
      labels <- as.character(1:6)
    } else {
      obs <- sample(c(1, 2), n, replace = TRUE)
      theo <- c(0.5, 0.5)
      labels <- c("Orzeł", "Reszka")
    }

    freq_data(list(obs = obs, theo = theo, labels = labels))
  })

  output$ch1_freq_vs_prob <- renderPlot({
    fd <- freq_data()
    req(fd)

    n_levels <- length(fd$labels)
    tab <- table(factor(fd$obs, levels = 1:n_levels)) / length(fd$obs)

    df_obs <- data.frame(
      outcome = factor(fd$labels, levels = fd$labels),
      value = as.numeric(tab)
    )
    df_theo <- data.frame(
      outcome = factor(fd$labels, levels = fd$labels),
      value = fd$theo
    )

    ggplot() +
      geom_col(data = df_obs, aes(x = outcome, y = value, fill = "Obserwowane"),
               alpha = 0.85, color = "white") +
      geom_line(data = df_theo, aes(x = outcome, y = value, color = "Teoretyczne", group = 1),
                linewidth = 1.2) +
      geom_point(data = df_theo, aes(x = outcome, y = value, color = "Teoretyczne"),
                 size = 4) +
      scale_fill_manual(values = c("Obserwowane" = col_primary), name = "") +
      scale_color_manual(values = c("Teoretyczne" = col_secondary), name = "") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = paste0("n = ", length(fd$obs), " obserwacji"),
           x = "Wynik", y = "Proporcja / Prawdopodobieństwo") +
      theme_upwr() +
      theme(legend.position = "top")
  })

  output$ch1_freq_vs_prob_text <- renderUI({
    fd <- freq_data()
    req(fd)
    n <- length(fd$obs)
    max_diff <- max(abs(table(factor(fd$obs, levels = 1:length(fd$labels))) / n - fd$theo))

    div(class = if (max_diff < 0.05) "callout-success" else "callout-info",
      paste0("Maksymalna różnica między częstością a prawdopodobieństwem: ",
             sprintf("%.3f", max_diff),
             if (max_diff < 0.05) " — dobra zgodność!" else " — spróbuj zwiększyć n")
    )
  })

  # --- Widget 3: Zbuduj wlasny rozklad ---
  output$ch1_sum_check <- renderUI({
    s <- input$ch1_p1 + input$ch1_p2 + input$ch1_p3 + input$ch1_p4
    if (abs(s - 1) < 0.005) {
      div(class = "stat-box", style = paste0("background: ", col_success, ";"),
          paste0("∑ = ", sprintf("%.2f", s), " ✔"))
    } else {
      div(class = "stat-box", style = paste0("background: ", col_secondary, ";"),
          paste0("∑ = ", sprintf("%.2f", s), " ≠ 1 ✘"))
    }
  })

  output$ch1_custom_dist <- renderPlot({
    probs <- c(input$ch1_p1, input$ch1_p2, input$ch1_p3, input$ch1_p4)
    s <- sum(probs)
    valid <- abs(s - 1) < 0.005

    df <- data.frame(
      outcome = c("A", "B", "C", "D"),
      prob = probs
    )

    ggplot(df, aes(x = outcome, y = prob)) +
      geom_col(fill = if (valid) col_success else "#95a5a6",
               color = "white", alpha = 0.85, width = 0.6) +
      geom_text(aes(label = sprintf("%.2f", prob)), vjust = -0.5, size = 5) +
      scale_y_continuous(limits = c(0, 1.1), expand = expansion(mult = c(0, 0))) +
      labs(title = if (valid) "Poprawny rozkład!" else "Suma musi wynosić 1",
           x = "Wynik", y = "Prawdopodobieństwo") +
      theme_upwr()
  })

}
