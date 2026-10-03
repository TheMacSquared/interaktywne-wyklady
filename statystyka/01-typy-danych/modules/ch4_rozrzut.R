# ============================================================================
# CHAPTER 4: Statystyki rozrzutu
# ============================================================================

ch4_ui <- list(
  id = "ch-rozrzut", num = "04", title = "Statystyki rozrzutu",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 04 · Statystyka opisowa",
      num    = "04",
      title  = "Statystyki rozrzutu.",
      lead   = "Średnia mówi gdzie jest środek, ale nic o tym, jak bardzo dane są
                rozproszone. Dwie grupy mogą mieć tę samą średnią, a wyglądać
                zupełnie inaczej — pora zmierzyć rozrzut."
    ),

    uiOutput("tracker_ch4"),

    tagList(
      p("W tym rozdziale poznamy miary rozrzutu: ", gloss("odchylenie standardowe"), ",
        ", gloss("wariancja", "wariancję"), ", ", gloss("rozstęp"), ", ",
        gloss("rozstęp międzykwartylowy"), " (IQR) oraz
        ", gloss("współczynnik zmienności"), ". Nauczymy się też budować ",
        gloss("wykres pudełkowy", "boxplot"), " od podstaw.")
    ),

    # ====================================================================
    # WIDGET 1: Bus scenario - "Mean is not everything"
    # ====================================================================
    lc_h2("ch4-srednia", "Średnia to nie wszystko"),

    tagList(
      p("Wyobraź sobie dwie linie autobusowe. Obie mają takie samo
        średnie spóźnienie -- około 2 minuty. Którą wybierzesz?"),
      p("Większość autobusów jest blisko rozkładu (0-4 min spóźnienia),
        rzadko który przyjeżdża za wcześnie, a od czasu do czasu
        zdarza się duże spóźnienie. Ale rozrzut tych spóźnień
        może być bardzo różny.")
    ),

    figure_panel(
      label = "Ryc. 4.1",
      lc_step_widget("ch4_spread",
        title = "Dwie linie autobusowe — ta sama średnia, inny rozrzut",
        steps = c("Dwie linie", "Ta sama średnia, ale...", "Wychodzisz wcześniej",
                  "Konsekwencje"),
        toolbar = lc_toolbar(
          lc_step_from(3,
            lc_slider("ch4_spread_buffer", "Wychodzisz wcześniej o (minuty)", 0, 10, 0, 1)
          ),
          lc_readouts(uiOutput("ch4_spread_reads"))
        ),
        plot_id = "ch4_spread_plot",
        ratio = "2/1"
      )
    ),

    # ====================================================================
    # WIDGET 2: SD step-by-step
    # ====================================================================
    lc_h2("ch4-odchylenie", "Odchylenie standardowe krok po kroku"),

    tagList(
      p("Jak obliczamy odchylenie standardowe? Krok po kroku.
        Zobaczmy to na przykładzie 10 pomiarów wzrostu.")
    ),

    figure_panel(
      label = "Ryc. 4.2",
      lc_step_widget("ch4_sd",
        title = "Obliczanie odchylenia standardowego",
        steps = c("Dane", "Odchylenia od średniej", "Wariancja i SD"),
        toolbar = lc_toolbar(
          lc_action("ch4_sd_new", "Losuj nowy zestaw", variant = "outline")
        ),
        plot_id = "ch4_sd_plot",
        ratio = "2/1",
        extra = uiOutput("ch4_sd_table")
      )
    ),

    # ====================================================================
    # WIDGET 2b: Empirical rule (68-95-99.7)
    # ====================================================================
    lc_h2("ch4-regula", "Reguła empiryczna (68-95-99.7)"),

    tagList(
      p("Wiemy juz jak obliczyć odchylenie standardowe. Ale co ono oznacza
        w praktyce? Dla rozkładow zbliżonych do normalnego obowiązuje
        ", gloss("reguła 68-95-99,7", "regula empiryczna"), ": okolo 68% danych miesci sie w zakresie
        srednia ±1 SD, 95% w ±2 SD, a 99.7% w ±3 SD.")
    ),

    figure_panel(
      label = "Ryc. 4.3",
      title = "Regula 68-95-99.7 -- czy zawsze dziala?",
      selectInput("ch4_emp_var", "Wybierz zmienna:",
        choices = c("Wzrost (cm)" = "wzrost",
                    "Waga (kg)" = "waga",
                    "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost"
      ),
      lc_plot("ch4_emp_plot", ratio = "1.6/1", max_height = "400px"),
      uiOutput("ch4_emp_text")
    ),

    # ====================================================================
    # WIDGET 3: Boxplot builder
    # ====================================================================
    lc_h2("ch4-boxplot", "Budujemy boxplot od podstaw"),

    tagList(
      p("Boxplot to wizualne podsumowanie rozkładu oparte na kwartylach.
        Zbudujmy go od podstaw, krok po kroku, aby zrozumieć co
        oznacza każdy element tego wykresu.")
    ),

    figure_panel(
      label = "Ryc. 4.4",
      lc_step_widget("ch4_bp",
        title = "Boxplot — budowa krok po kroku",
        steps = c("Surowe dane", "Mediana", "Kwartyle i pudełko", "Wąsy i outliers",
                  "Gotowy boxplot"),
        toolbar = lc_toolbar(
          lc_action("ch4_bp_new", "Losuj nowe dane", variant = "outline")
        ),
        plot_id = "ch4_bp_plot",
        ratio = "2/1"
      )
    ),

    # ====================================================================
    # WIDGET 3b: Group comparison -- side-by-side boxplots
    # ====================================================================
    lc_h2("ch4-porownanie", "Porównanie grup"),

    tagList(
      p("Dotychczas analizowalismy caly zbior danych naraz. Ale jednym z
        najczestszych pytan w statystyce jest: czy grupy sie roznia?
        Boxploty obok siebie to doskonałe narzędzie do porównywania rozkładow
        miedzy grupami.")
    ),

    figure_panel(
      label = "Ryc. 4.5",
      title = "Boxploty grupowane",
      fluidRow(
        column(4,
          selectInput("ch4_grp_var", "Zmienna ilościowa:",
            choices = c("Wzrost (cm)" = "wzrost",
                        "Waga (kg)" = "waga",
                        "Czas dojazdu (min)" = "czas_dojazdu",
                        "Średnia ocen" = "srednia_ocen"),
            selected = "wzrost"
          )
        ),
        column(4,
          selectInput("ch4_grp_by", "Grupuj wg:",
            choices = c("Płeć" = "plec",
                        "Kierunek" = "kierunek"),
            selected = "plec"
          )
        ),
        column(4,
          checkboxInput("ch4_grp_violin", "Pokaz violin plot", value = FALSE),
          checkboxInput("ch4_grp_points", "Pokaz punkty", value = TRUE)
        )
      ),
      lc_plot("ch4_grp_plot", ratio = "1.6/1", max_height = "400px"),
      uiOutput("ch4_grp_table")
    ),

    # ====================================================================
    # WIDGET 4: Spread measures comparison
    # ====================================================================
    lc_h2("ch4-miary", "Porównanie miar rozrzutu"),

    tagList(
      p("Porównajmy rozne miary rozrzutu i ich ", gloss("odporność"), " na ",
        gloss("wartość odstająca", "wartości odstające"), ". Dodaj outliera i obserwuj, ktore miary sie zmieniaja,
        a ktore pozostaja stabilne.")
    ),

    figure_panel(
      label = "Ryc. 4.6",
      title = "Porównanie miar rozrzutu i ich odporności",
      div(style = "margin-bottom: 10px;",
        actionButton("ch4_comp_add1", "Dodaj outlier (+30 cm)",
                     class = "lc-btn-warning", style = "margin-right: 6px;"),
        actionButton("ch4_comp_add5", "Dodaj 5 outlierow",
                     class = "lc-btn-danger", style = "margin-right: 6px;"),
        lc_action("ch4_comp_reset", icon = "reset", variant = "ghost", aria_label = "Reset")
      ),
      lc_plot("ch4_comp_plot", ratio = "1.8/1", max_height = "350px"),
      uiOutput("ch4_comp_table")
    ),

    inline_callout(
      label = "Wniosek",
      "Rozstęp jest bardzo wrażliwy na outliery — wystarczy jedna wartość
       odstająca, aby go zmienić. IQR i odchylenie standardowe są bardziej
       odporne, a IQR jest z nich najbardziej stabilny.",
      color = "uwaga"
    ),

    # ====================================================================
    # WIDGET 5: Coefficient of Variation
    # ====================================================================
    lc_h2("ch4-cv", "Współczynnik zmienności (CV)"),

    tagList(
      p("Odchylenie standardowe mówi o rozrzucie, ale w jakich jednostkach?
        SD wzrostu (w cm) i SD wagi (w kg) nie są porownywalne!
        Aby porownac zmiennosc zmiennych w roznych skalach, uzywamy
        współczynnika zmienności (CV = SD / średnia × 100%).")
    ),

    figure_panel(
      label = "Ryc. 4.7",
      title = "Porównanie zmienności miedzy zmiennymi",
      fluidRow(
        column(6,
          h5(style = "text-align: center; color: var(--upwr-reference);", "SD — nieporównywalne"),
          zoom_plot_ui("ch4_sd_compare_plot", height = "350px")
        ),
        column(6,
          h5(style = "text-align: center; color: var(--upwr-reference);", "CV — porównywalne"),
          zoom_plot_ui("ch4_cv_plot", height = "350px")
        )
      ),
      tableOutput("ch4_cv_table"),
      lc_feedback(type = "info",
        tags$strong("Interpretacja: "),
        "Lewy wykres pokazuje SD w oryginalnych jednostkach -- wartości są nieporównywalne,
         bo każda zmienna ma inna skalę. Prawy wykres pokazuje CV (%), które normalizuje
         rozrzut wzgledem średniej -- teraz widać, że czas dojazdu
        ma największa względna zmienność, choć jego SD nie jest największe."
      )
    ),

    lc_chapter_next(
      num       = "05",
      title     = "Kształt rozkładu",
      lead      = "dwa rozkłady z tą samą średnią i SD mogą mieć zupełnie inny kształt — asymetrię i ciężkość ogonów.",
      target_id = "ch-ksztalt"
    ),

    # Bottom spacing
    lc_spacer("md")

  )
)

# --------------------------------------------------------------------------
# Chapter 4 Server
# --------------------------------------------------------------------------

ch4_server <- function(input, output, session) {

  # --- Widget 1: Bus scenario ---

  # Krok widgetu (1..4) żyje w przeglądarce; suwak działa od kroku 3.
  ch4_spread_step <- lc_step_server("ch4_spread", input)$step

  # Helper: generate bus delay data (deterministic seed)
  ch4_bus_data <- function() {
    set.seed(123)
    data_a <- rgamma(1000, shape = 8, scale = 0.25) - 0.3
    data_b <- rgamma(1000, shape = 0.4, scale = 5)  - 0.3
    data_a <- data_a - mean(data_a) + 2
    data_b <- data_b - mean(data_b) + 2
    list(a = data_a, b = data_b,
         sd_a = round(sd(data_a), 1), sd_b = round(sd(data_b), 1))
  }

  # Odczyty SD zastępują legendę: kolor odczytu = kolor linii.
  output$ch4_spread_reads <- renderUI({
    bus <- ch4_bus_data()
    tagList(
      lc_readout("Linia A", paste0("SD = ", lc_fmt(bus$sd_a, 1), " min"),
                 color = STEP_ROLES$data$colour, swatch = TRUE),
      lc_readout("Linia B", paste0("SD = ", lc_fmt(bus$sd_b, 1), " min"),
                 color = STEP_ROLES$group$colour, swatch = TRUE)
    )
  })

  zoom_plot_server("ch4_spread_plot", reactive({
    step <- ch4_spread_step()

    buffer <- input$ch4_spread_buffer
    req(!is.null(buffer))
    bus <- ch4_bus_data()

    dens_a <- density(bus$a, from = -3, to = 30, n = 500)
    dens_b <- density(bus$b, from = -3, to = 30, n = 500)
    df_a <- data.frame(x = dens_a$x, y = dens_a$y)
    df_b <- data.frame(x = dens_b$x, y = dens_b$y)
    # Stała rama: oś Y z obu krzywych, wspólna dla kroków.
    y_hi <- max(df_a$y, df_b$y) * 1.08

    p <- ggplot(mapping = aes(x = x, y = y)) +
      step_line("known", xintercept = 2) +
      step_layer(geom_vline, "known", xintercept = 0, linewidth = 0.5, alpha = 0.5)

    if (step >= 3) {
      cutoff <- -buffer
      shade_a <- df_a[df_a$x >= cutoff, ]
      shade_b <- df_b[df_b$x >= cutoff, ]

      p <- p +
        geom_area(data = shade_a, fill = STEP_ROLES$data$colour, alpha = 0.25) +
        geom_area(data = shade_b, fill = STEP_ROLES$group$colour, alpha = 0.15) +
        step_line(step_role(step, 3), xintercept = cutoff)
    }

    p +
      step_layer(geom_line, "data", data = df_a, linewidth = 1.2, alpha = 1) +
      step_layer(geom_line, "group", data = df_b, linewidth = 1.2, alpha = 1) +
      labs(x = "Spóźnienie (minuty)    ← za wcześnie | za późno →",
           y = "Gęstość") +
      step_frame(xlim = c(-3, 25), ylim = c(0, y_hi))
  }))

  output$ch4_spread_text <- renderUI({
    step <- ch4_spread_step()
    buffer <- input$ch4_spread_buffer
    bus <- ch4_bus_data()

    if (step == 1) {
      "Obie linie mają średnie spóźnienie około 2 minut.
       Patrząc tylko na średnią, są identyczne.
       Wartości ujemne = przyjazd przed czasem (rzadko się zdarza)."
    } else if (step == 2) {
      pct_10_a <- round(mean(bus$a > 10) * 100, 1)
      pct_10_b <- round(mean(bus$b > 10) * 100, 1)
      mean_late_a <- if (any(bus$a > 10)) round(mean(bus$a[bus$a > 10]), 1) else 0
      mean_late_b <- if (any(bus$b > 10)) round(mean(bus$b[bus$b > 10]), 1) else 0
      tagList(
        paste0("Linia A ma SD = ", bus$sd_a, " min (spóźnienia skupione 0-4 min),
        a linia B ma SD = ", bus$sd_b, " min (zdarza się i punktualnie,
        i 10+ min spóźnienia)."),
        tags$br(),
        tags$strong("Spóźnienia >10 min:"),
        paste0(" Linia A: ", pct_10_a, "% kursów",
               if (pct_10_a > 0) paste0(" (śr. ", mean_late_a, " min)") else "",
               "; Linia B: ", pct_10_b, "% kursów",
               if (pct_10_b > 0) paste0(" (śr. ", mean_late_b, " min)") else "",
               ".")
      )
    } else if (step == 3) {
      lbl <- if (buffer == 0) "na stówkę (0 min zapasu)"
             else paste0(buffer, " min wcześniej")
      paste0("Wychodzisz ", lbl,
             ". Jesteś na przystanku o ", buffer,
             " min przed rozkładem. Zdążysz na każdy autobus,
             który nie odjedzie wcześniej niż ", buffer,
             " min przed rozkładem. Zacieniowany obszar = kursy,
             na które zdążysz. Przesuń suwak!")
    } else if (step == 4) {
      prob_a <- mean(bus$a >= -buffer)
      prob_b <- mean(bus$b >= -buffer)
      pct_10_b <- round(mean(bus$b > 10) * 100, 1)
      mean_late_b <- if (any(bus$b > 10)) round(mean(bus$b[bus$b > 10]), 1) else 0
      lbl <- if (buffer == 0) "na stówkę" else paste0(buffer, " min wcześniej")
      tagList(
        paste0("Wychodzisz ", lbl, ":"),
        tags$br(),
        paste0("Linia A: zdążysz na ", round(prob_a * 100, 1), "% kursów."),
        tags$br(),
        paste0("Linia B: zdążysz na ", round(prob_b * 100, 1), "% kursów."),
        tags$br(),
        if (pct_10_b > 0) tagList(
          tags$em(paste0("A gdy linia B się spóźni poważnie (>10 min, ",
                         pct_10_b, "% kursów), średnie czekasz ",
                         mean_late_b, " min. ",
                         "Linia A praktycznie nigdy tak się nie spóźnia.")),
          tags$br()
        ),
        "To dlatego sama średnia nie wystarczy -- rozrzut danych
        ma realne konsekwencje!"
      )
    }
  })

  # --- Widget 2: SD step-by-step ---

  # Krok widgetu (1..3) żyje w przeglądarce; nowy zestaw nie cofa kroku.
  ch4_sd_step <- lc_step_server("ch4_sd", input)$step
  ch4_sd_data <- reactiveVal(round(rnorm(10, mean = 170, sd = 8), 1))

  observeEvent(input$ch4_sd_new, {
    set.seed(NULL)
    ch4_sd_data(round(rnorm(10, mean = 170, sd = 8), 1))
  })

  zoom_plot_server("ch4_sd_plot", reactive({
    step <- ch4_sd_step()

    vals <- ch4_sd_data()
    n <- length(vals)
    x_bar <- mean(vals)
    s <- sd(vals)

    # Stała rama: dane i pas średnia ± SD, wspólne dla kroków.
    x_rng <- range(vals, x_bar - s, x_bar + s)
    x_pad <- diff(x_rng) * 0.08
    frame <- step_frame(xlim = x_rng + c(-x_pad, x_pad), ylim = c(0, n + 1.2),
                        y_axis = FALSE)

    if (step == 1) {
      # Krok 1: punkty na osi liczbowej
      df <- data.frame(x = vals)
      p <- ggplot(df, aes(x = x, y = (n + 1) / 2)) +
        step_layer(geom_point, "data", size = 4, alpha = 1) +
        labs(x = "Wzrost (cm)", y = "") +
        frame

    } else {
      # Kroki 2-3: punkty jedna pod drugą, posortowane wg odległości od średniej
      deviations <- vals - x_bar
      ord <- order(abs(deviations), decreasing = TRUE)
      df <- data.frame(
        x = vals[ord],
        dev = deviations[ord],
        y = seq(n, 1)  # najdalszy na górze
      )
      dev_role <- step_role(step, 2)

      p <- ggplot(df, aes(x = x, y = y))

      if (step >= 3) {
        p <- p +
          annotate("rect", xmin = x_bar - s, xmax = x_bar + s,
                   ymin = 0, ymax = n + 0.3, fill = STEP_ROLES$new$colour, alpha = 0.08) +
          step_line("new", xintercept = x_bar - s) +
          step_line("new", xintercept = x_bar + s) +
          step_label(x_bar - s, 0.3, paste0("śr. - SD\n", round(x_bar - s, 1)),
                     role = "new", hjust = 0.5) +
          step_label(x_bar + s, 0.3, paste0("śr. + SD\n", round(x_bar + s, 1)),
                     role = "new", hjust = 0.5) +
          step_label(x_bar, 0.5, paste0("SD = ", round(s, 2), " cm"),
                     role = "new", hjust = 0.5, vjust = 0.5, size = 4.2)
      }

      p <- p +
        step_line(dev_role, xintercept = x_bar) +
        step_layer(geom_segment, dev_role,
                   mapping = aes(x = x_bar, xend = x, y = y, yend = y),
                   arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
        step_layer(geom_point, "data", size = 4, alpha = 1) +
        step_label(x_bar, n + 0.8, paste0("średnia = ", round(x_bar, 2)),
                   role = dev_role, hjust = 0.5, vjust = 0.5) +
        labs(x = "Wzrost (cm)", y = "") +
        frame
    }

    p
  }))

  output$ch4_sd_table <- renderUI({
    step <- ch4_sd_step()
    if (step < 2) return(NULL)

    vals <- ch4_sd_data()
    n <- length(vals)
    x_bar <- mean(vals)

    deviations <- vals - x_bar
    sq_deviations <- deviations^2

    df <- data.frame(
      i = as.character(1:n),
      x = vals,
      dev = round(deviations, 2),
      sq = round(sq_deviations, 2)
    )
    # Od kroku 3: wiersz sumy kwadratów odchyleń.
    foot <- if (step >= 3) list(i = "SUMA", x = "", dev = "",
                                sq = round(sum(sq_deviations), 2))

    lc_table(df, cols = list(
      lc_col("i", "i", "row"),
      lc_col("x", "xi", digits = 1),
      lc_col("dev", "xi - x_bar", digits = 2),
      lc_col("sq", "(xi - x_bar)^2", digits = 2)
    ), foot = foot)
  })

  output$ch4_sd_text <- renderUI({
    step <- ch4_sd_step()

    if (step == 1) {
      "Mamy 10 pomiarów wzrostu. Na osi liczbowej każdy punkt to jedna
       obserwacja. Jak bardzo są rozproszone?"
    } else if (step == 2) {
      vals <- ch4_sd_data()
      x_bar <- mean(vals)
      paste0("Obliczamy srednia: x̄ = ", round(x_bar, 2),
             " cm. Nastepnie liczymy odchylenie każdego punktu od średniej
             (strzalki na wykresie). W tabeli widzisz odchylenia i ich kwadraty.
             Kwadraty gwarantuja, ze odchylenia dodatnie i ujemne sie nie
             znosa.")
    } else if (step == 3) {
      vals <- ch4_sd_data()
      n <- length(vals)
      x_bar <- mean(vals)
      deviations <- vals - x_bar
      sq_deviations <- deviations^2
      variance <- sum(sq_deviations) / (n - 1)
      s <- sqrt(variance)
      tagList(
        withMathJax(helpText(
          "$$s = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2}$$"
        )),
        paste0("Suma kwadratów odchyleń = ", round(sum(sq_deviations), 2)),
        tags$br(),
        paste0("Wariancja \\(s^2\\) = suma / (n-1) = ",
               round(sum(sq_deviations), 2), " / ", n - 1, " = ",
               round(variance, 2)),
        tags$br(),
        paste0("Odchylenie standardowe \\(s = \\sqrt{",
               round(variance, 2), "} = ", round(s, 2), "\\) cm"),
        tags$br(),
        "Zacieniowany pas na wykresie oznacza przedział \\(\\bar{x} \\pm s\\).
        W rozkładzie normalnym ok. 68% danych leży w tym przedziale."
      )
    }
  })

  # --- Widget 2b: Empirical rule (68-95-99.7) ---

  zoom_plot_server("ch4_emp_plot", reactive({
    var_name <- input$ch4_emp_var
    req(var_name)
    vals <- student_data[[var_name]]
    m <- mean(vals)
    s <- sd(vals)

    band_alphas <- c(0.22, 0.14, 0.07)

    p <- ggplot(data.frame(x = vals), aes(x = x))

    # Pasy w tle (od najszerszego do najwęższego, żeby ±1 SD był na wierzchu)
    for (k in 3:1) {
      p <- p + annotate("rect",
        xmin = m - k * s, xmax = m + k * s,
        ymin = -Inf, ymax = Inf,
        fill = upwr_accent, alpha = band_alphas[k]
      )
    }

    # Histogram na wierzchu pasów
    p <- p +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 25, fill = upwr_cat["niebo"], color = "white", alpha = 0.85) +
      geom_vline(xintercept = m, color = upwr_secondary, linewidth = 1.2, linetype = "solid") +
      annotate("text", x = m, y = Inf, label = paste0("x̄ = ", round(m, 1)),
               vjust = -0.5, color = upwr_secondary, fontface = "bold", size = 4.5) +
      annotate("text",
               x = c(m - s, m + s, m - 2 * s, m + 2 * s, m - 3 * s, m + 3 * s),
               y = -Inf,
               label = c("−1 SD", "+1 SD", "−2 SD", "+2 SD", "−3 SD", "+3 SD"),
               vjust = -0.5, hjust = c(1.1, -0.1, 1.1, -0.1, 1.1, -0.1),
               size = 3.2, color = upwr_secondary, fontface = "italic") +
      labs(
        x = variable_meta[[var_name]]$label,
        y = "Gęstość"
      ) +
      theme()

    p
  }))

  output$ch4_emp_text <- renderUI({
    var_name <- input$ch4_emp_var
    req(var_name)
    vals <- student_data[[var_name]]
    m <- mean(vals)
    s <- sd(vals)

    pct_in <- sapply(1:3, function(k) {
      round(mean(vals >= m - k * s & vals <= m + k * s) * 100, 1)
    })

    diff_1sd <- abs(pct_in[1] - 68)

    if (diff_1sd < 5) {
      lc_feedback(type = "info",
        tags$strong("Dobra zgodność z regułą! "),
        paste0("W przedziale ±1 SD leży ", pct_in[1], "% danych (teoria: 68%). "),
        "To oznacza, że rozkład tej zmiennej jest zbliżony do normalnego. ",
        "Odchylenie standardowe dobrze podsumowuje rozrzut."
      )
    } else {
      lc_feedback(type = "warning",
        tags$strong("Słaba zgodność z regułą! "),
        paste0("W przedziale ±1 SD leży ", pct_in[1], "% danych (teoria: 68%). "),
        "Dlaczego? Reguła 68-95-99.7 zakłada rozkład symetryczny ",
        "(zbliżony do normalnego). Gdy rozkład jest skośny, dane koncentrują się ",
        "asymetrycznie wokół średniej -- więcej obserwacji leży po jednej stronie ",
        "niż po drugiej, co łamie założenie reguły. ",
        "W takim przypadku IQR lepiej opisuje rozrzut niż odchylenie standardowe."
      )
    }
  })

  # --- Widget 3: Boxplot builder ---

  # Krok widgetu (1..5) żyje w przeglądarce; nowe dane nie cofają kroku.
  ch4_bp_step <- lc_step_server("ch4_bp", input)$step
  ch4_bp_data <- reactiveVal(round(c(rnorm(27, 170, 8), 145, 198, 200), 1))

  observeEvent(input$ch4_bp_new, {
    set.seed(NULL)
    ch4_bp_data(round(c(rnorm(27, 170, 8), 145, 198, 200), 1))
  })

  zoom_plot_server("ch4_bp_plot", reactive({
    step <- ch4_bp_step()

    vals <- ch4_bp_data()
    sorted_vals <- sort(vals)
    med <- median(vals)
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr_val <- q3 - q1
    lower_fence <- q1 - 1.5 * iqr_val
    upper_fence <- q3 + 1.5 * iqr_val
    whisker_low <- min(vals[vals >= lower_fence])
    whisker_high <- max(vals[vals <= upper_fence])
    outliers <- vals[vals < lower_fence | vals > upper_fence]

    # Stała rama osi X z pełnych danych, wspólna dla kroków.
    x_lim <- range(vals) + c(-1, 1) * diff(range(vals)) * 0.06
    frame <- step_frame(xlim = x_lim, ylim = c(-0.9, 0.9), y_axis = FALSE)

    if (step == 5) {
      # Final: clean boxplot + histogram for comparison
      df <- data.frame(x = vals)
      p_box <- ggplot(df, aes(x = x, y = "")) +
        step_result(geom_boxplot, outlier.colour = STEP_ROLES$known$colour,
                    outlier.size = 3, width = 0.4) +
        step_layer(geom_jitter, "known", width = 0, height = 0.05, alpha = 0.4, size = 2) +
        labs(x = "", y = "") +
        step_frame(xlim = x_lim, ylim = c(0.5, 1.5), y_axis = FALSE)

      p_hist <- ggplot(df, aes(x = x)) +
        step_result(geom_histogram, bins = 15)
      count_max <- max(layer_data(p_hist)$count)
      p_hist <- p_hist +
        step_line("known", xintercept = med, helper = FALSE) +
        step_line("known", xintercept = q1) +
        step_line("known", xintercept = q3) +
        labs(x = "Wzrost (cm)", y = "Liczebność") +
        step_frame(xlim = x_lim, ylim = c(0, count_max * 1.08))

      return(gridExtra::arrangeGrob(p_box, p_hist, nrow = 2, heights = c(1, 1.2)))
    }

    # Steps 1-4: manual construction
    df <- data.frame(x = vals, y = 0)

    if (step == 1) {
      # Jittered raw data
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        step_layer(geom_point, "data", size = 3) +
        labs(x = "Wzrost (cm)", y = "") +
        frame

    } else if (step == 2) {
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        step_layer(geom_point, "data", size = 3) +
        step_line("new", xintercept = med, helper = FALSE) +
        step_label(med, 0.65, "Mediana", role = "new", hjust = 0.5, vjust = 0.5) +
        labs(x = "Wzrost (cm)", y = "") +
        frame

    } else if (step == 3) {
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        # IQR box
        step_result(geom_rect, data = data.frame(xmin = q1, xmax = q3),
                    mapping = aes(xmin = xmin, xmax = xmax, ymin = -0.5, ymax = 0.5),
                    inherit.aes = FALSE, alpha = 0.2) +
        step_layer(geom_point, "data", size = 3) +
        step_line("known", xintercept = med, helper = FALSE) +
        step_line("new", xintercept = q1) +
        step_line("new", xintercept = q3) +
        step_label(med, 0.7, "Mediana", role = "known", hjust = 0.5, vjust = 0.5) +
        step_label(q1, -0.65, "Q1", role = "new", hjust = 0.5, vjust = 0.5) +
        step_label(q3, -0.65, "Q3", role = "new", hjust = 0.5, vjust = 0.5) +
        step_label((q1 + q3) / 2, 0.7, "IQR", role = "new", vjust = 0.5,
                   hjust = ifelse(abs(med - (q1 + q3) / 2) < 3, 2, 0.5)) +
        labs(x = "Wzrost (cm)", y = "") +
        frame

    } else if (step == 4) {
      is_outlier <- vals < lower_fence | vals > upper_fence
      df$outlier <- is_outlier
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.2, 0.2)

      whiskers <- data.frame(
        x    = c(whisker_low, whisker_low, q3, whisker_high),
        xend = c(q1, whisker_low, whisker_high, whisker_high),
        y    = c(0, -0.2, 0, -0.2),
        yend = c(0, 0.2, 0, 0.2)
      )

      p <- ggplot(df) +
        # IQR box
        step_result(geom_rect, data = data.frame(xmin = q1, xmax = q3),
                    mapping = aes(xmin = xmin, xmax = xmax, ymin = -0.4, ymax = 0.4),
                    alpha = 0.2) +
        # Median line inside box
        step_layer(geom_segment, "known",
                   data = data.frame(x = med, xend = med, y = -0.4, yend = 0.4),
                   mapping = aes(x = x, xend = xend, y = y, yend = yend),
                   linewidth = 1.2) +
        # Whiskers
        step_layer(geom_segment, "new", data = whiskers,
                   mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
        # Points: normal
        step_layer(geom_point, "data", data = df[!df$outlier, ],
                   mapping = aes(x = x, y = y_jit), size = 2.5, alpha = 0.5) +
        # Points: outliers
        step_layer(geom_point, "new", data = df[df$outlier, ],
                   mapping = aes(x = x, y = y_jit), size = 4, shape = 18) +
        labs(x = "Wzrost (cm)", y = "") +
        frame

      if (length(outliers) > 0) {
        p <- p +
          step_label(mean(outliers), 0.55, paste0(length(outliers), " outlier(s)"),
                     role = "new", hjust = 0.5, vjust = 0.5)
      }

      p
    }
  }))

  output$ch4_bp_text <- renderUI({
    step <- ch4_bp_step()
    if (step == 1) {
      "Zaczynamy od surowych danych. 30 pomiarów wzrostu rozrzuconych
       na osi liczbowej. Widać ogolny zakres, ale ciężko wyciagnac
       szybkie wnioski."
    } else if (step == 2) {
      vals <- ch4_bp_data()
      paste0("Sortujemy dane i wyznaczamy mediane = ", round(median(vals), 1),
             " cm. Mediana dzieli posortowane dane na dwie rowne polowy.")
    } else if (step == 3) {
      vals <- ch4_bp_data()
      q1 <- quantile(vals, 0.25)
      q3 <- quantile(vals, 0.75)
      paste0("Wyznaczamy kwartyle: Q1 = ", round(q1, 1),
             " (25% danych poniżej), Q3 = ", round(q3, 1),
             " (75% danych poniżej). Pudelko (box) rozciaga sie od Q1 do Q3
             i zawiera srodkowe 50% danych. IQR = Q3 - Q1 = ",
             round(q3 - q1, 1), " cm.")
    } else if (step == 4) {
      vals <- ch4_bp_data()
      q1 <- quantile(vals, 0.25)
      q3 <- quantile(vals, 0.75)
      iqr_val <- q3 - q1
      lower_fence <- q1 - 1.5 * iqr_val
      upper_fence <- q3 + 1.5 * iqr_val
      outliers <- vals[vals < lower_fence | vals > upper_fence]
      paste0("Wąsy siagaja do najdalszych punktow w granicach
             1.5 * IQR od pudełka — czyli od Q1 − 1.5·IQR = ",
             round(lower_fence, 1), " cm do Q3 + 1.5·IQR = ",
             round(upper_fence, 1),
             " cm. Wszystko poza wąsami to wartości odstające (outliers). ",
             if (length(outliers) > 0) {
               paste0("Znaleziono ", length(outliers),
                      " wartosc(i) odstająca(e): ",
                      paste(round(outliers, 1), collapse = ", "), " cm.")
             } else {
               "Brak wartości odstających."
             })
    } else if (step == 5) {
      "Gotowy boxplot (gora) w porownaniu z histogramem (dol).
       Boxplot kompaktowo podsumowuje rozkład: mediana, kwartyle,
       rozstęp i outlierow - wszystko w jednym wykresie. Histogram
       pokazuje więcej szczegółów o kształcie rozkładu."
    }
  })

  # --- Widget 3b: Group comparison ---

  zoom_plot_server("ch4_grp_plot", reactive({
    var_name <- input$ch4_grp_var
    grp_name <- input$ch4_grp_by
    req(var_name, grp_name)

    df <- data.frame(
      value = student_data[[var_name]],
      group = student_data[[grp_name]]
    )

    var_label <- names(which(c("wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
      "czas_dojazdu" = "Czas dojazdu (min)", "srednia_ocen" = "Średnia ocen") == var_name))
    if (length(var_label) == 0) var_label <- var_name

    grp_label <- ifelse(grp_name == "plec", "Płeć", "Kierunek")

    p <- ggplot(df, aes(x = group, y = value, fill = group))

    if (isTRUE(input$ch4_grp_violin)) {
      p <- p + geom_violin(alpha = 0.4, color = NA) +
        geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = NA)
    } else {
      p <- p + geom_boxplot(alpha = 0.7, outlier.color = upwr_accent, outlier.size = 3)
    }

    if (isTRUE(input$ch4_grp_points)) {
      p <- p + geom_jitter(width = 0.15, alpha = 0.3, size = 1.5)
    }

    p + scale_fill_upwr() +
      labs(x = grp_label, y = var_label) +
            theme(legend.position = "none")
  }))

  output$ch4_grp_table <- renderUI({
    var_name <- input$ch4_grp_var
    grp_name <- input$ch4_grp_by
    req(var_name, grp_name)

    df <- data.frame(
      value = student_data[[var_name]],
      group = student_data[[grp_name]]
    )

    stats <- df %>%
      group_by(group) %>%
      summarise(
        n = n(),
        mean = round(mean(value), 2),
        median = round(median(value), 2),
        sd = round(sd(value), 2),
        iqr = round(IQR(value), 2),
        .groups = "drop"
      ) %>%
      mutate(group = as.character(group))

    lc_table_split(stats,
      cols = list(
        lc_col("group", "Grupa", "row"),
        lc_col("n", "n"),
        lc_col("mean", "Średnia", digits = 2),
        lc_col("median", "Mediana", digits = 2),
        lc_col("sd", "SD", digits = 2),
        lc_col("iqr", "IQR", digits = 2)
      ),
      groups = list(c("n", "mean", "median"), c("sd", "iqr")),
      label = "Statystyki opisowe w grupach"
    )
  })

  # --- Widget 4: Spread measures comparison ---

  ch4_comp_data <- reactiveVal(NULL)

  observe({
    if (is.null(ch4_comp_data())) {
      ch4_comp_data(student_data$wzrost)
    }
  })

  observeEvent(input$ch4_comp_add1, {
    set.seed(NULL)
    current <- ch4_comp_data()
    outlier <- max(current) + 30 + runif(1, -5, 5)
    ch4_comp_data(c(current, round(outlier, 1)))
  })

  observeEvent(input$ch4_comp_add5, {
    set.seed(NULL)
    current <- ch4_comp_data()
    outliers <- sapply(1:5, function(i) max(current) + 30 + runif(1, -5, 5))
    ch4_comp_data(c(current, round(outliers, 1)))
  })

  observeEvent(input$ch4_comp_reset, {
    ch4_comp_data(student_data$wzrost)
  })

  zoom_plot_server("ch4_comp_plot", reactive({
    vals <- ch4_comp_data()
    if (is.null(vals)) return(NULL)

    df <- data.frame(x = vals)
    data_range <- range(vals)
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr_val <- q3 - q1

    ggplot(df, aes(x = x)) +
      geom_histogram(bins = 30, fill = upwr_cat["niebo"], color = "white", alpha = 0.7) +
      # Range
      annotate("segment", x = data_range[1], xend = data_range[2],
               y = -2, yend = -2, color = upwr_accent, linewidth = 2) +
      annotate("text",
               x = (data_range[1] + data_range[2]) / 2, y = -3.5,
               label = paste0("Rozstęp = ", round(diff(data_range), 1)),
               color = upwr_accent, size = 4, fontface = "bold") +
      # IQR
      annotate("segment", x = q1, xend = q3, y = -6, yend = -6,
               color = upwr_cat["szalwia"], linewidth = 2) +
      annotate("text", x = (q1 + q3) / 2, y = -7.5,
               label = paste0("IQR = ", round(iqr_val, 1)),
               color = upwr_cat["szalwia"], size = 4, fontface = "bold") +
      labs(x = "Wzrost (cm)", y = "Liczebność",
           title = paste0("Histogram wzrostu (n = ", length(vals), ")")) +
            coord_cartesian(clip = "off") +
      theme(plot.margin = margin(10, 10, 50, 10))
  }))

  output$ch4_comp_table <- renderUI({
    vals <- ch4_comp_data()
    if (is.null(vals)) return(NULL)

    df <- data.frame(
      measure = c("Rozstep", "IQR (rozstęp międzykwartylowy)",
                  "Odchylenie standardowe (SD)",
                  "Współczynnik zmienności (CV)"),
      value = c(
        paste0(lc_num(diff(range(vals)), 1), " cm"),
        paste0(lc_num(IQR(vals), 1), " cm"),
        paste0(lc_num(sd(vals), 2), " cm"),
        paste0(lc_num(sd(vals) / mean(vals) * 100, 1), "%")
      ),
      notes = c(
        "Bardzo wrażliwy na outlierow - zależy tylko od min i max",
        "Odporny na outlierow - oparty na kwartylach",
        "Umiarkowanie wrażliwy - bierze pod uwage wszystkie dane",
        "Bezjednostkowy - pozwala porownywac zmiennosc roznych zmiennych"
      ),
      stringsAsFactors = FALSE
    )

    lc_table(df,
      cols = list(
        lc_col("measure", "Miara", "row"),
        lc_col("value", "Wartość", "num"),
        lc_col("notes", "Wlasnosci", "text")
      ),
      narrow = "stack-last"
    )
  })

  # --- Widget 5: Coefficient of Variation ---

  zoom_plot_server("ch4_sd_compare_plot", reactive({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen")

    stats <- data.frame(
      Zmienna = factor(labels, levels = rev(labels)),
      SD = sapply(vars, function(v) sd(student_data[[v]]))
    )

    ggplot(stats, aes(x = Zmienna, y = SD, fill = SD)) +
      geom_col(alpha = 0.85, width = 0.6) +
      scale_fill_upwr_seq(variant = "burgundy", guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      coord_flip() +
      labs(x = NULL, y = "Odchylenie standardowe (oryg. jednostki)") +
      theme()
  }))

  zoom_plot_server("ch4_cv_plot", reactive({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen")

    stats <- data.frame(
      Zmienna = factor(labels, levels = rev(labels)),
      CV = sapply(vars, function(v) sd(student_data[[v]]) / mean(student_data[[v]]) * 100)
    )

    ggplot(stats, aes(x = Zmienna, y = CV, fill = CV)) +
      geom_col(alpha = 0.85, width = 0.6) +
      scale_fill_upwr_seq(variant = "burgundy", guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      coord_flip() +
      labs(x = NULL, y = "Współczynnik zmienności (%)") +
      theme()
  }))

  output$ch4_cv_table <- renderTable({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen")

    data.frame(
      Zmienna = labels,
      Średnia = sapply(vars, function(v) round(mean(student_data[[v]]), 2)),
      SD = sapply(vars, function(v) round(sd(student_data[[v]]), 2)),
      `CV (%)` = sapply(vars, function(v) round(sd(student_data[[v]]) / mean(student_data[[v]]) * 100, 1)),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

}
