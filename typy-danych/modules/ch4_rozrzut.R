# ============================================================================
# CHAPTER 4: Statystyki rozrzutu
# ============================================================================

ch4_ui <- tabPanel("4. Statystyki rozrzutu",
  fluidRow(column(8, offset = 2,

    # --- Introduction ---
    div(class = "chapter-recap",
      "Średnia, mediana, percentyle -- wiemy jak znalezc 'środek'. Ale środek to nie wszystko.
       Pora zmierzyc, jak bardzo dane są rozproszone wokol tego srodka."
    ),
    uiOutput("tracker_ch4"),
    div(class = "section-title", "Statystyki rozrzutu"),

    div(class = "narrative",
      p("Średnia mówi gdzie jest środek, ale nic o tym jak bardzo dane sa
        rozproszone. Dwie grupy mogą mieć te sama średnia, a wyglądać
        zupełnie inaczej."),
      p("W tym rozdziale poznamy miary rozrzutu: odchylenie standardowe,
        wariancje, rozstęp, rozstęp międzykwartylowy (IQR) oraz
        współczynnik zmienności. Nauczymy sie tez budowac boxplot od podstaw.")
    ),

    # ====================================================================
    # WIDGET 1: Bus scenario - "Mean is not everything"
    # ====================================================================
    div(class = "section-title", "Średnia to nie wszystko"),

    div(class = "narrative",
      p("Wyobraź sobie dwie linie autobusowe. Obie mają takie samo
        średnie spóźnienie -- około 2 minuty. Którą wybierzesz?"),
      p("Większość autobusów jest blisko rozkładu (0-4 min spóźnienia),
        rzadko który przyjeżdża za wcześnie, a od czasu do czasu
        zdarza się duże spóźnienie. Ale rozrzut tych spóźnień
        może być bardzo różny.")
    ),

    div(class = "widget-block",
      div(class = "step-buttons",
        actionButton("ch4_spread_s1", "1. Dwie linie",
                     class = "btn-outline-primary"),
        actionButton("ch4_spread_s2", "2. Ta sama średnia, ale...",
                     class = "btn-outline-primary"),
        actionButton("ch4_spread_s3", "3. Wychodzisz wcześniej",
                     class = "btn-outline-primary"),
        actionButton("ch4_spread_s4", "4. Konsekwencje",
                     class = "btn-outline-primary")
      ),
      sliderInput("ch4_spread_buffer", "Wychodzisz wcześniej o (minuty):",
                  min = 0, max = 10, value = 0, step = 1, width = "100%"),
      plotOutput("ch4_spread_plot", height = "450px"),
      uiOutput("ch4_spread_text")
    ),

    # ====================================================================
    # WIDGET 2: SD step-by-step
    # ====================================================================
    div(class = "section-title", "Odchylenie standardowe krok po kroku"),

    div(class = "narrative",
      p("Jak obliczamy odchylenie standardowe? Krok po kroku.
        Zobaczmy to na przykładzie 10 pomiarów wzrostu.")
    ),

    div(class = "widget-block",
      div(class = "step-buttons",
        actionButton("ch4_sd_s1", "1. Dane",
                     class = "btn-outline-primary"),
        actionButton("ch4_sd_s2", "2. Odchylenia od średniej",
                     class = "btn-outline-primary"),
        actionButton("ch4_sd_s3", "3. Wariancja i SD",
                     class = "btn-outline-primary")
      ),
      div(style = "margin-bottom: 10px;",
        actionButton("ch4_sd_new", "Losuj nowy zestaw",
                     class = "btn-success btn-sm", style = "margin-right: 6px;"),
        actionButton("ch4_sd_reset", "Reset",
                     class = "btn-secondary btn-sm")
      ),
      plotOutput("ch4_sd_plot", height = "400px"),
      tableOutput("ch4_sd_table"),
      uiOutput("ch4_sd_text")
    ),

    # ====================================================================
    # WIDGET 2b: Empirical rule (68-95-99.7)
    # ====================================================================
    div(class = "section-title", "Regula empiryczna (68-95-99.7)"),

    div(class = "narrative",
      p("Wiemy juz jak obliczyć odchylenie standardowe. Ale co ono oznacza
        w praktyce? Dla rozkładow zbliżonych do normalnego obowiązuje
        regula empiryczna: okolo 68% danych miesci sie w zakresie
        srednia ±1 SD, 95% w ±2 SD, a 99.7% w ±3 SD.")
    ),

    div(class = "widget-block",
      h4("Regula 68-95-99.7 -- czy zawsze dziala?"),
      selectInput("ch4_emp_var", "Wybierz zmienna:",
        choices = c("Wzrost (cm)" = "wzrost",
                    "Waga (kg)" = "waga",
                    "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost"
      ),
      plotOutput("ch4_emp_plot", height = "400px"),
      uiOutput("ch4_emp_text")
    ),

    # ====================================================================
    # WIDGET 3: Boxplot builder
    # ====================================================================
    div(class = "section-title", "Budujemy boxplot od podstaw"),

    div(class = "narrative",
      p("Boxplot to wizualne podsumowanie rozkładu oparte na kwartylach.
        Zbudujmy go od podstaw, krok po kroku, aby zrozumieć co
        oznacza każdy element tego wykresu.")
    ),

    div(class = "widget-block",
      div(class = "step-buttons",
        actionButton("ch4_bp_s1", "1. Surowe dane",
                     class = "btn-outline-primary"),
        actionButton("ch4_bp_s2", "2. Mediana",
                     class = "btn-outline-primary"),
        actionButton("ch4_bp_s3", "3. Kwartyle i pudełko",
                     class = "btn-outline-primary"),
        actionButton("ch4_bp_s4", "4. Wąsy i outliers",
                     class = "btn-outline-primary"),
        actionButton("ch4_bp_s5", "5. Gotowy boxplot",
                     class = "btn-outline-primary")
      ),
      div(style = "margin-bottom: 10px;",
        actionButton("ch4_bp_new", "Losuj nowe dane",
                     class = "btn-success btn-sm", style = "margin-right: 6px;"),
        actionButton("ch4_bp_reset", "Reset",
                     class = "btn-secondary btn-sm")
      ),
      plotOutput("ch4_bp_plot", height = "350px"),
      uiOutput("ch4_bp_text")
    ),

    # ====================================================================
    # WIDGET 3b: Group comparison -- side-by-side boxplots
    # ====================================================================
    div(class = "section-title", "Porównanie grup"),

    div(class = "narrative",
      p("Dotychczas analizowalismy caly zbior danych naraz. Ale jednym z
        najczestszych pytan w statystyce jest: czy grupy sie roznia?
        Boxploty obok siebie to doskonałe narzędzie do porównywania rozkładow
        miedzy grupami.")
    ),

    div(class = "widget-block",
      h4("Boxploty grupowane"),
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
      plotOutput("ch4_grp_plot", height = "400px"),
      tableOutput("ch4_grp_table")
    ),

    # ====================================================================
    # WIDGET 4: Spread measures comparison
    # ====================================================================
    div(class = "section-title", "Porównanie miar rozrzutu"),

    div(class = "narrative",
      p("Porównajmy rozne miary rozrzutu i ich odporność na wartości
        odstające. Dodaj outliera i obserwuj, ktore miary sie zmieniaja,
        a ktore pozostaja stabilne.")
    ),

    div(class = "widget-block",
      div(style = "margin-bottom: 10px;",
        actionButton("ch4_comp_add1", "Dodaj outlier (+30 cm)",
                     class = "btn-warning", style = "margin-right: 6px;"),
        actionButton("ch4_comp_add5", "Dodaj 5 outlierow",
                     class = "btn-danger", style = "margin-right: 6px;"),
        actionButton("ch4_comp_reset", "Reset",
                     class = "btn-secondary")
      ),
      plotOutput("ch4_comp_plot", height = "350px"),
      tableOutput("ch4_comp_table")
    ),

    div(class = "callout-info",
      tags$strong("Wniosek:"),
      " Rozstęp jest bardzo wrażliwy na outlierow - wystarczy jedna wartosc
      odstająca, aby go zmienić. IQR i odchylenie standardowe są bardziej
      odporne, a IQR jest z nich najbardziej stabilne."
    ),

    # ====================================================================
    # WIDGET 5: Coefficient of Variation
    # ====================================================================
    div(class = "section-title", "Współczynnik zmienności (CV)"),

    div(class = "narrative",
      p("Odchylenie standardowe mówi o rozrzucie, ale w jakich jednostkach?
        SD wzrostu (w cm) i SD wagi (w kg) nie są porownywalne!
        Aby porownac zmiennosc zmiennych w roznych skalach, uzywamy
        współczynnika zmienności (CV = SD / średnia × 100%).")
    ),

    div(class = "widget-block",
      h4("Porównanie zmienności miedzy zmiennymi"),
      fluidRow(
        column(6, plotOutput("ch4_sd_compare_plot", height = "350px")),
        column(6, plotOutput("ch4_cv_plot", height = "350px"))
      ),
      tableOutput("ch4_cv_table"),
      div(class = "callout-info",
        tags$strong("Interpretacja: "),
        "Lewy wykres pokazuje SD w oryginalnych jednostkach -- wartości są nieporównywalne,
         bo każda zmienna ma inna skalę. Prawy wykres pokazuje CV (%), które normalizuje
         rozrzut wzgledem średniej -- teraz widać, że czas dojazdu
        ma największa względna zmienność, choć jego SD nie jest największe."
      )
    ),

    div(class = "chapter-transition",
      p("Położenie i rozrzut to nie wszystko. Dwa rozkłady z ta sama średnia i
        odchyleniem standardowym mogą mieć zupełnie inny kształt -- asymetrię
        i różna 'ciężkość' ogonów."),
      actionButton("ch4_next", "Dalej: 5. Kształt rozkładu →",
                   class = "btn-primary btn-lg")
    ),

    # Bottom spacing
    div(style = "height: 40px;")

  ))
) # end ch4 tabPanel

# --------------------------------------------------------------------------
# Chapter 4 Server
# --------------------------------------------------------------------------

ch4_server <- function(input, output, session) {

  # --- Widget 1: Bus scenario ---

  ch4_spread_step <- reactiveVal(0)

  observeEvent(input$ch4_spread_s1, { ch4_spread_step(1) })
  observeEvent(input$ch4_spread_s2, { ch4_spread_step(2) })
  observeEvent(input$ch4_spread_s3, { ch4_spread_step(3) })
  observeEvent(input$ch4_spread_s4, { ch4_spread_step(4) })

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

  output$ch4_spread_plot <- renderPlot({
    step <- ch4_spread_step()
    if (step == 0) return(NULL)

    buffer <- input$ch4_spread_buffer
    bus <- ch4_bus_data()

    dens_a <- density(bus$a, from = -3, to = 30, n = 500)
    dens_b <- density(bus$b, from = -3, to = 30, n = 500)
    df_a <- data.frame(x = dens_a$x, y = dens_a$y,
                       linia = paste0("Linia A (SD = ", bus$sd_a, ")"))
    df_b <- data.frame(x = dens_b$x, y = dens_b$y,
                       linia = paste0("Linia B (SD = ", bus$sd_b, ")"))
    df_all <- rbind(df_a, df_b)

    col_a <- "#3498db"; col_b <- "#e74c3c"
    lbl_a <- paste0("Linia A (SD = ", bus$sd_a, ")")
    lbl_b <- paste0("Linia B (SD = ", bus$sd_b, ")")

    p <- ggplot(df_all, aes(x = x, y = y, color = linia, fill = linia)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = setNames(c(col_a, col_b), c(lbl_a, lbl_b))) +
      scale_fill_manual(values = setNames(c(col_a, col_b), c(lbl_a, lbl_b))) +
      geom_vline(xintercept = 2, linetype = "dashed", color = "#2c3e50",
                 linewidth = 0.8) +
      annotate("text", x = 2, y = max(df_a$y) * 1.08,
               label = "Średnie spóźnienie = 2 min",
               hjust = 0.5, size = 4.5, color = "#2c3e50", fontface = "bold") +
      geom_vline(xintercept = 0, linetype = "solid", color = "#95a5a6",
                 linewidth = 0.5, alpha = 0.5) +
      annotate("text", x = -0.3, y = max(df_a$y) * 0.3,
               label = "punktualny", angle = 90,
               size = 3.5, color = "#95a5a6") +
      labs(x = "Spóźnienie (minuty)    ← za wcześnie | za późno →",
           y = "Gęstość",
           title = "Rozkład spóźnień dwóch linii autobusowych",
           color = NULL, fill = NULL) +
      coord_cartesian(xlim = c(-3, 25)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")

    if (step >= 2) {
      p <- p +
        annotate("text", x = -1.5, y = max(df_a$y) * 0.85,
                 label = paste0("SD = ", bus$sd_a, " min\n(niezawodna)"),
                 size = 4, color = col_a, fontface = "bold") +
        annotate("text", x = 15, y = max(df_b$y) * 1.8,
                 label = paste0("SD = ", bus$sd_b, " min\n(nieprzewidywalna)"),
                 size = 4, color = col_b, fontface = "bold")
    }

    if (step >= 3) {
      cutoff <- -buffer  # wychodzisz buffer min wczesniej = jestes na -buffer
      # Zdazysz na autobus jesli delay >= -buffer (jeszcze nie odjechał)
      shade_a <- df_a[df_a$x >= cutoff, ]
      shade_b <- df_b[df_b$x >= cutoff, ]

      lbl <- if (buffer == 0) "Wychodzisz\nna stówkę"
             else paste0("Wychodzisz\n", buffer, " min wcześniej")

      p <- p +
        geom_area(data = shade_a, aes(x = x, y = y), alpha = 0.25) +
        geom_area(data = shade_b, aes(x = x, y = y), alpha = 0.15) +
        geom_vline(xintercept = cutoff, linetype = "dotted",
                   color = "#27ae60", linewidth = 1) +
        annotate("text", x = cutoff, y = max(df_a$y) * 0.95,
                 label = lbl,
                 hjust = 1.1, size = 3.8, color = "#27ae60", fontface = "bold")
    }

    if (step >= 4) {
      prob_a <- mean(bus$a >= -buffer)
      prob_b <- mean(bus$b >= -buffer)

      p <- p +
        annotate("label", x = 18, y = max(df_a$y) * 0.85,
                 label = paste0("P(zdążysz) A = ",
                                round(prob_a * 100, 1), "%"),
                 size = 4.5, fill = "#eaf4fc", color = col_a,
                 fontface = "bold", label.size = 0.5) +
        annotate("label", x = 18, y = max(df_a$y) * 0.70,
                 label = paste0("P(zdążysz) B = ",
                                round(prob_b * 100, 1), "%"),
                 size = 4.5, fill = "#fdedec", color = col_b,
                 fontface = "bold", label.size = 0.5)
    }

    p
  })

  output$ch4_spread_text <- renderUI({
    step <- ch4_spread_step()
    buffer <- input$ch4_spread_buffer
    bus <- ch4_bus_data()

    if (step == 0) {
      div(class = "callout-info",
          "Kliknij przycisk kroku, aby rozpocząć.")
    } else if (step == 1) {
      div(class = "callout-info",
          tags$strong("Krok 1:"),
          " Obie linie mają średnie spóźnienie około 2 minut.
          Patrząc tylko na średnią, są identyczne.
          Wartości ujemne = przyjazd przed czasem (rzadko się zdarza).")
    } else if (step == 2) {
      pct_10_a <- round(mean(bus$a > 10) * 100, 1)
      pct_10_b <- round(mean(bus$b > 10) * 100, 1)
      mean_late_a <- if (any(bus$a > 10)) round(mean(bus$a[bus$a > 10]), 1) else 0
      mean_late_b <- if (any(bus$b > 10)) round(mean(bus$b[bus$b > 10]), 1) else 0
      div(class = "callout-info",
          tags$strong("Krok 2:"),
          paste0(" Linia A ma SD = ", bus$sd_a, " min (spóźnienia skupione 0-4 min),
          a linia B ma SD = ", bus$sd_b, " min (zdarza się i punktualnie,
          i 10+ min spóźnienia)."),
          tags$br(), tags$br(),
          tags$strong("Spóźnienia >10 min: "),
          paste0("Linia A: ", pct_10_a, "% kursów",
                 if (pct_10_a > 0) paste0(" (śr. ", mean_late_a, " min)") else "",
                 "; Linia B: ", pct_10_b, "% kursów",
                 if (pct_10_b > 0) paste0(" (śr. ", mean_late_b, " min)") else "",
                 "."))
    } else if (step == 3) {
      lbl <- if (buffer == 0) "na stówkę (0 min zapasu)"
             else paste0(buffer, " min wcześniej")
      div(class = "callout-info",
          tags$strong("Krok 3:"),
          paste0(" Wychodzisz ", lbl,
                 ". Jesteś na przystanku o ", buffer,
                 " min przed rozkładem. Zdążysz na każdy autobus,
                 który nie odjedzie wcześniej niż ", buffer,
                 " min przed rozkładem. Zacieniowany obszar = kursy,
                 na które zdążysz. Przesuń suwak!"))
    } else if (step == 4) {
      prob_a <- mean(bus$a >= -buffer)
      prob_b <- mean(bus$b >= -buffer)
      pct_10_a <- round(mean(bus$a > 10) * 100, 1)
      pct_10_b <- round(mean(bus$b > 10) * 100, 1)
      mean_late_b <- if (any(bus$b > 10)) round(mean(bus$b[bus$b > 10]), 1) else 0
      lbl <- if (buffer == 0) "na stówkę" else paste0(buffer, " min wcześniej")
      div(class = "callout-info",
          tags$strong("Krok 4:"), " Konsekwencje",
          tags$br(),
          paste0("Wychodzisz ", lbl, ":"),
          tags$br(),
          paste0("Linia A: zdążysz na ", round(prob_a * 100, 1),
                             "% kursów."),
          tags$br(),
          paste0("Linia B: zdążysz na ", round(prob_b * 100, 1),
                             "% kursów."),
          tags$br(), tags$br(),
          if (pct_10_b > 0) tagList(
            tags$em(paste0("A gdy linia B się spóźni poważnie (>10 min, ",
                           pct_10_b, "% kursów), średnie czekasz ",
                           mean_late_b, " min. ",
                           "Linia A praktycznie nigdy tak się nie spóźnia.")),
            tags$br(), tags$br()
          ),
          "To dlatego sama średnia nie wystarczy -- rozrzut danych
          ma realne konsekwencje!")
    }
  })

  # --- Widget 2: SD step-by-step ---

  ch4_sd_step <- reactiveVal(0)
  ch4_sd_data <- reactiveVal(round(rnorm(10, mean = 170, sd = 8), 1))

  observeEvent(input$ch4_sd_s1, { ch4_sd_step(1) })
  observeEvent(input$ch4_sd_s2, { ch4_sd_step(2) })
  observeEvent(input$ch4_sd_s3, { ch4_sd_step(3) })

  observeEvent(input$ch4_sd_new, {
    set.seed(NULL)
    ch4_sd_data(round(rnorm(10, mean = 170, sd = 8), 1))
    ch4_sd_step(0)
  })

  observeEvent(input$ch4_sd_reset, {
    ch4_sd_step(0)
  })

  output$ch4_sd_plot <- renderPlot({
    step <- ch4_sd_step()
    if (step == 0) return(NULL)

    vals <- ch4_sd_data()
    n <- length(vals)
    x_bar <- mean(vals)
    s <- sd(vals)

    if (step == 1) {
      # Krok 1: punkty na osi liczbowej
      df <- data.frame(x = vals)
      p <- ggplot(df, aes(x = x, y = 0)) +
        geom_point(size = 4, color = "#3498db") +
        labs(x = "Wzrost (cm)", y = "",
             title = "Pomiary wzrostu (n = 10)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(-0.3, 0.3))

    } else {
      # Kroki 2-3: punkty jedna pod drugą, posortowane wg odległości od średniej
      deviations <- vals - x_bar
      ord <- order(abs(deviations), decreasing = TRUE)
      df <- data.frame(
        x = vals[ord],
        dev = deviations[ord],
        y = seq(n, 1)  # najdalszy na górze
      )

      p <- ggplot(df, aes(x = x, y = y)) +
        geom_vline(xintercept = x_bar, linetype = "dashed", color = "#e74c3c",
                   linewidth = 1) +
        geom_segment(aes(x = x_bar, xend = x, y = y, yend = y),
                     color = "#f39c12", linewidth = 0.8,
                     arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
        geom_point(size = 4, color = "#3498db") +
        geom_text(aes(label = paste0(ifelse(dev > 0, "+", ""),
                                     round(dev, 1))),
                  hjust = ifelse(df$dev >= 0, -0.3, 1.3),
                  size = 3.5, color = "#7f8c8d") +
        annotate("text", x = x_bar, y = n + 0.8,
                 label = paste0("średnia = ", round(x_bar, 2)),
                 color = "#e74c3c", size = 5, fontface = "bold") +
        labs(x = "Wzrost (cm)", y = "",
             title = "Odchylenia od średniej (posortowane wg odległości)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(0, n + 1.2))

      if (step >= 3) {
        p <- p +
          annotate("rect", xmin = x_bar - s, xmax = x_bar + s,
                   ymin = 0, ymax = n + 0.3, fill = "#27ae60", alpha = 0.08) +
          geom_vline(xintercept = x_bar - s, linetype = "dotted",
                     color = "#27ae60", linewidth = 0.8) +
          geom_vline(xintercept = x_bar + s, linetype = "dotted",
                     color = "#27ae60", linewidth = 0.8) +
          annotate("text", x = x_bar - s, y = 0.3,
                   label = paste0("śr. - SD\n", round(x_bar - s, 1)),
                   color = "#27ae60", size = 3.5, fontface = "bold", vjust = 0) +
          annotate("text", x = x_bar + s, y = 0.3,
                   label = paste0("śr. + SD\n", round(x_bar + s, 1)),
                   color = "#27ae60", size = 3.5, fontface = "bold", vjust = 0) +
          annotate("text", x = x_bar, y = 0.5,
                   label = paste0("SD = ", round(s, 2), " cm"),
                   color = "#27ae60", size = 4.5, fontface = "bold")
      }
    }

    p
  })

  output$ch4_sd_table <- renderTable({
    step <- ch4_sd_step()
    if (step < 2) return(NULL)

    vals <- ch4_sd_data()
    n <- length(vals)
    x_bar <- mean(vals)

    deviations <- vals - x_bar
    sq_deviations <- deviations^2

    df <- data.frame(
      i = 1:n,
      `xi` = vals,
      `xi - x_bar` = round(deviations, 2),
      `(xi - x_bar)^2` = round(sq_deviations, 2),
      check.names = FALSE
    )

    if (step >= 3) {
      variance <- sum(sq_deviations) / (n - 1)
      s <- sqrt(variance)
      summary_row <- data.frame(
        i = NA,
        `xi` = NA,
        `xi - x_bar` = NA,
        `(xi - x_bar)^2` = round(sum(sq_deviations), 2),
        check.names = FALSE
      )
      # Mark the summary row
      summary_row$i <- "SUMA"
      summary_row$`xi` <- ""
      summary_row$`xi - x_bar` <- ""
      df$i <- as.character(df$i)
      df$`xi` <- as.character(df$`xi`)
      df$`xi - x_bar` <- as.character(round(deviations, 2))
      summary_row$`(xi - x_bar)^2` <- as.character(round(sum(sq_deviations), 2))
      df$`(xi - x_bar)^2` <- as.character(round(sq_deviations, 2))
      df <- rbind(df, summary_row)
    }

    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%",
     align = "cccc")

  output$ch4_sd_text <- renderUI({
    step <- ch4_sd_step()

    if (step == 0) {
      div(class = "callout-info",
          "Kliknij przycisk kroku, aby rozpoczac obliczanie odchylenia standardowego.")
    } else if (step == 1) {
      div(class = "callout-info",
          tags$strong("Krok 1:"),
          " Mamy 10 pomiarów wzrostu. Na osi liczbowej każdy punkt to jedna
          obserwacja. Jak bardzo są rozproszone?")
    } else if (step == 2) {
      vals <- ch4_sd_data()
      x_bar <- mean(vals)
      div(class = "callout-info",
          tags$strong("Krok 2:"),
          paste0(" Obliczamy srednia: x̄ = ", round(x_bar, 2),
                 " cm. Nastepnie liczymy odchylenie każdego punktu od średniej
                 (strzalki na wykresie). W tabeli widzisz odchylenia i ich kwadraty.
                 Kwadraty gwarantuja, ze odchylenia dodatnie i ujemne sie nie
                 znosa."))
    } else if (step == 3) {
      vals <- ch4_sd_data()
      n <- length(vals)
      x_bar <- mean(vals)
      deviations <- vals - x_bar
      sq_deviations <- deviations^2
      variance <- sum(sq_deviations) / (n - 1)
      s <- sqrt(variance)
      div(class = "callout-info",
          tags$strong("Krok 3:"),
          tags$br(), tags$br(),
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
          tags$br(), tags$br(),
          "Zielony pas na wykresie oznacza przedział \\(\\bar{x} \\pm s\\).
          W rozkładzie normalnym ok. 68% danych leży w tym przedziale.")
    }
  })

  # --- Widget 2b: Empirical rule (68-95-99.7) ---

  output$ch4_emp_plot <- renderPlot({
    var_name <- input$ch4_emp_var
    req(var_name)
    vals <- student_data[[var_name]]
    m <- mean(vals)
    s <- sd(vals)

    band_colors <- c("#3498db", "#f39c12", "#e74c3c")
    band_alphas <- c(0.25, 0.15, 0.10)
    band_labels <- c("±1 SD", "±2 SD", "±3 SD")

    pct_in <- sapply(1:3, function(k) {
      round(mean(vals >= m - k * s & vals <= m + k * s) * 100, 1)
    })

    p <- ggplot(data.frame(x = vals), aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 25, fill = "grey70", color = "white", alpha = 0.7)

    for (k in 3:1) {
      p <- p + annotate("rect",
        xmin = m - k * s, xmax = m + k * s,
        ymin = -Inf, ymax = Inf,
        fill = band_colors[k], alpha = band_alphas[k]
      )
    }

    p <- p +
      geom_vline(xintercept = m, color = "#e74c3c", linewidth = 1.2, linetype = "solid") +
      annotate("text", x = m, y = Inf, label = paste0("x̄ = ", round(m, 1)),
               vjust = -0.5, color = "#e74c3c", fontface = "bold", size = 4.5) +
      labs(
        title = paste0("Regula empiryczna: ", pct_in[1], "% / ",
                        pct_in[2], "% / ", pct_in[3], "%",
                        "  (teoria: 68% / 95% / 99.7%)"),
        x = variable_meta[[var_name]]$label,
        y = "Gęstość"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", size = 13))

    p
  })

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
      div(class = "callout-info",
        tags$strong("Dobra zgodność z regułą! "),
        paste0("W przedziale ±1 SD leży ", pct_in[1], "% danych (teoria: 68%). "),
        "To oznacza, że rozkład tej zmiennej jest zbliżony do normalnego. ",
        "Odchylenie standardowe dobrze podsumowuje rozrzut."
      )
    } else {
      div(class = "callout-warning",
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

  ch4_bp_step <- reactiveVal(0)
  ch4_bp_data <- reactiveVal(round(c(rnorm(27, 170, 8), 145, 198, 200), 1))

  observeEvent(input$ch4_bp_s1, { ch4_bp_step(1) })
  observeEvent(input$ch4_bp_s2, { ch4_bp_step(2) })
  observeEvent(input$ch4_bp_s3, { ch4_bp_step(3) })
  observeEvent(input$ch4_bp_s4, { ch4_bp_step(4) })
  observeEvent(input$ch4_bp_s5, { ch4_bp_step(5) })

  observeEvent(input$ch4_bp_new, {
    set.seed(NULL)
    ch4_bp_data(round(c(rnorm(27, 170, 8), 145, 198, 200), 1))
    ch4_bp_step(0)
  })

  observeEvent(input$ch4_bp_reset, {
    ch4_bp_step(0)
  })

  output$ch4_bp_plot <- renderPlot({
    step <- ch4_bp_step()
    if (step == 0) return(NULL)

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

    if (step == 5) {
      # Final: clean boxplot + histogram for comparison
      df <- data.frame(x = vals)
      p_box <- ggplot(df, aes(y = x, x = "")) +
        geom_boxplot(fill = "#3498db", alpha = 0.5, color = "#2c3e50",
                     outlier.color = "#e74c3c", outlier.size = 3,
                     width = 0.4) +
        geom_jitter(width = 0.05, alpha = 0.4, size = 2, color = "#2c3e50") +
        coord_flip() +
        labs(x = "", y = "Wzrost (cm)", title = "Gotowy boxplot (geom_boxplot)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

      p_hist <- ggplot(df, aes(x = x)) +
        geom_histogram(bins = 15, fill = "#3498db", color = "white", alpha = 0.7) +
        geom_vline(xintercept = med, color = "#e74c3c", linewidth = 1,
                   linetype = "dashed") +
        geom_vline(xintercept = q1, color = "#f39c12", linewidth = 0.8,
                   linetype = "dotted") +
        geom_vline(xintercept = q3, color = "#f39c12", linewidth = 0.8,
                   linetype = "dotted") +
        labs(x = "Wzrost (cm)", y = "Liczebność",
             title = "Histogram (do porównania)") +
        theme_minimal(base_size = 14)

      gridExtra::grid.arrange(p_box, p_hist, nrow = 2, heights = c(1, 1.2))
      return()
    }

    # Steps 1-4: manual construction
    df <- data.frame(x = vals, y = 0)

    if (step == 1) {
      # Jittered raw data
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        geom_point(size = 3, color = "#3498db", alpha = 0.7) +
        labs(x = "Wzrost (cm)", y = "",
             title = "Krok 1: Surowe dane (n = 30)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(-0.8, 0.8))

    } else if (step == 2) {
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        geom_point(size = 3, color = "#3498db", alpha = 0.7) +
        geom_vline(xintercept = med, color = "#e74c3c", linewidth = 1.5) +
        annotate("text", x = med, y = 0.65,
                 label = paste0("Mediana = ", round(med, 1)),
                 color = "#e74c3c", size = 5, fontface = "bold") +
        labs(x = "Wzrost (cm)", y = "",
             title = "Krok 2: Mediana dzieli dane na polowy") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(-0.8, 0.8))

    } else if (step == 3) {
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.3, 0.3)

      ggplot(df, aes(x = x, y = y_jit)) +
        # IQR box
        annotate("rect", xmin = q1, xmax = q3, ymin = -0.5, ymax = 0.5,
                 fill = "#3498db", alpha = 0.2, color = "#3498db",
                 linewidth = 1) +
        geom_point(size = 3, color = "#3498db", alpha = 0.7) +
        geom_vline(xintercept = med, color = "#e74c3c", linewidth = 1.5) +
        geom_vline(xintercept = q1, color = "#f39c12", linewidth = 1,
                   linetype = "dashed") +
        geom_vline(xintercept = q3, color = "#f39c12", linewidth = 1,
                   linetype = "dashed") +
        annotate("text", x = med, y = 0.7,
                 label = paste0("Me = ", round(med, 1)),
                 color = "#e74c3c", size = 4.5, fontface = "bold") +
        annotate("text", x = q1, y = -0.65,
                 label = paste0("Q1 = ", round(q1, 1)),
                 color = "#f39c12", size = 4, fontface = "bold") +
        annotate("text", x = q3, y = -0.65,
                 label = paste0("Q3 = ", round(q3, 1)),
                 color = "#f39c12", size = 4, fontface = "bold") +
        annotate("text", x = (q1 + q3) / 2, y = 0.7,
                 label = paste0("IQR = ", round(iqr_val, 1)),
                 color = "#3498db", size = 4, fontface = "bold",
                 hjust = ifelse(abs(med - (q1 + q3) / 2) < 3, 2, 0.5)) +
        labs(x = "Wzrost (cm)", y = "",
             title = "Krok 3: Kwartyle Q1, Q3 i pudełko (IQR)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(-0.9, 0.9))

    } else if (step == 4) {
      is_outlier <- vals < lower_fence | vals > upper_fence
      df$outlier <- is_outlier
      set.seed(42)
      df$y_jit <- runif(nrow(df), -0.2, 0.2)

      p <- ggplot(df) +
        # IQR box
        annotate("rect", xmin = q1, xmax = q3, ymin = -0.4, ymax = 0.4,
                 fill = "#3498db", alpha = 0.2, color = "#3498db",
                 linewidth = 1) +
        # Median line inside box
        geom_segment(aes(x = med, xend = med, y = -0.4, yend = 0.4),
                     color = "#e74c3c", linewidth = 1.5) +
        # Left whisker
        geom_segment(aes(x = whisker_low, xend = q1, y = 0, yend = 0),
                     color = "#2c3e50", linewidth = 0.8) +
        geom_segment(aes(x = whisker_low, xend = whisker_low, y = -0.2, yend = 0.2),
                     color = "#2c3e50", linewidth = 0.8) +
        # Right whisker
        geom_segment(aes(x = q3, xend = whisker_high, y = 0, yend = 0),
                     color = "#2c3e50", linewidth = 0.8) +
        geom_segment(aes(x = whisker_high, xend = whisker_high, y = -0.2, yend = 0.2),
                     color = "#2c3e50", linewidth = 0.8) +
        # Points: normal
        geom_point(data = df[!df$outlier, ], aes(x = x, y = y_jit),
                   size = 2.5, color = "#3498db", alpha = 0.5) +
        # Points: outliers
        geom_point(data = df[df$outlier, ], aes(x = x, y = y_jit),
                   size = 4, color = "#e74c3c", shape = 18) +
        # Fence annotations
        annotate("text", x = lower_fence, y = -0.55,
                 label = paste0("Q1 - 1.5*IQR\n= ", round(lower_fence, 1)),
                 color = "#7f8c8d", size = 3.5) +
        annotate("text", x = upper_fence, y = -0.55,
                 label = paste0("Q3 + 1.5*IQR\n= ", round(upper_fence, 1)),
                 color = "#7f8c8d", size = 3.5) +
        labs(x = "Wzrost (cm)", y = "",
             title = "Krok 4: Wąsy i wartości odstające") +
        theme_minimal(base_size = 14) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(limits = c(-0.8, 0.7))

      if (length(outliers) > 0) {
        p <- p +
          annotate("label", x = mean(outliers), y = 0.55,
                   label = paste0(length(outliers), " outlier(s)"),
                   fill = "#fdedec", color = "#e74c3c", size = 4,
                   fontface = "bold", label.size = 0.5)
      }

      p
    }
  })

  output$ch4_bp_text <- renderUI({
    step <- ch4_bp_step()
    if (step == 0) {
      div(class = "callout-info",
          "Kliknij przycisk kroku, aby zacząć budowę boxplota.")
    } else if (step == 1) {
      div(class = "callout-info",
          tags$strong("Krok 1:"),
          " Zaczynamy od surowych danych. 30 pomiarów wzrostu rozrzuconych
          na osi liczbowej. Widać ogolny zakres, ale ciężko wyciagnac
          szybkie wnioski.")
    } else if (step == 2) {
      vals <- ch4_bp_data()
      div(class = "callout-info",
          tags$strong("Krok 2:"),
          paste0(" Sortujemy dane i wyznaczamy mediane = ", round(median(vals), 1),
                 " cm. Mediana dzieli posortowane dane na dwie rowne polowy."))
    } else if (step == 3) {
      vals <- ch4_bp_data()
      q1 <- quantile(vals, 0.25)
      q3 <- quantile(vals, 0.75)
      div(class = "callout-info",
          tags$strong("Krok 3:"),
          paste0(" Wyznaczamy kwartyle: Q1 = ", round(q1, 1),
                 " (25% danych poniżej), Q3 = ", round(q3, 1),
                 " (75% danych poniżej). Pudelko (box) rozciaga sie od Q1 do Q3
                 i zawiera srodkowe 50% danych. IQR = Q3 - Q1 = ",
                 round(q3 - q1, 1), " cm."))
    } else if (step == 4) {
      vals <- ch4_bp_data()
      q1 <- quantile(vals, 0.25)
      q3 <- quantile(vals, 0.75)
      iqr_val <- q3 - q1
      outliers <- vals[vals < q1 - 1.5 * iqr_val | vals > q3 + 1.5 * iqr_val]
      div(class = "callout-info",
          tags$strong("Krok 4:"),
          paste0(" Wąsy siagaja do najdalszych punktow w granicach
                 1.5 * IQR od pudełka. Wszystko poza wąsami to wartości
                 odstające (outliers). "),
          if (length(outliers) > 0) {
            paste0("Znaleziono ", length(outliers),
                   " wartosc(i) odstająca(e): ",
                   paste(round(outliers, 1), collapse = ", "), " cm.")
          } else {
            "Brak wartości odstających."
          })
    } else if (step == 5) {
      div(class = "callout-info",
          tags$strong("Krok 5:"),
          " Gotowy boxplot (gora) w porownaniu z histogramem (dol).
          Boxplot kompaktowo podsumowuje rozkład: mediana, kwartyle,
          rozstęp i outlierow - wszystko w jednym wykresie. Histogram
          pokazuje więcej szczegółów o kształcie rozkładu.")
    }
  })

  # --- Widget 3b: Group comparison ---

  output$ch4_grp_plot <- renderPlot({
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
      p <- p + geom_boxplot(alpha = 0.7, outlier.color = "#e74c3c", outlier.size = 3)
    }

    if (isTRUE(input$ch4_grp_points)) {
      p <- p + geom_jitter(width = 0.15, alpha = 0.3, size = 1.5)
    }

    p + scale_fill_brewer(palette = "Set2") +
      labs(x = grp_label, y = var_label) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })

  output$ch4_grp_table <- renderTable({
    var_name <- input$ch4_grp_var
    grp_name <- input$ch4_grp_by
    req(var_name, grp_name)

    df <- data.frame(
      value = student_data[[var_name]],
      group = student_data[[grp_name]]
    )

    df %>%
      group_by(Grupa = group) %>%
      summarise(
        n = n(),
        Średnia = round(mean(value), 2),
        Mediana = round(median(value), 2),
        SD = round(sd(value), 2),
        IQR = round(IQR(value), 2),
        .groups = "drop"
      )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

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

  output$ch4_comp_plot <- renderPlot({
    vals <- ch4_comp_data()
    if (is.null(vals)) return(NULL)

    df <- data.frame(x = vals)
    data_range <- range(vals)
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr_val <- q3 - q1

    ggplot(df, aes(x = x)) +
      geom_histogram(bins = 30, fill = "#3498db", color = "white", alpha = 0.7) +
      # Range
      annotate("segment", x = data_range[1], xend = data_range[2],
               y = -2, yend = -2, color = "#e74c3c", linewidth = 2) +
      annotate("text",
               x = (data_range[1] + data_range[2]) / 2, y = -3.5,
               label = paste0("Rozstęp = ", round(diff(data_range), 1)),
               color = "#e74c3c", size = 4, fontface = "bold") +
      # IQR
      annotate("segment", x = q1, xend = q3, y = -6, yend = -6,
               color = "#27ae60", linewidth = 2) +
      annotate("text", x = (q1 + q3) / 2, y = -7.5,
               label = paste0("IQR = ", round(iqr_val, 1)),
               color = "#27ae60", size = 4, fontface = "bold") +
      labs(x = "Wzrost (cm)", y = "Liczebność",
           title = paste0("Histogram wzrostu (n = ", length(vals), ")")) +
      theme_minimal(base_size = 14) +
      coord_cartesian(clip = "off") +
      theme(plot.margin = margin(10, 10, 50, 10))
  })

  output$ch4_comp_table <- renderTable({
    vals <- ch4_comp_data()
    if (is.null(vals)) return(NULL)

    data.frame(
      Miara = c("Rozstep", "IQR (rozstęp międzykwartylowy)",
                "Odchylenie standardowe (SD)",
                "Współczynnik zmienności (CV)"),
      Wartość = c(
        paste0(round(diff(range(vals)), 1), " cm"),
        paste0(round(IQR(vals), 1), " cm"),
        paste0(round(sd(vals), 2), " cm"),
        paste0(round(sd(vals) / mean(vals) * 100, 1), "%")
      ),
      Wlasnosci = c(
        "Bardzo wrażliwy na outlierow - zależy tylko od min i max",
        "Odporny na outlierow - oparty na kwartylach",
        "Umiarkowanie wrażliwy - bierze pod uwage wszystkie dane",
        "Bezjednostkowy - pozwala porownywac zmiennosc roznych zmiennych"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  # --- Widget 5: Coefficient of Variation ---

  output$ch4_sd_compare_plot <- renderPlot({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen")

    stats <- data.frame(
      Zmienna = factor(labels, levels = rev(labels)),
      SD = sapply(vars, function(v) sd(student_data[[v]]))
    )

    ggplot(stats, aes(x = Zmienna, y = SD, fill = SD)) +
      geom_col(alpha = 0.85, width = 0.6) +
      geom_text(aes(label = round(SD, 2)), hjust = -0.1, size = 5) +
      scale_fill_gradient(low = "#85c1e9", high = "#2980b9", guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      coord_flip() +
      labs(x = NULL, y = "Odchylenie standardowe (oryg. jednostki)",
           title = "SD -- nieporównywalne") +
      theme_minimal(base_size = 14)
  })

  output$ch4_cv_plot <- renderPlot({
    vars <- c("wzrost", "waga", "czas_dojazdu", "srednia_ocen")
    labels <- c("Wzrost (cm)", "Waga (kg)", "Czas dojazdu (min)", "Średnia ocen")

    stats <- data.frame(
      Zmienna = factor(labels, levels = rev(labels)),
      CV = sapply(vars, function(v) sd(student_data[[v]]) / mean(student_data[[v]]) * 100)
    )

    ggplot(stats, aes(x = Zmienna, y = CV, fill = CV)) +
      geom_col(alpha = 0.85, width = 0.6) +
      geom_text(aes(label = paste0(round(CV, 1), "%")), hjust = -0.1, size = 5) +
      scale_fill_gradient(low = "#3498db", high = "#e74c3c", guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      coord_flip() +
      labs(x = NULL, y = "Współczynnik zmienności (%)",
           title = "CV -- porównywalne") +
      theme_minimal(base_size = 14)
  })

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
