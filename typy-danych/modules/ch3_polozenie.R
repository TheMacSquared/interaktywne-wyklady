# ============================================================================
# CHAPTER 3: Statystyki położenia
# ============================================================================

ch3_ui <- list(
  id = "ch-polozenie", num = "03", title = "Statystyki położenia",
  content = tagList(

    # --- Chapter hero ---
    lc_chapter_hero(
      kicker = "Rozdział 03 · Statystyka opisowa",
      num    = "03",
      title  = "Statystyki położenia.",
      lead   = "Po co nam jedna liczba, skoro mamy 200 obserwacji? Średnia,
                mediana i moda to trzy różne odpowiedzi na pytanie „jak wygląda
                typowy student” — zobaczmy, kiedy każda z nich ma sens."
    ),

    uiOutput("tracker_ch3"),

    tagList(
      p("Zmienne ilościowe wymagają nowych narzędzi. Zanim przejdziemy do
        statystyk, poznajmy podstawową wizualizację — histogram.
        Potem zbadamy miary położenia: średnią, medianę i percentyle.")
    ),

    # ========================================================================
    # WIDGET: Histogram krok po kroku
    # ========================================================================
    lc_h2("ch3-histogram", "Histogram — krok po kroku"),

    tagList(
      p("Histogram to podstawowy wykres dla zmiennych ciągłych. Pokazuje
        jak często występują wartości w poszczególnych
        przedziałach (binach). Zbudujmy go krok po kroku.")
    ),

    figure_panel(
      label = "Ryc. 3.1",
      title = "Budowa histogramu",
      fluidRow(
        column(4,
          selectInput("ch3_hist_var", "Zmienna:",
            choices = c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                        "Czas dojazdu (min)" = "czas_dojazdu",
                        "Średnia ocen" = "srednia_ocen"),
            selected = "wzrost"
          ),
          uiOutput("ch3_hist_bin_slider"),
          actionButton("ch3_hist_step1", "1. Surowe dane",
                       class = "lc-btn-outline", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step2", "2. Posortuj dane",
                       class = "lc-btn-outline", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step3", "3. Podziel na przedziały",
                       class = "lc-btn-outline", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step4", "4. Przypisz do binów",
                       class = "lc-btn-outline", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step5", "5. Zlicz obserwacje",
                       class = "lc-btn-outline", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step6", "6. Zbuduj słupki",
                       class = "lc-btn-outline", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step7", "7. Gotowy histogram",
                       class = "lc-btn-outline", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step8", "8. Wpływ szerokości binu",
                       class = "lc-btn-outline", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_reset", "Reset",
                       class = "lc-btn-secondary-outline", width = "100%")
        ),
        column(8,
          plotOutput("ch3_hist_plot", height = "400px"),
          uiOutput("ch3_hist_text"),
          tableOutput("ch3_hist_table")
        )
      )
    ),

    tagList(
      p("Histogram pokazuje kształt rozkładu, ale nie daje jednej liczby
        opisującej 'środek'. Do tego służą statystyki położenia:
        średnia, mediana i percentyle. Każda odpowiada na to pytanie inaczej.")
    ),

    # ========================================================================
    # WIDGET 0a: Mean introduction
    # ========================================================================
    lc_h2("ch3-srednia", "Średnia arytmetyczna"),

    tagList(
      p("Średnia arytmetyczna to suma wszystkich wartości podzielona
        przez ich liczbę. Jest to 'punkt równowagi' danych -- gdybyśmy
        położyli dane na wadze, średnia byłaby punktem podparcia."),
      withMathJax(helpText(
        "$$\\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i = \\frac{x_1 + x_2 + \\ldots + x_n}{n}$$"
      ))
    ),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Średnia jako punkt równowagi",
      selectInput("ch3_mean_var", "Zmienna:",
        choices = c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "wzrost"
      ),
      plotOutput("ch3_mean_plot", height = "300px"),
      uiOutput("ch3_mean_text")
    ),

    # ========================================================================
    # WIDGET 0b: Median introduction
    # ========================================================================
    lc_h2("ch3-mediana", "Mediana"),

    tagList(
      p("Mediana to wartość, która dzieli posortowane dane na dwie
        równe połowy: 50% obserwacji leży poniżej, 50% powyżej.
        Nie zależy od tego, jak bardzo skrajne są wartości
        na końcach -- liczy się tylko pozycja środkowa.")
    ),

    figure_panel(
      label = "Ryc. 3.3",
      title = "Mediana dzieli dane na pół",
      selectInput("ch3_median_var", "Zmienna:",
        choices = c("Wzrost (cm)" = "wzrost", "Czas dojazdu (min)" = "czas_dojazdu",
                    "Średnia ocen" = "srednia_ocen"),
        selected = "czas_dojazdu"
      ),
      plotOutput("ch3_median_plot", height = "300px"),
      uiOutput("ch3_median_text")
    ),

    # ========================================================================
    # WIDGET 1: Mean vs Median -- comparison
    # ========================================================================
    lc_h2("ch3-srednia-vs-mediana", "Średnia vs mediana — kiedy się różnią?"),

    tagList(
      p("Dla danych symetrycznych średnia i mediana są blisko siebie.
        Ale co się dzieje, gdy rozkład jest skośny lub pojawi się
        wartość odstająca?"),
      p("Wyobraźmy sobie zarobki w pewnej firmie. Większość pracowników
        zarabia umiarkowanie, ale są też osoby z bardzo wysokimi pensjami.
        Zobaczmy, jak średnia i mediana reagują na nowe wartości.")
    ),

    figure_panel(
      label = "Ryc. 3.4",
      title = "Zarobki w firmie: średnia vs mediana",

      fluidRow(
        column(5,
          sliderInput("ch3_svm_new_value", "Nowa wartość:",
                      min = 2000, max = 25000, value = 5000, step = 500,
                      pre = "", post = " zł", width = "100%")
        ),
        column(7,
          div(style = "display: flex; gap: 8px; margin-top: 25px;",
            actionButton("ch3_svm_add", "Dodaj wartość",
                         class = "lc-btn-primary"),
            actionButton("ch3_svm_outlier", "Dodaj outlier (CEO)",
                         class = "lc-btn-danger"),
            actionButton("ch3_svm_reset", "Reset",
                         class = "lc-btn-secondary-outline")
          )
        )
      ),

      hr(),

      plotOutput("ch3_svm_hist", height = "280px"),
      plotOutput("ch3_svm_strip", height = "120px"),

      lc_center(
        uiOutput("ch3_svm_stats")
      )
    ),

    margin_callout(
      label = "Obserwacja",
      "Dodaj kilka „normalnych” zarobków — średnia i mediana będą blisko
       siebie. Teraz kliknij „Dodaj outlier (CEO)” — zobacz, jak średnia
       skacze w górę, a mediana prawie się nie zmienia!"
    ),

    # ========================================================================
    # WIDGET 2: Robustness mini-demo
    # ========================================================================
    lc_h2("ch3-odpornosc", "Odporność miar na outliery"),

    tagList(
      p("Która statystyka jest bardziej odporna na outliery? Średnia
        arytmetyczna bierze pod uwagę każdą wartość -- więc jedna
        ekstremalna obserwacja może ją znacząco przesunąć. Mediana
        ignoruje skrajne wartości, patrząc tylko na 'środek' danych."),
      p("Średnia ucinana (trimmed mean) to kompromis: odrzuca pewien
        procent najbardziej skrajnych obserwacji z obu stron, a następnie
        oblicza średnią z pozostałych. Dodajmy kilka ekstremalnych
        zarobków i zobaczmy, co się stanie.")
    ),

    figure_panel(
      label = "Ryc. 3.5",
      title = "Odporność: średnia vs mediana vs średnia ucinana",

      div(style = "display: flex; gap: 8px; margin-bottom: 15px;",
        actionButton("ch3_rob_add1", "Dodaj outlier (+50 000 zl)",
                     class = "lc-btn-warning"),
        actionButton("ch3_rob_add5", "Dodaj 5 outlierow",
                     class = "lc-btn-danger"),
        actionButton("ch3_rob_reset", "Reset",
                     class = "lc-btn-secondary-outline")
      ),

      plotOutput("ch3_rob_plot", height = "320px"),

      div(style = "margin-top: 15px;",
        tableOutput("ch3_rob_table")
      )
    ),

    margin_callout(
      label = "Wniosek",
      "Średnia arytmetyczna jest bardzo wrażliwa na wartości odstające.
       Mediana jest najbardziej odporna. Średnia ucinana oferuje
       kompromis — jest mniej wrażliwa niż średnia, ale bardziej niż mediana.
       Dlatego przy skośnych rozkładach (np. zarobki) mediana jest często
       lepszą miarą „typowej” wartości.",
      color = "uwaga"
    ),

    # ========================================================================
    # WIDGET 2b: Discrete variables
    # ========================================================================
    lc_h2("ch3-dyskretna", "Zmienne dyskretne — te same statystyki, inne wykresy"),

    tagList(
      p("Dotychczas uzywalismy zmiennych ciągłych (wzrost, zarobki). Ale co ze
        zmiennymi dyskretnymi -- takimi jak liczba kursow czy
        liczba nieobecnosci? Statystyki polozenia (średnia, mediana) obliczamy
        tak samo, ale wizualizacja wymaga uwagi.")
    ),

    figure_panel(
      label = "Ryc. 3.6",
      title = "Dyskretna vs ciągła -- porównanie wizualizacji",
      selectInput("ch3_disc_var", "Wybierz zmienna dyskretna:",
        choices = c("Liczba nieobecności" = "liczba_nieobecnosci",
                    "Liczba kursów" = "liczba_kursow"),
        selected = "liczba_nieobecnosci"
      ),
      fluidRow(
        column(6,
          h5(style = "text-align: center; color: var(--upwr-cat-szalwia);", "Wykres słupkowy (poprawny)"),
          plotOutput("ch3_disc_bar", height = "300px")
        ),
        column(6,
          h5(style = "text-align: center; color: var(--upwr-accent);", "Histogram (problematyczny)"),
          plotOutput("ch3_disc_hist", height = "300px")
        )
      ),
      tableOutput("ch3_disc_stats"),
      uiOutput("ch3_disc_explanation")
    ),

    # ========================================================================
    # WIDGET 2c: Multimodality in continuous distributions
    # ========================================================================
    lc_h2("ch3-modalnosc", "Modalność rozkładu — ile „górek” ma histogram?"),

    tagList(
      p("W rozdziale o zmiennych jakościowych poznaliśmy dominantę -- najczęstszą
        kategorię. Dla danych ciągłych dominanta pojedynczej wartości nie ma sensu
        (prawie każda wartość jest unikatowa). Ale pojęcie ",
        "mody działa na przedziałach -- szukamy, który bin histogramu jest najwyższy."),
      p("Co ważniejsze, rozkład może mieć więcej niż jeden szczyt (modę). To często
        sygnał, że dane pochodzą z kilku różnych grup.")
    ),

    figure_panel(
      label = "Ryc. 3.7",
      title = "Unimodalny vs bimodalny vs wielomodalny",
      radioButtons("ch3_modal_scenario", "Scenariusz:",
        choices = c(
          "Unimodalny -- wzrost kobiet" = "unimodal",
          "Bimodalny -- wzrost (kobiety + mężczyźni)" = "bimodal",
          "Wielomodalny -- czas dojazdu (autobus vs rower vs auto)" = "multimodal"
        ),
        selected = "unimodal"
      ),
      plotOutput("ch3_modal_plot", height = "350px"),
      uiOutput("ch3_modal_text")
    ),

    # ========================================================================
    # WIDGET 3: Percentile explorer
    # ========================================================================
    lc_h2("ch3-percentyle", "Percentyle i kwantyle"),

    margin_callout(
      label = "IQR",
      tagList(
        tags$strong("Rozstęp międzykwartylowy:"),
        " różnica Q3 − Q1. Miara rozrzutu odporna na outliery.
          Boxplot używa właśnie kwartyli do wizualizacji rozkładu danych.
          Więcej w kolejnym rozdziale."
      )
    ),

    tagList(
      p("Kwantyle i percentyle dziela dane na czesci. Percentyl mówi nam,
        jaki procent obserwacji jest poniżej danej wartości. Na przykład
        percentyl 75. oznacza, ze 75% obserwacji ma wartość mniejsza
        lub rowna tej wartości."),
      p("Trzy najwazniejsze kwantyle to kwartyle:"),
      tags$ul(
        tags$li(tags$strong("Q1 (25. percentyl)"), " - pierwsza cwiartka danych"),
        tags$li(tags$strong("Q2 (50. percentyl)"), " - mediana, czyli środek"),
        tags$li(tags$strong("Q3 (75. percentyl)"), " - trzecia cwiartka danych")
      ),
      p("Przesuwaj suwak, aby zobaczyc rozne percentyle wzrostu studentow
        z naszej ankiety.")
    ),

    figure_panel(
      label = "Ryc. 3.8",
      title = "Explorer percentyli: wzrost studentow",

      fluidRow(
        column(6,
          sliderInput("ch3_q_pct", "Percentyl:",
                      min = 0, max = 100, value = 50, step = 1,
                      post = "%", width = "100%")
        ),
        column(6,
          div(style = "display: flex; gap: 8px; margin-top: 25px;",
            actionButton("ch3_q_q1", "Q1 (25%)", class = "lc-btn-outline"),
            actionButton("ch3_q_med", "Mediana (50%)", class = "lc-btn-outline"),
            actionButton("ch3_q_q3", "Q3 (75%)", class = "lc-btn-outline")
          )
        )
      ),

      hr(),

      plotOutput("ch3_q_hist", height = "280px"),
      plotOutput("ch3_q_box", height = "120px"),

      lc_center(
        uiOutput("ch3_q_text")
      )
    ),

    # ====================================================================
    # WIDGET 4: Guess the statistic game
    # ====================================================================
    lc_h2("ch3-gra", "Gra: Zgadnij średnią i medianę!"),

    tagList(
      p("Sprawdzmy Twoją intuicję! Na histogramie zobaczysz rozkład danych. ",
        "Kliknij na wykres, aby postawić swój typ: najpierw średnia, potem mediana. Czy potrafisz je odroznic?")
    ),

    figure_panel(
      label = "Ryc. 3.9",
      title = "Kliknij na wykres, aby umieścić średnią i medianę",
      div(style = "margin-bottom: 10px;",
        actionButton("ch3_game_new", "Nowa runda",
                     class = "lc-btn-primary", style = "margin-right: 6px;"),
        actionButton("ch3_game_reveal", "Pokaż odpowiedź",
                     class = "lc-btn-ok", style = "margin-right: 6px;")
      ),
      uiOutput("ch3_game_status_banner"),
      plotOutput("ch3_game_plot", height = "350px", click = "ch3_game_click"),
      uiOutput("ch3_game_feedback")
    ),

    lc_chapter_next(
      num       = "04",
      title     = "Statystyki rozrzutu",
      lead      = "dwie grupy z tą samą średnią mogą wyglądać zupełnie inaczej — różni je rozrzut.",
      target_id = "ch-rozrzut"
    ),

    # Spacer at bottom
    lc_spacer("lg")

  )
)

# --------------------------------------------------------------------------
# Chapter 3 Server
# --------------------------------------------------------------------------

ch3_server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # Widget: Histogram krok po kroku
  # --------------------------------------------------------------------------

  ch3_hist_step <- reactiveVal(0)

  observeEvent(input$ch3_hist_var, { ch3_hist_step(0) })
  observeEvent(input$ch3_hist_reset, { ch3_hist_step(0) })
  observeEvent(input$ch3_hist_step1, { ch3_hist_step(1) })
  observeEvent(input$ch3_hist_step2, { ch3_hist_step(2) })
  observeEvent(input$ch3_hist_step3, { ch3_hist_step(3) })
  observeEvent(input$ch3_hist_step4, { ch3_hist_step(4) })
  observeEvent(input$ch3_hist_step5, { ch3_hist_step(5) })
  observeEvent(input$ch3_hist_step6, { ch3_hist_step(6) })
  observeEvent(input$ch3_hist_step7, { ch3_hist_step(7) })
  observeEvent(input$ch3_hist_step8, { ch3_hist_step(8) })

  # Default bin widths per variable
  ch3_hist_defaults <- list(
    wzrost = list(min = 1, max = 15, value = 3, step = 1, unit = "cm"),
    waga = list(min = 2, max = 20, value = 5, step = 1, unit = "kg"),
    czas_dojazdu = list(min = 2, max = 20, value = 5, step = 1, unit = "min"),
    srednia_ocen = list(min = 0.1, max = 1, value = 0.3, step = 0.05, unit = "pkt")
  )

  output$ch3_hist_bin_slider <- renderUI({
    d <- ch3_hist_defaults[[input$ch3_hist_var]]
    sliderInput("ch3_hist_bin_width",
                "Szerokość binu:",
                min = d$min, max = d$max,
                value = d$value, step = d$step)
  })

  # Compute bin breaks
  ch3_hist_breaks <- reactive({
    req(input$ch3_hist_bin_width)
    x <- student_data[[input$ch3_hist_var]]
    w <- input$ch3_hist_bin_width
    start <- floor(min(x) / w) * w
    end <- ceiling(max(x) / w) * w + w
    seq(start, end, by = w)
  })

  # Data with bin assignments
  ch3_hist_binned <- reactive({
    req(input$ch3_hist_bin_width)
    x <- student_data[[input$ch3_hist_var]]
    breaks <- ch3_hist_breaks()
    df <- data.frame(value = x)
    df$bin <- cut(df$value, breaks = breaks, include.lowest = TRUE, right = FALSE)
    df$bin_num <- as.numeric(df$bin)
    df
  })

  # Bin statistics
  ch3_hist_stats <- reactive({
    df <- ch3_hist_binned()
    breaks <- ch3_hist_breaks()
    all_bins <- data.frame(
      bin_start = breaks[-length(breaks)],
      bin_end = breaks[-1]
    )
    all_bins$bin_mid <- (all_bins$bin_start + all_bins$bin_end) / 2
    all_bins$bin_num <- seq_len(nrow(all_bins))

    counts <- df %>%
      filter(!is.na(bin)) %>%
      group_by(bin_num) %>%
      summarise(count = n(), .groups = "drop")
    all_bins <- all_bins %>% left_join(counts, by = "bin_num")
    all_bins$count[is.na(all_bins$count)] <- 0

    # Trim to relevant range
    min_d <- min(all_bins$bin_num[all_bins$count > 0])
    max_d <- max(all_bins$bin_num[all_bins$count > 0])
    all_bins %>%
      filter(bin_num >= max(1, min_d - 1),
             bin_num <= min(nrow(all_bins), max_d + 1))
  })

  # Variable labels
  ch3_hist_var_labels <- c(
    "wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
    "czas_dojazdu" = "Czas dojazdu (min)",
    "srednia_ocen" = "Średnia ocen"
  )

  output$ch3_hist_plot <- renderPlot({
    step <- ch3_hist_step()
    var_name <- input$ch3_hist_var
    req(var_name)
    x <- student_data[[var_name]]
    x_label <- ch3_hist_var_labels[var_name]
    n <- length(x)

    x_lo <- min(x) - diff(range(x)) * 0.05
    x_hi <- max(x) + diff(range(x)) * 0.05

    strip_theme <-       theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

    if (step == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij Krok 1", size = 6, color = "gray50") +
        theme_void() + xlim(0, 1) + ylim(0, 1)

    } else if (step == 1) {
      df <- data.frame(value = x)
      ggplot(df, aes(x = value, y = 0)) +
        geom_jitter(height = 0.3, size = 3, alpha = 0.6, color = upwr_cat["niebo"]) +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 2) {
      df <- data.frame(value = sort(x))
      ggplot(df, aes(x = value, y = 0)) +
        geom_point(size = 3, alpha = 0.7, color = upwr_cat["szalwia"]) +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 3) {
      breaks <- ch3_hist_breaks()
      df <- data.frame(value = sort(x))
      bin_rects <- data.frame(
        xmin = breaks[-length(breaks)], xmax = breaks[-1]
      ) %>% filter(xmax > x_lo, xmin < x_hi)

      ggplot() +
        geom_rect(data = bin_rects,
                  aes(xmin = xmin, xmax = xmax, ymin = -0.35, ymax = 0.35),
                  fill = NA, color = upwr_secondary, linewidth = 0.8,
                  linetype = "dashed") +
        geom_point(data = df, aes(x = value, y = 0),
                   size = 2.5, alpha = 0.5, color = upwr_reference) +
        geom_text(data = bin_rects,
                  aes(x = (xmin + xmax) / 2, y = -0.45,
                      label = paste0("[", xmin, ", ", xmax, ")")),
                  size = 2.8, color = upwr_secondary) +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.55, 0.5))

    } else if (step == 4) {
      df <- ch3_hist_binned()
      breaks <- ch3_hist_breaks()
      bin_rects <- data.frame(
        xmin = breaks[-length(breaks)], xmax = breaks[-1],
        bin_num = seq_len(length(breaks) - 1)
      ) %>% filter(xmax > x_lo, xmin < x_hi)

      ggplot() +
        geom_rect(data = bin_rects,
                  aes(xmin = xmin, xmax = xmax, ymin = -0.35, ymax = 0.35,
                      fill = factor(bin_num)),
                  alpha = 0.15, color = upwr_secondary, linewidth = 0.5) +
        geom_jitter(data = df %>% filter(!is.na(bin)),
                    aes(x = value, y = 0, color = factor(bin_num)),
                    height = 0.2, size = 3, alpha = 0.8) +
        scale_fill_upwr(guide = "none") +
        scale_color_upwr(guide = "none") +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 5) {
      df <- ch3_hist_binned()
      stats <- ch3_hist_stats()

      ggplot() +
        geom_rect(data = stats,
                  aes(xmin = bin_start, xmax = bin_end,
                      ymin = -0.35, ymax = 0.35,
                      fill = factor(bin_num)),
                  alpha = 0.15, color = upwr_secondary, linewidth = 0.5) +
        geom_jitter(data = df %>% filter(!is.na(bin)),
                    aes(x = value, y = 0, color = factor(bin_num)),
                    height = 0.2, size = 2, alpha = 0.6) +
        geom_text(data = stats,
                  aes(x = bin_mid, y = 0.45,
                      label = ifelse(count > 0, paste0("n=", count), "")),
                  size = 4, fontface = "bold", color = upwr_secondary) +
        scale_fill_upwr(guide = "none") +
        scale_color_upwr(guide = "none") +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.6))

    } else if (step == 6) {
      stats <- ch3_hist_stats()
      w <- input$ch3_hist_bin_width

      ggplot(stats, aes(x = bin_mid, y = count)) +
        geom_col(aes(fill = factor(bin_num)),
                 width = w * 0.95, alpha = 0.7,
                 color = upwr_secondary, linewidth = 0.3) +
        geom_text(aes(label = count), vjust = -0.5, size = 4, fontface = "bold") +
        scale_fill_upwr(guide = "none") +
        labs(x = x_label, y = "Liczba obserwacji") +
                coord_cartesian(xlim = c(x_lo, x_hi))

    } else if (step == 7) {
      df <- data.frame(value = x)
      w <- input$ch3_hist_bin_width

      ggplot(df, aes(x = value)) +
        geom_histogram(binwidth = w, fill = upwr_cat["niebo"], alpha = 0.7,
                       color = upwr_secondary, linewidth = 0.3) +
        labs(x = x_label, y = "Liczba obserwacji",
             title = paste0("Histogram: ", x_label),
             subtitle = paste0("n = ", n, " | szerokość binu = ", w)) +
        theme()

    } else if (step == 8) {
      df <- data.frame(value = x)
      w <- input$ch3_hist_bin_width
      widths <- c(w / 2, w, w * 2)
      unit <- ch3_hist_defaults[[var_name]]$unit
      labels <- paste0("Bin = ", widths, " ", unit)

      plots <- lapply(seq_along(widths), function(i) {
        ggplot(df, aes(x = value)) +
          geom_histogram(binwidth = widths[i],
                         fill = c(upwr_accent, upwr_cat["niebo"], upwr_cat["szalwia"])[i],
                         alpha = 0.7, color = upwr_secondary, linewidth = 0.3) +
          labs(x = if (i == 2) x_label else "",
               y = if (i == 1) "Liczba obs." else "",
               title = labels[i]) +
                    theme(plot.title = element_text(
            size = 12, face = "bold",
            color = c(upwr_accent, upwr_cat["niebo"], upwr_cat["szalwia"])[i]))
      })
      gridExtra::grid.arrange(grobs = plots, ncol = 3)
    }
  })

  output$ch3_hist_text <- renderUI({
    step <- ch3_hist_step()
    var_name <- input$ch3_hist_var
    req(var_name)
    x <- student_data[[var_name]]
    n <- length(x)
    unit <- ch3_hist_defaults[[var_name]]$unit

    txt <- switch(as.character(step),
      "0" = "Kliknij Krok 1, aby rozpocząć budowę histogramu.",
      "1" = paste0("Mamy ", n, " obserwacji — każdy punkt to jedna wartość. ",
                   "Trudno z tego odczytać rozkład, prawda?"),
      "2" = paste0("Sortujemy od min = ", round(min(x), 1),
                   " do max = ", round(max(x), 1), " ", unit,
                   ". Widać zagęszczenia, ale wciąż nieczytelne."),
      "3" = paste0("Dzielimy oś na równe przedziały (biny) o szerokości ",
                   input$ch3_hist_bin_width, " ", unit,
                   ". Każdy bin to 'koszyk' na obserwacje."),
      "4" = "Każda obserwacja trafia do swojego binu — kolor = przynależność.",
      "5" = "Liczymy obserwacje w każdym binie. Te liczby staną się wysokością słupków.",
      "6" = "Zamieniamy punkty na słupki — wysokość = liczba obserwacji. To już prawie histogram!",
      "7" = paste0("Gotowy histogram (n = ", n, ", bin = ", input$ch3_hist_bin_width,
                   " ", unit, "). Spróbuj zmienić szerokość binu suwakiem!"),
      "8" = paste0("Te same dane z trzema szerokościami binu. ",
                   "Za wąskie → szum. Za szerokie → utrata szczegółów.")
    )
    lc_feedback(type = "info", p(txt))
  })

  output$ch3_hist_table <- renderTable({
    if (ch3_hist_step() < 5) return(NULL)
    stats <- ch3_hist_stats()
    n <- length(student_data[[input$ch3_hist_var]])

    result <- stats %>% filter(count > 0) %>%
      mutate(pct = round(count / n * 100, 1))
    out <- data.frame(
      a = paste0("[", result$bin_start, ", ", result$bin_end, ")"),
      b = result$count,
      c = paste0(result$pct, "%")
    )
    names(out) <- c("Przedział", "Liczba obs.", "Procent")
    out
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # --------------------------------------------------------------------------
  # Widget 0a: Mean introduction
  # --------------------------------------------------------------------------

  output$ch3_mean_plot <- renderPlot({
    var_name <- input$ch3_mean_var
    req(var_name)
    x <- student_data[[var_name]]
    m <- mean(x)
    var_labels <- c("wzrost" = "Wzrost (cm)", "waga" = "Waga (kg)",
                    "srednia_ocen" = "Średnia ocen")
    df <- data.frame(val = x)

    ggplot(df, aes(x = val)) +
      geom_histogram(bins = 25, fill = upwr_rule, color = "white", alpha = 0.8) +
      geom_vline(xintercept = m, color = upwr_accent, linewidth = 1.5, linetype = "solid") +
      annotate("text", x = m, y = Inf, label = paste0("Średnia = ", round(m, 2)),
               vjust = 2, hjust = -0.1, color = upwr_accent, size = 5, fontface = "bold") +
      annotate("segment", x = min(x), xend = m, y = -0.5, yend = -0.5,
               color = upwr_cat["niebo"], linewidth = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
      annotate("segment", x = max(x), xend = m, y = -0.5, yend = -0.5,
               color = upwr_cat["niebo"], linewidth = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
      labs(x = var_labels[var_name], y = "Liczebność",
           title = "Średnia jako punkt równowagi") +
      theme()
  })

  output$ch3_mean_text <- renderUI({
    var_name <- input$ch3_mean_var
    req(var_name)
    x <- student_data[[var_name]]
    m <- mean(x)
    s <- sum(x)
    n <- length(x)
    lc_feedback(type = "info",
      withMathJax(paste0(
        "$$\\bar{x} = \\frac{", round(s, 1), "}{", n, "} = ", round(m, 2), "$$"
      )),
      tags$em("Średnia uwzględnia każdą wartość -- jest wrażliwa
              na wartości skrajne, bo przeciąga ją w ich stronę.")
    )
  })

  # --------------------------------------------------------------------------
  # Widget 0b: Median introduction
  # --------------------------------------------------------------------------

  output$ch3_median_plot <- renderPlot({
    var_name <- input$ch3_median_var
    req(var_name)
    x <- student_data[[var_name]]
    med <- median(x)
    x_sorted <- sort(x)
    n <- length(x_sorted)
    n_below <- sum(x_sorted < med)
    n_above <- sum(x_sorted > med)
    var_labels <- c("wzrost" = "Wzrost (cm)", "czas_dojazdu" = "Czas dojazdu (min)",
                    "srednia_ocen" = "Średnia ocen")
    df <- data.frame(val = x)

    ggplot(df, aes(x = val)) +
      geom_histogram(bins = 25, fill = upwr_rule, color = "white", alpha = 0.8) +
      geom_vline(xintercept = med, color = upwr_cat["indygo"], linewidth = 1.5) +
      annotate("rect", xmin = min(x) - 1, xmax = med, ymin = -Inf, ymax = Inf,
               fill = upwr_cat["niebo"], alpha = 0.08) +
      annotate("rect", xmin = med, xmax = max(x) + 1, ymin = -Inf, ymax = Inf,
               fill = upwr_accent, alpha = 0.08) +
      annotate("text", x = (min(x) + med) / 2, y = Inf,
               label = paste0("50% (", n_below, " obs.)"),
               vjust = 2, color = upwr_secondary, size = 5, fontface = "bold") +
      annotate("text", x = (max(x) + med) / 2, y = Inf,
               label = paste0("50% (", n_above, " obs.)"),
               vjust = 2, color = upwr_secondary, size = 5, fontface = "bold") +
      annotate("text", x = med, y = Inf, label = paste0("Me = ", round(med, 1)),
               vjust = 4, hjust = -0.1, color = upwr_cat["indygo"], size = 5, fontface = "bold") +
      geom_histogram(bins = 25, fill = upwr_rule, color = "white", alpha = 0.8) +
      geom_vline(xintercept = med, color = upwr_cat["indygo"], linewidth = 1.5) +
      labs(x = var_labels[var_name], y = "Liczebność",
           title = "Mediana dzieli dane na dwie równe połowy") +
      theme()
  })

  output$ch3_median_text <- renderUI({
    var_name <- input$ch3_median_var
    req(var_name)
    x <- student_data[[var_name]]
    med <- median(x)
    m <- mean(x)
    diff <- abs(m - med)

    lc_feedback(type = "info",
      paste0("Mediana = ", round(med, 1)),
      " | Średnia = ", round(m, 2),
      " | Różnica = ", round(diff, 2)
    )
  })

  # --------------------------------------------------------------------------
  # Widget 1: Mean vs Median comparison
  # --------------------------------------------------------------------------

  ch3_svm_generate <- function() {
    round(rgamma(30, shape = 3, scale = 1500) + 2000)
  }

  ch3_svm_data <- reactiveVal(NULL)

  observe({
    if (is.null(ch3_svm_data())) {
      set.seed(NULL)
      ch3_svm_data(ch3_svm_generate())
    }
  })

  observeEvent(input$ch3_svm_add, {
    ch3_svm_data(c(ch3_svm_data(), input$ch3_svm_new_value))
  })

  observeEvent(input$ch3_svm_outlier, {
    ch3_svm_data(c(ch3_svm_data(), 50000))
  })

  observeEvent(input$ch3_svm_reset, {
    set.seed(NULL)
    ch3_svm_data(ch3_svm_generate())
  })

  output$ch3_svm_hist <- renderPlot({
    req(ch3_svm_data())
    d <- data.frame(x = ch3_svm_data())
    m <- mean(d$x)
    med <- median(d$x)

    ggplot(d, aes(x = x)) +
      geom_histogram(fill = upwr_reference, color = "white", bins = 25) +
      geom_vline(aes(xintercept = m, color = "Srednia"),
                 linewidth = 1.2, linetype = "solid") +
      geom_vline(aes(xintercept = med, color = "Mediana"),
                 linewidth = 1.2, linetype = "dashed") +
      scale_color_manual(
        name = NULL,
        breaks = c("Srednia", "Mediana"),
        values = c("Srednia" = upwr_accent, "Mediana" = upwr_cat["niebo"])
      ) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zl)", y = "Liczba osob",
           title = "Rozkład zarobkow") +
            theme(legend.position = "top",
            plot.title = element_text(face = "bold"))
  })

  output$ch3_svm_strip <- renderPlot({
    req(ch3_svm_data())
    d <- data.frame(x = ch3_svm_data())
    m <- mean(d$x)
    med <- median(d$x)

    ggplot(d, aes(x = x, y = 0)) +
      geom_jitter(height = 0.3, width = 0, size = 2.5,
                  alpha = 0.6, color = upwr_secondary) +
      geom_point(aes(x = m), y = 0, color = upwr_accent,
                 size = 5, shape = 18) +
      geom_point(aes(x = med), y = 0, color = upwr_cat["niebo"],
                 size = 5, shape = 18) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zl)", y = NULL) +
            theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })

  output$ch3_svm_stats <- renderUI({
    req(ch3_svm_data())
    d <- ch3_svm_data()
    m <- mean(d)
    med <- median(d)
    diff_val <- m - med

    diff_color <- if (abs(diff_val) < 500) upwr_cat["szalwia"] else upwr_cat["bursztyn"]

    tagList(
      lc_stat_box("Srednia", format(round(m), big.mark = " "), " zl",
                  color = "var(--upwr-accent)"),
      lc_stat_box("Mediana", format(round(med), big.mark = " "), " zl",
                  color = "var(--upwr-cat-niebo)"),
      lc_stat_box("Roznica", format(round(diff_val), big.mark = " "), " zl",
                  color = diff_color)
    )
  })

  # --------------------------------------------------------------------------
  # Widget 2: Robustness mini-demo
  # --------------------------------------------------------------------------

  ch3_rob_generate <- function() {
    round(rgamma(40, shape = 3, scale = 1500) + 2000)
  }

  ch3_rob_base <- reactiveVal(NULL)
  ch3_rob_outliers <- reactiveVal(numeric(0))

  observe({
    if (is.null(ch3_rob_base())) {
      set.seed(NULL)
      ch3_rob_base(ch3_rob_generate())
    }
  })

  ch3_rob_all <- reactive({
    c(ch3_rob_base(), ch3_rob_outliers())
  })

  # Store baseline stats for comparison
  ch3_rob_base_stats <- reactive({
    req(ch3_rob_base())
    d <- ch3_rob_base()
    list(
      mean = mean(d),
      median = median(d),
      trimmed = mean(d, trim = 0.1)
    )
  })

  observeEvent(input$ch3_rob_add1, {
    new_outlier <- 50000 + runif(1, -5000, 5000)
    ch3_rob_outliers(c(ch3_rob_outliers(), round(new_outlier)))
  })

  observeEvent(input$ch3_rob_add5, {
    new_outliers <- round(50000 + runif(5, -5000, 5000))
    ch3_rob_outliers(c(ch3_rob_outliers(), new_outliers))
  })

  observeEvent(input$ch3_rob_reset, {
    set.seed(NULL)
    ch3_rob_base(ch3_rob_generate())
    ch3_rob_outliers(numeric(0))
  })

  output$ch3_rob_plot <- renderPlot({
    req(ch3_rob_all())
    d <- data.frame(x = ch3_rob_all())
    m <- mean(d$x)
    med <- median(d$x)
    tr <- mean(d$x, trim = 0.1)

    line_data <- data.frame(
      xval = c(m, med, tr),
      Statystyka = factor(
        c("Srednia", "Mediana", "Sr. ucinana (10%)"),
        levels = c("Srednia", "Mediana", "Sr. ucinana (10%)")
      ),
      ltype = c("solid", "dashed", "dotted")
    )

    n_outliers <- length(ch3_rob_outliers())
    subtitle_text <- if (n_outliers == 0) {
      "Brak outlierow"
    } else {
      paste0("Liczba dodanych outlierow: ", n_outliers)
    }

    ggplot(d, aes(x = x)) +
      geom_histogram(fill = upwr_reference, color = "white", bins = 30) +
      geom_vline(data = line_data,
                 aes(xintercept = xval, color = Statystyka,
                     linetype = Statystyka),
                 linewidth = 1.2) +
      scale_color_manual(
        name = NULL,
        breaks = c("Srednia", "Mediana", "Sr. ucinana (10%)"),
        values = c("Srednia" = upwr_accent,
                   "Mediana" = upwr_cat["niebo"],
                   "Sr. ucinana (10%)" = upwr_cat["szalwia"])
      ) +
      scale_linetype_manual(
        name = NULL,
        breaks = c("Srednia", "Mediana", "Sr. ucinana (10%)"),
        values = c("Srednia" = "solid",
                   "Mediana" = "dashed",
                   "Sr. ucinana (10%)" = "dotted")
      ) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zl)", y = "Liczba osob",
           title = "Porównanie miar polozenia",
           subtitle = subtitle_text) +
            theme(legend.position = "top",
            plot.title = element_text(face = "bold"))
  })

  output$ch3_rob_table <- renderTable({
    req(ch3_rob_all())
    req(ch3_rob_base_stats())

    d <- ch3_rob_all()
    base <- ch3_rob_base_stats()

    current_mean <- mean(d)
    current_med <- median(d)
    current_tr <- mean(d, trim = 0.1)

    data.frame(
      Statystyka = c("Srednia", "Mediana", "Średnia ucinana (10%)"),
      Wartość = paste0(
        format(round(c(current_mean, current_med, current_tr)),
               big.mark = " "), " zl"),
      `Zmiana vs bazowa` = paste0(
        ifelse(c(current_mean - base$mean,
                 current_med - base$median,
                 current_tr - base$trimmed) >= 0, "+", ""),
        format(round(c(current_mean - base$mean,
                       current_med - base$median,
                       current_tr - base$trimmed)),
               big.mark = " "), " zl"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lcr")

  # --------------------------------------------------------------------------
  # Widget 2b: Discrete variables

  output$ch3_disc_bar <- renderPlot({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]
    df <- data.frame(x = factor(vals))

    ggplot(df, aes(x = x)) +
      geom_bar(fill = type_colors["ilosciowa_dyskretna"], color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = variable_meta[[var_name]]$label, y = "Liczebność") +
      theme()
  })

  output$ch3_disc_hist <- renderPlot({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]

    ggplot(data.frame(x = vals), aes(x = x)) +
      geom_histogram(bins = 15, fill = upwr_accent, color = "white", alpha = 0.6) +
      labs(x = variable_meta[[var_name]]$label, y = "Liczebność") +
      theme()
  })

  output$ch3_disc_stats <- renderTable({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]

    mode_val <- as.numeric(names(sort(table(vals), decreasing = TRUE))[1])

    data.frame(
      Statystyka = c("Srednia", "Mediana", "Dominanta (moda)", "SD", "Rozstep"),
      Wartość = c(round(mean(vals), 2), median(vals), mode_val,
                  round(sd(vals), 2), paste0(min(vals), " - ", max(vals))),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "c")

  output$ch3_disc_explanation <- renderUI({
    lc_feedback(type = "info",
      tags$strong("Dlaczego wykres słupkowy jest lepszy? "),
      "Zmienna dyskretna przyjmuje skończenie wiele wartości calkowitych.
       Wykres słupkowy pokazuje każdą wartość osobno i poprawnie oddaje liczebnośći.
       Histogram natomiast grupuje dane w 'kubly' (bins), co moze niepoprawnie
       rozbic lub polaczyc wartości całkowite. ",
      tags$em("Statystyki (średnia, mediana, SD) liczymy tak samo jak dla zmiennych ciągłych.")
    )
  })

  # Widget 2c: Multimodality in continuous distributions
  # --------------------------------------------------------------------------

  output$ch3_modal_plot <- renderPlot({
    scenario <- input$ch3_modal_scenario
    req(scenario)

    set.seed(42)
    if (scenario == "unimodal") {
      x <- rnorm(500, mean = 165, sd = 6)
      df <- data.frame(val = x)
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = upwr_cat["niebo"], color = "white", alpha = 0.7) +
        geom_density(linewidth = 1.2, color = upwr_secondary) +
        geom_vline(xintercept = mean(x), color = upwr_accent, linewidth = 1, linetype = "dashed") +
        annotate("text", x = mean(x) + 1, y = Inf, label = "moda ≈ średnia ≈ mediana",
                 hjust = 0, vjust = 2, color = upwr_accent, size = 4.5, fontface = "bold") +
        labs(title = "Unimodalny -- jeden wyraźny szczyt",
             x = "Wzrost kobiet (cm)", y = "Gęstość") +
        theme()

    } else if (scenario == "bimodal") {
      x_k <- rnorm(250, mean = 162, sd = 5)
      x_m <- rnorm(250, mean = 182, sd = 5)
      df <- data.frame(val = c(x_k, x_m),
                       grupa = rep(c("Kobiety", "Mężczyźni"), each = 250))
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 35,
                       fill = upwr_reference, color = "white", alpha = 0.5) +
        geom_density(linewidth = 1.2, color = upwr_secondary) +
        geom_density(aes(color = grupa), linewidth = 0.8, linetype = "dashed") +
        scale_color_manual(values = c("Kobiety" = upwr_accent, "Mężczyźni" = upwr_cat["niebo"])) +
        labs(title = "Bimodalny -- dwa szczyty (dwie grupy!)",
             x = "Wzrost (cm)", y = "Gęstość", color = NULL) +
                theme(legend.position = "top")

    } else {
      x1 <- rnorm(150, mean = 12, sd = 3)
      x2 <- rnorm(120, mean = 25, sd = 4)
      x3 <- rnorm(130, mean = 40, sd = 5)
      df <- data.frame(val = c(x1, x2, x3),
                       grupa = c(rep("Rower", 150), rep("Autobus", 120), rep("Auto", 130)))
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 40,
                       fill = upwr_reference, color = "white", alpha = 0.5) +
        geom_density(linewidth = 1.2, color = upwr_secondary) +
        geom_density(aes(color = grupa), linewidth = 0.8, linetype = "dashed") +
        scale_color_manual(values = c("Rower" = upwr_cat["szalwia"], "Autobus" = upwr_cat["bursztyn"], "Auto" = upwr_accent)) +
        labs(title = "Wielomodalny -- trzy szczyty (trzy środki transportu)",
             x = "Czas dojazdu (min)", y = "Gęstość", color = NULL) +
                theme(legend.position = "top")
    }
  })

  output$ch3_modal_text <- renderUI({
    scenario <- input$ch3_modal_scenario
    req(scenario)

    if (scenario == "unimodal") {
      lc_feedback(type = "info",
        tags$b("Rozkład unimodalny: "), "jeden szczyt, jedna 'górka'. ",
        "Dla rozkładu symetrycznego moda ≈ średnia ≈ mediana. ",
        "Większość statystyk opisowych zakłada właśnie taki rozkład.")
    } else if (scenario == "bimodal") {
      lc_feedback(type = "warning",
        tags$b("Rozkład bimodalny: "), "dwa szczyty! ",
        "To sygnał, że dane prawdopodobnie pochodzą z ",
        "dwóch różnych grup. ",
        "Podawanie jednej średniej dla całości jest mylace -- ",
        "średnia wyląduje między szczytami, gdzie prawie nikt nie jest!",
        tags$br(), tags$br(),
        tags$em("Praktyka: rozdziel grupy i analizuj osobno."))
    } else {
      lc_feedback(type = "warning",
        tags$b("Rozkład wielomodalny: "), "trzy szczyty = trzy podgrupy. ",
        "Każda podgrupa (rowerzyści, pasażerowie autobusów, kierowcy) ",
        "ma własną 'typową' wartość. ",
        tags$br(), tags$br(),
        tags$em("Wielomodalność to jeden z najważniejszych sygnałów w danych -- ",
                "mówi, że patrzenie na całość bez podziału na grupy ",
                "może prowadzić do błędnych wniosków."))
    }
  })

  # Widget 3: Percentile explorer
  # --------------------------------------------------------------------------

  # Quick-select buttons
  observeEvent(input$ch3_q_q1, {
    updateSliderInput(session, "ch3_q_pct", value = 25)
  })

  observeEvent(input$ch3_q_med, {
    updateSliderInput(session, "ch3_q_pct", value = 50)
  })

  observeEvent(input$ch3_q_q3, {
    updateSliderInput(session, "ch3_q_pct", value = 75)
  })

  output$ch3_q_hist <- renderPlot({
    pct <- input$ch3_q_pct / 100
    wzrost <- student_data$wzrost
    q_val <- quantile(wzrost, probs = pct)

    d <- data.frame(x = wzrost)
    d$below <- d$x <= q_val

    ggplot(d, aes(x = x)) +
      geom_histogram(aes(fill = below), color = "white", bins = 25,
                     boundary = q_val, show.legend = FALSE) +
      geom_vline(xintercept = q_val, color = upwr_secondary,
                 linewidth = 1.2, linetype = "solid") +
      annotate("text", x = q_val, y = Inf,
               label = paste0(round(q_val, 1), " cm"),
               vjust = -0.5, hjust = -0.1,
               fontface = "bold", size = 5, color = upwr_secondary) +
      scale_fill_manual(values = c("TRUE" = upwr_cat["niebo"], "FALSE" = upwr_reference)) +
      labs(x = "Wzrost (cm)", y = "Liczba studentow",
           title = paste0(input$ch3_q_pct, ". percentyl wzrostu studentow")) +
            theme(plot.title = element_text(face = "bold"))
  })

  output$ch3_q_box <- renderPlot({
    pct <- input$ch3_q_pct / 100
    wzrost <- student_data$wzrost
    q_val <- quantile(wzrost, probs = pct)

    d <- data.frame(x = wzrost)

    ggplot(d, aes(x = x, y = 0)) +
      geom_boxplot(fill = upwr_rule, color = upwr_secondary,
                   width = 0.5, outlier.alpha = 0.4) +
      geom_point(aes(x = q_val), y = 0,
                 color = upwr_accent, size = 5, shape = 18) +
      annotate("text", x = q_val, y = 0.35,
               label = paste0("P", input$ch3_q_pct),
               fontface = "bold", size = 4.5, color = upwr_accent) +
      labs(x = "Wzrost (cm)", y = NULL) +
            theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
  })

  output$ch3_q_text <- renderUI({
    pct <- input$ch3_q_pct / 100
    wzrost <- student_data$wzrost
    q_val <- round(quantile(wzrost, probs = pct), 1)

    actual_pct <- round(100 * mean(wzrost <= q_val), 1)

    div(style = "font-size: 18px; color: var(--upwr-ink); padding: 10px;",
      paste0(input$ch3_q_pct, "% studentow"),
      paste0(" ma wzrost poniżej ", q_val, " cm."),
      br(),
      tags$span(style = "font-size: 14px; color: var(--upwr-ink-soft);",
        paste0("(Dokladnie ", actual_pct, "% obserwacji <= ", q_val, " cm)"))
    )
  })

  # ========================================================================
  # Widget 4: Guess the statistic game
  # ========================================================================

  ch3_game_data <- reactiveVal(NULL)
  ch3_game_guesses <- reactiveVal(list(mean = NULL, median = NULL))
  ch3_game_revealed <- reactiveVal(FALSE)
  ch3_game_round <- reactiveVal(0)
  ch3_game_score <- reactiveVal(list(total = 0, good = 0))

  generate_game_distribution <- function() {
    type <- sample(c("symmetric", "right_skew", "left_skew"), 1)
    n <- 200
    if (type == "symmetric") {
      vals <- rnorm(n, mean = sample(40:80, 1), sd = sample(8:15, 1))
    } else if (type == "right_skew") {
      vals <- rgamma(n, shape = sample(2:4, 1), scale = sample(5:12, 1)) + sample(10:30, 1)
    } else {
      vals <- 100 - rgamma(n, shape = sample(2:4, 1), scale = sample(5:12, 1))
    }
    round(vals, 1)
  }

  observe({
    if (is.null(ch3_game_data())) {
      ch3_game_data(generate_game_distribution())
    }
  })

  observeEvent(input$ch3_game_new, {
    ch3_game_data(generate_game_distribution())
    ch3_game_guesses(list(mean = NULL, median = NULL))
    ch3_game_revealed(FALSE)
    ch3_game_round(ch3_game_round() + 1)
  })

  observeEvent(input$ch3_game_click, {
    if (ch3_game_revealed()) return()
    g <- ch3_game_guesses()
    if (is.null(g$mean)) {
      g$mean <- input$ch3_game_click$x
    } else if (is.null(g$median)) {
      g$median <- input$ch3_game_click$x
    }
    ch3_game_guesses(g)
  })

  observeEvent(input$ch3_game_reveal, {
    g <- ch3_game_guesses()
    req(g$mean, g$median)
    ch3_game_revealed(TRUE)
    vals <- ch3_game_data()
    real_mean <- mean(vals)
    real_med <- median(vals)
    rng <- diff(range(vals))
    mean_err <- abs(g$mean - real_mean) / rng
    med_err <- abs(g$median - real_med) / rng
    sc <- ch3_game_score()
    sc$total <- sc$total + 1
    if (mean_err < 0.08 && med_err < 0.08) sc$good <- sc$good + 1
    ch3_game_score(sc)
  })

  output$ch3_game_status_banner <- renderUI({
    g <- ch3_game_guesses()
    if (is.null(g$mean)) {
      div(style = "text-align: center; padding: 12px; margin-bottom: 10px;
                    background: var(--upwr-accent-tint); border-radius: 8px; font-size: 18px;
                    font-weight: bold; color: var(--upwr-accent);",
        "↓ Kliknij na wykres, aby postawić ŚREDNIĄ"
      )
    } else if (is.null(g$median)) {
      div(style = "text-align: center; padding: 12px; margin-bottom: 10px;
                    background: var(--upwr-accent-tint); border-radius: 8px; font-size: 18px;
                    font-weight: bold; color: var(--upwr-cat-niebo);",
        "↓ Teraz kliknij, aby postawić MEDIANĘ"
      )
    } else if (!ch3_game_revealed()) {
      div(style = "text-align: center; padding: 12px; margin-bottom: 10px;
                    background: var(--upwr-sage-tint); border-radius: 8px; font-size: 18px;
                    font-weight: bold; color: var(--upwr-cat-szalwia);",
        "Gotowe! Kliknij 'Pokaż odpowiedź'"
      )
    } else {
      sc <- ch3_game_score()
      div(style = "text-align: center; padding: 8px; margin-bottom: 10px;
                    background: var(--upwr-surface-sunken); border-radius: 8px; font-size: 14px;
                    color: var(--upwr-ink);",
        paste0("Wynik: ", sc$good, "/", sc$total, " trafionych rund")
      )
    }
  })

  output$ch3_game_plot <- renderPlot({
    vals <- ch3_game_data()
    req(vals)
    g <- ch3_game_guesses()
    revealed <- ch3_game_revealed()

    p <- ggplot(data.frame(x = vals), aes(x = x)) +
      geom_histogram(bins = 25, fill = "grey70", color = "white", alpha = 0.7) +
            labs(x = "Wartość", y = "Liczebność", title = "Gdzie jest srednia? Gdzie mediana?")

    if (!is.null(g$mean)) {
      p <- p + geom_vline(xintercept = g$mean, color = upwr_accent,
                          linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = g$mean, y = Inf, label = "Twoja\nśrednia",
                 vjust = 2, color = upwr_accent, fontface = "bold", size = 3.5)
    }
    if (!is.null(g$median)) {
      p <- p + geom_vline(xintercept = g$median, color = upwr_cat["niebo"],
                          linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = g$median, y = Inf, label = "Twoja\nmediana",
                 vjust = 3.5, color = upwr_cat["niebo"], fontface = "bold", size = 3.5)
    }

    if (revealed) {
      real_mean <- mean(vals)
      real_med <- median(vals)
      p <- p +
        geom_vline(xintercept = real_mean, color = upwr_accent, linewidth = 1.5) +
        annotate("text", x = real_mean, y = Inf, label = paste0("Srednia\n", round(real_mean, 1)),
                 vjust = 1, color = upwr_accent, fontface = "bold", size = 4) +
        geom_vline(xintercept = real_med, color = upwr_cat["niebo"], linewidth = 1.5) +
        annotate("text", x = real_med, y = Inf, label = paste0("Mediana\n", round(real_med, 1)),
                 vjust = 2.5, color = upwr_cat["niebo"], fontface = "bold", size = 4)
    }

    p
  })

  output$ch3_game_feedback <- renderUI({
    if (!ch3_game_revealed()) return(NULL)
    vals <- ch3_game_data()
    g <- ch3_game_guesses()
    real_mean <- mean(vals)
    real_med <- median(vals)

    mean_err <- round(abs(g$mean - real_mean), 1)
    med_err <- round(abs(g$median - real_med), 1)
    rng <- diff(range(vals))

    overall_err <- (abs(g$mean - real_mean) + abs(g$median - real_med)) / rng
    if (overall_err < 0.08) {
      grade <- "Doskonale!"
      cls <- "info"
    } else if (overall_err < 0.15) {
      grade <- "Nieźle!"
      cls <- "warning"
    } else {
      grade <- "Mozna lepiej!"
      cls <- "danger"
    }

    lc_feedback(type = cls,
      tags$strong(paste0(grade, " ")),
      paste0("Błąd średniej: ", mean_err, ", błąd mediany: ", med_err, ".")
    )
  })

}
