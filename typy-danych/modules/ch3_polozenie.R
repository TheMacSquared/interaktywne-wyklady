# ============================================================================
# CHAPTER 3: Statystyki położenia
# ============================================================================

ch3_ui <- tabPanel("3. Statystyki polozenia",
  fluidRow(column(8, offset = 2,

    # --- Introduction ---
    div(class = "chapter-recap",
      "Zmienne jakościowe podsumowaliśmy tabelami częstości i wykresami słupkowymi.
       Teraz przechodzimy do zmiennych ilościowych -- zaczynajac od pytan o 'środek' danych."
    ),
    uiOutput("tracker_ch3"),
    div(class = "section-title", "Statystyki polozenia"),

    div(class = "narrative",
      p("Zmienne ilo\u015bciowe wymagaj\u0105 nowych narz\u0119dzi. Zanim przejdziemy do
        statystyk, poznajmy podstawow\u0105 wizualizacj\u0119 \u2014 ", tags$b("histogram"),
        ". Potem zbadamy miary po\u0142o\u017cenia: ", tags$b("\u015bredni\u0105"), ", ",
        tags$b("median\u0119"), " i ", tags$b("percentyle"), ".")
    ),

    # ========================================================================
    # WIDGET: Histogram krok po kroku
    # ========================================================================
    div(class = "section-title", "Histogram \u2014 krok po kroku"),

    div(class = "narrative",
      p("Histogram to podstawowy wykres dla zmiennych ci\u0105g\u0142ych. Pokazuje
        jak cz\u0119sto wyst\u0119puj\u0105 warto\u015bci w poszczeg\u00f3lnych ",
        tags$b("przedzia\u0142ach (binach)"),
        ". Zbudujmy go krok po kroku.")
    ),

    div(class = "widget-block",
      h4("Budowa histogramu"),
      fluidRow(
        column(4,
          selectInput("ch3_hist_var", "Zmienna:",
            choices = c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                        "Czas dojazdu (min)" = "czas_dojazdu",
                        "\u015arednia ocen" = "srednia_ocen"),
            selected = "wzrost"
          ),
          uiOutput("ch3_hist_bin_slider"),
          actionButton("ch3_hist_step1", "1. Surowe dane",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step2", "2. Posortuj dane",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step3", "3. Podziel na przedzia\u0142y",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step4", "4. Przypisz do bin\u00f3w",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step5", "5. Zlicz obserwacje",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step6", "6. Zbuduj s\u0142upki",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step7", "7. Gotowy histogram",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_step8", "8. Wp\u0142yw szeroko\u015bci binu",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch3_hist_reset", "Reset",
                       class = "btn-outline-secondary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_hist_plot", height = "400px"),
          uiOutput("ch3_hist_text"),
          tableOutput("ch3_hist_table")
        )
      )
    ),

    # ========================================================================
    # Transition to location statistics
    # ========================================================================
    div(class = "section-title", "Statystyki po\u0142o\u017cenia"),

    div(class = "narrative",
      p("Histogram pokazuje kszta\u0142t rozk\u0142adu, ale nie daje jednej liczby
        opisuj\u0105cej '\u015brodek'. Do tego s\u0142u\u017c\u0105 statystyki po\u0142o\u017cenia:
        ", tags$b("\u015brednia"), ", ", tags$b("mediana"), " i ",
        tags$b("percentyle"), ". Ka\u017cda odpowiada na to pytanie inaczej.")
    ),

    # ========================================================================
    # WIDGET 0a: Mean introduction
    # ========================================================================
    div(class = "section-title", "\u015arednia arytmetyczna"),

    div(class = "narrative",
      p("\u015arednia arytmetyczna to suma wszystkich warto\u015bci podzielona
        przez ich liczb\u0119. Jest to 'punkt r\u00f3wnowagi' danych -- gdyby\u015bmy
        po\u0142o\u017cyli dane na wadze, \u015brednia by\u0142aby punktem podparcia."),
      withMathJax(helpText(
        "$$\\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i = \\frac{x_1 + x_2 + \\ldots + x_n}{n}$$"
      ))
    ),

    div(class = "widget-block",
      h4("\u015arednia jako punkt r\u00f3wnowagi"),
      selectInput("ch3_mean_var", "Zmienna:",
        choices = c("Wzrost (cm)" = "wzrost", "Waga (kg)" = "waga",
                    "\u015arednia ocen" = "srednia_ocen"),
        selected = "wzrost"
      ),
      plotOutput("ch3_mean_plot", height = "300px"),
      uiOutput("ch3_mean_text")
    ),

    # ========================================================================
    # WIDGET 0b: Median introduction
    # ========================================================================
    div(class = "section-title", "Mediana"),

    div(class = "narrative",
      p("Mediana to warto\u015b\u0107, kt\u00f3ra dzieli posortowane dane na dwie
        r\u00f3wne po\u0142owy: 50% obserwacji le\u017cy poni\u017cej, 50% powy\u017cej.
        Nie zale\u017cy od tego, jak bardzo skrajne s\u0105 warto\u015bci
        na ko\u0144cach -- liczy si\u0119 tylko pozycja \u015brodkowa.")
    ),

    div(class = "widget-block",
      h4("Mediana dzieli dane na p\u00f3\u0142"),
      selectInput("ch3_median_var", "Zmienna:",
        choices = c("Wzrost (cm)" = "wzrost", "Czas dojazdu (min)" = "czas_dojazdu",
                    "\u015arednia ocen" = "srednia_ocen"),
        selected = "czas_dojazdu"
      ),
      plotOutput("ch3_median_plot", height = "300px"),
      uiOutput("ch3_median_text")
    ),

    # ========================================================================
    # WIDGET 1: Mean vs Median -- comparison
    # ========================================================================
    div(class = "section-title", "\u015arednia vs mediana -- kiedy si\u0119 r\u00f3\u017cni\u0105?"),

    div(class = "narrative",
      p("Dla danych symetrycznych \u015brednia i mediana s\u0105 blisko siebie.
        Ale co si\u0119 dzieje, gdy rozk\u0142ad jest sko\u015bny lub pojawi si\u0119
        warto\u015b\u0107 odstaj\u0105ca?"),
      p("Wyobra\u017amy sobie zarobki w pewnej firmie. Wi\u0119kszo\u015b\u0107 pracownik\u00f3w
        zarabia umiarkowanie, ale s\u0105 te\u017c osoby z bardzo wysokimi pensjami.
        Zobaczmy, jak \u015brednia i mediana reaguj\u0105 na nowe warto\u015bci.")
    ),

    div(class = "widget-block",
      h4("Zarobki w firmie: \u015brednia vs mediana"),

      fluidRow(
        column(5,
          sliderInput("ch3_svm_new_value", "Nowa warto\u015b\u0107:",
                      min = 2000, max = 25000, value = 5000, step = 500,
                      pre = "", post = " z\u0142", width = "100%")
        ),
        column(7,
          div(style = "display: flex; gap: 8px; margin-top: 25px;",
            actionButton("ch3_svm_add", "Dodaj warto\u015b\u0107",
                         class = "btn-primary"),
            actionButton("ch3_svm_outlier", "Dodaj outlier (CEO)",
                         class = "btn-danger"),
            actionButton("ch3_svm_reset", "Reset",
                         class = "btn-default")
          )
        )
      ),

      hr(),

      plotOutput("ch3_svm_hist", height = "280px"),
      plotOutput("ch3_svm_strip", height = "120px"),

      div(style = "text-align: center; margin-top: 10px;",
        uiOutput("ch3_svm_stats")
      )
    ),

    div(class = "callout-info",
      tags$strong("Obserwacja:"),
      " Dodaj kilka 'normalnych' zarobk\u00f3w -- \u015brednia i mediana b\u0119d\u0105 blisko
        siebie. Teraz kliknij 'Dodaj outlier (CEO)' -- zobacz, jak \u015brednia
        skacze w g\u00f3r\u0119, a mediana prawie si\u0119 nie zmienia!"
    ),

    # ========================================================================
    # WIDGET 2: Robustness mini-demo
    # ========================================================================
    div(class = "section-title", "Odporno\u015b\u0107 miar na outliery"),

    div(class = "narrative",
      p("Kt\u00f3ra statystyka jest bardziej odporna na outliery? \u015arednia
        arytmetyczna bierze pod uwag\u0119 ka\u017cd\u0105 warto\u015b\u0107 -- wi\u0119c jedna
        ekstremalna obserwacja mo\u017ce j\u0105 znacz\u0105co przesun\u0105\u0107. Mediana
        ignoruje skrajne warto\u015bci, patrz\u0105c tylko na '\u015brodek' danych."),
      p("\u015arednia ucinana (trimmed mean) to kompromis: odrzuca pewien
        procent najbardziej skrajnych obserwacji z obu stron, a nast\u0119pnie
        oblicza \u015bredni\u0105 z pozosta\u0142ych. Dodajmy kilka ekstremalnych
        zarobk\u00f3w i zobaczmy, co si\u0119 stanie.")
    ),

    div(class = "widget-block",
      h4("Odporność: średnia vs mediana vs średnia ucinana"),

      div(style = "display: flex; gap: 8px; margin-bottom: 15px;",
        actionButton("ch3_rob_add1", "Dodaj outlier (+50 000 zl)",
                     class = "btn-warning"),
        actionButton("ch3_rob_add5", "Dodaj 5 outlierow",
                     class = "btn-danger"),
        actionButton("ch3_rob_reset", "Reset",
                     class = "btn-default")
      ),

      plotOutput("ch3_rob_plot", height = "320px"),

      div(style = "margin-top: 15px;",
        tableOutput("ch3_rob_table")
      )
    ),

    div(class = "callout-warning",
      tags$strong("Wniosek:"),
      " Średnia arytmetyczna jest bardzo wrażliwa na wartości odstające.
        Mediana jest najbardziej odporna. Średnia ucinana oferuje
        kompromis - jest mniej wrażliwa niż średnia, ale bardziej niż mediana.
        Dlatego przy skośnych rozkładach (np. zarobki) mediana jest często
        lepsza miara 'typowej' wartości."
    ),

    # ========================================================================
    # WIDGET 2b: Discrete variables
    # ========================================================================
    div(class = "section-title", "Zmienne dyskretne -- te same statystyki, inne wykresy"),

    div(class = "narrative",
      p("Dotychczas uzywalismy zmiennych ciągłych (wzrost, zarobki). Ale co ze
        zmiennymi ", tags$b("dyskretnymi"), " -- takimi jak liczba kursow czy
        liczba nieobecnosci? Statystyki polozenia (średnia, mediana) obliczamy
        tak samo, ale ", tags$b("wizualizacja"), " wymaga uwagi.")
    ),

    div(class = "widget-block",
      h4("Dyskretna vs ciągła -- porównanie wizualizacji"),
      selectInput("ch3_disc_var", "Wybierz zmienna dyskretna:",
        choices = c("Liczba nieobecności" = "liczba_nieobecnosci",
                    "Liczba kursów" = "liczba_kursow"),
        selected = "liczba_nieobecnosci"
      ),
      fluidRow(
        column(6,
          h5(style = "text-align: center; color: #27ae60;", "Wykres słupkowy (poprawny)"),
          plotOutput("ch3_disc_bar", height = "300px")
        ),
        column(6,
          h5(style = "text-align: center; color: #e74c3c;", "Histogram (problematyczny)"),
          plotOutput("ch3_disc_hist", height = "300px")
        )
      ),
      tableOutput("ch3_disc_stats"),
      uiOutput("ch3_disc_explanation")
    ),

    # ========================================================================
    # WIDGET 2c: Multimodality in continuous distributions
    # ========================================================================
    div(class = "section-title", "Modalno\u015b\u0107 rozk\u0142adu -- ile 'g\u00f3rek' ma histogram?"),

    div(class = "narrative",
      p("W rozdziale o zmiennych jako\u015bciowych poznali\u015bmy dominant\u0119 -- najcz\u0119stsz\u0105
        kategori\u0119. Dla danych ci\u0105g\u0142ych dominanta pojedynczej warto\u015bci nie ma sensu
        (prawie ka\u017cda warto\u015b\u0107 jest unikatowa). Ale poj\u0119cie ",
        tags$b("mody"), " dzia\u0142a na ", tags$b("przedzia\u0142ach"),
        " -- szukamy, kt\u00f3ry bin histogramu jest najwy\u017cszy."),
      p("Co wa\u017cniejsze, rozk\u0142ad mo\u017ce mie\u0107 ",
        tags$b("wi\u0119cej ni\u017c jeden szczyt"), " (mod\u0119). To cz\u0119sto
        sygna\u0142, \u017ce dane pochodz\u0105 z kilku r\u00f3\u017cnych grup.")
    ),

    div(class = "widget-block",
      h4("Unimodalny vs bimodalny vs wielomodalny"),
      radioButtons("ch3_modal_scenario", "Scenariusz:",
        choices = c(
          "Unimodalny -- wzrost kobiet" = "unimodal",
          "Bimodalny -- wzrost (kobiety + m\u0119\u017cczy\u017ani)" = "bimodal",
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
    div(class = "section-title", "Percentyle i kwantyle"),

    div(class = "narrative",
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

    div(class = "widget-block",
      h4("Explorer percentyli: wzrost studentow"),

      fluidRow(
        column(6,
          sliderInput("ch3_q_pct", "Percentyl:",
                      min = 0, max = 100, value = 50, step = 1,
                      post = "%", width = "100%")
        ),
        column(6,
          div(style = "display: flex; gap: 8px; margin-top: 25px;",
            actionButton("ch3_q_q1", "Q1 (25%)", class = "btn-outline-primary"),
            actionButton("ch3_q_med", "Mediana (50%)", class = "btn-outline-primary"),
            actionButton("ch3_q_q3", "Q3 (75%)", class = "btn-outline-primary")
          )
        )
      ),

      hr(),

      plotOutput("ch3_q_hist", height = "280px"),
      plotOutput("ch3_q_box", height = "120px"),

      div(style = "text-align: center; margin-top: 10px;",
        uiOutput("ch3_q_text")
      )
    ),

    # ====================================================================
    # WIDGET 4: Guess the statistic game
    # ====================================================================
    div(class = "section-title", "Gra: Zgadnij średnia i mediane!"),

    div(class = "narrative",
      p("Sprawdzmy Twoją intuicję! Na histogramie zobaczysz rozkład danych.
        Kliknij na wykres, aby postawić swój typ: najpierw ", tags$b("średnia"),
        ", potem ", tags$b("mediana"), ". Czy potrafisz je odroznic?")
    ),

    div(class = "widget-block",
      h4("Kliknij na wykres, aby umie\u015bci\u0107 \u015bredni\u0105 i median\u0119"),
      div(style = "margin-bottom: 10px;",
        actionButton("ch3_game_new", "Nowa runda",
                     class = "btn-primary", style = "margin-right: 6px;"),
        actionButton("ch3_game_reveal", "Poka\u017c odpowied\u017a",
                     class = "btn-success", style = "margin-right: 6px;")
      ),
      uiOutput("ch3_game_status_banner"),
      plotOutput("ch3_game_plot", height = "350px", click = "ch3_game_click"),
      uiOutput("ch3_game_feedback")
    ),

    div(class = "callout-info",
      tags$strong("Rozstęp międzykwartylowy (IQR):"),
      " Roznica miedzy Q3 a Q1 to IQR - miara rozrzutu, która jest odporna
        na outliery. Boxplot (wykres pudełkowy) uzywa wlasnie kwartyli
        do wizualizacji rozkładu danych. Więcej o tym w kolejnym rozdziale!"
    ),

    div(class = "chapter-transition",
      p("Wiemy gdzie jest 'środek' danych. Ale dwie grupy z ta sama srednia
        mogą wyglądać zupełnie inaczej -- rozni je rozrzut. Jak go mierzyc i wizualizować?"),
      actionButton("ch3_next", "Dalej: 4. Statystyki rozrzutu \u2192",
                   class = "btn-primary btn-lg")
    ),

    # Spacer at bottom
    div(style = "height: 60px;")

  ))
) # end ch3 tabPanel

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
                "Szeroko\u015b\u0107 binu:",
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
    "srednia_ocen" = "\u015arednia ocen"
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

    strip_theme <- theme_minimal(base_size = 14) +
      theme(axis.text.y = element_blank(),
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
        geom_jitter(height = 0.3, size = 3, alpha = 0.6, color = "#3498db") +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.5))

    } else if (step == 2) {
      df <- data.frame(value = sort(x))
      ggplot(df, aes(x = value, y = 0)) +
        geom_point(size = 3, alpha = 0.7, color = "#27ae60") +
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
                  fill = NA, color = "#2c3e50", linewidth = 0.8,
                  linetype = "dashed") +
        geom_point(data = df, aes(x = value, y = 0),
                   size = 2.5, alpha = 0.5, color = "#95a5a6") +
        geom_text(data = bin_rects,
                  aes(x = (xmin + xmax) / 2, y = -0.45,
                      label = paste0("[", xmin, ", ", xmax, ")")),
                  size = 2.8, color = "#2c3e50") +
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
                  alpha = 0.15, color = "#2c3e50", linewidth = 0.5) +
        geom_jitter(data = df %>% filter(!is.na(bin)),
                    aes(x = value, y = 0, color = factor(bin_num)),
                    height = 0.2, size = 3, alpha = 0.8) +
        scale_fill_viridis_d(guide = "none") +
        scale_color_viridis_d(guide = "none") +
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
                  alpha = 0.15, color = "#2c3e50", linewidth = 0.5) +
        geom_jitter(data = df %>% filter(!is.na(bin)),
                    aes(x = value, y = 0, color = factor(bin_num)),
                    height = 0.2, size = 2, alpha = 0.6) +
        geom_text(data = stats,
                  aes(x = bin_mid, y = 0.45,
                      label = ifelse(count > 0, paste0("n=", count), "")),
                  size = 4, fontface = "bold", color = "#2c3e50") +
        scale_fill_viridis_d(guide = "none") +
        scale_color_viridis_d(guide = "none") +
        labs(x = x_label, y = "") + strip_theme +
        coord_cartesian(xlim = c(x_lo, x_hi), ylim = c(-0.5, 0.6))

    } else if (step == 6) {
      stats <- ch3_hist_stats()
      w <- input$ch3_hist_bin_width

      ggplot(stats, aes(x = bin_mid, y = count)) +
        geom_col(aes(fill = factor(bin_num)),
                 width = w * 0.95, alpha = 0.7,
                 color = "#2c3e50", linewidth = 0.3) +
        geom_text(aes(label = count), vjust = -0.5, size = 4, fontface = "bold") +
        scale_fill_viridis_d(guide = "none") +
        labs(x = x_label, y = "Liczba obserwacji") +
        theme_minimal(base_size = 14) +
        coord_cartesian(xlim = c(x_lo, x_hi))

    } else if (step == 7) {
      df <- data.frame(value = x)
      w <- input$ch3_hist_bin_width

      ggplot(df, aes(x = value)) +
        geom_histogram(binwidth = w, fill = "#3498db", alpha = 0.7,
                       color = "#2c3e50", linewidth = 0.3) +
        labs(x = x_label, y = "Liczba obserwacji",
             title = paste0("Histogram: ", x_label),
             subtitle = paste0("n = ", n, " | szeroko\u015b\u0107 binu = ", w)) +
        theme_minimal(base_size = 14)

    } else if (step == 8) {
      df <- data.frame(value = x)
      w <- input$ch3_hist_bin_width
      widths <- c(w / 2, w, w * 2)
      unit <- ch3_hist_defaults[[var_name]]$unit
      labels <- paste0("Bin = ", widths, " ", unit)

      plots <- lapply(seq_along(widths), function(i) {
        ggplot(df, aes(x = value)) +
          geom_histogram(binwidth = widths[i],
                         fill = c("#e74c3c", "#3498db", "#27ae60")[i],
                         alpha = 0.7, color = "#2c3e50", linewidth = 0.3) +
          labs(x = if (i == 2) x_label else "",
               y = if (i == 1) "Liczba obs." else "",
               title = labels[i]) +
          theme_minimal(base_size = 11) +
          theme(plot.title = element_text(
            size = 12, face = "bold",
            color = c("#e74c3c", "#3498db", "#27ae60")[i]))
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
      "0" = "Kliknij Krok 1, aby rozpocz\u0105\u0107 budow\u0119 histogramu.",
      "1" = paste0("Mamy ", n, " obserwacji \u2014 ka\u017cdy punkt to jedna warto\u015b\u0107. ",
                   "Trudno z tego odczyta\u0107 rozk\u0142ad, prawda?"),
      "2" = paste0("Sortujemy od min = ", round(min(x), 1),
                   " do max = ", round(max(x), 1), " ", unit,
                   ". Wida\u0107 zag\u0119szczenia, ale wci\u0105\u017c nieczytelne."),
      "3" = paste0("Dzielimy o\u015b na r\u00f3wne przedzia\u0142y (biny) o szeroko\u015bci ",
                   input$ch3_hist_bin_width, " ", unit,
                   ". Ka\u017cdy bin to 'koszyk' na obserwacje."),
      "4" = "Ka\u017cda obserwacja trafia do swojego binu \u2014 kolor = przynale\u017cno\u015b\u0107.",
      "5" = "Liczymy obserwacje w ka\u017cdym binie. Te liczby stan\u0105 si\u0119 wysoko\u015bci\u0105 s\u0142upk\u00f3w.",
      "6" = "Zamieniamy punkty na s\u0142upki \u2014 wysoko\u015b\u0107 = liczba obserwacji. To ju\u017c prawie histogram!",
      "7" = paste0("Gotowy histogram (n = ", n, ", bin = ", input$ch3_hist_bin_width,
                   " ", unit, "). Spr\u00f3buj zmieni\u0107 szeroko\u015b\u0107 binu suwakiem!"),
      "8" = paste0("Te same dane z trzema szeroko\u015bciami binu. ",
                   "Za w\u0105skie \u2192 szum. Za szerokie \u2192 utrata szczeg\u00f3\u0142\u00f3w.")
    )
    div(class = "callout-info", p(txt))
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
    names(out) <- c("Przedzia\u0142", "Liczba obs.", "Procent")
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
                    "srednia_ocen" = "\u015arednia ocen")
    df <- data.frame(val = x)

    ggplot(df, aes(x = val)) +
      geom_histogram(bins = 25, fill = "#d5d8dc", color = "white", alpha = 0.8) +
      geom_vline(xintercept = m, color = "#e74c3c", linewidth = 1.5, linetype = "solid") +
      annotate("text", x = m, y = Inf, label = paste0("\u015arednia = ", round(m, 2)),
               vjust = 2, hjust = -0.1, color = "#e74c3c", size = 5, fontface = "bold") +
      annotate("segment", x = min(x), xend = m, y = -0.5, yend = -0.5,
               color = "#3498db", linewidth = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
      annotate("segment", x = max(x), xend = m, y = -0.5, yend = -0.5,
               color = "#3498db", linewidth = 2,
               arrow = arrow(length = unit(0.2, "cm"), ends = "last")) +
      labs(x = var_labels[var_name], y = "Liczebno\u015b\u0107",
           title = "\u015arednia jako punkt r\u00f3wnowagi") +
      theme_minimal(base_size = 14)
  })

  output$ch3_mean_text <- renderUI({
    var_name <- input$ch3_mean_var
    req(var_name)
    x <- student_data[[var_name]]
    m <- mean(x)
    s <- sum(x)
    n <- length(x)
    div(class = "callout-info",
      withMathJax(paste0(
        "$$\\bar{x} = \\frac{", round(s, 1), "}{", n, "} = ", round(m, 2), "$$"
      )),
      tags$em("\u015arednia uwzgl\u0119dnia ka\u017cd\u0105 warto\u015b\u0107 -- jest wra\u017cliwa
              na warto\u015bci skrajne, bo przeci\u0105ga j\u0105 w ich stron\u0119.")
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
                    "srednia_ocen" = "\u015arednia ocen")
    df <- data.frame(val = x)

    ggplot(df, aes(x = val)) +
      geom_histogram(bins = 25, fill = "#d5d8dc", color = "white", alpha = 0.8) +
      geom_vline(xintercept = med, color = "#2980b9", linewidth = 1.5) +
      annotate("rect", xmin = min(x) - 1, xmax = med, ymin = -Inf, ymax = Inf,
               fill = "#3498db", alpha = 0.08) +
      annotate("rect", xmin = med, xmax = max(x) + 1, ymin = -Inf, ymax = Inf,
               fill = "#e74c3c", alpha = 0.08) +
      annotate("text", x = (min(x) + med) / 2, y = Inf,
               label = paste0("50% (", n_below, " obs.)"),
               vjust = 2, color = "#2c3e50", size = 5, fontface = "bold") +
      annotate("text", x = (max(x) + med) / 2, y = Inf,
               label = paste0("50% (", n_above, " obs.)"),
               vjust = 2, color = "#2c3e50", size = 5, fontface = "bold") +
      annotate("text", x = med, y = Inf, label = paste0("Me = ", round(med, 1)),
               vjust = 4, hjust = -0.1, color = "#2980b9", size = 5, fontface = "bold") +
      geom_histogram(bins = 25, fill = "#d5d8dc", color = "white", alpha = 0.8) +
      geom_vline(xintercept = med, color = "#2980b9", linewidth = 1.5) +
      labs(x = var_labels[var_name], y = "Liczebno\u015b\u0107",
           title = "Mediana dzieli dane na dwie r\u00f3wne po\u0142owy") +
      theme_minimal(base_size = 14)
  })

  output$ch3_median_text <- renderUI({
    var_name <- input$ch3_median_var
    req(var_name)
    x <- student_data[[var_name]]
    med <- median(x)
    m <- mean(x)
    diff <- abs(m - med)

    div(class = "callout-info",
      tags$b("Mediana = ", round(med, 1)),
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
      geom_histogram(fill = "#bdc3c7", color = "white", bins = 25) +
      geom_vline(aes(xintercept = m, color = "Srednia"),
                 linewidth = 1.2, linetype = "solid") +
      geom_vline(aes(xintercept = med, color = "Mediana"),
                 linewidth = 1.2, linetype = "dashed") +
      scale_color_manual(
        name = NULL,
        breaks = c("Srednia", "Mediana"),
        values = c("Srednia" = "#e74c3c", "Mediana" = "#3498db")
      ) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zl)", y = "Liczba osob",
           title = "Rozkład zarobkow") +
      theme_minimal(base_size = 14) +
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
                  alpha = 0.6, color = "#2c3e50") +
      geom_point(aes(x = m), y = 0, color = "#e74c3c",
                 size = 5, shape = 18) +
      geom_point(aes(x = med), y = 0, color = "#3498db",
                 size = 5, shape = 18) +
      scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
      labs(x = "Zarobki (zl)", y = NULL) +
      theme_minimal(base_size = 14) +
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

    diff_color <- if (abs(diff_val) < 500) "#27ae60" else "#f39c12"

    tagList(
      div(class = "stat-box", style = "background: #e74c3c;",
          paste0("Srednia: ", format(round(m), big.mark = " "), " zl")),
      div(class = "stat-box", style = "background: #3498db;",
          paste0("Mediana: ", format(round(med), big.mark = " "), " zl")),
      div(class = "stat-box", style = paste0("background: ", diff_color, ";"),
          paste0("Roznica: ", format(round(diff_val), big.mark = " "), " zl"))
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
      geom_histogram(fill = "#bdc3c7", color = "white", bins = 30) +
      geom_vline(data = line_data,
                 aes(xintercept = xval, color = Statystyka,
                     linetype = Statystyka),
                 linewidth = 1.2) +
      scale_color_manual(
        name = NULL,
        breaks = c("Srednia", "Mediana", "Sr. ucinana (10%)"),
        values = c("Srednia" = "#e74c3c",
                   "Mediana" = "#3498db",
                   "Sr. ucinana (10%)" = "#27ae60")
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
      theme_minimal(base_size = 14) +
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
      geom_bar(fill = col_discrete, color = "white", alpha = 0.85) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 4) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = variable_meta[[var_name]]$label, y = "Liczebność") +
      theme_minimal(base_size = 14)
  })

  output$ch3_disc_hist <- renderPlot({
    var_name <- input$ch3_disc_var
    req(var_name)
    vals <- student_data[[var_name]]

    ggplot(data.frame(x = vals), aes(x = x)) +
      geom_histogram(bins = 15, fill = "#e74c3c", color = "white", alpha = 0.6) +
      labs(x = variable_meta[[var_name]]$label, y = "Liczebność") +
      theme_minimal(base_size = 14)
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
    div(class = "callout-info",
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
                       fill = "#3498db", color = "white", alpha = 0.7) +
        geom_density(linewidth = 1.2, color = "#2c3e50") +
        geom_vline(xintercept = mean(x), color = "#e74c3c", linewidth = 1, linetype = "dashed") +
        annotate("text", x = mean(x) + 1, y = Inf, label = "moda \u2248 \u015brednia \u2248 mediana",
                 hjust = 0, vjust = 2, color = "#e74c3c", size = 4.5, fontface = "bold") +
        labs(title = "Unimodalny -- jeden wyra\u017any szczyt",
             x = "Wzrost kobiet (cm)", y = "G\u0119sto\u015b\u0107") +
        theme_minimal(base_size = 14)

    } else if (scenario == "bimodal") {
      x_k <- rnorm(250, mean = 162, sd = 5)
      x_m <- rnorm(250, mean = 182, sd = 5)
      df <- data.frame(val = c(x_k, x_m),
                       grupa = rep(c("Kobiety", "M\u0119\u017cczy\u017ani"), each = 250))
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 35,
                       fill = "#95a5a6", color = "white", alpha = 0.5) +
        geom_density(linewidth = 1.2, color = "#2c3e50") +
        geom_density(aes(color = grupa), linewidth = 0.8, linetype = "dashed") +
        scale_color_manual(values = c("Kobiety" = "#e74c3c", "M\u0119\u017cczy\u017ani" = "#3498db")) +
        labs(title = "Bimodalny -- dwa szczyty (dwie grupy!)",
             x = "Wzrost (cm)", y = "G\u0119sto\u015b\u0107", color = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")

    } else {
      x1 <- rnorm(150, mean = 12, sd = 3)
      x2 <- rnorm(120, mean = 25, sd = 4)
      x3 <- rnorm(130, mean = 40, sd = 5)
      df <- data.frame(val = c(x1, x2, x3),
                       grupa = c(rep("Rower", 150), rep("Autobus", 120), rep("Auto", 130)))
      ggplot(df, aes(x = val)) +
        geom_histogram(aes(y = after_stat(density)), bins = 40,
                       fill = "#95a5a6", color = "white", alpha = 0.5) +
        geom_density(linewidth = 1.2, color = "#2c3e50") +
        geom_density(aes(color = grupa), linewidth = 0.8, linetype = "dashed") +
        scale_color_manual(values = c("Rower" = "#27ae60", "Autobus" = "#f39c12", "Auto" = "#e74c3c")) +
        labs(title = "Wielomodalny -- trzy szczyty (trzy \u015brodki transportu)",
             x = "Czas dojazdu (min)", y = "G\u0119sto\u015b\u0107", color = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")
    }
  })

  output$ch3_modal_text <- renderUI({
    scenario <- input$ch3_modal_scenario
    req(scenario)

    if (scenario == "unimodal") {
      div(class = "callout-info",
        tags$b("Rozk\u0142ad unimodalny: "), "jeden szczyt, jedna 'g\u00f3rka'. ",
        "Dla rozk\u0142adu symetrycznego moda \u2248 \u015brednia \u2248 mediana. ",
        "Wi\u0119kszo\u015b\u0107 statystyk opisowych zak\u0142ada w\u0142a\u015bnie taki rozk\u0142ad.")
    } else if (scenario == "bimodal") {
      div(class = "callout-warning",
        tags$b("Rozk\u0142ad bimodalny: "), "dwa szczyty! ",
        "To sygna\u0142, \u017ce dane prawdopodobnie pochodz\u0105 z ",
        tags$b("dw\u00f3ch r\u00f3\u017cnych grup"), ". ",
        "Podawanie jednej \u015bredniej dla ca\u0142o\u015bci jest mylace -- ",
        "\u015brednia wyl\u0105duje mi\u0119dzy szczytami, gdzie prawie nikt nie jest!",
        tags$br(), tags$br(),
        tags$em("Praktyka: rozdziel grupy i analizuj osobno."))
    } else {
      div(class = "callout-warning",
        tags$b("Rozk\u0142ad wielomodalny: "), "trzy szczyty = trzy podgrupy. ",
        "Ka\u017cda podgrupa (rowerzy\u015bci, pasa\u017cerowie autobus\u00f3w, kierowcy) ",
        "ma w\u0142asn\u0105 'typow\u0105' warto\u015b\u0107. ",
        tags$br(), tags$br(),
        tags$em("Wielomodalno\u015b\u0107 to jeden z najwa\u017cniejszych sygna\u0142\u00f3w w danych -- ",
                "m\u00f3wi, \u017ce patrzenie na ca\u0142o\u015b\u0107 bez podzia\u0142u na grupy ",
                "mo\u017ce prowadzi\u0107 do b\u0142\u0119dnych wniosk\u00f3w."))
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
      geom_vline(xintercept = q_val, color = "#2c3e50",
                 linewidth = 1.2, linetype = "solid") +
      annotate("text", x = q_val, y = Inf,
               label = paste0(round(q_val, 1), " cm"),
               vjust = -0.5, hjust = -0.1,
               fontface = "bold", size = 5, color = "#2c3e50") +
      scale_fill_manual(values = c("TRUE" = "#3498db", "FALSE" = "#bdc3c7")) +
      labs(x = "Wzrost (cm)", y = "Liczba studentow",
           title = paste0(input$ch3_q_pct, ". percentyl wzrostu studentow")) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })

  output$ch3_q_box <- renderPlot({
    pct <- input$ch3_q_pct / 100
    wzrost <- student_data$wzrost
    q_val <- quantile(wzrost, probs = pct)

    d <- data.frame(x = wzrost)

    ggplot(d, aes(x = x, y = 0)) +
      geom_boxplot(fill = "#ecf0f1", color = "#2c3e50",
                   width = 0.5, outlier.alpha = 0.4) +
      geom_point(aes(x = q_val), y = 0,
                 color = "#e74c3c", size = 5, shape = 18) +
      annotate("text", x = q_val, y = 0.35,
               label = paste0("P", input$ch3_q_pct),
               fontface = "bold", size = 4.5, color = "#e74c3c") +
      labs(x = "Wzrost (cm)", y = NULL) +
      theme_minimal(base_size = 14) +
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

    div(style = "font-size: 18px; color: #2c3e50; padding: 10px;",
      tags$strong(paste0(input$ch3_q_pct, "% studentow")),
      paste0(" ma wzrost poniżej ", q_val, " cm."),
      br(),
      tags$span(style = "font-size: 14px; color: #7f8c8d;",
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
                    background: #fdedec; border-radius: 8px; font-size: 18px;
                    font-weight: bold; color: #e74c3c;",
        "\u2193 Kliknij na wykres, aby postawi\u0107 \u015aREDNI\u0104"
      )
    } else if (is.null(g$median)) {
      div(style = "text-align: center; padding: 12px; margin-bottom: 10px;
                    background: #eaf4fc; border-radius: 8px; font-size: 18px;
                    font-weight: bold; color: #3498db;",
        "\u2193 Teraz kliknij, aby postawi\u0107 MEDIAN\u0118"
      )
    } else if (!ch3_game_revealed()) {
      div(style = "text-align: center; padding: 12px; margin-bottom: 10px;
                    background: #eafaf1; border-radius: 8px; font-size: 18px;
                    font-weight: bold; color: #27ae60;",
        "Gotowe! Kliknij 'Poka\u017c odpowied\u017a'"
      )
    } else {
      sc <- ch3_game_score()
      div(style = "text-align: center; padding: 8px; margin-bottom: 10px;
                    background: #f8f9fa; border-radius: 8px; font-size: 14px;
                    color: #2c3e50;",
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
      theme_minimal(base_size = 14) +
      labs(x = "Wartość", y = "Liczebność", title = "Gdzie jest srednia? Gdzie mediana?")

    if (!is.null(g$mean)) {
      p <- p + geom_vline(xintercept = g$mean, color = "#e74c3c",
                          linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = g$mean, y = Inf, label = "Twoja\nśrednia",
                 vjust = 2, color = "#e74c3c", fontface = "bold", size = 3.5)
    }
    if (!is.null(g$median)) {
      p <- p + geom_vline(xintercept = g$median, color = "#3498db",
                          linewidth = 1.2, linetype = "dashed") +
        annotate("text", x = g$median, y = Inf, label = "Twoja\nmediana",
                 vjust = 3.5, color = "#3498db", fontface = "bold", size = 3.5)
    }

    if (revealed) {
      real_mean <- mean(vals)
      real_med <- median(vals)
      p <- p +
        geom_vline(xintercept = real_mean, color = "#e74c3c", linewidth = 1.5) +
        annotate("text", x = real_mean, y = Inf, label = paste0("Srednia\n", round(real_mean, 1)),
                 vjust = 1, color = "#e74c3c", fontface = "bold", size = 4) +
        geom_vline(xintercept = real_med, color = "#3498db", linewidth = 1.5) +
        annotate("text", x = real_med, y = Inf, label = paste0("Mediana\n", round(real_med, 1)),
                 vjust = 2.5, color = "#3498db", fontface = "bold", size = 4)
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
      cls <- "callout-info"
    } else if (overall_err < 0.15) {
      grade <- "Nieźle!"
      cls <- "callout-warning"
    } else {
      grade <- "Mozna lepiej!"
      cls <- "callout-danger"
    }

    div(class = cls,
      tags$strong(paste0(grade, " ")),
      paste0("Błąd średniej: ", mean_err, ", błąd mediany: ", med_err, ".")
    )
  })

}
