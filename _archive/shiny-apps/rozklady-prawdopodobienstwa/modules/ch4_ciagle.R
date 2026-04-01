# ============================================================================
# CHAPTER 4: Rozklady ciagle
# ============================================================================

ch4_ui <- tabPanel("4. Rozk\u0142ady ci\u0105g\u0142e",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Rozk\u0142ady dyskretne opisuj\u0105 zmienne o sko\u0144czonej liczbie warto\u015bci.
       Ale co, gdy zmienna mo\u017ce przyj\u0105\u0107 dowoln\u0105 warto\u015b\u0107 z przedzia\u0142u?"
    ),

    div(class = "section-title", "Od histogramu do krzywej g\u0119sto\u015bci"),

    div(class = "narrative",
      p("Znasz ju\u017c histogramy ze statystyki opisowej. Teraz zobaczymy,
        jak histogram ", tags$b("przechodzi w g\u0142adk\u0105 krzyw\u0105"),
        " gdy zwi\u0119kszamy pr\u00f3b\u0119 i zw\u0119\u017camy przedzia\u0142y.
        Ta krzywa to ", tags$b("funkcja g\u0119sto\u015bci prawdopodobie\u0144stwa (PDF)"),
        " \u2014 ci\u0105g\u0142y odpowiednik PMF.")
    ),

    # ========================================================================
    # WIDGET 1: Od histogramu do krzywej (krok po kroku)
    # ========================================================================
    div(class = "widget-block",
      h4("Od histogramu do krzywej g\u0119sto\u015bci"),
      fluidRow(
        column(4,
          selectInput("ch4_step_dist", "Rozk\u0142ad \u017ar\u00f3d\u0142owy:",
            choices = c("Normalny" = "normal", "Wyk\u0142adniczy" = "exp",
                        "Jednostajny" = "unif"),
            selected = "normal"
          ),
          sliderInput("ch4_step_n", "Wielko\u015b\u0107 pr\u00f3by:",
                      min = 50, max = 10000, value = 500, step = 50),
          hr(),
          actionButton("ch4_step1", "1. Surowe dane (rug)",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch4_step2", "2. Histogram (5 bin\u00f3w)",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch4_step3", "3. Wi\u0119cej bin\u00f3w (15)",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch4_step4", "4. Jeszcze wi\u0119cej (30)",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch4_step5", "5. Skala g\u0119sto\u015bci",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch4_step6", "6. Krzywa g\u0119sto\u015bci",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch4_step7", "7. Tylko PDF",
                       class = "btn-outline-primary", width = "100%"),
          br(), br(),
          actionButton("ch4_step_reset", "Reset",
                       class = "btn-outline-secondary", width = "100%")
        ),
        column(8,
          plotOutput("ch4_step_plot", height = "400px"),
          uiOutput("ch4_step_text")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Prawdopodobienstwo = pole
    # ========================================================================
    div(class = "section-title", "Prawdopodobie\u0144stwo = pole pod krzyw\u0105"),

    div(class = "narrative",
      p("W rozk\u0142adach ci\u0105g\u0142ych prawdopodobie\u0144stwo to ",
        tags$b("pole pod krzyw\u0105 g\u0119sto\u015bci"), " w danym przedziale.
        Wysoko\u015b\u0107 krzywej to NIE prawdopodobie\u0144stwo!"),
      p("Wa\u017cna konsekwencja: ", tags$b("P(X = dok\u0142adnie 5.0) = 0"),
        " dla rozk\u0142ad\u00f3w ci\u0105g\u0142ych. Sens ma tylko pytanie o przedzia\u0142y.")
    ),

    div(class = "widget-block",
      h4("Zacieniuj przedzia\u0142 i odczytaj prawdopodobie\u0144stwo"),
      fluidRow(
        column(4,
          selectInput("ch4_area_dist", "Rozk\u0142ad:",
            choices = c("Normalny N(0,1)" = "norm",
                        "Wyk\u0142adniczy Exp(1)" = "exp",
                        "Jednostajny U(0,10)" = "unif"),
            selected = "norm"
          ),
          sliderInput("ch4_area_a", "Dolna granica (a):",
                      min = -4, max = 4, value = -1, step = 0.1),
          sliderInput("ch4_area_b", "G\u00f3rna granica (b):",
                      min = -4, max = 4, value = 1, step = 0.1)
        ),
        column(8,
          plotOutput("ch4_area_plot", height = "350px"),
          uiOutput("ch4_area_stats")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Zapami\u0119taj:"),
      " W rozk\u0142adzie ci\u0105g\u0142ym g\u0119sto\u015b\u0107 f(x) mo\u017ce by\u0107 > 1 (np. U(0, 0.5) ma f(x) = 2),
        ale ", tags$b("pole pod ca\u0142\u0105 krzyw\u0105 zawsze wynosi 1"), "."
    ),

    # ========================================================================
    # WIDGET 3: Jednostajny ciagly i wykladniczy
    # ========================================================================
    div(class = "section-title", "Dwa podstawowe rozk\u0142ady ci\u0105g\u0142e"),

    div(class = "narrative",
      p(tags$b("Jednostajny ci\u0105g\u0142y U(a, b)"), " \u2014 ka\u017cda warto\u015b\u0107 w przedziale
        [a, b] jest jednakowo prawdopodobna. Przyk\u0142ad: losowa liczba z generatora."),
      p(tags$b("Wyk\u0142adniczy Exp(\u03bb)"), " \u2014 modeluje czas oczekiwania mi\u0119dzy
        zdarzeniami. Przyk\u0142ad: czas mi\u0119dzy wiadomo\u015bciami na WhatsAppie, czas mi\u0119dzy awariami maszyn.")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnanie: jednostajny vs wyk\u0142adniczy"),
      fluidRow(
        column(6,
          h5("Jednostajny U(a, b)"),
          sliderInput("ch4_unif_a", "a:", min = 0, max = 5, value = 0, step = 0.5),
          sliderInput("ch4_unif_b", "b:", min = 1, max = 10, value = 5, step = 0.5),
          plotOutput("ch4_unif_plot", height = "250px"),
          uiOutput("ch4_unif_stats")
        ),
        column(6,
          h5("Wyk\u0142adniczy Exp(\u03bb)"),
          sliderInput("ch4_exp_lambda", "\u03bb (rate):",
                      min = 0.1, max = 3, value = 1, step = 0.1),
          br(),
          plotOutput("ch4_exp_plot", height = "250px"),
          uiOutput("ch4_exp_stats")
        )
      ),
      div(class = "formula-box",
        withMathJax(
          helpText("Jednostajny: $$f(x) = \\frac{1}{b-a}, \\quad E(X) = \\frac{a+b}{2}, \\quad Var(X) = \\frac{(b-a)^2}{12}$$"),
          helpText("Wyk\u0142adniczy: $$f(x) = \\lambda e^{-\\lambda x}, \\quad E(X) = \\frac{1}{\\lambda}, \\quad Var(X) = \\frac{1}{\\lambda^2}$$")
        )
      )
    ),

    # ========================================================================
    # WIDGET 4: Rozklad t-Studenta
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad t-Studenta"),

    div(class = "narrative",
      p("Rozk\u0142ad t-Studenta wygl\u0105da jak normalny, ale ma ",
        tags$b("ci\u0119\u017csze ogony"), " \u2014 warto\u015bci ekstremalne s\u0105 bardziej
        prawdopodobne. Jest kluczowy we ", tags$b("wnioskowaniu statystycznym"),
        " (test t, przedzia\u0142y ufno\u015bci)."),
      p("Parametr ", tags$b("df"), " (stopnie swobody) kontroluje 'grubo\u015b\u0107' ogon\u00f3w.
        Im wi\u0119cej df, tym bli\u017cej do rozk\u0142adu normalnego.")
    ),

    div(class = "widget-block",
      h4("Rozk\u0142ad t-Studenta t(df)"),
      fluidRow(
        column(4,
          sliderInput("ch4_t_df", "df (stopnie swobody):",
                      min = 1, max = 50, value = 5, step = 1),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch4_t_preset1", "df=1\n(Cauchy)",
                         class = "btn-outline-primary"),
            actionButton("ch4_t_preset2", "df=5",
                         class = "btn-outline-warning"),
            actionButton("ch4_t_preset3", "df=30\n(\u2248 normalny)",
                         class = "btn-outline-success")
          ),
          hr(),
          checkboxInput("ch4_t_show_normal", "Poka\u017c N(0,1) dla por\u00f3wnania", value = TRUE),
          checkboxInput("ch4_t_show_stats", "Poka\u017c E(X) i SD", value = FALSE)
        ),
        column(8,
          plotOutput("ch4_t_plot", height = "350px"),
          uiOutput("ch4_t_stats")
        )
      ),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$E(X) = 0 \\; (df > 1), \\quad Var(X) = \\frac{df}{df - 2} \\; (df > 2)$$"
        ))
      )
    ),

    div(class = "callout-info",
      tags$strong("Dlaczego t-Studenta?"),
      " Gdy nie znamy prawdziwego odchylenia standardowego populacji
        i szacujemy je z pr\u00f3by, rozk\u0142ad statystyki testowej nie jest
        normalny, ale w\u0142a\u015bnie t-Studenta. Przy ma\u0142ych pr\u00f3bach (n < 30)
        r\u00f3\u017cnica jest znacz\u0105ca!"
    ),

    # ========================================================================
    # WIDGET 5: Rozklad chi-kwadrat
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad chi-kwadrat (\u03c7\u00b2)"),

    div(class = "narrative",
      p("Rozk\u0142ad chi-kwadrat powstaje jako ", tags$b("suma kwadrat\u00f3w"),
        " niezale\u017cnych zmiennych N(0,1). Jest zawsze ",
        tags$b("nieujemny i prawosko\u015bny"), "."),
      p("Zastosowania: testy niezale\u017cno\u015bci, testy dopasowania,
        estymacja wariancji.")
    ),

    div(class = "widget-block",
      h4("Rozk\u0142ad \u03c7\u00b2(df)"),
      fluidRow(
        column(4,
          sliderInput("ch4_chisq_df", "df (stopnie swobody):",
                      min = 1, max = 30, value = 5, step = 1),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch4_chisq_preset1", "df=1",
                         class = "btn-outline-primary"),
            actionButton("ch4_chisq_preset2", "df=5",
                         class = "btn-outline-warning"),
            actionButton("ch4_chisq_preset3", "df=15",
                         class = "btn-outline-success")
          ),
          hr(),
          checkboxInput("ch4_chisq_show_stats", "Poka\u017c E(X) i SD", value = TRUE)
        ),
        column(8,
          plotOutput("ch4_chisq_plot", height = "350px"),
          uiOutput("ch4_chisq_stats")
        )
      ),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$E(X) = df, \\quad Var(X) = 2 \\cdot df$$"
        ))
      )
    ),

    div(class = "callout-info",
      tags$strong("Obserwacja:"),
      " Przy ma\u0142ym df rozk\u0142ad jest mocno prawosko\u015bny.
        Gdy df ro\u015bnie, rozk\u0142ad staje si\u0119 coraz bardziej symetryczny
        i zbli\u017ca si\u0119 do normalnego (dzi\u0119ki CTG!)."
    ),

    # ========================================================================
    # WIDGET 6: Rozklad log-normalny
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad log-normalny"),

    div(class = "narrative",
      p("Je\u015bli ", tags$b("ln(X) ~ N(\u03bc, \u03c3)"), ", to X ma rozk\u0142ad log-normalny.
        Zmienna jest zawsze ", tags$b("dodatnia i prawosko\u015bna"), "."),
      p("Pojawia si\u0119 wsz\u0119dzie tam, gdzie dane rosn\u0105 ",
        tags$b("multiplikatywnie"), ": dochody, ceny akcji,
        czasy reakcji, st\u0119\u017cenia substancji.")
    ),

    div(class = "widget-block",
      h4("Rozk\u0142ad LogN(\u03bc, \u03c3)"),
      fluidRow(
        column(4,
          sliderInput("ch4_lnorm_mu", "\u03bc (meanlog):",
                      min = -1, max = 3, value = 0, step = 0.1),
          sliderInput("ch4_lnorm_sigma", "\u03c3 (sdlog):",
                      min = 0.1, max = 2, value = 0.5, step = 0.1),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch4_lnorm_preset1", "Dochody\n(\u03bc=3, \u03c3=0.8)",
                         class = "btn-outline-primary"),
            actionButton("ch4_lnorm_preset2", "Ceny akcji\n(\u03bc=1, \u03c3=0.5)",
                         class = "btn-outline-warning"),
            actionButton("ch4_lnorm_preset3", "Czas reakcji\n(\u03bc=0, \u03c3=0.5)",
                         class = "btn-outline-success")
          ),
          hr(),
          checkboxInput("ch4_lnorm_show_stats", "Poka\u017c E(X) i SD", value = TRUE),
          checkboxInput("ch4_lnorm_show_log", "Poka\u017c na skali logarytmicznej", value = FALSE)
        ),
        column(8,
          plotOutput("ch4_lnorm_plot", height = "350px"),
          uiOutput("ch4_lnorm_stats")
        )
      ),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$E(X) = e^{\\mu + \\sigma^2/2}, \\quad Var(X) = \\left(e^{\\sigma^2} - 1\\right) \\cdot e^{2\\mu + \\sigma^2}$$"
        ))
      )
    ),

    div(class = "callout-warning",
      tags$strong("Uwaga na \u015bredni\u0105!"),
      " W rozk\u0142adzie log-normalnym \u015brednia jest zawsze wi\u0119ksza od mediany.
        Mediana = e^\u03bc, \u015brednia = e^(\u03bc + \u03c3\u00b2/2).
        Dlatego ", tags$b("mediana dochod\u00f3w"), " jest lepsz\u0105 miar\u0105
        'typowego' dochodu ni\u017c \u015brednia."
    ),

    # --- Transition ---
    div(class = "chapter-transition",
      p("Spo\u015br\u00f3d wszystkich rozk\u0142ad\u00f3w ci\u0105g\u0142ych, jeden g\u00f3ruje nad innymi.
        Pojawia si\u0119 wsz\u0119dzie w naturze i statystyce.
        Czas pozna\u0107 ", tags$b("rozk\u0142ad normalny"), " dogł\u0119bnie."),
      actionButton("ch4_next", "Dalej: 5. Rozk\u0142ad normalny \u2192",
                   class = "btn-primary btn-lg")
    ),

    br(), br()
  ))
)

# --------------------------------------------------------------------------
# Chapter 4 Server
# --------------------------------------------------------------------------

ch4_server <- function(input, output, session) {

  # --- Widget 1: Krok po kroku ---
  ch4_step <- reactiveVal(0)
  ch4_sample_data <- reactiveVal(NULL)

  observe({
    n <- input$ch4_step_n
    dist <- input$ch4_step_dist
    data <- switch(dist,
      "normal" = rnorm(n, mean = 5, sd = 1.5),
      "exp"    = rexp(n, rate = 0.5),
      "unif"   = runif(n, min = 0, max = 10)
    )
    ch4_sample_data(data)
    ch4_step(0)
  })

  observeEvent(input$ch4_step1, ch4_step(1))
  observeEvent(input$ch4_step2, ch4_step(2))
  observeEvent(input$ch4_step3, ch4_step(3))
  observeEvent(input$ch4_step4, ch4_step(4))
  observeEvent(input$ch4_step5, ch4_step(5))
  observeEvent(input$ch4_step6, ch4_step(6))
  observeEvent(input$ch4_step7, ch4_step(7))
  observeEvent(input$ch4_step_reset, ch4_step(0))

  output$ch4_step_plot <- renderPlot({
    step <- ch4_step()
    data <- ch4_sample_data()
    req(data)

    df <- data.frame(x = data)

    if (step == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij krok 1, aby zacz\u0105\u0107",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else if (step == 1) {
      ggplot(df, aes(x = x)) +
        geom_rug(color = col_primary, alpha = 0.3) +
        labs(title = "Krok 1: Surowe dane", x = "Warto\u015b\u0107", y = "") +
        theme_prob()
    } else if (step == 2) {
      ggplot(df, aes(x = x)) +
        geom_histogram(bins = 5, fill = col_primary, color = "white", alpha = 0.7) +
        geom_rug(alpha = 0.2) +
        labs(title = "Krok 2: Histogram (5 bin\u00f3w)", x = "Warto\u015b\u0107", y = "Liczebno\u015b\u0107") +
        theme_prob()
    } else if (step == 3) {
      ggplot(df, aes(x = x)) +
        geom_histogram(bins = 15, fill = col_primary, color = "white", alpha = 0.7) +
        labs(title = "Krok 3: Histogram (15 bin\u00f3w)", x = "Warto\u015b\u0107", y = "Liczebno\u015b\u0107") +
        theme_prob()
    } else if (step == 4) {
      ggplot(df, aes(x = x)) +
        geom_histogram(bins = 30, fill = col_primary, color = "white", alpha = 0.7) +
        labs(title = "Krok 4: Histogram (30 bin\u00f3w)", x = "Warto\u015b\u0107", y = "Liczebno\u015b\u0107") +
        theme_prob()
    } else if (step == 5) {
      ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = col_primary, color = "white", alpha = 0.7) +
        labs(title = "Krok 5: Skala g\u0119sto\u015bci (o\u015b Y = g\u0119sto\u015b\u0107)",
             x = "Warto\u015b\u0107", y = "G\u0119sto\u015b\u0107") +
        theme_prob()
    } else if (step == 6) {
      ggplot(df, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = col_primary, color = "white", alpha = 0.5) +
        geom_density(color = col_secondary, linewidth = 1.5) +
        labs(title = "Krok 6: Histogram + krzywa g\u0119sto\u015bci",
             x = "Warto\u015b\u0107", y = "G\u0119sto\u015b\u0107") +
        theme_prob()
    } else {
      ggplot(df, aes(x = x)) +
        geom_density(fill = col_primary, color = col_dark, linewidth = 1.2, alpha = 0.3) +
        labs(title = "Krok 7: Funkcja g\u0119sto\u015bci (PDF)",
             subtitle = "Matematyczna idealizacja histogramu",
             x = "Warto\u015b\u0107", y = "G\u0119sto\u015b\u0107 f(x)") +
        theme_prob()
    }
  })

  output$ch4_step_text <- renderUI({
    step <- ch4_step()
    texts <- c(
      "",
      "Ka\u017cda kreska to jedna obserwacja. Trudno co\u015b z tego odczyta\u0107.",
      "5 bin\u00f3w \u2014 widzimy og\u00f3lny zarys, ale ma\u0142o szczeg\u00f3\u0142\u00f3w.",
      "15 bin\u00f3w \u2014 kszta\u0142t staje si\u0119 wyra\u017aniejszy.",
      "30 bin\u00f3w \u2014 jeszcze wi\u0119cej szczeg\u00f3\u0142\u00f3w, ale s\u0142upki s\u0105 nier\u00f3wne.",
      "Zmiana osi Y na g\u0119sto\u015b\u0107 \u2014 teraz pole s\u0142upk\u00f3w = 1.",
      "Nak\u0142adamy g\u0142adk\u0105 krzyw\u0105, kt\u00f3ra przybli\u017ca kszta\u0142t danych.",
      "To jest PDF \u2014 teoretyczny model opisuj\u0105cy rozk\u0142ad. Pole pod krzyw\u0105 = 1."
    )
    if (step > 0) div(class = "callout-info", texts[step + 1])
  })

  # --- Widget 2: Prawdopodobienstwo = pole ---
  observe({
    dist <- input$ch4_area_dist
    if (dist == "norm") {
      updateSliderInput(session, "ch4_area_a", min = -4, max = 4, value = -1, step = 0.1)
      updateSliderInput(session, "ch4_area_b", min = -4, max = 4, value = 1, step = 0.1)
    } else if (dist == "exp") {
      updateSliderInput(session, "ch4_area_a", min = 0, max = 8, value = 0, step = 0.1)
      updateSliderInput(session, "ch4_area_b", min = 0, max = 8, value = 2, step = 0.1)
    } else {
      updateSliderInput(session, "ch4_area_a", min = 0, max = 10, value = 2, step = 0.1)
      updateSliderInput(session, "ch4_area_b", min = 0, max = 10, value = 7, step = 0.1)
    }
  })

  output$ch4_area_plot <- renderPlot({
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

    # Zacieniowany obszar
    shade_x <- seq(max(a, x_range[1]), min(b, x_range[2]), length.out = 300)
    shade_df <- data.frame(x = shade_x, y = dfn(shade_x))

    ggplot() +
      geom_area(data = shade_df, aes(x = x, y = y),
                fill = col_primary, alpha = 0.35) +
      geom_line(data = df_curve, aes(x = x, y = y),
                color = col_dark, linewidth = 1.2) +
      geom_vline(xintercept = a, color = col_secondary, linetype = "dashed") +
      geom_vline(xintercept = b, color = col_secondary, linetype = "dashed") +
      annotate("text", x = (a + b) / 2, y = max(dfn(x_seq)) * 0.5,
               label = sprintf("P = %.4f", prob),
               size = 6, fontface = "bold", color = col_dark) +
      labs(title = paste0("P(", a, " < X < ", b, ")"),
           x = "x", y = "f(x)") +
      theme_prob()
  })

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

    div(style = "text-align: center; margin-top: 10px;",
      div(class = "stat-box", style = paste0("background: ", col_primary, ";"),
          paste0("P(", a, " < X < ", b, ") = ", sprintf("%.4f", max(0, prob)))),
      div(class = "stat-box", style = paste0("background: ", col_dark, ";"),
          paste0(sprintf("%.1f", max(0, prob) * 100), "%"))
    )
  })

  # --- Widget 3: Jednostajny i wykladniczy ---
  output$ch4_unif_plot <- renderPlot({
    a <- input$ch4_unif_a
    b <- input$ch4_unif_b
    req(b > a)

    x_seq <- seq(a - 1, b + 1, length.out = 500)
    y_seq <- dunif(x_seq, a, b)
    df <- data.frame(x = x_seq, y = y_seq)

    ggplot(df, aes(x = x, y = y)) +
      geom_area(fill = col_uniform, alpha = 0.3) +
      geom_line(color = col_uniform, linewidth = 1.2) +
      labs(title = paste0("U(", a, ", ", b, ")"), x = "x", y = "f(x)") +
      theme_prob(base_size = 12)
  })

  output$ch4_unif_stats <- renderUI({
    a <- input$ch4_unif_a
    b <- input$ch4_unif_b
    req(b > a)
    mu <- (a + b) / 2
    v <- (b - a)^2 / 12
    div(style = "font-size: 13px;",
      paste0("E(X) = ", round(mu, 2), " | SD = ", round(sqrt(v), 2))
    )
  })

  output$ch4_exp_plot <- renderPlot({
    lambda <- input$ch4_exp_lambda
    x_seq <- seq(0, 8, length.out = 500)
    y_seq <- dexp(x_seq, rate = lambda)
    df <- data.frame(x = x_seq, y = y_seq)

    ggplot(df, aes(x = x, y = y)) +
      geom_area(fill = col_exponential, alpha = 0.3) +
      geom_line(color = col_exponential, linewidth = 1.2) +
      labs(title = paste0("Exp(\u03bb=", lambda, ")"), x = "x", y = "f(x)") +
      theme_prob(base_size = 12)
  })

  output$ch4_exp_stats <- renderUI({
    lambda <- input$ch4_exp_lambda
    mu <- 1 / lambda
    sigma <- 1 / lambda
    div(style = "font-size: 13px;",
      paste0("E(X) = 1/\u03bb = ", round(mu, 2), " | SD = 1/\u03bb = ", round(sigma, 2))
    )
  })

  # --- Widget 4: t-Studenta ---
  observeEvent(input$ch4_t_preset1, updateSliderInput(session, "ch4_t_df", value = 1))
  observeEvent(input$ch4_t_preset2, updateSliderInput(session, "ch4_t_df", value = 5))
  observeEvent(input$ch4_t_preset3, updateSliderInput(session, "ch4_t_df", value = 30))

  output$ch4_t_plot <- renderPlot({
    df_val <- input$ch4_t_df
    show_normal <- input$ch4_t_show_normal
    show_stats <- input$ch4_t_show_stats

    x_seq <- seq(-5, 5, length.out = 500)
    y_t <- dt(x_seq, df = df_val)
    df_plot <- data.frame(x = x_seq, y = y_t)

    pl <- ggplot(df_plot, aes(x = x, y = y)) +
      geom_area(fill = col_t_student, alpha = 0.3) +
      geom_line(color = col_t_student, linewidth = 1.2)

    if (show_normal) {
      y_norm <- dnorm(x_seq)
      df_norm <- data.frame(x = x_seq, y = y_norm)
      pl <- pl +
        geom_line(data = df_norm, aes(x = x, y = y),
                  color = col_normal, linewidth = 1, linetype = "dashed")
    }

    if (show_stats && df_val > 2) {
      mu <- 0
      sigma <- sqrt(df_val / (df_val - 2))
      pl <- pl +
        geom_vline(xintercept = mu, color = col_secondary, linewidth = 1, linetype = "dashed") +
        annotate("rect", xmin = mu - sigma, xmax = mu + sigma,
                 ymin = 0, ymax = Inf, fill = col_secondary, alpha = 0.08)
    }

    pl +
      labs(title = paste0("t(df=", df_val, ")",
                          if (show_normal) "  vs  N(0,1)" else ""),
           x = "x", y = "f(x)") +
      theme_prob()
  })

  output$ch4_t_stats <- renderUI({
    df_val <- input$ch4_t_df
    mu_text <- if (df_val > 1) "E(X) = 0" else "E(X) = niezdef."
    var_text <- if (df_val > 2) {
      paste0("Var = df/(df-2) = ", round(df_val / (df_val - 2), 2))
    } else {
      "Var = \u221e"
    }
    sd_text <- if (df_val > 2) {
      paste0("SD = ", round(sqrt(df_val / (df_val - 2)), 2))
    } else {
      "SD = \u221e"
    }

    div(style = "text-align: center; margin-top: 10px;",
      div(class = "stat-box", style = paste0("background: ", col_t_student, ";"), mu_text),
      div(class = "stat-box", style = paste0("background: ", col_dark, ";"), sd_text),
      div(class = "stat-box", style = paste0("background: ", col_warning, ";"), var_text)
    )
  })

  # --- Widget 5: Chi-kwadrat ---
  observeEvent(input$ch4_chisq_preset1, updateSliderInput(session, "ch4_chisq_df", value = 1))
  observeEvent(input$ch4_chisq_preset2, updateSliderInput(session, "ch4_chisq_df", value = 5))
  observeEvent(input$ch4_chisq_preset3, updateSliderInput(session, "ch4_chisq_df", value = 15))

  output$ch4_chisq_plot <- renderPlot({
    df_val <- input$ch4_chisq_df
    show_stats <- input$ch4_chisq_show_stats

    x_max <- max(15, qchisq(0.999, df_val))
    x_seq <- seq(0.01, x_max, length.out = 500)
    y_seq <- dchisq(x_seq, df = df_val)
    df_plot <- data.frame(x = x_seq, y = y_seq)

    mu <- df_val
    sigma <- sqrt(2 * df_val)

    pl <- ggplot(df_plot, aes(x = x, y = y)) +
      geom_area(fill = col_chi_sq, alpha = 0.3) +
      geom_line(color = col_chi_sq, linewidth = 1.2)

    if (show_stats) {
      pl <- pl +
        geom_vline(xintercept = mu, color = col_secondary, linewidth = 1.2, linetype = "dashed") +
        annotate("rect", xmin = max(0, mu - sigma), xmax = mu + sigma,
                 ymin = 0, ymax = Inf, fill = col_secondary, alpha = 0.08)
    }

    pl +
      labs(title = paste0("\u03c7\u00b2(df=", df_val, ")"),
           x = "x", y = "f(x)") +
      theme_prob()
  })

  output$ch4_chisq_stats <- renderUI({
    df_val <- input$ch4_chisq_df
    mu <- df_val
    sigma <- sqrt(2 * df_val)
    div(style = "text-align: center; margin-top: 10px;",
      div(class = "stat-box", style = paste0("background: ", col_chi_sq, ";"),
          paste0("E(X) = df = ", mu)),
      div(class = "stat-box", style = paste0("background: ", col_dark, ";"),
          paste0("SD = \u221a(2\u00b7df) = ", round(sigma, 2))),
      div(class = "stat-box", style = paste0("background: ", col_warning, ";"),
          paste0("Var = 2\u00b7df = ", 2 * df_val))
    )
  })

  # --- Widget 6: Log-normalny ---
  observeEvent(input$ch4_lnorm_preset1, {
    updateSliderInput(session, "ch4_lnorm_mu", value = 3)
    updateSliderInput(session, "ch4_lnorm_sigma", value = 0.8)
  })
  observeEvent(input$ch4_lnorm_preset2, {
    updateSliderInput(session, "ch4_lnorm_mu", value = 1)
    updateSliderInput(session, "ch4_lnorm_sigma", value = 0.5)
  })
  observeEvent(input$ch4_lnorm_preset3, {
    updateSliderInput(session, "ch4_lnorm_mu", value = 0)
    updateSliderInput(session, "ch4_lnorm_sigma", value = 0.5)
  })

  output$ch4_lnorm_plot <- renderPlot({
    mu <- input$ch4_lnorm_mu
    sigma <- input$ch4_lnorm_sigma
    show_stats <- input$ch4_lnorm_show_stats
    show_log <- input$ch4_lnorm_show_log

    x_max <- qlnorm(0.995, mu, sigma)
    x_seq <- seq(0.001, x_max, length.out = 500)
    y_seq <- dlnorm(x_seq, mu, sigma)
    df_plot <- data.frame(x = x_seq, y = y_seq)

    ev <- exp(mu + sigma^2 / 2)
    med <- exp(mu)
    sd_val <- sqrt((exp(sigma^2) - 1) * exp(2 * mu + sigma^2))

    if (show_log) {
      # Wykres na skali log — pokaz ze ln(X) jest normalny
      log_x <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 500)
      log_y <- dnorm(log_x, mu, sigma)
      df_log <- data.frame(x = log_x, y = log_y)

      pl <- ggplot(df_log, aes(x = x, y = y)) +
        geom_area(fill = col_normal, alpha = 0.3) +
        geom_line(color = col_normal, linewidth = 1.2) +
        labs(title = paste0("ln(X) ~ N(\u03bc=", mu, ", \u03c3=", sigma, ")"),
             subtitle = "Logarytm zmiennej ma rozk\u0142ad normalny",
             x = "ln(x)", y = "f(ln(x))") +
        theme_prob()
    } else {
      pl <- ggplot(df_plot, aes(x = x, y = y)) +
        geom_area(fill = col_lognormal, alpha = 0.3) +
        geom_line(color = col_lognormal, linewidth = 1.2)

      if (show_stats) {
        pl <- pl +
          geom_vline(xintercept = ev, color = col_secondary, linewidth = 1, linetype = "dashed") +
          geom_vline(xintercept = med, color = col_primary, linewidth = 1, linetype = "dotted") +
          annotate("text", x = ev, y = max(y_seq) * 0.9,
                   label = paste0("\u015brednia = ", round(ev, 1)),
                   hjust = -0.1, color = col_secondary, fontface = "bold", size = 4) +
          annotate("text", x = med, y = max(y_seq) * 0.75,
                   label = paste0("mediana = ", round(med, 1)),
                   hjust = -0.1, color = col_primary, fontface = "bold", size = 4)
      }

      pl <- pl +
        labs(title = paste0("LogN(\u03bc=", mu, ", \u03c3=", sigma, ")"),
             x = "x", y = "f(x)") +
        theme_prob()
    }
    pl
  })

  output$ch4_lnorm_stats <- renderUI({
    mu <- input$ch4_lnorm_mu
    sigma <- input$ch4_lnorm_sigma

    ev <- exp(mu + sigma^2 / 2)
    med <- exp(mu)
    sd_val <- sqrt((exp(sigma^2) - 1) * exp(2 * mu + sigma^2))

    div(style = "text-align: center; margin-top: 10px;",
      div(class = "stat-box", style = paste0("background: ", col_lognormal, ";"),
          paste0("E(X) = ", round(ev, 2))),
      div(class = "stat-box", style = paste0("background: ", col_primary, ";"),
          paste0("Me = e\u1d58 = ", round(med, 2))),
      div(class = "stat-box", style = paste0("background: ", col_dark, ";"),
          paste0("SD = ", round(sd_val, 2)))
    )
  })

}
