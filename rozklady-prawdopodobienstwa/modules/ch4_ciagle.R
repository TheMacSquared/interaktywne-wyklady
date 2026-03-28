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
        zdarzeniami. Przyk\u0142ad: czas do przyjazdu autobusu, czas mi\u0119dzy awariami.")
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

    # --- Transition ---
    div(class = "chapter-transition",
      p("Spo\u015br\u00f3d wszystkich rozk\u0142ad\u00f3w ci\u0105g\u0142ych, jeden g\u00f3ruje nad innymi.
        Pojawia si\u0119 wsz\u0119dzie w naturze i statystyce.
        Czas pozna\u0107 ", tags$b("rozk\u0142ad normalny"), "."),
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

}
