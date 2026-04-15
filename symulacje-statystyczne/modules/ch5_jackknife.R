# ============================================================================
# CHAPTER 5: Jackknife
# ============================================================================

ch5_ui <- tabPanel("5. Jackknife",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Bootstrap daje CI. Jackknife robi co\u015b innego:
       szacuje, jak bardzo statystyka jest ", tags$em("obci\u0105\u017cona"),
      " i jaki jest jej b\u0142\u0105d standardowy \u2014 przez usuni\u0119cie
       kolejnych obserwacji."
    ),

    div(class = "section-title", "Czym jest jackknife?"),

    div(class = "narrative",
      p("Jackknife to starsza metoda resamplingowa (Quenouille, 1949).
         Idea: oblicz statystyk\u0119 n razy, za ka\u017cdym razem pomijaj\u0105c
         inn\u0105 obserwacj\u0119."),
      p("W ten spos\u00f3b uzyskujemy n ", tags$b("pseudowarto\u015bci"),
        " \u03b8\u0302\u208b\u1d35 \u2014 rozk\u0142ad statystyki, gdy ka\u017cda obserwacja
        jest kolejno \u201evykluczona\u201f. To pozwala oszacowa\u0107 obci\u0105\u017cenie i SE.")
    ),

    div(class = "callout-info",
      tags$strong("Wzory jackknife:"),
      div(class = "formula-box",
        withMathJax(
          "$$\\text{Bias} = (n-1)(\\bar{\\theta}_{-} - \\hat{\\theta})$$",
          "$$\\text{SE} = \\sqrt{\\frac{n-1}{n} \\sum_{i=1}^{n}
             (\\hat{\\theta}_{-i} - \\bar{\\theta}_{-})^2}$$",
          "$$\\hat{\\theta}_{BC} = \\hat{\\theta} - \\text{Bias}
             \\quad \\text{(bias-corrected estimate)}$$"
        )
      ),
      p(style = "font-size:13px;",
        "gdzie \u03b8\u0302\u208b\u1d35 = statystyka z pr\u00f3by bez i-tej obserwacji,
         \u03b8\u0305\u208b = \u015brednia z n pseudowarto\u015bci,
         \u03b8\u0302 = oryginalna statystyka.")
    ),

    # ========================================================================
    # WIDGET 1: Jackknife w akcji
    # ========================================================================
    div(class = "section-title", "Jackknife w akcji"),

    div(class = "widget-block",
      h4("Pseudowarto\u015bci i estymacja obci\u0105\u017cenia"),
      fluidRow(
        column(4,
          selectInput("ch5_stat", "Statystyka:",
            choices = c(
              "\u015aredniana" = "mean",
              "Mediana"     = "median",
              "Odch. stand." = "sd",
              "Sko\u015bno\u015b\u0107"   = "skewness"
            ),
            selected = "mean"
          ),
          selectInput("ch5_dist", "Rozk\u0142ad:",
            choices = c(
              "Normalny"               = "normal",
              "Prawoskos\u015bny (Gamma)" = "skewed",
              "Dwumodalny"             = "bimodal",
              "Grube ogony"            = "heavy_tail"
            ),
            selected = "skewed"
          ),
          sliderInput("ch5_n", "n:", min = 10, max = 80, value = 25, step = 5),
          hr(),
          actionButton("ch5_run", "Oblicz jackknife",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch5_jack_stats")
        ),
        column(8,
          plotOutput("ch5_jack_plot", height = "360px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Bootstrap vs Jackknife SE
    # ========================================================================
    div(class = "section-title", "Bootstrap vs Jackknife \u2014 por\u00f3wnanie SE"),

    div(class = "narrative",
      p("Dla wi\u0119kszo\u015bci statystyk przy du\u017cych pr\u00f3bach oba podej\u015bcia daj\u0105
         podobne SE. Rozchodz\u0105 si\u0119 przy:",
        tags$ul(
          tags$li("Silnie sko\u015bnych rozk\u0142adach"),
          tags$li("Statystykach nieliniowych (mediana, sko\u015bno\u015b\u0107)"),
          tags$li("Ma\u0142ych pr\u00f3bach")
        )),
      p("Bootstrap jest wtedy bardziej wiarygodny \u2014
         jackknife mo\u017ce nie\u0142apiwa\u0107 niestabilno\u015bci mediany.")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnanie SE: Bootstrap vs Jackknife"),
      fluidRow(
        column(4,
          selectInput("ch5_cmp_stat", "Statystyka:",
            choices = c(
              "\u015aredniana" = "mean",
              "Mediana"     = "median",
              "Sko\u015bno\u015b\u0107"   = "skewness"
            ),
            selected = "mean"
          ),
          selectInput("ch5_cmp_dist", "Rozk\u0142ad:",
            choices = c(
              "Normalny"               = "normal",
              "Prawoskos\u015bny (Gamma)" = "skewed",
              "Grube ogony"            = "heavy_tail"
            ),
            selected = "normal"
          ),
          sliderInput("ch5_cmp_n", "n:", min = 10, max = 100, value = 30, step = 5),
          actionButton("ch5_cmp_run", "Por\u00f3wnaj",
                       class = "btn-warning", width = "100%"),
          br(), br(),
          uiOutput("ch5_cmp_stats")
        ),
        column(8,
          plotOutput("ch5_cmp_plot", height = "300px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Kiedy bootstrap jest lepszy ni\u017c jackknife:"),
      tags$ul(
        tags$li("Mediana i inne statystyki oparte na rangach (jackknife mo\u017ce nie dzia\u0142a\u0107 dobrze)"),
        tags$li("Silna sko\u015bno\u015b\u0107 danych"),
        tags$li("Potrzebujesz pe\u0142nego CI, nie tylko SE")
      )
    ),

    div(class = "chapter-transition",
      p("Dalej: cross-validation \u2014 jak oceni\u0107 jako\u015b\u0107 modelu predykcyjnego"),
      actionButton("ch5_next",
                   "Dalej \u2192 6. Cross-validation",
                   class = "btn-primary btn-lg")
    )

  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  ch5_jack_result <- reactiveVal(NULL)
  ch5_data        <- reactiveVal(NULL)

  ch5_stat_fn <- reactive({
    switch(input$ch5_stat,
      "mean"     = mean,
      "median"   = median,
      "sd"       = sd,
      "skewness" = compute_skewness
    )
  })

  ch5_stat_label <- reactive({
    switch(input$ch5_stat,
      "mean"     = "\u015aredniana",
      "median"   = "Mediana",
      "sd"       = "Odch. stand.",
      "skewness" = "Sko\u015bno\u015b\u0107"
    )
  })

  observeEvent(input$ch5_run, {
    x      <- generate_sample_data(input$ch5_n, dist = input$ch5_dist)
    result <- run_jackknife(x, ch5_stat_fn())
    ch5_data(x)
    ch5_jack_result(result)
  })

  output$ch5_jack_plot <- renderPlot({
    result <- ch5_jack_result()
    if (is.null(result)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Oblicz jackknife'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
      return()
    }
    plot_jackknife_pseudovalues(result, stat_label = ch5_stat_label(),
                                 col_primary   = col_primary,
                                 col_secondary = col_secondary,
                                 col_success   = col_success)
  })

  output$ch5_jack_stats <- renderUI({
    result <- ch5_jack_result()
    if (is.null(result)) return(NULL)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_secondary, ";"),
          paste0("obs = ", round(result$observed, 4))),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("SE = ", round(result$se, 4))),
      div(class = "stat-box", style = paste0("background:", col_warning, ";"),
          paste0("Obci\u0105\u017cenie = ", round(result$bias, 4))),
      div(class = "stat-box", style = paste0("background:", col_success, ";"),
          paste0("BC = ", round(result$bias_corrected, 4)))
    )
  })

  # --- Widget 2: Porownanie SE ---
  ch5_cmp_result <- reactiveVal(NULL)

  ch5_cmp_stat_fn <- reactive({
    switch(input$ch5_cmp_stat,
      "mean"     = mean,
      "median"   = median,
      "skewness" = compute_skewness
    )
  })

  observeEvent(input$ch5_cmp_run, {
    x    <- generate_sample_data(input$ch5_cmp_n, dist = input$ch5_cmp_dist)
    jack <- run_jackknife(x, ch5_cmp_stat_fn())
    boot <- run_bootstrap(x, ch5_cmp_stat_fn(), B = 1000)
    ch5_cmp_result(list(jack = jack, boot = boot, x = x))
  })

  output$ch5_cmp_plot <- renderPlot({
    res <- ch5_cmp_result()
    if (is.null(res)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Por\u00f3wnaj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
      return()
    }

    obs    <- res$jack$observed
    se_j   <- res$jack$se
    se_b   <- res$boot$se

    df <- data.frame(
      method = factor(c("Jackknife", "Bootstrap"),
                       levels = c("Jackknife", "Bootstrap")),
      obs    = rep(obs, 2),
      lower  = c(obs - 1.96 * se_j, obs - 1.96 * se_b),
      upper  = c(obs + 1.96 * se_j, obs + 1.96 * se_b),
      se     = c(se_j, se_b)
    )

    ggplot(df, aes(y = method, color = method)) +
      geom_point(aes(x = obs), size = 5) +
      geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3, linewidth = 2) +
      scale_color_manual(values = c("Jackknife" = col_primary, "Bootstrap" = col_warning),
                         guide  = "none") +
      labs(title = "Szacunki \u00b1 1.96 SE",
           subtitle = paste0("Jackknife SE = ", round(se_j, 4),
                             "  |  Bootstrap SE = ", round(se_b, 4)),
           x = "Warto\u015b\u0107 statystyki", y = NULL) +
      theme_sim()
  })

  output$ch5_cmp_stats <- renderUI({
    res <- ch5_cmp_result()
    if (is.null(res)) return(NULL)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("SE (jack) = ", round(res$jack$se, 4))),
      div(class = "stat-box", style = paste0("background:", col_warning, ";"),
          paste0("SE (boot) = ", round(res$boot$se, 4)))
    )
  })

}
