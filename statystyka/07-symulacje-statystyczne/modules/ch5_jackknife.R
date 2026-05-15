# ============================================================================
# CHAPTER 5: Jackknife
# ============================================================================

ch5_ui <- lecture_chapter(
  id = "ch-jackknife",
  num = "05",
  title = "Jackknife",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 05 · Symulacje statystyczne",
      num    = "05",
      title  = "Jackknife",
      lead   = "Metoda leave-one-out pokazuje wpływ pojedynczych obserwacji i pozwala oszacować SE."
    ),

    lc_feedback(type = "info",
      "Bootstrap daje CI. Jackknife robi coś innego:
       szacuje, jak bardzo statystyka jest ", tags$em("obciążona"),
      " i jaki jest jej błąd standardowy — przez usunięcie
       kolejnych obserwacji."
    ),

    lc_h2("ch5-sec-01", "Czym jest jackknife?"),

    tagList(
      p("Jackknife to starsza metoda resamplingowa (Quenouille, 1949).
         Idea: oblicz statystykę n razy, za każdym razem pomijając
         inną obserwację."),
      p("W ten sposób uzyskujemy n ", tags$b("pseudowartości"),
        " θ̂₋ᴵ — rozkład statystyki, gdy każda obserwacja
        jest kolejno „vykluczona‟. To pozwala oszacować obciążenie i SE.")
    ),

    lc_feedback(type = "info",
      tags$strong("Wzory jackknife:"),
      lc_formula_box(
        withMathJax(
          "$$\\text{Bias} = (n-1)(\\bar{\\theta}_{-} - \\hat{\\theta})$$",
          "$$\\text{SE} = \\sqrt{\\frac{n-1}{n} \\sum_{i=1}^{n}
             (\\hat{\\theta}_{-i} - \\bar{\\theta}_{-})^2}$$",
          "$$\\hat{\\theta}_{BC} = \\hat{\\theta} - \\text{Bias}
             \\quad \\text{(bias-corrected estimate)}$$"
        )
      ),
      p(style = "font-size:13px;",
        "gdzie θ̂₋ᴵ = statystyka z próby bez i-tej obserwacji,
         θ̅₋ = średnia z n pseudowartości,
         θ̂ = oryginalna statystyka.")
    ),

    # ========================================================================
    # WIDGET 1: Jackknife w akcji
    # ========================================================================
    lc_h2("ch5-sec-02", "Jackknife w akcji"),

    figure_panel(label = "Ryc. 5.1", title = "Pseudowartości i estymacja obciążenia",
      fluidRow(
        column(4,
          selectInput("ch5_stat", "Statystyka:",
            choices = c(
              "Średniana" = "mean",
              "Mediana"     = "median",
              "Odch. stand." = "sd",
              "Skośność"   = "skewness"
            ),
            selected = "mean"
          ),
          selectInput("ch5_dist", "Rozkład:",
            choices = c(
              "Normalny"               = "normal",
              "Prawoskosśny (Gamma)" = "skewed",
              "Dwumodalny"             = "bimodal",
              "Grube ogony"            = "heavy_tail"
            ),
            selected = "skewed"
          ),
          sliderInput("ch5_n", "n:", min = 10, max = 80, value = 25, step = 5),
          hr(),
          actionButton("ch5_run", "Oblicz jackknife",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch5_jack_stats")
        ),
        column(8,
          zoom_plot_ui("ch5_jack_plot", height = "360px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Bootstrap vs Jackknife SE
    # ========================================================================
    lc_h2("ch5-sec-03", "Bootstrap vs Jackknife — porównanie SE"),

    tagList(
      p("Dla większości statystyk przy dużych próbach oba podejścia dają
         podobne SE. Rozchodzą się przy:",
        tags$ul(
          tags$li("Silnie skośnych rozkładach"),
          tags$li("Statystykach nieliniowych (mediana, skośność)"),
          tags$li("Małych próbach")
        )),
      p("Bootstrap jest wtedy bardziej wiarygodny —
         jackknife może niełapiwać niestabilności mediany.")
    ),

    figure_panel(label = "Ryc. 5.2", title = "Porównanie SE: Bootstrap vs Jackknife",
      fluidRow(
        column(4,
          selectInput("ch5_cmp_stat", "Statystyka:",
            choices = c(
              "Średniana" = "mean",
              "Mediana"     = "median",
              "Skośność"   = "skewness"
            ),
            selected = "mean"
          ),
          selectInput("ch5_cmp_dist", "Rozkład:",
            choices = c(
              "Normalny"               = "normal",
              "Prawoskosśny (Gamma)" = "skewed",
              "Grube ogony"            = "heavy_tail"
            ),
            selected = "normal"
          ),
          sliderInput("ch5_cmp_n", "n:", min = 10, max = 100, value = 30, step = 5),
          actionButton("ch5_cmp_run", "Porównaj",
                       class = "lc-btn-warning", width = "100%"),
          br(), br(),
          uiOutput("ch5_cmp_stats")
        ),
        column(8,
          zoom_plot_ui("ch5_cmp_plot", height = "300px")
        )
      )
    ),

    lc_feedback(type = "warning",
      tags$strong("Kiedy bootstrap jest lepszy niż jackknife:"),
      tags$ul(
        tags$li("Mediana i inne statystyki oparte na rangach (jackknife może nie działać dobrze)"),
        tags$li("Silna skośność danych"),
        tags$li("Potrzebujesz pełnego CI, nie tylko SE")
      )
    ),

    lc_chapter_next(
      num = "06",
      title = "Cross-validation",
      lead = "ocena modeli przez podział trening-test.",
      target_id = "ch-cv"
    )

  )
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
      "mean"     = "Średniana",
      "median"   = "Mediana",
      "sd"       = "Odch. stand.",
      "skewness" = "Skośność"
    )
  })

  observeEvent(input$ch5_run, {
    x      <- generate_sample_data(input$ch5_n, dist = input$ch5_dist)
    result <- run_jackknife(x, ch5_stat_fn())
    ch5_data(x)
    ch5_jack_result(result)
  })

  zoom_plot_server("ch5_jack_plot", reactive({
    result <- ch5_jack_result()
    if (is.null(result)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Oblicz jackknife'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }
    plot_jackknife_pseudovalues(result, stat_label = ch5_stat_label(),
                                 sim_bootstrap   = sim_bootstrap,
                                 sim_observed = sim_observed,
                                 sim_success   = sim_success)
  }))

  output$ch5_jack_stats <- renderUI({
    result <- ch5_jack_result()
    if (is.null(result)) return(NULL)
    lc_stat_grid(
      lc_stat_box("obs", round(result$observed, 4), color = sim_observed),
      lc_stat_box("SE", round(result$se, 4), color = sim_bootstrap),
      lc_stat_box("Obciążenie", round(result$bias, 4), color = sim_warning),
      lc_stat_box("BC", round(result$bias_corrected, 4), color = sim_success),
      columns = 4
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

  zoom_plot_server("ch5_cmp_plot", reactive({
    res <- ch5_cmp_result()
    if (is.null(res)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Porównaj'",
                 size = 6, color = upwr_reference) +
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
      scale_color_manual(values = c("Jackknife" = sim_bootstrap, "Bootstrap" = sim_warning),
                         guide  = "none") +
      labs(
           
           x = "Wartość statystyki", y = NULL) +
      theme_upwr()
  }))

  output$ch5_cmp_stats <- renderUI({
    res <- ch5_cmp_result()
    if (is.null(res)) return(NULL)
    lc_stat_grid(
      lc_stat_box("SE (jack)", round(res$jack$se, 4), color = sim_bootstrap),
      lc_stat_box("SE (boot)", round(res$boot$se, 4), color = sim_warning)
    )
  })

}
