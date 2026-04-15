# ============================================================================
# CHAPTER 2: Bootstrap — przedzialy ufnosci
# ============================================================================

ch2_ui <- tabPanel("2. Bootstrap \u2014 przedzia\u0142y",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Wiemy ju\u017c, \u017ce rozk\u0142ad bootstrapowy odzwierciedla zmienno\u015b\u0107 statystyki.
       Czas zobaczy\u0107, jak go zamieni\u0107 w przedzia\u0142 ufno\u015bci."
    ),

    div(class = "section-title", "Metoda percentylowa"),

    div(class = "narrative",
      p("Najprostrzy spos\u00f3b: we\u017a rozk\u0142ad bootstrapowy i obetnij ogony."),
      p("Dla 95% CI: we\u017a 2.5. i 97.5. percentyl z ",
        withMathJax("\\(B\\)"), " warto\u015bci bootstrapowych.")
    ),

    div(class = "formula-box",
      withMathJax(
        "\\[\\text{CI}_{0.95} = \\left[ Q_{0.025}(\\theta^*_1, \\ldots, \\theta^*_B),\\;
          Q_{0.975}(\\theta^*_1, \\ldots, \\theta^*_B) \\right]\\]"
      ),
      p(style = "font-size:13px; margin-top:8px;",
        "gdzie ", withMathJax("\\(\\theta^*_b\\)"),
        " to warto\u015b\u0107 statystyki z ", withMathJax("\\(b\\)"),
        "-tej pr\u00f3by bootstrapowej.")
    ),

    # ========================================================================
    # WIDGET 1: Glowny engine CI
    # ========================================================================
    div(class = "section-title", "Bootstrap CI dla dowolnej statystyki"),

    div(class = "narrative",
      p("Kluczowa zaleta bootstrapu: dzia\u0142a tak samo dla ",
        tags$b("ka\u017cdej statystyki"),
        " \u2014 \u015bredniej, mediany, odchylenia standardowego, sko\u015bno\u015bci.
         Nie potrzebujemy wzoru analitycznego.")
    ),

    div(class = "widget-block",
      h4("Bootstrap CI \u2014 silnik"),
      fluidRow(
        column(4,
          selectInput("ch2_stat", "Statystyka:",
            choices = c(
              "\u015aredniana" = "mean",
              "Mediana"     = "median",
              "Odch. stand." = "sd",
              "Sko\u015bno\u015b\u0107"   = "skewness"
            ),
            selected = "mean"
          ),
          selectInput("ch2_dist", "Rozk\u0142ad danych:",
            choices = c(
              "Normalny"               = "normal",
              "Prawoskos\u015bny (Gamma)" = "skewed",
              "Dwumodalny"             = "bimodal",
              "Grube ogony"            = "heavy_tail"
            ),
            selected = "skewed"
          ),
          sliderInput("ch2_n",    "n (wielko\u015b\u0107 pr\u00f3by):",
                      min = 10, max = 100, value = 25, step = 5),
          sliderInput("ch2_B",    "B (pr\u00f3by bootstrapowe):",
                      min = 200, max = 5000, value = 1000, step = 200),
          sliderInput("ch2_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          actionButton("ch2_run", "Uruchom bootstrap",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          actionButton("ch2_new_sample", "\u21ba Nowa pr\u00f3ba",
                       class = "btn-outline-secondary", width = "100%"),
          br(), br(),
          uiOutput("ch2_ci_result")
        ),
        column(8,
          plotOutput("ch2_boot_dist", height = "360px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Bootstrap vs t-CI
    # ========================================================================
    div(class = "section-title", "Bootstrap vs klasyczny przedzia\u0142 t"),

    div(class = "narrative",
      p("Kiedy dane s\u0105 normalne i pr\u00f3ba du\u017ca, bootstrap i t-CI daj\u0105 prawie identyczne wyniki.
         Kiedy dane s\u0105 sko\u015bne i pr\u00f3ba ma\u0142a \u2014 rozchodz\u0105 si\u0119."),
      p("Bootstrap CI jest wtedy ", tags$b("asymetryczny"),
        ", bo odzwierciedla asymetri\u0119 danych.
         T-CI jest symetryczny z za\u0142o\u017cenia.")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnanie: Bootstrap vs t-Student"),
      fluidRow(
        column(4,
          selectInput("ch2_cmp_dist", "Rozk\u0142ad:",
            choices = c(
              "Normalny"               = "normal",
              "Prawoskos\u015bny (Gamma)" = "skewed",
              "Dwumodalny"             = "bimodal",
              "Grube ogony"            = "heavy_tail"
            ),
            selected = "skewed"
          ),
          sliderInput("ch2_cmp_n", "n:", min = 8, max = 100, value = 20, step = 2),
          sliderInput("ch2_cmp_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          actionButton("ch2_cmp_run", "Por\u00f3wnaj metody",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch2_comparison_plot", height = "300px"),
          uiOutput("ch2_comparison_text")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Kiedy bootstrap ma przewag\u0119:"),
      tags$ul(
        tags$li("Ma\u0142a pr\u00f3ba + sko\u015bny rozk\u0142ad"),
        tags$li("Statystyki bez analitycznego wzoru na SE (mediana, sko\u015bno\u015b\u0107)"),
        tags$li("Brak pewno\u015bci co do rozk\u0142adu")
      )
    ),

    # ========================================================================
    # WIDGET 3: CI dla proporcji
    # ========================================================================
    div(class = "section-title", "Bootstrap CI dla proporcji"),

    div(class = "narrative",
      p("Dla proporcji mamy trzy konkurencyjne metody: Wald (prosta formu\u0142a),
         Wilson (dok\u0142adniejszy), Bootstrap (symulacyjny)."),
      p("Przy skrajnych proporcjach (bliskie 0 lub 1) i ma\u0142ym n: ",
        tags$b("Wald zawodzi"), " (mo\u017ce wyj\u015b\u0107 poza [0,1]),
         Wilson i Bootstrap s\u0105 lepsze.")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnanie CI dla proporcji"),
      fluidRow(
        column(4,
          sliderInput("ch2_prop_p", "Prawdziwe p (proporcja):",
                      min = 0.02, max = 0.98, value = 0.10, step = 0.01),
          sliderInput("ch2_prop_n", "n:", min = 10, max = 200, value = 30, step = 5),
          sliderInput("ch2_prop_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          actionButton("ch2_prop_run", "Uruchom",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch2_prop_plot", height = "260px"),
          uiOutput("ch2_prop_stats")
        )
      )
    ),

    div(class = "chapter-transition",
      p("Dalej: bootstrap dla jednej ma\u0142ej pr\u00f3by \u2014 krok po kroku"),
      actionButton("ch2_next",
                   "Dalej \u2192 3. Bootstrap jednej pr\u00f3by",
                   class = "btn-primary btn-lg")
    )

  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # --- Widget 1: Glowny engine CI ---
  ch2_sample      <- reactiveVal(NULL)
  ch2_boot_result <- reactiveVal(NULL)

  ch2_stat_fn <- reactive({
    switch(input$ch2_stat,
      "mean"     = mean,
      "median"   = median,
      "sd"       = sd,
      "skewness" = compute_skewness
    )
  })

  ch2_stat_label <- reactive({
    switch(input$ch2_stat,
      "mean"     = "\u015aredniana*",
      "median"   = "Mediana*",
      "sd"       = "Odch. stand.*",
      "skewness" = "Sko\u015bno\u015b\u0107*"
    )
  })

  observeEvent(input$ch2_run, {
    x <- generate_sample_data(input$ch2_n, dist = input$ch2_dist)
    ch2_sample(x)
    result <- run_bootstrap(x, ch2_stat_fn(), B = input$ch2_B)
    ch2_boot_result(result)
  })

  observeEvent(input$ch2_new_sample, {
    ch2_sample(NULL); ch2_boot_result(NULL)
  })

  # Reset przy zmianie parametrow
  observeEvent(list(input$ch2_dist, input$ch2_n, input$ch2_stat), {
    ch2_boot_result(NULL); ch2_sample(NULL)
  }, ignoreInit = TRUE)

  output$ch2_boot_dist <- renderPlot({
    result <- ch2_boot_result()
    if (is.null(result)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Uruchom bootstrap'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ci <- bootstrap_ci_percentile(result, conf_level = input$ch2_conf)
      plot_bootstrap_distribution(result, ci,
                                   stat_label = ch2_stat_label(),
                                   col_primary = col_primary,
                                   col_secondary = col_secondary,
                                   col_success = col_success,
                                   conf_level = input$ch2_conf)
    }
  })

  output$ch2_ci_result <- renderUI({
    result <- ch2_boot_result()
    if (is.null(result)) return(NULL)
    ci <- bootstrap_ci_percentile(result, conf_level = input$ch2_conf)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_success, ";"),
          paste0("D\u00f3\u0142: ", round(ci$lower, 3))),
      div(class = "stat-box", style = paste0("background:", col_secondary, ";"),
          paste0("Obs: ", round(result$observed, 3))),
      div(class = "stat-box", style = paste0("background:", col_success, ";"),
          paste0("G\u00f3ra: ", round(ci$upper, 3))),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("SE: ", round(result$se, 4)))
    )
  })

  # --- Widget 2: Porownanie Bootstrap vs t-CI ---
  ch2_cmp_result <- reactiveVal(NULL)

  observeEvent(input$ch2_cmp_run, {
    x    <- generate_sample_data(input$ch2_cmp_n, dist = input$ch2_cmp_dist)
    boot <- run_bootstrap(x, mean, B = 1000)
    ci_boot <- bootstrap_ci_percentile(boot, conf_level = input$ch2_cmp_conf)
    ci_t    <- classical_ci_mean(x, conf_level = input$ch2_cmp_conf)
    ch2_cmp_result(list(
      x       = x,
      ci_boot = ci_boot,
      ci_t    = ci_t,
      boot    = boot
    ))
  })

  output$ch2_comparison_plot <- renderPlot({
    res <- ch2_cmp_result()
    if (is.null(res)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Por\u00f3wnaj metody'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
      return()
    }
    ci_df <- rbind(res$ci_boot, res$ci_t)
    plot_ci_comparison(ci_df,
                       col_primary = col_primary,
                       col_secondary = col_secondary,
                       col_success = col_success,
                       col_warning = col_warning)
  })

  output$ch2_comparison_text <- renderUI({
    res <- ch2_cmp_result()
    if (is.null(res)) return(NULL)
    cb <- res$ci_boot
    ct <- res$ci_t
    width_diff <- abs(cb$width - ct$width)
    asymmetry_boot <- abs((res$boot$observed - cb$lower) - (cb$upper - res$boot$observed))

    if (asymmetry_boot > 0.5 || width_diff / ct$width > 0.1) {
      div(class = "callout-warning",
        tags$strong("Rozbie\u017cno\u015b\u0107:"),
        paste0(" Bootstrap CI jest asymetryczny (r\u00f3\u017cnica lewej/prawej ramki: ",
               round(asymmetry_boot, 2), "). T-CI by\u0142by symetryczny z za\u0142o\u017cenia.
               Przy sko\u015bnych danych lub ma\u0142ej pr\u00f3bie bootstrap lepiej odzwierciedla
               niepewno\u015b\u0107.")
      )
    } else {
      div(class = "callout-success",
        tags$strong("Zgodno\u015b\u0107:"),
        " Oba metody daj\u0105 zbli\u017cone wyniki. Przy normalnych danych lub du\u017cym n
         t-CI i bootstrap s\u0105 r\u00f3wnowa\u017cne."
      )
    }
  })

  # --- Widget 3: CI dla proporcji ---
  ch2_prop_result <- reactiveVal(NULL)

  observeEvent(input$ch2_prop_run, {
    p_true  <- input$ch2_prop_p
    n       <- input$ch2_prop_n
    conf    <- input$ch2_prop_conf
    # Losuj jednorazowo sukces/porazka
    k       <- rbinom(1, n, p_true)
    phat    <- k / n
    ci_classical <- classical_ci_proportion(phat, n, conf_level = conf)
    ci_boot      <- bootstrap_ci_proportion(k, n, B = 1000, conf_level = conf)
    ch2_prop_result(list(
      phat         = phat,
      p_true       = p_true,
      k            = k,
      n            = n,
      ci_boot      = ci_boot,
      ci_classical = ci_classical
    ))
  })

  output$ch2_prop_plot <- renderPlot({
    res <- ch2_prop_result()
    if (is.null(res)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Uruchom'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
      return()
    }
    ci_df <- rbind(res$ci_boot, res$ci_classical)
    plot_ci_comparison(ci_df,
                       true_value = res$p_true,
                       col_primary = col_primary,
                       col_secondary = col_secondary,
                       col_success = col_success,
                       col_warning = col_warning)
  })

  output$ch2_prop_stats <- renderUI({
    res <- ch2_prop_result()
    if (is.null(res)) return(NULL)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("n = ", res$n, ", k = ", res$k)),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("p\u0302 = ", round(res$phat, 3))),
      div(class = "stat-box", style = paste0("background:", col_success, ";"),
          paste0("p (prawdziwe) = ", round(res$p_true, 3)))
    )
  })

}
