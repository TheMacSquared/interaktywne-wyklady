# ============================================================================
# CHAPTER 3: Przedzial dla sredniej
# ============================================================================

ch3_ui <- tabPanel("3. Przedzia\u0142 dla \u015bredniej",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Wiemy ju\u017c, czym jest przedzia\u0142 ufno\u015bci i jak go interpretowa\u0107.
       Czas na konkrety: wz\u00f3r i obliczenia."
    ),

    div(class = "section-title", "Z czy t? Dwa podej\u015bcia"),

    div(class = "narrative",
      p("Przedzia\u0142 ufno\u015bci dla \u015bredniej mo\u017cna konstruowa\u0107 na dwa sposoby:"),
      tags$ul(
        tags$li(tags$b("Z-przedzia\u0142"), " \u2014 gdy znamy ",
                withMathJax("\\(\\sigma\\)"), " populacji (rzadko w praktyce)"),
        tags$li(tags$b("T-przedzia\u0142"), " \u2014 gdy szacujemy ",
                withMathJax("\\(\\sigma\\)"), " z pr\u00f3by jako s (prawie zawsze)")
      ),
      p("Rozk\u0142ad t ma ci\u0119\u017csze ogony ni\u017c normalny \u2014 daje ",
        tags$b("szersze przedzia\u0142y"), ", bo uwzgl\u0119dnia dodatkow\u0105 niepewno\u015b\u0107
        z estymacji \u03c3. Ale z rosn\u0105cym n, rozk\u0142ad t zbiega do normalnego.")
    ),

    # ========================================================================
    # WIDGET 1: z vs t
    # ========================================================================
    div(class = "section-title", "Rozk\u0142ad z vs rozk\u0142ad t"),

    div(class = "widget-block",
      h4("Por\u00f3wnanie krzywych"),
      fluidRow(
        column(4,
          sliderInput("ch3_df", "Stopnie swobody (df = n\u22121):",
                      min = 1, max = 100, value = 5, step = 1),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch3_df2", "df = 2", class = "btn-outline-secondary"),
            actionButton("ch3_df5", "df = 5", class = "btn-outline-secondary"),
            actionButton("ch3_df30", "df = 30", class = "btn-outline-secondary"),
            actionButton("ch3_df100", "df = 100", class = "btn-outline-secondary")
          ),
          hr(),
          uiOutput("ch3_crit_values")
        ),
        column(8,
          plotOutput("ch3_zt_plot", height = "350px")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Regu\u0142a kciuka:"),
      " Przy df \u2265 30 rozk\u0142ad t jest ju\u017c prawie identyczny z normalnym.
        Ale nawet wtedy u\u017cywamy t \u2014 to nic nie kosztuje, a jest poprawniejsze."
    ),

    # ========================================================================
    # WIDGET 2: Kalkulator przedzialow
    # ========================================================================
    div(class = "section-title", "Kalkulator przedzia\u0142u ufno\u015bci"),

    div(class = "narrative",
      p("Wybierz zbi\u00f3r danych lub wpisz w\u0142asne statystyki.
        Aplikacja obliczy przedzia\u0142 i poka\u017ce ka\u017cdy krok wzoru."),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$CI = \\bar{x} \\pm t^*_{\\alpha/2, n-1} \\cdot \\frac{s}{\\sqrt{n}}$$"
        ))
      )
    ),

    div(class = "widget-block",
      h4("Oblicz przedzia\u0142"),
      fluidRow(
        column(4,
          selectInput("ch3_data_source", "Dane:",
            choices = c(
              "Wzrost student\u00f3w (n=30)" = "height",
              "Czas dojazdu (n=50)" = "commute",
              "Oceny z egzaminu (n=40)" = "grades",
              "W\u0142asne statystyki" = "custom"
            ),
            selected = "height"
          ),
          conditionalPanel(
            condition = "input.ch3_data_source == 'custom'",
            numericInput("ch3_custom_xbar", "\u015arednia (x\u0304):", value = 170),
            numericInput("ch3_custom_s", "Odch. std. (s):", value = 10, min = 0.01),
            numericInput("ch3_custom_n", "n:", value = 30, min = 2)
          ),
          sliderInput("ch3_calc_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          actionButton("ch3_calc", "Oblicz",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_calc_plot", height = "250px"),
          uiOutput("ch3_calc_steps")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Pokrycie z vs t
    # ========================================================================
    div(class = "section-title", "Dlaczego t, a nie z?"),

    div(class = "narrative",
      p("Co si\u0119 stanie, je\u015bli u\u017cyjemy z-przedzia\u0142u,
        gdy nie znamy prawdziwego \u03c3? Symulacja poka\u017ce, \u017ce ",
        tags$b("pokrycie spada poni\u017cej nominalnego poziomu"),
        ", szczeg\u00f3lnie dla ma\u0142ych pr\u00f3b.")
    ),

    div(class = "widget-block",
      h4("Por\u00f3wnanie pokrycia: z-interval vs t-interval"),
      fluidRow(
        column(4,
          sliderInput("ch3_cov_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 5, max = 100, value = 10, step = 5),
          helpText("Oba przedzia\u0142y przy 95% ufno\u015bci.
                    Z-interval u\u017cywa prawdziwego \u03c3.
                    T-interval szacuje \u03c3 z pr\u00f3by."),
          actionButton("ch3_cov_sim", "Symuluj 200 przedzia\u0142\u00f3w",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch3_cov_results")
        ),
        column(8,
          plotOutput("ch3_cov_plot", height = "400px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Wniosek:"),
      " Z-interval dzia\u0142a dobrze tylko gdy znamy \u03c3 (np. z wcze\u015bniejszych bada\u0144).
        W praktyce prawie zawsze u\u017cywamy t-intervalu."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: przedzia\u0142 ufno\u015bci dla proporcji"),
      actionButton("ch3_next", "Dalej \u2192 4. Przedzia\u0142 dla proporcji",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # --- Widget 1: z vs t ---
  observeEvent(input$ch3_df2, updateSliderInput(session, "ch3_df", value = 2))
  observeEvent(input$ch3_df5, updateSliderInput(session, "ch3_df", value = 5))
  observeEvent(input$ch3_df30, updateSliderInput(session, "ch3_df", value = 30))
  observeEvent(input$ch3_df100, updateSliderInput(session, "ch3_df", value = 100))

  output$ch3_zt_plot <- renderPlot({
    df_val <- input$ch3_df
    x <- seq(-4, 4, length.out = 500)

    plot_df <- data.frame(
      x = rep(x, 2),
      y = c(dnorm(x), dt(x, df = df_val)),
      dist = rep(c("N(0,1)", paste0("t(df=", df_val, ")")), each = 500)
    )

    ggplot(plot_df, aes(x = x, y = y, color = dist, linetype = dist)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(values = c(col_primary, col_secondary), name = "Rozk\u0142ad") +
      scale_linetype_manual(values = c("solid", "dashed"), name = "Rozk\u0142ad") +
      labs(title = paste0("Rozk\u0142ad normalny vs t (df = ", df_val, ")"),
           x = "x", y = "g\u0119sto\u015b\u0107") +
      theme_ci() +
      theme(legend.position = "top")
  })

  output$ch3_crit_values <- renderUI({
    df_val <- input$ch3_df
    z_star <- round(qnorm(0.975), 3)
    t_star <- round(qt(0.975, df = df_val), 3)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("z* = ", z_star)),
      div(class = "stat-box", style = paste0("background:", col_secondary, ";"),
          paste0("t* = ", t_star))
    )
  })

  # --- Widget 2: Kalkulator ---
  ch3_calc_result <- reactiveVal(NULL)

  get_sample_stats <- function() {
    src <- input$ch3_data_source
    if (src == "custom") {
      list(xbar = input$ch3_custom_xbar,
           s = input$ch3_custom_s,
           n = input$ch3_custom_n,
           label = "W\u0142asne dane")
    } else {
      set.seed(42)
      samp <- switch(src,
        "height"  = rnorm(30, mean = 170, sd = 10),
        "commute" = rgamma(50, shape = 4, scale = 7.5),
        "grades"  = pmin(pmax(rnorm(40, mean = 3.5, sd = 0.7), 2), 5)
      )
      label <- switch(src,
        "height" = "Wzrost student\u00f3w",
        "commute" = "Czas dojazdu (min)",
        "grades" = "Oceny z egzaminu"
      )
      list(xbar = mean(samp), s = sd(samp), n = length(samp), label = label)
    }
  }

  observeEvent(input$ch3_calc, {
    stats <- get_sample_stats()
    conf <- input$ch3_calc_conf
    t_star <- qt(1 - (1 - conf) / 2, df = stats$n - 1)
    me <- t_star * stats$s / sqrt(stats$n)
    ch3_calc_result(list(
      xbar = stats$xbar, s = stats$s, n = stats$n,
      t_star = t_star, me = me, conf = conf,
      lower = stats$xbar - me, upper = stats$xbar + me,
      label = stats$label
    ))
  })

  output$ch3_calc_plot <- renderPlot({
    res <- ch3_calc_result()
    if (is.null(res)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Oblicz'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      pad <- res$me * 2
      ggplot() +
        geom_errorbarh(aes(xmin = res$lower, xmax = res$upper, y = 0),
                       height = 0.2, color = col_ci, linewidth = 2) +
        geom_point(aes(x = res$xbar, y = 0), color = col_estimate,
                   size = 5, shape = 18) +
        annotate("text", x = res$xbar, y = 0.3,
                 label = paste0(res$label, "\n",
                                round(res$conf * 100), "% CI: [",
                                round(res$lower, 2), " ; ",
                                round(res$upper, 2), "]"),
                 size = 5, fontface = "bold") +
        xlim(res$xbar - pad, res$xbar + pad) +
        ylim(-0.5, 0.5) +
        labs(x = "Warto\u015b\u0107", y = "") +
        theme_ci() +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    }
  })

  output$ch3_calc_steps <- renderUI({
    res <- ch3_calc_result()
    if (is.null(res)) return(NULL)
    div(class = "callout-info",
      p(tags$strong("Obliczenia krok po kroku:")),
      p(withMathJax(paste0("\\(\\bar{x} = ", round(res$xbar, 3), "\\)"))),
      p(withMathJax(paste0("\\(s = ", round(res$s, 3), "\\)"))),
      p(withMathJax(paste0("\\(n = ", res$n, ", \\quad df = ", res$n - 1, "\\)"))),
      p(withMathJax(paste0("\\(t^*_{", round((1 - res$conf) / 2, 3), ", ",
                           res$n - 1, "} = ", round(res$t_star, 3), "\\)"))),
      p(withMathJax(paste0("\\(ME = ", round(res$t_star, 3), " \\cdot \\frac{",
                           round(res$s, 3), "}{\\sqrt{", res$n, "}} = ",
                           round(res$me, 3), "\\)"))),
      p(tags$strong(withMathJax(paste0(
        "\\(CI = [", round(res$lower, 3), " \\;; \\; ", round(res$upper, 3), "]\\)"
      ))))
    )
  })

  # --- Widget 3: Pokrycie z vs t ---
  ch3_cov_data <- reactiveVal(NULL)

  observeEvent(input$ch3_cov_sim, {
    n <- input$ch3_cov_n
    params <- get_population_params("normal")

    results_z <- simulate_coverage("normal", n, 0.95, 200, method = "z")
    results_t <- simulate_coverage("normal", n, 0.95, 200, method = "t")

    results_z$method <- "z-interval"
    results_t$method <- "t-interval"
    ch3_cov_data(rbind(results_z, results_t))
  })

  output$ch3_cov_plot <- renderPlot({
    df <- ch3_cov_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Symuluj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      params <- get_population_params("normal")

      # Show first 50 for each method
      df_plot <- df %>% group_by(method) %>% slice_head(n = 50)

      ggplot(df_plot, aes(y = sim)) +
        geom_vline(xintercept = params$mu, color = col_true,
                   linewidth = 1, linetype = "dashed") +
        geom_segment(aes(x = lower, xend = upper, yend = sim, color = covers),
                     linewidth = 0.6) +
        geom_point(aes(x = xbar, color = covers), size = 1) +
        scale_color_manual(values = c("TRUE" = col_hit, "FALSE" = col_miss),
                           labels = c("TRUE" = "Trafiony", "FALSE" = "Chybiony"),
                           name = NULL) +
        facet_wrap(~method) +
        labs(title = paste0("Pokrycie przy n = ", input$ch3_cov_n),
             x = "Warto\u015b\u0107", y = "Numer pr\u00f3by") +
        theme_ci() +
        theme(legend.position = "top")
    }
  })

  output$ch3_cov_results <- renderUI({
    df <- ch3_cov_data()
    if (is.null(df)) return(NULL)
    cov_z <- mean(df$covers[df$method == "z-interval"]) * 100
    cov_t <- mean(df$covers[df$method == "t-interval"]) * 100
    tagList(
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("z: ", cov_z, "%")),
      div(class = "stat-box", style = paste0("background:", col_secondary, ";"),
          paste0("t: ", cov_t, "%")),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          "Cel: 95%")
    )
  })
}
