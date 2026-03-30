# ============================================================================
# CHAPTER 2: Jedna zmienna ilosciowa
# ============================================================================

ch2_ui <- tabPanel("3. Jedna zmienna ilo\u015bciowa",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Znamy logik\u0119 testowania i formu\u0142owanie hipotez.
       Czas na pierwszy konkretny test: czy \u015brednia/mediana r\u00f3\u017cni si\u0119 od zak\u0142adanej warto\u015bci?"
    ),

    div(class = "section-title", "Test t jednej pr\u00f3by"),

    div(class = "narrative",
      p("Pytanie: ", tags$b("Czy \u015brednia w populacji r\u00f3\u017cni si\u0119 od warto\u015bci \u03bc\u2080?"), ""),
      div(class = "formula-box",
        p(withMathJax("\\(H_0: \\mu = \\mu_0 \\quad\\text{vs}\\quad H_1: \\mu \\neq \\mu_0\\)")),
        p(withMathJax("\\(t = \\frac{\\bar{x} - \\mu_0}{s / \\sqrt{n}}, \\quad df = n - 1\\)"))
      ),
      p("Wymaga: dane ilo\u015bciowe, w przybli\u017ceniu normalne (lub du\u017ce n dzi\u0119ki CTG).")
    ),

    # ========================================================================
    # WIDGET 1: Test t jednej proby
    # ========================================================================
    div(class = "section-title", "Interaktywny test t"),

    div(class = "widget-block",
      h4("Test t jednej pr\u00f3by"),
      fluidRow(
        column(4,
          selectInput("ch2_scenario", "Scenariusz:",
            choices = c(
              "Wzrost student\u00f3w (\u03bc\u2080 = 170 cm)" = "height",
              "Czas dojazdu (\u03bc\u2080 = 30 min)" = "commute",
              "Oceny (\u03bc\u2080 = 3.5)" = "grades"
            ),
            selected = "height"
          ),
          sliderInput("ch2_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 10, max = 100, value = 30, step = 5),
          actionButton("ch2_run_t", "Losuj pr\u00f3b\u0119 i testuj",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch2_t_hist", height = "250px"),
          plotOutput("ch2_t_dist", height = "200px"),
          uiOutput("ch2_t_result")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Test Wilcoxona jednej proby
    # ========================================================================
    div(class = "section-title", "Test Wilcoxona jednej pr\u00f3by"),

    div(class = "narrative",
      p("Alternatywa nieparametryczna dla testu t. Testuje, czy ",
        tags$b("mediana"), " r\u00f3\u017cni si\u0119 od warto\u015bci \u03bc\u2080."),
      p("Nie wymaga normalno\u015bci \u2014 dzia\u0142a na rankingach. Mniej moc gdy
        dane s\u0105 normalne, ale odporny na outliery i sko\u015bno\u015b\u0107.")
    ),

    div(class = "widget-block",
      h4("Wilcoxon jednej pr\u00f3by"),
      fluidRow(
        column(4,
          helpText("U\u017cywa tych samych danych co test t powy\u017cej."),
          actionButton("ch2_run_wilcox", "Testuj Wilcoxonem",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch2_wilcox_result")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Porownanie t vs Wilcoxon
    # ========================================================================
    div(class = "section-title", "Por\u00f3wnanie: t vs Wilcoxon"),

    div(class = "narrative",
      p("Kt\u00f3ry test daje lepsze wyniki? To zale\u017cy od rozk\u0142adu danych.
        Por\u00f3wnajmy oba na danych z r\u00f3\u017cnych rozk\u0142ad\u00f3w.")
    ),

    div(class = "widget-block",
      h4("Symulacja: 200 pr\u00f3b z r\u00f3\u017cnych rozk\u0142ad\u00f3w"),
      fluidRow(
        column(4,
          selectInput("ch2_cmp_dist", "Rozk\u0142ad populacji:",
            choices = c(
              "Normalny"                  = "normal",
              "Prawosko\u015bny (Gamma)" = "skewed",
              "Z outlierami"              = "outliers"
            ),
            selected = "normal"
          ),
          sliderInput("ch2_cmp_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 10, max = 100, value = 20, step = 5),
          helpText("Generujemy 200 pr\u00f3b z H\u2081 prawdziw\u0105 (ma\u0142y efekt).
                    Por\u00f3wnujemy moc obu test\u00f3w."),
          actionButton("ch2_cmp_run", "Symuluj",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          uiOutput("ch2_cmp_result"),
          plotOutput("ch2_cmp_plot", height = "250px")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Kiedy kt\u00f3ry?"),
      tags$ul(
        tags$li(tags$b("Test t"), " \u2014 dane w przybli\u017ceniu normalne, brak ekstremalnych outlier\u00f3w"),
        tags$li(tags$b("Wilcoxon"), " \u2014 wyra\u017ana sko\u015bno\u015b\u0107, outliery, ma\u0142a pr\u00f3ba + w\u0105tpliwa normalno\u015b\u0107")
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: testy dla jednej zmiennej jako\u015bciowej"),
      actionButton("ch2_next", "Dalej \u2192 4. Jedna zmienna jako\u015bciowa",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # Shared sample data
  ch2_sample <- reactiveVal(NULL)

  observeEvent(input$ch2_run_t, {
    n <- input$ch2_n
    samp <- switch(input$ch2_scenario,
      "height"  = rnorm(n, mean = 172, sd = 10),
      "commute" = rgamma(n, shape = 3, scale = 10),
      "grades"  = pmin(pmax(rnorm(n, mean = 3.6, sd = 0.7), 2), 5)
    )
    ch2_sample(samp)
  })

  get_mu0 <- reactive({
    switch(input$ch2_scenario,
      "height" = 170, "commute" = 30, "grades" = 3.5)
  })

  # --- Widget 1: Test t ---
  output$ch2_t_hist <- renderPlot({
    samp <- ch2_sample()
    if (is.null(samp)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Losuj pr\u00f3b\u0119 i testuj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      mu0 <- get_mu0()
      ggplot(data.frame(x = samp), aes(x = x)) +
        geom_histogram(bins = 15, fill = col_h0, alpha = 0.6, color = "white") +
        geom_vline(xintercept = mu0, color = col_reject, linewidth = 1.2,
                   linetype = "dashed") +
        geom_vline(xintercept = mean(samp), color = col_pvalue, linewidth = 1.2) +
        annotate("text", x = mu0, y = Inf, vjust = 2,
                 label = paste0("\u03bc\u2080 = ", mu0), color = col_reject,
                 fontface = "bold") +
        annotate("text", x = mean(samp), y = Inf, vjust = 3.5,
                 label = paste0("x\u0304 = ", round(mean(samp), 2)),
                 color = col_pvalue, fontface = "bold") +
        labs(title = "Histogram pr\u00f3by", x = "Warto\u015b\u0107", y = "Liczba") +
        theme_test()
    }
  })

  output$ch2_t_dist <- renderPlot({
    samp <- ch2_sample()
    if (is.null(samp)) return(NULL)

    mu0 <- get_mu0()
    n <- length(samp)
    t_stat <- (mean(samp) - mu0) / (sd(samp) / sqrt(n))

    plot_test_distribution(t_stat, df = n - 1, test_type = "t")
  })

  output$ch2_t_result <- renderUI({
    samp <- ch2_sample()
    if (is.null(samp)) return(NULL)

    mu0 <- get_mu0()
    df_data <- data.frame(value = samp)
    result <- rstatix::t_test(df_data, value ~ 1, mu = mu0)
    tidy_res <- as.data.frame(result)

    p_val <- tidy_res$p
    res <- format_test_result(p_val)

    div(class = "callout-info",
      p(tags$strong("Wynik testu t jednej pr\u00f3by:")),
      p(paste0("t(", length(samp) - 1, ") = ",
               round(tidy_res$statistic, 3))),
      p(paste0("p = ", format.pval(p_val, digits = 4))),
      p(style = paste0("color: ", res$color, "; font-weight: bold;"),
        res$decision),
      p(res$explanation)
    )
  })

  # --- Widget 2: Wilcoxon ---
  output$ch2_wilcox_result <- renderUI({
    req(input$ch2_run_wilcox)
    samp <- isolate(ch2_sample())
    if (is.null(samp)) {
      return(div(class = "callout-warning", "Najpierw wygeneruj pr\u00f3b\u0119 testem t."))
    }

    mu0 <- isolate(get_mu0())
    df_data <- data.frame(value = samp)
    result <- rstatix::wilcox_test(df_data, value ~ 1, mu = mu0)
    tidy_res <- as.data.frame(result)

    p_val <- tidy_res$p
    res <- format_test_result(p_val)

    div(class = "callout-info",
      p(tags$strong("Wynik testu Wilcoxona jednej pr\u00f3by:")),
      p(paste0("V = ", round(tidy_res$statistic, 1))),
      p(paste0("p = ", format.pval(p_val, digits = 4))),
      p(style = paste0("color: ", res$color, "; font-weight: bold;"),
        res$decision),
      p(res$explanation)
    )
  })

  # --- Widget 3: Porownanie t vs Wilcoxon ---
  ch2_cmp_results <- reactiveVal(NULL)

  observeEvent(input$ch2_cmp_run, {
    dist <- input$ch2_cmp_dist
    n <- input$ch2_cmp_n

    # Generuj dane z H1 prawdziwa (maly efekt)
    power_t <- 0
    power_w <- 0
    n_sims <- 200

    results <- sapply(1:n_sims, function(i) {
      samp <- switch(dist,
        "normal"   = rnorm(n, mean = 172, sd = 10),
        "skewed"   = rgamma(n, shape = 4, scale = 8),   # mediana ~28, testujemy 30
        "outliers" = c(rnorm(n - 3, mean = 172, sd = 10),
                       rnorm(3, mean = 220, sd = 5))
      )
      mu0 <- switch(dist, "normal" = 170, "skewed" = 30, "outliers" = 170)

      df_data <- data.frame(value = samp)
      p_t <- rstatix::t_test(df_data, value ~ 1, mu = mu0)$p
      p_w <- rstatix::wilcox_test(df_data, value ~ 1, mu = mu0)$p

      c(p_t = p_t, p_w = p_w)
    })

    results_df <- data.frame(t(results))
    ch2_cmp_results(results_df)
  })

  output$ch2_cmp_result <- renderUI({
    df <- ch2_cmp_results()
    if (is.null(df)) return(NULL)

    power_t <- mean(df$p_t < 0.05) * 100
    power_w <- mean(df$p_w < 0.05) * 100

    tagList(
      div(class = "stat-box", style = paste0("background:", col_h0, ";"),
          paste0("Moc t: ", round(power_t, 1), "%")),
      div(class = "stat-box", style = paste0("background:", col_paired, ";"),
          paste0("Moc Wilcoxon: ", round(power_w, 1), "%"))
    )
  })

  output$ch2_cmp_plot <- renderPlot({
    df <- ch2_cmp_results()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Symuluj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      long <- data.frame(
        test = rep(c("Test t", "Wilcoxon"), each = nrow(df)),
        p = c(df$p_t, df$p_w)
      )

      ggplot(long, aes(x = p, fill = test)) +
        geom_histogram(breaks = seq(0, 1, by = 0.05), alpha = 0.6,
                       color = "white", position = "identity") +
        geom_vline(xintercept = 0.05, color = col_reject, linetype = "dashed") +
        scale_fill_manual(values = c(col_h0, col_paired), name = NULL) +
        labs(title = "Rozk\u0142ad p-warto\u015bci (H\u2081 prawdziwa)",
             x = "p-warto\u015b\u0107", y = "Liczba") +
        theme_test() +
        theme(legend.position = "top")
    }
  })
}
