# ============================================================================
# CHAPTER 2: Bootstrap — przedzialy ufnosci
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-bootstrap-ci",
  num = "02",
  title = "Bootstrap — przedziały",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · Symulacje statystyczne",
      num    = "02",
      title  = "Bootstrap — przedziały",
      lead   = "Przedziały percentylowe, pokrycie i porównanie z klasycznymi konstrukcjami."
    ),

    lc_feedback(type = "info",
      "Wiemy już, że rozkład bootstrapowy odzwierciedla zmienność statystyki.
       Czas zobaczyć, jak go zamienić w przedział ufności."
    ),

    lc_h2("ch2-sec-01", "Metoda percentylowa"),

    tagList(
      p("Najprostrzy sposób: weź rozkład bootstrapowy i obetnij ogony."),
      p("Dla 95% CI: weź 2.5. i 97.5. percentyl z ",
        withMathJax("\\(B\\)"), " wartości bootstrapowych.")
    ),

    lc_formula_box(
      withMathJax(
        "\\[\\text{CI}_{0.95} = \\left[ Q_{0.025}(\\theta^*_1, \\ldots, \\theta^*_B),\\;
          Q_{0.975}(\\theta^*_1, \\ldots, \\theta^*_B) \\right]\\]"
      ),
      p(style = "font-size:13px; margin-top:8px;",
        "gdzie ", withMathJax("\\(\\theta^*_b\\)"),
        " to wartość statystyki z ", withMathJax("\\(b\\)"),
        "-tej próby bootstrapowej.")
    ),

    # ========================================================================
    # WIDGET 1a: Narastajace belki CI
    # ========================================================================
    lc_h2("ch2-sec-02", "Bootstrap CI dla dowolnej statystyki"),

    tagList(
      p("Kluczowa zaleta bootstrapu: działa tak samo dla ",
        tags$b("każdej statystyki"),
        " — średniej, mediany, odchylenia standardowego, skośności.
         Nie potrzebujemy wzoru analitycznego.")
    ),

    figure_panel(label = "Ryc. 2.1", title = "A) Narastające przedziały — jak bardzo CI skacze między próbami?",
      p(class = "text-muted",
        "Każde kliknięcie losuje nową próbę i dodaje jej CI jako belkę.
         Widoczny jest rozrzut między próbami i asymetria CI."),
      fluidRow(
        column(4,
          selectInput("ch2a_stat", "Statystyka:",
            choices = c(
              "Średnia"      = "mean",
              "Mediana"          = "median",
              "Odch. stand."     = "sd",
              "Skośność" = "skewness"
            ),
            selected = "mean"
          ),
          selectInput("ch2a_dist", "Rozkład danych:",
            choices = c(
              "Normalny"                = "normal",
              "Prawoskośny (Gamma)" = "skewed",
              "Dwumodalny"              = "bimodal",
              "Grube ogony"             = "heavy_tail"
            ),
            selected = "skewed"
          ),
          sliderInput("ch2a_n",    "n:", min = 10, max = 100, value = 25, step = 5),
          sliderInput("ch2a_conf", "Poziom ufności:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          actionButton("ch2a_add", "+ Nowa próba i CI",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          actionButton("ch2a_clear", "Wyczyść",
                       class = "lc-btn-secondary-outline", width = "100%"),
          br(), br(),
          uiOutput("ch2a_stats")
        ),
        column(8,
          plotOutput("ch2a_plot")
        )
      )
    ),

    # ========================================================================
    # WIDGET 1b: Belki CI + histogram aktualnej proby
    # ========================================================================
    figure_panel(label = "Ryc. 2.2", title = "B) Dane i CI razem — jak wyglądają dane które go wygęnerowały?",
      p(class = "text-muted",
        "Lewy panel: histogram aktualnej próby.
         Prawy panel: narastające belki CI z kolejnych prób.
         Pozwala zobaczyć jak kształt danych wpływa na położenie i asymetrię CI."),
      fluidRow(
        column(4,
          selectInput("ch2b_stat", "Statystyka:",
            choices = c(
              "Średnia"      = "mean",
              "Mediana"          = "median",
              "Odch. stand."     = "sd",
              "Skośność" = "skewness"
            ),
            selected = "mean"
          ),
          selectInput("ch2b_dist", "Rozkład danych:",
            choices = c(
              "Normalny"                = "normal",
              "Prawoskośny (Gamma)" = "skewed",
              "Dwumodalny"              = "bimodal",
              "Grube ogony"             = "heavy_tail"
            ),
            selected = "skewed"
          ),
          sliderInput("ch2b_n",    "n:", min = 10, max = 100, value = 25, step = 5),
          sliderInput("ch2b_conf", "Poziom ufności:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          actionButton("ch2b_add", "+ Nowa próba i CI",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          actionButton("ch2b_clear", "Wyczyść",
                       class = "lc-btn-secondary-outline", width = "100%"),
          br(), br(),
          uiOutput("ch2b_stats")
        ),
        column(8,
          plotOutput("ch2b_plot", height = "420px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 1c: Symulacja pokrycia CI
    # ========================================================================
    figure_panel(label = "Ryc. 2.3", title = "C) Pokrycie CI — ile razy CI zawiera prawdziwą wartość?",
      p(class = "text-muted",
        "Symuluje N prób naraz i sprawdza ile CI zawiera prawdziwą wartość parametru.
         Zielony = CI trafiło, czerwony = nie trafiło.
         Teoretycznie przy CL=95% powinno trafić ~95% CI."),
      fluidRow(
        column(4,
          selectInput("ch2c_dist", "Rozkład danych:",
            choices = c(
              "Normalny"                = "normal",
              "Prawoskośny (Gamma)" = "skewed",
              "Dwumodalny"              = "bimodal",
              "Grube ogony"             = "heavy_tail"
            ),
            selected = "normal"
          ),
          sliderInput("ch2c_n",    "n:", min = 10, max = 100, value = 25, step = 5),
          sliderInput("ch2c_conf", "Poziom ufności:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          sliderInput("ch2c_nsim", "Liczba symulacji:",
                      min = 20, max = 100, value = 50, step = 10),
          hr(),
          actionButton("ch2c_run", "Symuluj pokrycie",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch2c_stats")
        ),
        column(8,
          plotOutput("ch2c_plot", height = "500px")
        )
      )
    ),

    lc_feedback(type = "warning",
      tags$strong("Kiedy bootstrap ma przewagę:"),
      tags$ul(
        tags$li("Mała próba + skośny rozkład"),
        tags$li("Statystyki bez analitycznego wzoru na SE (mediana, skośność)"),
        tags$li("Brak pewności co do rozkładu")
      )
    ),

    # ========================================================================
    # WIDGET 2: Bootstrap vs t-CI
    # ========================================================================
    lc_h2("ch2-sec-03", "Bootstrap vs klasyczny przedział t"),

    tagList(
      p("Kiedy dane są normalne i próba duża, bootstrap i t-CI dają prawie identyczne wyniki.
         Kiedy dane są skośne i próba mała — rozchodzą się."),
      p("Bootstrap CI jest wtedy ", tags$b("asymetryczny"),
        ", bo odzwierciedla asymetrię danych.
         T-CI jest symetryczny z założenia.")
    ),

    figure_panel(label = "Ryc. 2.4", title = "Porównanie: Bootstrap vs t-Student",
      fluidRow(
        column(4,
          selectInput("ch2_cmp_dist", "Rozkład:",
            choices = c(
              "Normalny"                = "normal",
              "Prawoskośny (Gamma)" = "skewed",
              "Dwumodalny"              = "bimodal",
              "Grube ogony"             = "heavy_tail"
            ),
            selected = "skewed"
          ),
          sliderInput("ch2_cmp_n", "n:", min = 8, max = 100, value = 20, step = 2),
          sliderInput("ch2_cmp_conf", "Poziom ufności:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          actionButton("ch2_cmp_run", "Porównaj metody",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch2_comparison_plot", height = "300px"),
          uiOutput("ch2_comparison_text")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: CI dla proporcji
    # ========================================================================
    lc_h2("ch2-sec-04", "Bootstrap CI dla proporcji"),

    tagList(
      p("Dla proporcji mamy trzy konkurencyjne metody: Wald (prosta formuła),
         Wilson (dokładniejszy), Bootstrap (symulacyjny)."),
      p("Przy skrajnych proporcjach (bliskie 0 lub 1) i małym n: ",
        tags$b("Wald zawodzi"), " (może wyjść poza [0,1]),
         Wilson i Bootstrap są lepsze.")
    ),

    figure_panel(label = "Ryc. 2.5", title = "Porównanie CI dla proporcji",
      fluidRow(
        column(4,
          sliderInput("ch2_prop_p", "Prawdziwe p (proporcja):",
                      min = 0.02, max = 0.98, value = 0.10, step = 0.01),
          sliderInput("ch2_prop_n", "n:", min = 10, max = 200, value = 30, step = 5),
          sliderInput("ch2_prop_conf", "Poziom ufności:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          actionButton("ch2_prop_run", "Uruchom",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch2_prop_plot", height = "260px"),
          uiOutput("ch2_prop_stats")
        )
      )
    ),

    lc_chapter_next(
      num = "03",
      title = "Bootstrap jednej próby",
      lead = "jak bootstrapować średnie, mediany i inne statystyki jednej próby.",
      target_id = "ch-bootstrap-jednopr"
    )

  )
)
# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  # Pomocnicza: label statystyki
  stat_label_for <- function(s) {
    switch(s,
      "mean"     = "Średnia",
      "median"   = "Mediana",
      "sd"       = "Odch. stand.",
      "skewness" = "Skośność"
    )
  }

  stat_fn_for <- function(s) {
    switch(s,
      "mean"     = mean,
      "median"   = median,
      "sd"       = sd,
      "skewness" = compute_skewness
    )
  }

  # Pomocnicza: prawdziwa wartosc parametru dla danego rozkladu
  true_param_for <- function(dist, stat) {
    switch(dist,
      "normal"     = switch(stat, "mean" = 0,    "median" = 0,    "sd" = 1,   "skewness" = 0),
      "skewed"     = switch(stat, "mean" = 2,    "median" = 1.68, "sd" = 2,   "skewness" = 2),
      "bimodal"    = switch(stat, "mean" = 0,    "median" = 0,    "sd" = 2.2, "skewness" = 0),
      "heavy_tail" = switch(stat, "mean" = 0,    "median" = 0,    "sd" = 1.4, "skewness" = 0)
    )
  }

  # Pomocnicza: wykres narastajacych belek CI
  plot_ci_bands <- function(ci_list, stat_label, conf_level, true_val = NULL) {
    df <- do.call(rbind, lapply(seq_along(ci_list), function(i) {
      ci <- ci_list[[i]]
      data.frame(
        i     = i,
        obs   = ci$obs,
        lower = ci$lower,
        upper = ci$upper
      )
    }))

    covers <- if (!is.null(true_val)) {
      df$lower <= true_val & true_val <= df$upper
    } else {
      rep(TRUE, nrow(df))
    }
    df$covers <- covers
    df$color  <- ifelse(covers, sim_success, sim_observed)

    p <- ggplot(df, aes(y = i)) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, color = covers),
                     height = 0.4, linewidth = 1.2) +
      geom_point(aes(x = obs, color = covers), size = 3) +
      scale_color_manual(values = c("TRUE" = sim_success, "FALSE" = sim_observed),
                         labels = c("TRUE" = "Trafiło", "FALSE" = "Nie trafiło"),
                         name = NULL) +
      scale_y_continuous(breaks = seq_len(nrow(df)),
                         labels = paste0("Próba ", seq_len(nrow(df)))) +
      labs(
        title    = paste0("Bootstrap CI (", round(conf_level * 100), "%) dla ", stat_label),
        subtitle = if (!is.null(true_val))
                     paste0("Prawdziwa wartość = ", round(true_val, 3))
                   else
                     paste0("Liczba prób: ", nrow(df)),
        x = stat_label, y = NULL
      ) +
      theme_upwr() +
      theme(axis.text.y = element_text(size = 10))

    if (!is.null(true_val)) {
      p <- p + geom_vline(xintercept = true_val, color = sim_secondary,
                          linewidth = 1.2, linetype = "dashed")
    }
    p
  }

  # ==========================================================================
  # Widget 1a: narastajace belki CI
  # ==========================================================================
  ch2a_ci_list <- reactiveVal(list())

  observeEvent(list(input$ch2a_stat, input$ch2a_dist, input$ch2a_n), {
    ch2a_ci_list(list())
  }, ignoreInit = TRUE)

  observeEvent(input$ch2a_clear, { ch2a_ci_list(list()) })

  observeEvent(input$ch2a_add, {
    x      <- generate_sample_data(input$ch2a_n, dist = input$ch2a_dist)
    result <- run_bootstrap(x, stat_fn_for(input$ch2a_stat), B = 1000)
    ci     <- bootstrap_ci_percentile(result, conf_level = input$ch2a_conf)
    entry  <- list(obs = result$observed, lower = ci$lower, upper = ci$upper,
                   se = result$se)
    current <- ch2a_ci_list()
    if (length(current) >= 15) current <- current[-1]
    ch2a_ci_list(c(current, list(entry)))
  })

  output$ch2a_plot <- renderPlot({
    cis <- ch2a_ci_list()
    if (length(cis) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij '+ Nowa próba i CI'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }
    true_val <- true_param_for(input$ch2a_dist, input$ch2a_stat)
    plot_ci_bands(cis, stat_label_for(input$ch2a_stat),
                  input$ch2a_conf, true_val = true_val)
  }, height = function() max(300, 80 + length(ch2a_ci_list()) * 40))

  output$ch2a_stats <- renderUI({
    cis <- ch2a_ci_list()
    if (length(cis) == 0) return(NULL)
    last <- cis[[length(cis)]]
    tagList(
      div(class = "lc-stat-box", style = paste0("background:", sim_secondary, ";"),
          paste0("Liczba prób: ", length(cis))),
      div(class = "lc-stat-box", style = paste0("background:", sim_success, ";"),
          paste0("CI: [", round(last$lower, 3), ", ", round(last$upper, 3), "]")),
      div(class = "lc-stat-box", style = paste0("background:", sim_bootstrap, ";"),
          paste0("Szerokość: ", round(last$upper - last$lower, 3)))
    )
  })

  # ==========================================================================
  # Widget 1b: histogram + narastajace belki
  # ==========================================================================
  ch2b_ci_list   <- reactiveVal(list())
  ch2b_last_data <- reactiveVal(NULL)

  observeEvent(list(input$ch2b_stat, input$ch2b_dist, input$ch2b_n), {
    ch2b_ci_list(list()); ch2b_last_data(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$ch2b_clear, { ch2b_ci_list(list()); ch2b_last_data(NULL) })

  observeEvent(input$ch2b_add, {
    x      <- generate_sample_data(input$ch2b_n, dist = input$ch2b_dist)
    result <- run_bootstrap(x, stat_fn_for(input$ch2b_stat), B = 1000)
    ci     <- bootstrap_ci_percentile(result, conf_level = input$ch2b_conf)
    entry  <- list(obs = result$observed, lower = ci$lower, upper = ci$upper,
                   se = result$se)
    current <- ch2b_ci_list()
    if (length(current) >= 12) current <- current[-1]
    ch2b_ci_list(c(current, list(entry)))
    ch2b_last_data(x)
  })

  output$ch2b_plot <- renderPlot({
    cis  <- ch2b_ci_list()
    x    <- ch2b_last_data()

    if (length(cis) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij '+ Nowa próba i CI'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }

    stat_lbl <- stat_label_for(input$ch2b_stat)
    true_val <- true_param_for(input$ch2b_dist, input$ch2b_stat)

    # Lewy panel: histogram ostatniej proby
    last_obs <- cis[[length(cis)]]$obs
    p_hist <- ggplot(data.frame(x = x), aes(x = x)) +
      geom_histogram(fill = sim_bootstrap, color = "white", alpha = 0.8, bins = 15) +
      geom_vline(xintercept = last_obs, color = sim_observed,
                 linewidth = 1.3, linetype = "dashed") +
      labs(title = "Ostatnia próba",
           subtitle = paste0(stat_lbl, " = ", round(last_obs, 3)),
           x = "Wartość", y = "Liczba") +
      theme_upwr()

    # Prawy panel: belki CI
    p_ci <- plot_ci_bands(cis, stat_lbl, input$ch2b_conf, true_val = true_val)

    gridExtra::grid.arrange(p_hist, p_ci, ncol = 2, widths = c(1, 1.4))
  }, height = function() max(360, 100 + length(ch2b_ci_list()) * 35))

  output$ch2b_stats <- renderUI({
    cis <- ch2b_ci_list()
    if (length(cis) == 0) return(NULL)
    last <- cis[[length(cis)]]
    tagList(
      div(class = "lc-stat-box", style = paste0("background:", sim_secondary, ";"),
          paste0("Liczba prób: ", length(cis))),
      div(class = "lc-stat-box", style = paste0("background:", sim_success, ";"),
          paste0("CI: [", round(last$lower, 3), ", ", round(last$upper, 3), "]")),
      div(class = "lc-stat-box", style = paste0("background:", sim_bootstrap, ";"),
          paste0("SE: ", round(last$se, 4)))
    )
  })

  # ==========================================================================
  # Widget 1c: symulacja pokrycia
  # ==========================================================================
  ch2c_result <- reactiveVal(NULL)

  observeEvent(input$ch2c_run, {
    nsim     <- input$ch2c_nsim
    n        <- input$ch2c_n
    conf     <- input$ch2c_conf
    dist     <- input$ch2c_dist
    true_val <- true_param_for(dist, "mean")

    withProgress(message = "Symulowanie CI...", value = 0, {
      cis <- lapply(seq_len(nsim), function(i) {
        setProgress(i / nsim)
        x      <- generate_sample_data(n, dist = dist)
        result <- run_bootstrap(x, mean, B = 500)
        ci     <- bootstrap_ci_percentile(result, conf_level = conf)
        list(obs = result$observed, lower = ci$lower, upper = ci$upper)
      })
    })

    covers <- sapply(cis, function(ci) ci$lower <= true_val & true_val <= ci$upper)
    ch2c_result(list(cis = cis, covers = covers, true_val = true_val,
                     conf = conf, n = n))
  })

  output$ch2c_plot <- renderPlot({
    res <- ch2c_result()
    if (is.null(res)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Symuluj pokrycie'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }

    cis      <- res$cis
    covers   <- res$covers
    true_val <- res$true_val

    df <- do.call(rbind, lapply(seq_along(cis), function(i) {
      data.frame(i = i, obs = cis[[i]]$obs,
                 lower = cis[[i]]$lower, upper = cis[[i]]$upper,
                 covers = covers[i])
    }))

    coverage_pct <- round(mean(covers) * 100, 1)

    ggplot(df, aes(y = i)) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, color = covers),
                     height = 0.5, linewidth = 0.9, alpha = 0.85) +
      geom_point(aes(x = obs, color = covers), size = 2) +
      geom_vline(xintercept = true_val, color = sim_secondary,
                 linewidth = 1.3, linetype = "dashed") +
      scale_color_manual(
        values = c("TRUE" = sim_success, "FALSE" = sim_observed),
        labels = c("TRUE" = "Trafiło", "FALSE" = "Nie trafiło"),
        name = NULL
      ) +
      scale_y_continuous(breaks = NULL) +
      labs(
        title    = paste0("Pokrycie CI: ", coverage_pct, "% (cel: ",
                          round(res$conf * 100), "%)"),
        subtitle = paste0("Prawdziwa wartość średniej = ", round(true_val, 3),
                          "  |  n = ", res$n,
                          "  |  ", sum(covers), " z ", length(covers), " CI trafiło"),
        x = "Średnia", y = "Symulacja"
      ) +
      theme_upwr()
  }, height = function() {
    res <- ch2c_result()
    if (is.null(res)) 500 else max(400, 60 + length(res$cis) * 9)
  })

  output$ch2c_stats <- renderUI({
    res <- ch2c_result()
    if (is.null(res)) return(NULL)
    coverage <- mean(res$covers)
    col_cov  <- if (abs(coverage - res$conf) < 0.05) sim_success else sim_warning
    tagList(
      div(class = "lc-stat-box", style = paste0("background:", col_cov, ";"),
          paste0("Pokrycie: ", round(coverage * 100, 1), "%")),
      div(class = "lc-stat-box", style = paste0("background:", sim_secondary, ";"),
          paste0("Cel: ", round(res$conf * 100), "%")),
      div(class = "lc-stat-box", style = paste0("background:", sim_bootstrap, ";"),
          paste0("Trafiło: ", sum(res$covers), " / ", length(res$covers)))
    )
  })

  # ==========================================================================
  # Widget 2: Bootstrap vs t-CI
  # ==========================================================================
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
                 label = "Kliknij 'Porównaj metody'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }
    ci_df <- rbind(res$ci_boot, res$ci_t)
    plot_ci_comparison(ci_df,
                       sim_bootstrap   = sim_bootstrap,
                       sim_observed = sim_observed,
                       sim_success   = sim_success,
                       sim_warning   = sim_warning)
  })

  output$ch2_comparison_text <- renderUI({
    res <- ch2_cmp_result()
    if (is.null(res)) return(NULL)
    cb <- res$ci_boot
    ct <- res$ci_t
    width_diff     <- abs(cb$width - ct$width)
    asymmetry_boot <- abs((res$boot$observed - cb$lower) -
                          (cb$upper - res$boot$observed))

    if (asymmetry_boot > 0.5 || width_diff / ct$width > 0.1) {
      lc_feedback(type = "warning",
        tags$strong("Rozbieżność:"),
        paste0(" Bootstrap CI jest asymetryczny (różnica lewej/prawej ramki: ",
               round(asymmetry_boot, 2), "). T-CI byłby symetryczny z założenia.
               Przy skośnych danych lub małej próbie bootstrap lepiej odzwierciedla
               niepewność.")
      )
    } else {
      lc_feedback(type = "ok",
        tags$strong("Zgodność:"),
        " Oba metody dają zbliżone wyniki. Przy normalnych danych lub dużym n
         t-CI i bootstrap są równoważne."
      )
    }
  })

  # ==========================================================================
  # Widget 3: CI dla proporcji
  # ==========================================================================
  ch2_prop_result <- reactiveVal(NULL)

  observeEvent(input$ch2_prop_run, {
    p_true <- input$ch2_prop_p
    n      <- input$ch2_prop_n
    conf   <- input$ch2_prop_conf
    k      <- rbinom(1, n, p_true)
    phat   <- k / n
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
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }
    ci_df <- rbind(res$ci_boot, res$ci_classical)
    plot_ci_comparison(ci_df,
                       true_value    = res$p_true,
                       sim_bootstrap   = sim_bootstrap,
                       sim_observed = sim_observed,
                       sim_success   = sim_success,
                       sim_warning   = sim_warning)
  })

  output$ch2_prop_stats <- renderUI({
    res <- ch2_prop_result()
    if (is.null(res)) return(NULL)
    tagList(
      div(class = "lc-stat-box", style = paste0("background:", sim_secondary, ";"),
          paste0("n = ", res$n, ", k = ", res$k)),
      div(class = "lc-stat-box", style = paste0("background:", sim_bootstrap, ";"),
          paste0("p̂ = ", round(res$phat, 3))),
      div(class = "lc-stat-box", style = paste0("background:", sim_success, ";"),
          paste0("p (prawdziwe) = ", round(res$p_true, 3)))
    )
  })

}
