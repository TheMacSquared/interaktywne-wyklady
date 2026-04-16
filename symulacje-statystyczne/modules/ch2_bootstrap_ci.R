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
    # WIDGET 1a: Narastajace belki CI
    # ========================================================================
    div(class = "section-title", "Bootstrap CI dla dowolnej statystyki"),

    div(class = "narrative",
      p("Kluczowa zaleta bootstrapu: dzia\u0142a tak samo dla ",
        tags$b("ka\u017cdej statystyki"),
        " \u2014 \u015bredniej, mediany, odchylenia standardowego, sko\u015bno\u015bci.
         Nie potrzebujemy wzoru analitycznego.")
    ),

    div(class = "widget-block",
      h4("A) Narastaj\u0105ce przedzia\u0142y \u2014 jak bardzo CI skacze mi\u0119dzy pr\u00f3bami?"),
      p(class = "text-muted",
        "Ka\u017cde klikni\u0119cie losuje now\u0105 pr\u00f3b\u0119 i dodaje jej CI jako belk\u0119.
         Widoczny jest rozrzut mi\u0119dzy pr\u00f3bami i asymetria CI."),
      fluidRow(
        column(4,
          selectInput("ch2a_stat", "Statystyka:",
            choices = c(
              "\u015arednia"      = "mean",
              "Mediana"          = "median",
              "Odch. stand."     = "sd",
              "Sko\u015bno\u015b\u0107" = "skewness"
            ),
            selected = "mean"
          ),
          selectInput("ch2a_dist", "Rozk\u0142ad danych:",
            choices = c(
              "Normalny"                = "normal",
              "Prawosko\u015bny (Gamma)" = "skewed",
              "Dwumodalny"              = "bimodal",
              "Grube ogony"             = "heavy_tail"
            ),
            selected = "skewed"
          ),
          sliderInput("ch2a_n",    "n:", min = 10, max = 100, value = 25, step = 5),
          sliderInput("ch2a_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          actionButton("ch2a_add", "+ Nowa pr\u00f3ba i CI",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          actionButton("ch2a_clear", "Wyczy\u015b\u0107",
                       class = "btn-outline-secondary", width = "100%"),
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
    div(class = "widget-block",
      h4("B) Dane i CI razem \u2014 jak wygl\u0105daj\u0105 dane kt\u00f3re go wyg\u0119nerowa\u0142y?"),
      p(class = "text-muted",
        "Lewy panel: histogram aktualnej pr\u00f3by.
         Prawy panel: narastaj\u0105ce belki CI z kolejnych pr\u00f3b.
         Pozwala zobaczy\u0107 jak kszta\u0142t danych wp\u0142ywa na po\u0142o\u017cenie i asymetri\u0119 CI."),
      fluidRow(
        column(4,
          selectInput("ch2b_stat", "Statystyka:",
            choices = c(
              "\u015arednia"      = "mean",
              "Mediana"          = "median",
              "Odch. stand."     = "sd",
              "Sko\u015bno\u015b\u0107" = "skewness"
            ),
            selected = "mean"
          ),
          selectInput("ch2b_dist", "Rozk\u0142ad danych:",
            choices = c(
              "Normalny"                = "normal",
              "Prawosko\u015bny (Gamma)" = "skewed",
              "Dwumodalny"              = "bimodal",
              "Grube ogony"             = "heavy_tail"
            ),
            selected = "skewed"
          ),
          sliderInput("ch2b_n",    "n:", min = 10, max = 100, value = 25, step = 5),
          sliderInput("ch2b_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          hr(),
          actionButton("ch2b_add", "+ Nowa pr\u00f3ba i CI",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          actionButton("ch2b_clear", "Wyczy\u015b\u0107",
                       class = "btn-outline-secondary", width = "100%"),
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
    div(class = "widget-block",
      h4("C) Pokrycie CI \u2014 ile razy CI zawiera prawdziw\u0105 warto\u015b\u0107?"),
      p(class = "text-muted",
        "Symuluje N pr\u00f3b naraz i sprawdza ile CI zawiera prawdziw\u0105 warto\u015b\u0107 parametru.
         Zielony = CI trafi\u0142o, czerwony = nie trafi\u0142o.
         Teoretycznie przy CL=95% powinno trafi\u0107 ~95% CI."),
      fluidRow(
        column(4,
          selectInput("ch2c_dist", "Rozk\u0142ad danych:",
            choices = c(
              "Normalny"                = "normal",
              "Prawosko\u015bny (Gamma)" = "skewed",
              "Dwumodalny"              = "bimodal",
              "Grube ogony"             = "heavy_tail"
            ),
            selected = "normal"
          ),
          sliderInput("ch2c_n",    "n:", min = 10, max = 100, value = 25, step = 5),
          sliderInput("ch2c_conf", "Poziom ufno\u015bci:",
                      min = 0.80, max = 0.99, value = 0.95, step = 0.01),
          sliderInput("ch2c_nsim", "Liczba symulacji:",
                      min = 20, max = 100, value = 50, step = 10),
          hr(),
          actionButton("ch2c_run", "Symuluj pokrycie",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch2c_stats")
        ),
        column(8,
          plotOutput("ch2c_plot", height = "500px")
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
              "Normalny"                = "normal",
              "Prawosko\u015bny (Gamma)" = "skewed",
              "Dwumodalny"              = "bimodal",
              "Grube ogony"             = "heavy_tail"
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

  # Pomocnicza: label statystyki
  stat_label_for <- function(s) {
    switch(s,
      "mean"     = "\u015arednia",
      "median"   = "Mediana",
      "sd"       = "Odch. stand.",
      "skewness" = "Sko\u015bno\u015b\u0107"
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
    df$color  <- ifelse(covers, "#27ae60", "#e74c3c")

    p <- ggplot(df, aes(y = i)) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, color = covers),
                     height = 0.4, linewidth = 1.2) +
      geom_point(aes(x = obs, color = covers), size = 3) +
      scale_color_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#e74c3c"),
                         labels = c("TRUE" = "Trafi\u0142o", "FALSE" = "Nie trafi\u0142o"),
                         name = NULL) +
      scale_y_continuous(breaks = seq_len(nrow(df)),
                         labels = paste0("Pr\u00f3ba ", seq_len(nrow(df)))) +
      labs(
        title    = paste0("Bootstrap CI (", round(conf_level * 100), "%) dla ", stat_label),
        subtitle = if (!is.null(true_val))
                     paste0("Prawdziwa warto\u015b\u0107 = ", round(true_val, 3))
                   else
                     paste0("Liczba pr\u00f3b: ", nrow(df)),
        x = stat_label, y = NULL
      ) +
      theme_educational() +
      theme(axis.text.y = element_text(size = 10))

    if (!is.null(true_val)) {
      p <- p + geom_vline(xintercept = true_val, color = col_dark,
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
                 label = "Kliknij '+ Nowa pr\u00f3ba i CI'",
                 size = 6, color = "#7f8c8d") +
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
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("Liczba pr\u00f3b: ", length(cis))),
      div(class = "stat-box", style = paste0("background:", col_success, ";"),
          paste0("CI: [", round(last$lower, 3), ", ", round(last$upper, 3), "]")),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("Szeroko\u015b\u0107: ", round(last$upper - last$lower, 3)))
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
                 label = "Kliknij '+ Nowa pr\u00f3ba i CI'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
      return()
    }

    stat_lbl <- stat_label_for(input$ch2b_stat)
    true_val <- true_param_for(input$ch2b_dist, input$ch2b_stat)

    # Lewy panel: histogram ostatniej proby
    last_obs <- cis[[length(cis)]]$obs
    p_hist <- ggplot(data.frame(x = x), aes(x = x)) +
      geom_histogram(fill = col_primary, color = "white", alpha = 0.8, bins = 15) +
      geom_vline(xintercept = last_obs, color = "#e74c3c",
                 linewidth = 1.3, linetype = "dashed") +
      labs(title = "Ostatnia pr\u00f3ba",
           subtitle = paste0(stat_lbl, " = ", round(last_obs, 3)),
           x = "Warto\u015b\u0107", y = "Liczba") +
      theme_educational()

    # Prawy panel: belki CI
    p_ci <- plot_ci_bands(cis, stat_lbl, input$ch2b_conf, true_val = true_val)

    gridExtra::grid.arrange(p_hist, p_ci, ncol = 2, widths = c(1, 1.4))
  }, height = function() max(360, 100 + length(ch2b_ci_list()) * 35))

  output$ch2b_stats <- renderUI({
    cis <- ch2b_ci_list()
    if (length(cis) == 0) return(NULL)
    last <- cis[[length(cis)]]
    tagList(
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("Liczba pr\u00f3b: ", length(cis))),
      div(class = "stat-box", style = paste0("background:", col_success, ";"),
          paste0("CI: [", round(last$lower, 3), ", ", round(last$upper, 3), "]")),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
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
                 size = 6, color = "#7f8c8d") +
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
      geom_vline(xintercept = true_val, color = col_dark,
                 linewidth = 1.3, linetype = "dashed") +
      scale_color_manual(
        values = c("TRUE" = "#27ae60", "FALSE" = "#e74c3c"),
        labels = c("TRUE" = "Trafi\u0142o", "FALSE" = "Nie trafi\u0142o"),
        name = NULL
      ) +
      scale_y_continuous(breaks = NULL) +
      labs(
        title    = paste0("Pokrycie CI: ", coverage_pct, "% (cel: ",
                          round(res$conf * 100), "%)"),
        subtitle = paste0("Prawdziwa warto\u015b\u0107 \u015bredniej = ", round(true_val, 3),
                          "  |  n = ", res$n,
                          "  |  ", sum(covers), " z ", length(covers), " CI trafi\u0142o"),
        x = "\u015arednia", y = "Symulacja"
      ) +
      theme_educational()
  }, height = function() {
    res <- ch2c_result()
    if (is.null(res)) 500 else max(400, 60 + length(res$cis) * 9)
  })

  output$ch2c_stats <- renderUI({
    res <- ch2c_result()
    if (is.null(res)) return(NULL)
    coverage <- mean(res$covers)
    col_cov  <- if (abs(coverage - res$conf) < 0.05) col_success else col_warning
    tagList(
      div(class = "stat-box", style = paste0("background:", col_cov, ";"),
          paste0("Pokrycie: ", round(coverage * 100, 1), "%")),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
          paste0("Cel: ", round(res$conf * 100), "%")),
      div(class = "stat-box", style = paste0("background:", col_primary, ";"),
          paste0("Trafi\u0142o: ", sum(res$covers), " / ", length(res$covers)))
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
                 label = "Kliknij 'Por\u00f3wnaj metody'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
      return()
    }
    ci_df <- rbind(res$ci_boot, res$ci_t)
    plot_ci_comparison(ci_df,
                       col_primary   = col_primary,
                       col_secondary = col_secondary,
                       col_success   = col_success,
                       col_warning   = col_warning)
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
                 size = 6, color = "#7f8c8d") +
        theme_void()
      return()
    }
    ci_df <- rbind(res$ci_boot, res$ci_classical)
    plot_ci_comparison(ci_df,
                       true_value    = res$p_true,
                       col_primary   = col_primary,
                       col_secondary = col_secondary,
                       col_success   = col_success,
                       col_warning   = col_warning)
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
