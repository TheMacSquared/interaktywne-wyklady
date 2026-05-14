# ============================================================================
# CHAPTER 7: Monte Carlo
# ============================================================================

ch7_ui <- lecture_chapter(
  id = "ch-monte-carlo",
  num = "07",
  title = "Monte Carlo",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 07 · Symulacje statystyczne",
      num    = "07",
      title  = "Monte Carlo",
      lead   = "Losujemy wiele eksperymentów, żeby zobaczyć moc testu i zachowanie p-wartości."
    ),

    lc_feedback(type = "info",
      "Bootstrap i permutacje korzystają z danych, które mamy.
       Monte Carlo korzysta z ", tags$em("modelu"),
      " — losujemy z rozkładu, aby odpowiedzieć na pytania
       statystyczne bez analitycznych wzorów."
    ),

    lc_h2("ch7-sec-01", "Ogólna idea Monte Carlo"),

    tagList(
      p("Monte Carlo (MC) to rodzina metod opartych na masowym losowaniu.
         Ich siła: jeśli potrafisz ", tags$em("zasymulować"), " eksperyment,
         możesz odpowiedzieć na prawie każde pytanie probabilistyczne."),
      p("Dwa kluczowe zastosowania w statystyce:"),
      tags$ol(
        tags$li(tags$b("Symulacja mocy testu"), " — ile obserwacji potrzebujesz?"),
        tags$li(tags$b("Symulacja pod H₀"), " — uogólniony test permutacyjny")
      )
    ),

    # ========================================================================
    # SEKCJA A: Symulacja mocy testu
    # ========================================================================
    lc_h2("ch7-sec-02", "Symulacja mocy testu"),

    tagList(
      p("Moc testu = P(odrzucenie H₀ | H₁ prawdziwa)."),
      p("Obliczamy ją MC: symulujemy B eksperymentów, w każdym losujemy
         dwie grupy z prawdziwą różnicą δ i sprawdzamy,
         czy test daje p < α. Proporcja takich przypadków to moc.")
    ),

    figure_panel(label = "Ryc. 7.1", title = "Moc testu t — symulacja MC",
      fluidRow(
        column(4,
          sliderInput("ch7_delta", "Prawdziwa różnica δ:",
                      min = 0, max = 30, value = 5, step = 1),
          sliderInput("ch7_n",     "n (na grupę):",
                      min = 5, max = 100, value = 20, step = 5),
          selectInput("ch7_alpha", "Poziom istotności α:",
            choices = c("0.01" = "0.01", "0.05" = "0.05", "0.10" = "0.10"),
            selected = "0.05"
          ),
          sliderInput("ch7_B",     "B (liczba symulacji):",
                      min = 500, max = 5000, value = 1000, step = 500),
          hr(),
          actionButton("ch7_run_power", "Symuluj moc",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch7_power_stats")
        ),
        column(8,
          zoom_plot_ui("ch7_power_hist", height = "320px")
        )
      )
    ),

    lc_feedback(type = "info",
      tags$strong("Moc = P(odrzucenie H₀ | H₁ prawdziwa)."),
      " Tu: proporcja p-wartości < α z B symulowanych eksperymentów.
       Przy δ = 0 moc powinna równać się α (to poziom istotności)."
    ),

    # ========================================================================
    # WIDGET: Krzywa mocy
    # ========================================================================
    lc_h2("ch7-sec-03", "Krzywa mocy"),

    tagList(
      p("Jak zmienia się moc testu wraz z efektem δ?
         Dla dużych efektów test prawie zawsze wykrywa różnicę.
         Dla małych — często nie.")
    ),

    figure_panel(label = "Ryc. 7.2", title = "Krzywa mocy testu t",
      fluidRow(
        column(4,
          sliderInput("ch7_pc_n",    "n (na grupę):",
                      min = 5, max = 100, value = 20, step = 5),
          sliderInput("ch7_pc_d_max", "Maksymalne δ na wykresie:",
                      min = 10, max = 50, value = 30, step = 5),
          selectInput("ch7_pc_alpha", "α:",
            choices = c("0.01" = "0.01", "0.05" = "0.05", "0.10" = "0.10"),
            selected = "0.05"
          ),
          actionButton("ch7_pc_run", "Oblicz krzywą mocy",
                       class = "lc-btn-warning", width = "100%"),
          br(),
          p(class = "text-muted", style = "font-size:12px;",
            "Uwaga: obliczenie krzywej trwa kilka sekund (B=500 na punkt).")
        ),
        column(8,
          zoom_plot_ui("ch7_power_curve", height = "300px")
        )
      )
    ),

    # ========================================================================
    # SEKCJA B: Symulacja pod H0
    # ========================================================================
    lc_h2("ch7-sec-04", "Symulacja pod H₀"),

    tagList(
      p("Test permutacyjny przetasowuje etykiety. Ale co jeśli H₀ określa
         ", tags$b("konkretny rozkład"), " (np. H₀: μ = 100), a nie wymienność grup?"),
      p("Wtedy losujemy statystykę testowaną z rozkładu pod H₀ przez symulację MC.
         Uzyskujemy p-wartość MC, która może być porównana z klasyczną.")
    ),

    figure_panel(label = "Ryc. 7.3", title = "Symulacja rozkładu pod H₀",
      fluidRow(
        column(4,
          selectInput("ch7_null_scenario", "Scenariusz:",
            choices = c(
              "Test chi-kwadrat (zgodność)" = "chisq",
              "Test t jednej próby (H₀: μ=0)" = "ttest_one",
              "Test proporcji (H₀: p=0.5)"    = "proportion"
            ),
            selected = "ttest_one"
          ),
          sliderInput("ch7_null_n", "n:", min = 10, max = 80, value = 25, step = 5),
          sliderInput("ch7_null_B", "B (symulacje MC):",
                      min = 1000, max = 10000, value = 3000, step = 1000),
          actionButton("ch7_null_run", "Symuluj pod H₀",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch7_null_stats")
        ),
        column(8,
          zoom_plot_ui("ch7_null_plot", height = "340px")
        )
      )
    ),

    lc_feedback(type = "info",
      tags$strong("MC pod H₀ vs test permutacyjny:"),
      tags$ul(
        tags$li("Test permutacyjny: przetasowuje etykiety — działa gdy H₀
                mówi, że grupy są jednorodne"),
        tags$li("MC pod H₀: losuje z parametrycznego rozkładu — gdy H₀ określa
                konkretny rozkład")
      )
    ),

    lc_chapter_next(
      num = "08",
      title = "Kiedy stosować?",
      lead = "mapa decyzji dla metod resamplingowych.",
      target_id = "ch-kiedy"
    )

  )
)
# ============================================================================
# SERVER
# ============================================================================

ch7_server <- function(input, output, session) {

  # --- Widget 1: Symulacja mocy ---
  ch7_power_result <- reactiveVal(NULL)

  observeEvent(input$ch7_run_power, {
    withProgress(message = "Symuluję moc testu...", value = 0, {
      result <- run_mc_power(
        n     = input$ch7_n,
        delta = input$ch7_delta,
        alpha = as.numeric(input$ch7_alpha),
        B     = input$ch7_B
      )
      setProgress(1)
    })
    ch7_power_result(result)
  })

  zoom_plot_server("ch7_power_hist", reactive({
    result <- ch7_power_result()
    if (is.null(result)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Symuluj moc'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }
    plot_power_histogram(result, sim_bootstrap = sim_bootstrap, sim_observed = sim_observed)
  }))

  output$ch7_power_stats <- renderUI({
    result <- ch7_power_result()
    if (is.null(result)) return(NULL)
    moc     <- result$power
    alpha   <- result$alpha
    moc_col <- if (moc >= 0.80) sim_success else if (moc >= 0.50) sim_warning else sim_observed
    tagList(
      div(class = "lc-stat-box", style = paste0("background:", moc_col, ";"),
          paste0("Moc = ", round(moc * 100, 1), "%")),
      div(class = "lc-stat-box", style = paste0("background:", sim_secondary, ";"),
          paste0("α = ", alpha)),
      div(class = "lc-stat-box", style = paste0("background:", sim_bootstrap, ";"),
          paste0("n = ", result$n, ", δ = ", result$delta))
    )
  })

  # --- Widget: Krzywa mocy ---
  ch7_pc_result <- reactiveVal(NULL)

  observeEvent(input$ch7_pc_run, {
    withProgress(message = "Obliczam krzywą mocy...", value = 0, {
      d_seq  <- seq(0, input$ch7_pc_d_max, by = max(1, input$ch7_pc_d_max / 15))
      df_pow <- compute_power_curve(
        n         = input$ch7_pc_n,
        delta_seq = d_seq,
        alpha     = as.numeric(input$ch7_pc_alpha),
        B         = 500
      )
      attr(df_pow, "n") <- input$ch7_pc_n
      setProgress(1)
    })
    ch7_pc_result(df_pow)
  })

  zoom_plot_server("ch7_power_curve", reactive({
    df_pow <- ch7_pc_result()
    if (is.null(df_pow)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Oblicz krzywą mocy'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }
    plot_power_curve(
      power_df      = df_pow,
      current_delta = input$ch7_delta,
      alpha         = as.numeric(input$ch7_pc_alpha),
      sim_bootstrap   = sim_bootstrap,
      sim_observed = sim_observed
    )
  }))

  # --- Widget 2: Symulacja pod H0 ---
  ch7_null_result <- reactiveVal(NULL)

  observeEvent(input$ch7_null_run, {
    n        <- input$ch7_null_n
    scenario <- input$ch7_null_scenario

    # Generuj dane obserwowane (z pewnym efektem, nie pod H0)
    obs_data <- switch(scenario,
      "chisq"       = sample(c("A", "B", "C", "D"),
                              size = n, replace = TRUE,
                              prob = c(0.35, 0.25, 0.25, 0.15)),
      "ttest_one"   = rnorm(n, mean = 3, sd = 10),   # H0: mu=0, efekt=3
      "proportion"  = rbinom(n, 1, 0.65)             # H0: p=0.5, efekt: p=0.65
    )

    withProgress(message = "Symuluję pod H₀...", value = 0, {
      result <- run_mc_null(obs_data, scenario = scenario, B = input$ch7_null_B)
      setProgress(1)
    })
    ch7_null_result(result)
  })

  zoom_plot_server("ch7_null_plot", reactive({
    result <- ch7_null_result()
    if (is.null(result)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Symuluj pod H₀'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }

    df      <- data.frame(stat = result$null_stats)
    obs     <- result$observed_stat
    extreme <- abs(df$stat) >= abs(obs)

    stat_label <- switch(result$scenario,
      "chisq"      = "Statystyka chi-kwadrat*",
      "ttest_one"  = "Statystyka t*",
      "proportion" = "Statystyka z* (proporcja)"
    )

    ggplot(df, aes(x = stat, fill = extreme)) +
      geom_histogram(bins = 50, color = "white", alpha = 0.85) +
      scale_fill_manual(values = c("FALSE" = sim_null_dist, "TRUE" = sim_observed),
                        guide = "none") +
      geom_vline(xintercept  = obs,       color = sim_observed, linewidth = 1.6) +
      geom_vline(xintercept = -abs(obs),  color = sim_observed,
                 linewidth = 1.2, linetype = "dashed") +
      annotate("text", x = obs, y = Inf,
               label = paste0("obs = ", round(obs, 3)),
               vjust = -0.3, hjust = -0.1, color = sim_observed, size = 4.5) +
      labs(
        
        
        x        = stat_label,
        y        = "Liczba symulacji"
      ) +
      theme_upwr()
  }))

  output$ch7_null_stats <- renderUI({
    result <- ch7_null_result()
    if (is.null(result)) return(NULL)
    pv_mc  <- format_pval_pl(result$p_value_mc)
    pv_cl  <- format_pval_pl(result$classical_p)
    tagList(
      div(class = "lc-stat-box", style = paste0("background:", sim_bootstrap, ";"),
          paste0("obs = ", round(result$observed_stat, 3))),
      div(class = "lc-stat-box",
          style = paste0("background:", pv_mc$color, ";"),
          paste0("p MC = ", round(result$p_value_mc, 4))),
      div(class = "lc-stat-box",
          style = paste0("background:", pv_cl$color, ";"),
          paste0("p klas. = ", round(result$classical_p, 4)))
    )
  })

}
