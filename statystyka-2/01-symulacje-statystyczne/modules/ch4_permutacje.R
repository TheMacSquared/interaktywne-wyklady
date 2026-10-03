# ============================================================================
# CHAPTER 4: Testy permutacyjne
# ============================================================================

ch4_ui <- lecture_chapter(
  id = "ch-permutacje",
  num = "04",
  title = "Testy permutacyjne",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 04 · Symulacje statystyczne",
      num    = "04",
      title  = "Testy permutacyjne",
      lead   = "Symulujemy świat hipotezy zerowej przez permutacje i porównujemy obserwowany efekt z rozkładem losowym."
    ),

    lc_feedback(type = "info",
      "Bootstrap pozwala budować przedziały bez założenia normalności,
       jeśli schemat losowania odpowiada strukturze danych. Teraz poznamy
       testy permutacyjne: przetasowanie etykiet wymaga wymienności pod H₀."
    ),

    lc_h2("ch4-sec-01", "Idea testu permutacyjnego"),

    tagList(
      p("Wyobraźmy sobie eksperyment: dwie grupy roślin, nawóz A i nawóz B.
         Pytamy: czy nawóz wpływa na plony?"),
      p("H₀ mówi: nawóz nie ma wpływu. Jeśli tak, to do której grupy trafiła
         dana roślina jest ", tags$b("bez znaczenia"),
        " — plony byłyby takie same niezależnie od przypisania.")
    ),

    lc_feedback(type = "info",
      tags$strong("Kluczowa idea:"),
      " H₀ mówi, że grupy są jednorodne. Jeśli tak, przypisanie
      „Grupa A‟ vs „Grupa B‟ jest arbitralne — możemy je losowo zamienić.
      Test permutacyjny sprawdza: jak ekstremalna jest nasza obserwowana różnica,
      gdy losujemy takie zamiany?"
    ),

    # ========================================================================
    # WIDGET 1: Test permutacyjny 5 krokow (showpiece)
    # ========================================================================
    lc_feedback(type = "warning",
      tags$strong("Kiedy wolno mieszać:"),
      " w pokazanym modelu H₀ oznacza identyczne rozkłady w niezależnych grupach. Sama równość średnich nie wystarcza. Przy pomiarach przed i po u tych samych osób trzeba zachować pary; swobodne mieszanie wszystkich obserwacji niszczy plan badania."
    ),

    lc_h2("ch4-sec-02", "Test permutacyjny — krok po kroku"),

    figure_panel(label = "Ryc. 4.1",
      lc_step_widget("ch4_perm",
        title = "Permutacyjny test różnicy średnich",
        steps = c("Dane", "Permutacja", "Rozkład", "p-wartość"),
        toolbar = lc_toolbar(
          lc_slider("ch4_n_per_group", "n na grupę", 10, 50, 20, 5),
          lc_slider("ch4_true_diff", "Prawdziwa różnica średnich (efekt)", 0, 20, 0, 1),
          lc_step_from(3, lc_slider("ch4_n_perms", "Liczba permutacji (B)", 200, 5000, 1000, 200)),
          selectInput("ch4_dist", "Rozkład",
            choices = c(
              "Prawoskosśny (Gamma)" = "skewed",
              "Normalny"               = "normal",
              "Grube ogony"            = "heavy_tail"
            ),
            selected = "skewed"
          ),
          lc_step_from(2, lc_action("ch4_perm_shuffle", "Nowa permutacja", icon = "shuffle",
                                    variant = "outline")),
          lc_action("ch4_perm_new", "Nowe dane", icon = "shuffle", variant = "outline")
        ),
        plot_id = "ch4_perm_plot",
        extra = uiOutput("ch4_perm_result")
      )
    ),

    lc_feedback(type = "ok",
      tags$strong("Aha-moment:"),
      " Rozkład permutacyjny to empiryczny rozkład pod H₀.
       Nie zakładamy żadnego rozkładu analitycznego — „budujemy‟ H₀ z danych."
    ),

    # ========================================================================
    # WIDGET 2: Permutacyjny test korelacji
    # ========================================================================
    lc_h2("ch4-sec-03", "Permutacyjny test korelacji"),

    tagList(
      p("To samo podejście działa dla korelacji.
         Jeśli H₀: brak związku między x i y, to kolejność x względem y
         jest dowolna — możemy przetasowywać jedną zmienną.")
    ),

    figure_panel(label = "Ryc. 4.2", title = "Permutacyjny test korelacji",
      fluidRow(
        column(4,
          lc_slider("ch4_cor_n", "n", 15, 80, 30, 5),
          lc_slider("ch4_cor_true_r", "Prawdziwa korelacja ρ", 0, 0.8, 0.4, 0.1),
          lc_slider("ch4_cor_B", "B permutacji", 500, 5000, 1000, 500),
          lc_action("ch4_cor_run", "Uruchom", variant = "solid"),
          br(), br(),
          uiOutput("ch4_cor_result")
        ),
        column(8,
          zoom_plot_ui("ch4_cor_plot", height = "380px")
        )
      )
    ),

    lc_feedback(type = "warning",
      tags$strong("Kiedy stosować test permutacyjny dla korelacji:"),
      " gdy mamy obserwacje odstające, rozkłady dalekie od normalnych lub
       małą próbę. Klasyczny Pearson wymaga normalności dwuwymiarowej —
       test permutacyjny nie."
    ),

    lc_chapter_next(
      num = "05",
      title = "Jackknife",
      lead = "leave-one-out jako szybka diagnostyka obciążenia i błędu standardowego.",
      target_id = "ch-jackknife"
    )

  )
)
# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # Krok widgetu (1..4) żyje w przeglądarce. Losowania są reaktywne: dane od
  # parametrów i „Nowe dane”, permutacja od danych i „Nowa permutacja”,
  # rozkład permutacyjny od danych i B. Przyciski losowania przesuwają krok.
  ch4_s    <- lc_step_server("ch4_perm", input)
  ch4_step <- ch4_s$step

  # Krok 1: dane
  ch4_data <- reactive({
    input$ch4_perm_new
    generate_two_groups_data(
      n_per_group = input$ch4_n_per_group,
      effect      = input$ch4_true_diff,
      dist        = input$ch4_dist
    )
  })

  observeEvent(input$ch4_perm_new, ch4_s$set(1), ignoreInit = TRUE)

  # Krok 2: jedna permutacja
  ch4_one_perm <- reactive({
    input$ch4_perm_shuffle
    df_perm       <- ch4_data()
    df_perm$group <- sample(df_perm$group)
    df_perm
  })

  observeEvent(input$ch4_perm_shuffle, ch4_s$set(2), ignoreInit = TRUE)

  # Krok 3: rozklad permutacyjny (B permutacji)
  ch4_perm_res <- reactive({
    df <- ch4_data()
    B  <- input$ch4_n_perms
    withProgress(message = "Wykonuję permutacje...", value = 0, {
      result <- run_permutation_test_twosample(df, B = B)
      setProgress(1)
    })
    result
  })

  # Stała rama kroków 1–2 (te same wartości, przetasowane etykiety).
  ch4_data_ylim <- reactive({
    v <- ch4_data()$value
    c(min(v) - diff(range(v)) * 0.05, max(v) + diff(range(v)) * 0.15)
  })

  # Stała rama kroków 3–4: zakres i liczebności rozkładu permutacyjnego.
  ch4_perm_frame <- reactive({
    result <- ch4_perm_res()
    lims   <- range(result$perm_diffs, result$observed_diff, -abs(result$observed_diff))
    pad    <- diff(lims) * 0.06 + 1e-9
    breaks <- seq(lims[1] - pad, lims[2] + pad, length.out = 41)
    counts <- hist(result$perm_diffs, breaks = breaks, plot = FALSE)$counts
    list(breaks = breaks, xlim = range(breaks), ylim = c(0, max(counts) * 1.15))
  })

  # Dwie grupy: A = dane (niebo), B = grupa (bursztyn); kontury pudełek czarne.
  ch4_group_plot <- function(df, label, x_lab) {
    ylim <- ch4_data_ylim()
    ggplot(df, aes(x = group, y = value)) +
      geom_boxplot(aes(fill = group), alpha = STEP_ROLES$data$alpha,
                   colour = STEP_EDGE$colour, linewidth = STEP_EDGE$linewidth,
                   outlier.shape = NA) +
      geom_jitter(aes(colour = group), width = 0.15, size = 2,
                  alpha = STEP_ROLES$data$alpha) +
      scale_fill_manual(values = c("A" = STEP_ROLES$data$colour,
                                   "B" = STEP_ROLES$group$colour)) +
      scale_colour_manual(values = c("A" = STEP_ROLES$data$colour,
                                     "B" = STEP_ROLES$group$colour)) +
      step_label(1.5, ylim[2], label, role = "new", hjust = 0.5, vjust = 1, size = 4.6) +
      labs(x = x_lab, y = "Wartość") +
      step_frame(xlim = c(0.4, 2.6), ylim = ylim)
  }

  zoom_plot_server("ch4_perm_plot", reactive({
    step <- ch4_step()
    df   <- ch4_data()

    if (step == 1) {
      obs_diff <- mean(df$value[df$group == "B"]) - mean(df$value[df$group == "A"])
      ch4_group_plot(df, paste0("Δ obs = ", round(obs_diff, 2)), "Grupa")
    } else if (step == 2) {
      perm_df   <- ch4_one_perm()
      perm_diff <- mean(perm_df$value[perm_df$group == "B"]) -
                   mean(perm_df$value[perm_df$group == "A"])
      ch4_group_plot(perm_df, paste0("Δ perm = ", round(perm_diff, 2)),
                     "Grupa (przetasowana)")
    } else {
      # Krok 3 i 4: rozklad permutacyjny; ogony równie ekstremalne jako druga grupa.
      result   <- ch4_perm_res()
      fr       <- ch4_perm_frame()
      obs_diff <- result$observed_diff
      df_perm_dist <- data.frame(diff = result$perm_diffs,
                                 extreme = abs(result$perm_diffs) >= abs(obs_diff))
      line_role <- step_role(step, 3)

      p <- ggplot(df_perm_dist, aes(x = diff, fill = extreme)) +
        geom_histogram(breaks = fr$breaks, alpha = STEP_ROLES$data$alpha,
                       colour = STEP_EDGE$colour, linewidth = STEP_EDGE$linewidth) +
        scale_fill_manual(values = c("FALSE" = STEP_ROLES$data$colour,
                                     "TRUE"  = STEP_ROLES$group$colour)) +
        step_line(line_role, xintercept = obs_diff, helper = FALSE) +
        step_line(line_role, xintercept = -abs(obs_diff)) +
        labs(x = "Permutacyjna różnica średnich (Δ*)", y = "Liczba permutacji")

      if (step == 4) {
        p <- p + step_label(obs_diff, fr$ylim[2],
                            paste0(" obs Δ = ", round(obs_diff, 2)),
                            role = "new", vjust = 1)
      }
      p + step_frame(xlim = fr$xlim, ylim = fr$ylim)
    }
  }))

  output$ch4_perm_text <- renderUI({
    step <- ch4_step()
    switch(as.character(step),
      "1" = "Dane pobrane. Obserwujemy różnicę średnich między grupami.",
      "2" = "Jedna permutacja: etykiety grup przetasowane losowo pod H₀.",
      "3" = paste0("Rozkład z B = ", input$ch4_n_perms,
                   " permutacji gotowy. To empiryczny rozkład pod H₀."),
      "4" = format_pval_pl(ch4_perm_res()$p_value)$decision,
      ""
    )
  })

  output$ch4_perm_result <- renderUI({
    req(ch4_step() >= 3)
    result <- ch4_perm_res()
    # Ttest do porownania
    tt <- tryCatch(classical_ttest_twosample(ch4_data()), error = function(e) NULL)

    lc_status(
      lc_readouts(
        lc_readout("Δ obs", lc_fmt(result$observed_diff, 3), color = STEP_ROLES$known$colour),
        lc_readout("p (perm)", lc_fmt(result$p_value, 4),
                   color = format_pval_pl(result$p_value)$color),
        if (!is.null(tt)) lc_readout("p (t-test)", lc_fmt(tt$p, 4), color = sim_classical)
      ),
      p(simulation_precision_note(result))
    )
  })

  # --- Widget 2: Test permutacyjny korelacji ---
  ch4_cor_result_rv <- reactiveVal(NULL)
  ch4_cor_data_rv   <- reactiveVal(NULL)

  observeEvent(input$ch4_cor_run, {
    df     <- generate_bivariate_data(n = input$ch4_cor_n, true_r = input$ch4_cor_true_r)
    result <- run_permutation_test_correlation(df, B = input$ch4_cor_B)
    ch4_cor_data_rv(df)
    ch4_cor_result_rv(result)
  })

  zoom_plot_server("ch4_cor_plot", reactive({
    res <- ch4_cor_result_rv()
    df  <- ch4_cor_data_rv()

    if (is.null(res)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Uruchom'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }

    # Dwa panele: scatter + rozklad permutacyjny
    p1 <- ggplot(df, aes(x = x, y = y)) +
      geom_point(color = sim_bootstrap, size = 2.5, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = sim_observed, linewidth = 1) +
      annotate("text", x = min(df$x), y = max(df$y),
               label = paste0("r = ", round(res$observed_r, 3)),
               hjust = 0, vjust = 1, size = 5, fontface = "bold", color = sim_observed) +
      labs(
           x = "x", y = "y") +
      theme_upwr()

    df_perm <- data.frame(r = res$perm_cors)
    extreme <- abs(df_perm$r) >= abs(res$observed_r)

    p2 <- ggplot(df_perm, aes(x = r, fill = extreme)) +
      geom_histogram(bins = 40, color = "white", alpha = 0.85) +
      scale_fill_manual(values = c("FALSE" = sim_null_dist, "TRUE" = sim_observed),
                        guide = "none") +
      geom_vline(xintercept  = res$observed_r, color = sim_observed, linewidth = 1.5) +
      geom_vline(xintercept = -abs(res$observed_r), color = sim_observed,
                 linewidth = 1.2, linetype = "dashed") +
      labs(
        
        
        x        = "Korelacja r*",
        y        = "Liczba permutacji"
      ) +
      theme_upwr()

    gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(1.4, 1))
  }))

  output$ch4_cor_result <- renderUI({
    res <- ch4_cor_result_rv()
    if (is.null(res)) return(NULL)
    pv  <- format_pval_pl(res$p_value)
    lc_stat_grid(
      lc_stat_box("r", round(res$observed_r, 3), color = sim_observed),
      lc_stat_box("p (perm)", round(res$p_value, 4), color = pv$color)
    ) |> tagList(lc_feedback(type = "info", simulation_precision_note(res)))
  })

}
