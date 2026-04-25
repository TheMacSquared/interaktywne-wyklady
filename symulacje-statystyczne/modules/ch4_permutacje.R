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
      "Mamy przedziały ufności bez założeń.
       Teraz testy hipotez bez założeń — przez przetasowanie etykiet."
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
    lc_h2("ch4-sec-02", "Test permutacyjny — krok po kroku"),

    figure_panel(label = "Ryc. 4.1", title = "Permutacyjny test różnicy średnich",
      fluidRow(
        column(4,
          sliderInput("ch4_n_per_group", "n na grupę:",
                      min = 10, max = 50, value = 20, step = 5),
          sliderInput("ch4_true_diff", "Prawdziwa różnica średnich (efekt):",
                      min = 0, max = 20, value = 0, step = 1),
          sliderInput("ch4_n_perms", "Liczba permutacji (B):",
                      min = 200, max = 5000, value = 1000, step = 200),
          selectInput("ch4_dist", "Rozkład:",
            choices = c(
              "Prawoskosśny (Gamma)" = "skewed",
              "Normalny"               = "normal",
              "Grube ogony"            = "heavy_tail"
            ),
            selected = "skewed"
          ),
          hr(),
          div(class = "step-buttons",
            actionButton("ch4_perm_step1", "1. Dane",
                         class = "lc-btn-outline"),
            actionButton("ch4_perm_step2", "2. Permutacja",
                         class = "lc-btn-outline")
          ),
          div(class = "step-buttons",
            actionButton("ch4_perm_step3", "3. Rozkład",
                         class = "lc-btn-outline"),
            actionButton("ch4_perm_step4", "4. p-wartość",
                         class = "lc-btn-ok-outline")
          ),
          br(),
          actionButton("ch4_perm_new", "↺ Nowe dane",
                       class = "lc-btn-secondary-outline lc-btn-sm", width = "100%"),
          br(), br(),
          uiOutput("ch4_perm_explanation")
        ),
        column(8,
          plotOutput("ch4_perm_plot", height = "440px"),
          uiOutput("ch4_perm_result")
        )
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
          sliderInput("ch4_cor_n",      "n:", min = 15, max = 80, value = 30, step = 5),
          sliderInput("ch4_cor_true_r", "Prawdziwa korelacja ρ:",
                      min = 0, max = 0.8, value = 0.4, step = 0.1),
          sliderInput("ch4_cor_B",      "B permutacji:",
                      min = 500, max = 5000, value = 1000, step = 500),
          actionButton("ch4_cor_run", "Uruchom",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch4_cor_result")
        ),
        column(8,
          plotOutput("ch4_cor_plot", height = "380px")
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

  ch4_step      <- reactiveVal(0)
  ch4_data      <- reactiveVal(NULL)
  ch4_one_perm  <- reactiveVal(NULL)
  ch4_perm_res  <- reactiveVal(NULL)

  # Reset
  observeEvent(input$ch4_perm_new, {
    ch4_step(0); ch4_data(NULL); ch4_one_perm(NULL); ch4_perm_res(NULL)
  })
  observeEvent(list(input$ch4_n_per_group, input$ch4_true_diff, input$ch4_dist), {
    ch4_step(0); ch4_data(NULL); ch4_one_perm(NULL); ch4_perm_res(NULL)
  }, ignoreInit = TRUE)

  # Krok 1: dane
  observeEvent(input$ch4_perm_step1, {
    df <- generate_two_groups_data(
      n_per_group = input$ch4_n_per_group,
      effect      = input$ch4_true_diff,
      dist        = input$ch4_dist
    )
    ch4_data(df)
    ch4_step(1)
  })

  # Krok 2: jedna permutacja
  observeEvent(input$ch4_perm_step2, {
    req(ch4_data())
    df          <- ch4_data()
    df_perm     <- df
    df_perm$group <- sample(df$group)
    ch4_one_perm(df_perm)
    ch4_step(2)
  })

  # Krok 3: pokazuje, ze uruchomimy duzo permutacji (uruchamia je)
  observeEvent(input$ch4_perm_step3, {
    req(ch4_data())
    withProgress(message = "Wykonuję permutacje...", value = 0, {
      result <- run_permutation_test_twosample(ch4_data(), B = input$ch4_n_perms)
      setProgress(1)
    })
    ch4_perm_res(result)
    ch4_step(3)
  })

  # Krok 4: p-wartosc (dane juz sa, tylko zmiana widoku)
  observeEvent(input$ch4_perm_step4, {
    req(ch4_perm_res())
    ch4_step(4)
  })

  output$ch4_perm_plot <- renderPlot({
    step <- ch4_step()
    df   <- ch4_data()

    if (step == 0 || is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij '1. Dane'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }

    if (step == 1) {
      # Boxplots + dane punktowe
      obs_diff <- mean(df$value[df$group == "B"]) - mean(df$value[df$group == "A"])
      ggplot(df, aes(x = group, y = value, fill = group)) +
        geom_boxplot(alpha = 0.6, outlier.shape = NA) +
        geom_jitter(width = 0.15, size = 2, alpha = 0.7, aes(color = group)) +
        scale_fill_manual(values  = c("A" = sim_bootstrap,  "B" = sim_warning),
                          guide   = "none") +
        scale_color_manual(values = c("A" = sim_bootstrap,  "B" = sim_warning),
                           guide  = "none") +
        annotate("text", x = 1.5, y = max(df$value) * 1.05,
                 label = paste0("Δ obs = ", round(obs_diff, 2)),
                 size = 5, fontface = "bold", color = sim_observed) +
        labs(title = "Krok 1: Dane oryginalne",
             subtitle = paste0("Różnica obserwowana: Δ = ", round(obs_diff, 2)),
             x = "Grupa", y = "Wartość") +
        theme_upwr()
    } else if (step == 2) {
      # Jedna permutacja
      perm_df  <- ch4_one_perm()
      perm_diff <- mean(perm_df$value[perm_df$group == "B"]) -
                   mean(perm_df$value[perm_df$group == "A"])
      ggplot(perm_df, aes(x = group, y = value, fill = group)) +
        geom_boxplot(alpha = 0.6, outlier.shape = NA) +
        geom_jitter(width = 0.15, size = 2, alpha = 0.7, aes(color = group)) +
        scale_fill_manual(values  = c("A" = sim_bootstrap, "B" = sim_warning),
                          guide   = "none") +
        scale_color_manual(values = c("A" = sim_bootstrap, "B" = sim_warning),
                           guide  = "none") +
        annotate("text", x = 1.5, y = max(perm_df$value) * 1.05,
                 label = paste0("Δ perm = ", round(perm_diff, 2)),
                 size = 5, fontface = "bold", color = sim_success) +
        labs(title = "Krok 2: Jedna permutacja etykiet",
             subtitle = "Etykiety grup przetasowane losowo",
             x = "Grupa (przetasowana)", y = "Wartość") +
        theme_upwr()
    } else {
      # Krok 3 i 4: rozklad permutacyjny
      result <- ch4_perm_res()
      df_perm_dist <- data.frame(diff = result$perm_diffs)
      obs_diff     <- result$observed_diff
      extreme      <- abs(df_perm_dist$diff) >= abs(obs_diff)

      p <- ggplot(df_perm_dist, aes(x = diff, fill = extreme)) +
        geom_histogram(bins = 40, color = "white", alpha = 0.85) +
        scale_fill_manual(values = c("FALSE" = sim_null_dist, "TRUE" = sim_observed),
                          guide  = "none") +
        geom_vline(xintercept = obs_diff,
                   color = sim_observed, linewidth = 1.6) +
        geom_vline(xintercept = -abs(obs_diff),
                   color = sim_observed, linewidth = 1.2, linetype = "dashed")

      if (step == 4) {
        p_val <- result$p_value
        p <- p + annotate("text", x = obs_diff, y = Inf,
                           label = paste0("obs Δ = ", round(obs_diff, 2)),
                           vjust = -0.3, hjust = -0.1,
                           color = sim_observed, size = 4.5, fontface = "bold") +
          labs(
            title    = paste0("Krok 4: Rozkład permutacyjny (B = ", length(result$perm_diffs), ")"),
            subtitle = paste0("p-wartość permutacyjna = ", round(p_val, 4)),
            x        = "Permutacyjna różnica średnich (Δ*)",
            y        = "Liczba permutacji"
          )
      } else {
        p <- p + labs(
          title    = paste0("Krok 3: Rozkład permutacyjny (B = ", length(result$perm_diffs), ")"),
          subtitle = "Czerwone słupki = wyniki równie lub bardziej ekstremalne",
          x        = "Permutacyjna różnica średnich (Δ*)",
          y        = "Liczba permutacji"
        )
      }
      p + theme_upwr()
    }
  })

  output$ch4_perm_explanation <- renderUI({
    step <- ch4_step()
    txt <- switch(as.character(step),
      "0" = "Ustaw parametry i kliknij kolejne kroki.",
      "1" = "Dane pobrane. Obserwujemy różnicę średnich między grupami.",
      "2" = "Jedna permutacja: etykiety grup przetasowane losowo pod H₀.",
      "3" = paste0("Rozkład z B = ", input$ch4_n_perms,
                   " permutacji gotowy. To empiryczny rozkład pod H₀."),
      "4" = {
        res <- ch4_perm_res()
        pv  <- format_pval_pl(res$p_value)
        pv$decision
      },
      ""
    )
    lc_feedback(type = "info", txt)
  })

  output$ch4_perm_result <- renderUI({
    req(ch4_step() >= 3, ch4_perm_res())
    result <- ch4_perm_res()
    # Ttest do porownania
    tt <- tryCatch(classical_ttest_twosample(ch4_data()), error = function(e) NULL)

    out <- tagList(
      div(class = "lc-stat-box", style = paste0("background:", sim_observed, ";"),
          paste0("Δ obs = ", round(result$observed_diff, 3))),
      div(class = "lc-stat-box",
          style = paste0("background:", format_pval_pl(result$p_value)$color, ";"),
          paste0("p (perm) = ", round(result$p_value, 4)))
    )
    if (!is.null(tt)) {
      out <- tagList(out,
        div(class = "lc-stat-box", style = paste0("background:", sim_classical, ";"),
            paste0("p (t-test) = ", round(tt$p, 4)))
      )
    }
    out
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

  output$ch4_cor_plot <- renderPlot({
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
      labs(title = paste0("Dane (n = ", nrow(df), ")"),
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
        title    = paste0("Rozkład permutacyjny (B = ", length(res$perm_cors), ")"),
        subtitle = paste0("p = ", round(res$p_value, 4)),
        x        = "Korelacja r*",
        y        = "Liczba permutacji"
      ) +
      theme_upwr()

    gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(1.4, 1))
  })

  output$ch4_cor_result <- renderUI({
    res <- ch4_cor_result_rv()
    if (is.null(res)) return(NULL)
    pv  <- format_pval_pl(res$p_value)
    tagList(
      div(class = "lc-stat-box", style = paste0("background:", sim_observed, ";"),
          paste0("r = ", round(res$observed_r, 3))),
      div(class = "lc-stat-box",
          style = paste0("background:", pv$color, ";"),
          paste0("p (perm) = ", round(res$p_value, 4)))
    )
  })

}
