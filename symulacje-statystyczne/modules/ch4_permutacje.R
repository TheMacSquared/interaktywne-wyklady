# ============================================================================
# CHAPTER 4: Testy permutacyjne
# ============================================================================

ch4_ui <- tabPanel("4. Testy permutacyjne",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Mamy przedzia\u0142y ufno\u015bci bez za\u0142o\u017ce\u0144.
       Teraz testy hipotez bez za\u0142o\u017ce\u0144 \u2014 przez przetasowanie etykiet."
    ),

    div(class = "section-title", "Idea testu permutacyjnego"),

    div(class = "narrative",
      p("Wyobra\u017amy sobie eksperyment: dwie grupy ro\u015blin, naw\u00f3z A i naw\u00f3z B.
         Pytamy: czy naw\u00f3z wp\u0142ywa na plony?"),
      p("H\u2080 m\u00f3wi: naw\u00f3z nie ma wp\u0142ywu. Je\u015bli tak, to do kt\u00f3rej grupy trafi\u0142a
         dana ro\u015blina jest ", tags$b("bez znaczenia"),
        " \u2014 plony by\u0142yby takie same niezale\u017cnie od przypisania.")
    ),

    div(class = "callout-info",
      tags$strong("Kluczowa idea:"),
      " H\u2080 m\u00f3wi, \u017ce grupy s\u0105 jednorodne. Je\u015bli tak, przypisanie
      \u201eGrupa A\u201f vs \u201eGrupa B\u201f jest arbitralne \u2014 mo\u017cemy je losowo zamieni\u0107.
      Test permutacyjny sprawdza: jak ekstremalna jest nasza obserwowana r\u00f3\u017cnica,
      gdy losujemy takie zamiany?"
    ),

    # ========================================================================
    # WIDGET 1: Test permutacyjny 5 krokow (showpiece)
    # ========================================================================
    div(class = "section-title", "Test permutacyjny \u2014 krok po kroku"),

    div(class = "widget-block",
      h4("Permutacyjny test r\u00f3\u017cnicy \u015brednich"),
      fluidRow(
        column(4,
          sliderInput("ch4_n_per_group", "n na grup\u0119:",
                      min = 10, max = 50, value = 20, step = 5),
          sliderInput("ch4_true_diff", "Prawdziwa r\u00f3\u017cnica \u015brednich (efekt):",
                      min = 0, max = 20, value = 0, step = 1),
          sliderInput("ch4_n_perms", "Liczba permutacji (B):",
                      min = 200, max = 5000, value = 1000, step = 200),
          selectInput("ch4_dist", "Rozk\u0142ad:",
            choices = c(
              "Prawoskos\u015bny (Gamma)" = "skewed",
              "Normalny"               = "normal",
              "Grube ogony"            = "heavy_tail"
            ),
            selected = "skewed"
          ),
          hr(),
          div(class = "step-buttons",
            actionButton("ch4_perm_step1", "1. Dane",
                         class = "btn-outline-primary"),
            actionButton("ch4_perm_step2", "2. Permutacja",
                         class = "btn-outline-primary")
          ),
          div(class = "step-buttons",
            actionButton("ch4_perm_step3", "3. Rozk\u0142ad",
                         class = "btn-outline-primary"),
            actionButton("ch4_perm_step4", "4. p-warto\u015b\u0107",
                         class = "btn-outline-success")
          ),
          br(),
          actionButton("ch4_perm_new", "\u21ba Nowe dane",
                       class = "btn-outline-secondary btn-sm", width = "100%"),
          br(), br(),
          uiOutput("ch4_perm_explanation")
        ),
        column(8,
          plotOutput("ch4_perm_plot", height = "440px"),
          uiOutput("ch4_perm_result")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Aha-moment:"),
      " Rozk\u0142ad permutacyjny to empiryczny rozk\u0142ad pod H\u2080.
       Nie zak\u0142adamy \u017cadnego rozk\u0142adu analitycznego \u2014 \u201ebudujemy\u201f H\u2080 z danych."
    ),

    # ========================================================================
    # WIDGET 2: Permutacyjny test korelacji
    # ========================================================================
    div(class = "section-title", "Permutacyjny test korelacji"),

    div(class = "narrative",
      p("To samo podej\u015bcie dzia\u0142a dla korelacji.
         Je\u015bli H\u2080: brak zwi\u0105zku mi\u0119dzy x i y, to kolejno\u015b\u0107 x wzgl\u0119dem y
         jest dowolna \u2014 mo\u017cemy przetasowywa\u0107 jedn\u0105 zmienn\u0105.")
    ),

    div(class = "widget-block",
      h4("Permutacyjny test korelacji"),
      fluidRow(
        column(4,
          sliderInput("ch4_cor_n",      "n:", min = 15, max = 80, value = 30, step = 5),
          sliderInput("ch4_cor_true_r", "Prawdziwa korelacja \u03c1:",
                      min = 0, max = 0.8, value = 0.4, step = 0.1),
          sliderInput("ch4_cor_B",      "B permutacji:",
                      min = 500, max = 5000, value = 1000, step = 500),
          actionButton("ch4_cor_run", "Uruchom",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch4_cor_result")
        ),
        column(8,
          plotOutput("ch4_cor_plot", height = "380px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Kiedy stosowa\u0107 test permutacyjny dla korelacji:"),
      " gdy mamy obserwacje odstaj\u0105ce, rozk\u0142ady dalekie od normalnych lub
       ma\u0142\u0105 pr\u00f3b\u0119. Klasyczny Pearson wymaga normalno\u015bci dwuwymiarowej \u2014
       test permutacyjny nie."
    ),

    div(class = "chapter-transition",
      p("Dalej: jackknife \u2014 estymacja obci\u0105\u017cenia i SE przez leave-one-out"),
      actionButton("ch4_next",
                   "Dalej \u2192 5. Jackknife",
                   class = "btn-primary btn-lg")
    )

  ))
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
    withProgress(message = "Wykonuj\u0119 permutacje...", value = 0, {
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
                 size = 6, color = "#7f8c8d") +
        theme_void()
      return()
    }

    if (step == 1) {
      # Boxplots + dane punktowe
      obs_diff <- mean(df$value[df$group == "B"]) - mean(df$value[df$group == "A"])
      ggplot(df, aes(x = group, y = value, fill = group)) +
        geom_boxplot(alpha = 0.6, outlier.shape = NA) +
        geom_jitter(width = 0.15, size = 2, alpha = 0.7, aes(color = group)) +
        scale_fill_manual(values  = c("A" = col_primary,  "B" = col_warning),
                          guide   = "none") +
        scale_color_manual(values = c("A" = col_primary,  "B" = col_warning),
                           guide  = "none") +
        annotate("text", x = 1.5, y = max(df$value) * 1.05,
                 label = paste0("\u0394 obs = ", round(obs_diff, 2)),
                 size = 5, fontface = "bold", color = col_secondary) +
        labs(title = "Krok 1: Dane oryginalne",
             subtitle = paste0("R\u00f3\u017cnica obserwowana: \u0394 = ", round(obs_diff, 2)),
             x = "Grupa", y = "Warto\u015b\u0107") +
        theme_sim()
    } else if (step == 2) {
      # Jedna permutacja
      perm_df  <- ch4_one_perm()
      perm_diff <- mean(perm_df$value[perm_df$group == "B"]) -
                   mean(perm_df$value[perm_df$group == "A"])
      ggplot(perm_df, aes(x = group, y = value, fill = group)) +
        geom_boxplot(alpha = 0.6, outlier.shape = NA) +
        geom_jitter(width = 0.15, size = 2, alpha = 0.7, aes(color = group)) +
        scale_fill_manual(values  = c("A" = col_primary, "B" = col_warning),
                          guide   = "none") +
        scale_color_manual(values = c("A" = col_primary, "B" = col_warning),
                           guide  = "none") +
        annotate("text", x = 1.5, y = max(perm_df$value) * 1.05,
                 label = paste0("\u0394 perm = ", round(perm_diff, 2)),
                 size = 5, fontface = "bold", color = col_success) +
        labs(title = "Krok 2: Jedna permutacja etykiet",
             subtitle = "Etykiety grup przetasowane losowo",
             x = "Grupa (przetasowana)", y = "Warto\u015b\u0107") +
        theme_sim()
    } else {
      # Krok 3 i 4: rozklad permutacyjny
      result <- ch4_perm_res()
      df_perm_dist <- data.frame(diff = result$perm_diffs)
      obs_diff     <- result$observed_diff
      extreme      <- abs(df_perm_dist$diff) >= abs(obs_diff)

      p <- ggplot(df_perm_dist, aes(x = diff, fill = extreme)) +
        geom_histogram(bins = 40, color = "white", alpha = 0.85) +
        scale_fill_manual(values = c("FALSE" = col_null_dist, "TRUE" = col_secondary),
                          guide  = "none") +
        geom_vline(xintercept = obs_diff,
                   color = col_secondary, linewidth = 1.6) +
        geom_vline(xintercept = -abs(obs_diff),
                   color = col_secondary, linewidth = 1.2, linetype = "dashed")

      if (step == 4) {
        p_val <- result$p_value
        p <- p + annotate("text", x = obs_diff, y = Inf,
                           label = paste0("obs \u0394 = ", round(obs_diff, 2)),
                           vjust = -0.3, hjust = -0.1,
                           color = col_secondary, size = 4.5, fontface = "bold") +
          labs(
            title    = paste0("Krok 4: Rozk\u0142ad permutacyjny (B = ", length(result$perm_diffs), ")"),
            subtitle = paste0("p-warto\u015b\u0107 permutacyjna = ", round(p_val, 4)),
            x        = "Permutacyjna r\u00f3\u017cnica \u015brednich (\u0394*)",
            y        = "Liczba permutacji"
          )
      } else {
        p <- p + labs(
          title    = paste0("Krok 3: Rozk\u0142ad permutacyjny (B = ", length(result$perm_diffs), ")"),
          subtitle = "Czerwone s\u0142upki = wyniki r\u00f3wnie lub bardziej ekstremalne",
          x        = "Permutacyjna r\u00f3\u017cnica \u015brednich (\u0394*)",
          y        = "Liczba permutacji"
        )
      }
      p + theme_sim()
    }
  })

  output$ch4_perm_explanation <- renderUI({
    step <- ch4_step()
    txt <- switch(as.character(step),
      "0" = "Ustaw parametry i kliknij kolejne kroki.",
      "1" = "Dane pobrane. Obserwujemy r\u00f3\u017cnic\u0119 \u015brednich mi\u0119dzy grupami.",
      "2" = "Jedna permutacja: etykiety grup przetasowane losowo pod H\u2080.",
      "3" = paste0("Rozk\u0142ad z B = ", input$ch4_n_perms,
                   " permutacji gotowy. To empiryczny rozk\u0142ad pod H\u2080."),
      "4" = {
        res <- ch4_perm_res()
        pv  <- format_pval_pl(res$p_value)
        pv$decision
      },
      ""
    )
    div(class = "callout-info", txt)
  })

  output$ch4_perm_result <- renderUI({
    req(ch4_step() >= 3, ch4_perm_res())
    result <- ch4_perm_res()
    # Ttest do porownania
    tt <- tryCatch(classical_ttest_twosample(ch4_data()), error = function(e) NULL)

    out <- tagList(
      div(class = "stat-box", style = paste0("background:", col_secondary, ";"),
          paste0("\u0394 obs = ", round(result$observed_diff, 3))),
      div(class = "stat-box",
          style = paste0("background:", format_pval_pl(result$p_value)$color, ";"),
          paste0("p (perm) = ", round(result$p_value, 4)))
    )
    if (!is.null(tt)) {
      out <- tagList(out,
        div(class = "stat-box", style = paste0("background:", col_classical, ";"),
            paste0("p (t-test) = ", round(tt$p.value, 4)))
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
                 size = 6, color = "#7f8c8d") +
        theme_void()
      return()
    }

    # Dwa panele: scatter + rozklad permutacyjny
    p1 <- ggplot(df, aes(x = x, y = y)) +
      geom_point(color = col_primary, size = 2.5, alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = col_secondary, linewidth = 1) +
      annotate("text", x = min(df$x), y = max(df$y),
               label = paste0("r = ", round(res$observed_r, 3)),
               hjust = 0, vjust = 1, size = 5, fontface = "bold", color = col_secondary) +
      labs(title = paste0("Dane (n = ", nrow(df), ")"),
           x = "x", y = "y") +
      theme_sim()

    df_perm <- data.frame(r = res$perm_cors)
    extreme <- abs(df_perm$r) >= abs(res$observed_r)

    p2 <- ggplot(df_perm, aes(x = r, fill = extreme)) +
      geom_histogram(bins = 40, color = "white", alpha = 0.85) +
      scale_fill_manual(values = c("FALSE" = col_null_dist, "TRUE" = col_secondary),
                        guide = "none") +
      geom_vline(xintercept  = res$observed_r, color = col_secondary, linewidth = 1.5) +
      geom_vline(xintercept = -abs(res$observed_r), color = col_secondary,
                 linewidth = 1.2, linetype = "dashed") +
      labs(
        title    = paste0("Rozk\u0142ad permutacyjny (B = ", length(res$perm_cors), ")"),
        subtitle = paste0("p = ", round(res$p_value, 4)),
        x        = "Korelacja r*",
        y        = "Liczba permutacji"
      ) +
      theme_sim()

    gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(1.4, 1))
  })

  output$ch4_cor_result <- renderUI({
    res <- ch4_cor_result_rv()
    if (is.null(res)) return(NULL)
    pv  <- format_pval_pl(res$p_value)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_secondary, ";"),
          paste0("r = ", round(res$observed_r, 3))),
      div(class = "stat-box",
          style = paste0("background:", pv$color, ";"),
          paste0("p (perm) = ", round(res$p_value, 4)))
    )
  })

}
