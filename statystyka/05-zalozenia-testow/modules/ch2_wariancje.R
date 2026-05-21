# ============================================================================
# CHAPTER 2: Jednorodnosc wariancji
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-wariancje",
  num = "02",
  title = "Jednorodne wariancje",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · Założenia testów",
      num    = "02",
      title  = "Jednorodne wariancje.",
      lead   = "Normalność to nie jedyne założenie. Wiele testów porównawczych wymaga równych wariancji między grupami."
    ),

    lc_h2("ch2-homoscedastycznosc", "Homoscedastyczność — równe wariancje"),

    tagList(
      p("Założenie jednorodnych wariancji (homoscedastyczność) dotyczy:"),
      tags$ul(
        tags$li(tags$b("Test t niezależny"), " — wariancje w obu grupach powinny być podobne"),
        tags$li(tags$b("ANOVA"), " — wariancje we wszystkich grupach porównywalne"),
        tags$li(tags$b("Regresja liniowa"), " — wariancja reszt stała (homoscedastyczność reszt)")
      )
    ),

    # ========================================================================
    # WIDGET 1: Wizualizacja
    # ========================================================================
    lc_h2("ch2-naruszenie", "Jak wygląda naruszenie?"),

    figure_panel(
      label = "Ryc. 2.1",
      title = "Dwie grupy o różnej wariancji",
      fluidRow(
        column(4,
          sliderInput("ch2_sd1", "SD grupy A:", min = 2, max = 30, value = 10, step = 1),
          sliderInput("ch2_sd2", "SD grupy B:", min = 2, max = 30, value = 10, step = 1),
          sliderInput("ch2_n_per", "n (na grupę):", min = 15, max = 100, value = 40, step = 5),
          actionButton("ch2_gen", "Generuj dane",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch2_boxplot", height = "300px"),
          uiOutput("ch2_var_stats")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Testy
    # ========================================================================
    lc_h2("ch2-testy", "Testy jednorodnych wariancji"),

    tagList(
      p("Dwa popularne testy:"),
      tags$ul(
        tags$li(tags$b("Test Levene'a"), " — odporny na naruszenie normalności, zalecany"),
        tags$li(tags$b("Test Bartletta"), " — mocniejszy, ale wrażliwy na brak normalności")
      ),
      p(withMathJax("\\(H_0\\)"), ": wariancje są równe we wszystkich grupach.")
    ),

    figure_panel(
      label = "Ryc. 2.2",
      title = "Levene i Bartlett",
      fluidRow(
        column(4,
          helpText("Używa danych z widgetu powyżej."),
          actionButton("ch2_test_var", "Testuj",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch2_test_results")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Co robic?
    # ========================================================================
    lc_h2("ch2-nierowne", "Gdy wariancje są nierówne"),

    tagList(
      p("Opcje:"),
      tags$ol(
        tags$li(tags$b("Test t Welcha"), " — domyślny w R! Nie zakłada równych wariancji.
                 W praktyce zawsze można używać Welcha zamiast klasycznego t."),
        tags$li(tags$b("Mann-Whitney U"), " — nieparametryczny, nie zakłada równych wariancji"),
        tags$li(tags$b("ANOVA Welcha"), " — odpowiednik dla 3+ grup (", tags$code("oneway.test()"), ")"),
        tags$li(tags$b("Regresja: odporne błędy std."), " — ",
                tags$code("sandwich::vcovHC()"), " + ", tags$code("lmtest::coeftest()"))
      )
    ),

    figure_panel(
      label = "Ryc. 2.3",
      title = "Test t klasyczny vs Welcha",
      fluidRow(
        column(4,
          helpText("Porównanie wyniku: klasyczny test t (zakłada równe wariancje)
                    vs test Welcha (nie zakłada)."),
          actionButton("ch2_compare_t", "Porównaj testy",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          uiOutput("ch2_t_comparison")
        )
      )
    ),

    lc_feedback(type = "ok",
      tags$strong("Praktyczna rada:"),
      " Zawsze używaj testu Welcha (", tags$code("t.test(var.equal = FALSE)"),
      ") — to domyślne zachowanie w R. Klasyczny test t z równymi wariancjami
        ma sens tylko gdy masz pewność, że wariancje są równe."
    ),

    lc_chapter_next(
      num = "03",
      title = "Założenia χ² i Fishera",
      lead = "minimalne liczności i wybór testu dla tabel.",
      target_id = "ch-chi-fisher"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  ch2_data <- reactiveVal(NULL)

  observeEvent(input$ch2_gen, {
    ch2_data(generate_two_groups(
      n1 = input$ch2_n_per, n2 = input$ch2_n_per,
      sd1 = input$ch2_sd1, sd2 = input$ch2_sd2
    ))
  })

  zoom_plot_server("ch2_boxplot", reactive({
    df <- ch2_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj dane'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ggplot(df, aes(x = group, y = value, fill = group)) +
        geom_boxplot(alpha = 0.6) +
        geom_jitter(width = 0.15, alpha = 0.3) +
        scale_fill_manual(values = c(col_test, col_alt)) +
        labs(x = "Grupa", y = "Wartość") +
        theme_upwr() +
        theme(legend.position = "none")
    }
  }))

  output$ch2_var_stats <- renderUI({
    df <- ch2_data()
    if (is.null(df)) return(NULL)
    stats <- df %>% group_by(group) %>%
      summarise(sd = sd(value), var = var(value), .groups = "drop")
    ratio <- max(stats$var) / min(stats$var)

    tagList(
      lc_stat_box("SD(A)", round(stats$sd[1], 2), color = col_test),
      lc_stat_box("SD(B)", round(stats$sd[2], 2), color = col_alt),
      lc_stat_box("Ratio var", round(ratio, 2),
                  color = if (ratio < 4) col_ok else col_fail)
    )
  })

  # --- Testy ---
  output$ch2_test_results <- renderUI({
    req(input$ch2_test_var)
    df <- isolate(ch2_data())
    if (is.null(df)) return(lc_feedback(type = "warning", "Najpierw wygeneruj dane."))

    lev <- rstatix::levene_test(df, value ~ group)
    bart <- bartlett.test(value ~ group, data = df)

    lev_color <- if (lev$p >= 0.05) col_ok else col_fail
    bart_color <- if (bart$p.value >= 0.05) col_ok else col_fail

    lc_feedback(type = "info",
      fluidRow(
        column(6,
          p(tags$strong("Test Levene'a:")),
          p(paste0("F = ", round(lev$statistic, 3))),
          p(paste0("p = ", format_p_value(lev$p))),
          p(style = paste0("color:", lev_color, "; font-weight: bold;"),
            if (lev$p >= 0.05) "Wariancje jednorodne" else "Wariancje nierówne!")
        ),
        column(6,
          p(tags$strong("Test Bartletta:")),
          p(paste0("χ² = ", round(bart$statistic, 3))),
          p(paste0("p = ", format_p_value(bart$p.value))),
          p(style = paste0("color:", bart_color, "; font-weight: bold;"),
            if (bart$p.value >= 0.05) "Wariancje jednorodne" else "Wariancje nierówne!")
        )
      )
    )
  })

  # --- Porownanie t ---
  output$ch2_t_comparison <- renderUI({
    req(input$ch2_compare_t)
    df <- isolate(ch2_data())
    if (is.null(df)) return(lc_feedback(type = "warning", "Najpierw wygeneruj dane."))

    t_classic <- t_test(df, value ~ group, var.equal = TRUE)
    t_welch <- t_test(df, value ~ group, var.equal = FALSE)

    lc_feedback(type = "info",
      fluidRow(
        column(6,
          p(tags$strong("Test t klasyczny"), " (var.equal=TRUE):"),
          p(paste0("t(", round(t_classic$df, 1), ") = ",
                   round(t_classic$statistic, 3))),
          p(paste0("p = ", format_p_value(t_classic$p)))
        ),
        column(6,
          p(tags$strong("Test Welcha"), " (var.equal=FALSE):"),
          p(paste0("t(", round(t_welch$df, 1), ") = ",
                   round(t_welch$statistic, 3))),
          p(paste0("p = ", format_p_value(t_welch$p)))
        )
      ),
      p(style = "margin-top: 10px;",
        tags$em("Przy nierównych wariancjach wyniki mogą się różnić.
                 Welch jest bezpieczniejszy."))
    )
  })
}
