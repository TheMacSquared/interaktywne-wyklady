# ============================================================================
# CHAPTER 3: Zalozenia regresji liniowej
# ============================================================================

ch3_ui <- lecture_chapter(
  id = "ch-regresja",
  num = "03",
  title = "Założenia regresji",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 03 · Założenia testów",
      num    = "03",
      title  = "Założenia regresji.",
      lead   = "Regresja liniowa ma własny zestaw założeń. Naruszenie każdego prowadzi do innego typu problemów."
    ),

    lc_h2("ch3-piec-zalozen", "Pięć założeń regresji liniowej"),

    tagList(
      tags$ol(
        tags$li(tags$b("Liniowość"), " — związek Y~X jest liniowy"),
        tags$li(tags$b("Niezależność reszt"), " — reszty nie są skorelowane"),
        tags$li(tags$b("Homoscedastyczność"), " — wariancja reszt stała"),
        tags$li(tags$b("Normalność reszt"), " — reszty ~N(0, σ)"),
        tags$li(tags$b("Brak współliniowości"), " — predyktory nie są silnie skorelowane")
      ),
      p("Kluczowe: założenia dotyczą ", tags$b("reszt"), ", nie surowych danych!")
    ),

    # ========================================================================
    # WIDGET 1: Diagnostyka wizualna
    # ========================================================================
    lc_h2("ch3-diagnostyka", "Diagnostyka wizualna"),

    figure_panel(
      label = "Ryc. 3.1",
      title = "Wykresy diagnostyczne",
      fluidRow(
        column(4,
          selectInput("ch3_violation", "Typ naruszenia:",
            choices = c(
              "Brak (dane OK)" = "none",
              "Heteroscedastyczność" = "heteroscedasticity",
              "Nieliniowość" = "nonlinear",
              "Nienormalne reszty" = "non_normal_resid",
              "Autokorelacja" = "autocorrelation"
            ),
            selected = "none"
          ),
          sliderInput("ch3_n", "n:", min = 50, max = 200, value = 100, step = 25),
          actionButton("ch3_gen", "Generuj dane",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_scatter", height = "250px"),
          plotOutput("ch3_diag_plots", height = "300px")
        )
      )
    ),

    lc_feedback(type = "info",
      tags$strong("Jak czytać wykresy diagnostyczne:"),
      tags$ul(
        tags$li(tags$b("Reszty vs dopasowane:"), " losowy rozrzut = OK, wzór = problem"),
        tags$li(tags$b("Q-Q reszt:"), " punkty na linii = normalność OK"),
        tags$li(tags$b("Scale-Location:"), " płaska linia = homoscedastyczność OK")
      )
    ),

    # ========================================================================
    # WIDGET 2: Testy formalne
    # ========================================================================
    lc_h2("ch3-testy", "Testy formalne założeń regresji"),

    figure_panel(
      label = "Ryc. 3.2",
      title = "Testy diagnostyczne",
      fluidRow(
        column(4,
          helpText("Używa modelu z widgetu powyżej."),
          actionButton("ch3_run_tests", "Uruchom diagnostykę",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch3_diag_results")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Alternatywy
    # ========================================================================
    lc_h2("ch3-naruszenia", "Gdy założenia są naruszone"),

    lc_feedback(type = "ok",
      tags$table(class = "lc-table lc-table-bordered", style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Naruszenie"), tags$th("Rozwiązanie"))
        ),
        tags$tbody(
          tags$tr(tags$td("Nieliniowość"),
                  tags$td("Dodaj składnik kwadratowy, transformuj X, użyj GAM")),
          tags$tr(tags$td("Heteroscedastyczność"),
                  tags$td("Odporne błędy std. (HC), WLS, transformacja log(Y)")),
          tags$tr(tags$td("Nienormalne reszty"),
                  tags$td("Transformacja Y, bootstrap CI, GLM")),
          tags$tr(tags$td("Autokorelacja"),
                  tags$td("Modele szeregów czasowych (ARIMA), GLS")),
          tags$tr(tags$td("Współliniowość"),
                  tags$td("Usuń skorelowane predyktory, PCA, regularyzacja (ridge/lasso)"))
        )
      )
    ),

    lc_chapter_next(
      num = "04",
      title = "Założenia χ² i Fishera",
      lead = "minimalne liczności i wybór testu dla tabel.",
      target_id = "ch-chi-fisher"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  ch3_data <- reactiveVal(NULL)
  ch3_model <- reactiveVal(NULL)

  observeEvent(input$ch3_gen, {
    df <- generate_reg_violations(input$ch3_n, input$ch3_violation)
    ch3_data(df)
    ch3_model(lm(y ~ x, data = df))
  })

  output$ch3_scatter <- renderPlot({
    df <- ch3_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj dane'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ggplot(df, aes(x = x, y = y)) +
        geom_point(color = col_test, alpha = 0.5) +
        geom_smooth(method = "lm", se = TRUE, color = col_ok, fill = col_ok, alpha = 0.1) +
        labs(x = "X", y = "Y") +
        theme_upwr()
    }
  })

  output$ch3_diag_plots <- renderPlot({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    df <- data.frame(
      fitted = fitted(model),
      residuals = residuals(model),
      std_resid = rstandard(model),
      sqrt_abs_resid = sqrt(abs(rstandard(model)))
    )

    p1 <- ggplot(df, aes(x = fitted, y = residuals)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = upwr_secondary) +
      geom_point(color = col_test, alpha = 0.5) +
      geom_smooth(se = FALSE, color = col_fail, linewidth = 0.8) +
      labs(x = "Dopasowane", y = "Reszty") +
      theme_upwr()

    p2 <- ggplot(df, aes(sample = std_resid)) +
      stat_qq(color = col_test, alpha = 0.5) +
      stat_qq_line(color = col_ok) +
      theme_upwr()

    p3 <- ggplot(df, aes(x = fitted, y = sqrt_abs_resid)) +
      geom_point(color = col_test, alpha = 0.5) +
      geom_smooth(se = FALSE, color = col_fail, linewidth = 0.8) +
      labs(x = "Dopasowane",
           y = expression(sqrt("|Std. reszty|"))) +
      theme_upwr()

    gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
  })

  # --- Testy formalne ---
  output$ch3_diag_results <- renderUI({
    req(input$ch3_run_tests)
    model <- isolate(ch3_model())
    if (is.null(model)) return(lc_feedback(type = "warning", "Najpierw wygeneruj dane."))

    # Shapiro-Wilk na resztach
    resid <- residuals(model)
    sw <- shapiro_test(data.frame(value = resid), value)

    # Breusch-Pagan (heteroscedastycznosc)
    bp <- lmtest::bptest(model)

    # Durbin-Watson (autokorelacja)
    dw <- lmtest::dwtest(model)

    results <- list(
      list(name = "Normalność reszt (Shapiro-Wilk)",
           stat = paste0("W = ", round(sw$statistic, 4)),
           p = sw$p,
           ok_msg = "Reszty normalne", fail_msg = "Reszty nienormalne!"),
      list(name = "Homoscedastyczność (Breusch-Pagan)",
           stat = paste0("BP = ", round(bp$statistic, 3)),
           p = bp$p.value,
           ok_msg = "Wariancja stała", fail_msg = "Heteroscedastyczność!"),
      list(name = "Niezależność reszt (Durbin-Watson)",
           stat = paste0("DW = ", round(dw$statistic, 3)),
           p = dw$p.value,
           ok_msg = "Brak autokorelacji", fail_msg = "Autokorelacja!")
    )

    lc_feedback(type = "info",
      lapply(results, function(r) {
        color <- if (r$p >= 0.05) col_ok else col_fail
        msg <- if (r$p >= 0.05) r$ok_msg else r$fail_msg
        div(style = "margin-bottom: 10px;",
          p(tags$strong(r$name)),
          p(paste0(r$stat, ", ", format_p(r$p))),
          p(style = paste0("color:", color, "; font-weight: bold;"), msg)
        )
      })
    )
  })
}
