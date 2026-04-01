# ============================================================================
# CHAPTER 3: Zalozenia regresji liniowej
# ============================================================================

ch3_ui <- tabPanel("3. Za\u0142o\u017cenia regresji",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Regresja liniowa ma w\u0142asny zestaw za\u0142o\u017ce\u0144. Naruszenie ka\u017cdego
       prowadzi do innego typu problem\u00f3w."
    ),

    div(class = "section-title", "Pi\u0119\u0107 za\u0142o\u017ce\u0144 regresji liniowej"),

    div(class = "narrative",
      tags$ol(
        tags$li(tags$b("Liniowo\u015b\u0107"), " \u2014 zwi\u0105zek Y~X jest liniowy"),
        tags$li(tags$b("Niezale\u017cno\u015b\u0107 reszt"), " \u2014 reszty nie s\u0105 skorelowane"),
        tags$li(tags$b("Homoscedastyczno\u015b\u0107"), " \u2014 wariancja reszt sta\u0142a"),
        tags$li(tags$b("Normalno\u015b\u0107 reszt"), " \u2014 reszty ~N(0, \u03c3)"),
        tags$li(tags$b("Brak wsp\u00f3\u0142liniowo\u015bci"), " \u2014 predyktory nie s\u0105 silnie skorelowane")
      ),
      p("Kluczowe: za\u0142o\u017cenia dotycz\u0105 ", tags$b("reszt"), ", nie surowych danych!")
    ),

    # ========================================================================
    # WIDGET 1: Diagnostyka wizualna
    # ========================================================================
    div(class = "section-title", "Diagnostyka wizualna"),

    div(class = "widget-block",
      h4("Wykresy diagnostyczne"),
      fluidRow(
        column(4,
          selectInput("ch3_violation", "Typ naruszenia:",
            choices = c(
              "Brak (dane OK)" = "none",
              "Heteroscedastyczno\u015b\u0107" = "heteroscedasticity",
              "Nieliniowo\u015b\u0107" = "nonlinear",
              "Nienormalne reszty" = "non_normal_resid",
              "Autokorelacja" = "autocorrelation"
            ),
            selected = "none"
          ),
          sliderInput("ch3_n", "n:", min = 50, max = 200, value = 100, step = 25),
          actionButton("ch3_gen", "Generuj dane",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_scatter", height = "250px"),
          plotOutput("ch3_diag_plots", height = "300px")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Jak czyta\u0107 wykresy diagnostyczne:"),
      tags$ul(
        tags$li(tags$b("Reszty vs dopasowane:"), " losowy rozrzut = OK, wz\u00f3r = problem"),
        tags$li(tags$b("Q-Q reszt:"), " punkty na linii = normalno\u015b\u0107 OK"),
        tags$li(tags$b("Scale-Location:"), " p\u0142aska linia = homoscedastyczno\u015b\u0107 OK")
      )
    ),

    # ========================================================================
    # WIDGET 2: Testy formalne
    # ========================================================================
    div(class = "section-title", "Testy formalne za\u0142o\u017ce\u0144 regresji"),

    div(class = "widget-block",
      h4("Testy diagnostyczne"),
      fluidRow(
        column(4,
          helpText("U\u017cywa modelu z widgetu powy\u017cej."),
          actionButton("ch3_run_tests", "Uruchom diagnostyk\u0119",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch3_diag_results")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Alternatywy
    # ========================================================================
    div(class = "section-title", "Gdy za\u0142o\u017cenia s\u0105 naruszone"),

    div(class = "callout-success",
      tags$table(class = "table table-bordered", style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Naruszenie"), tags$th("Rozwi\u0105zanie"))
        ),
        tags$tbody(
          tags$tr(tags$td("Nieliniowo\u015b\u0107"),
                  tags$td("Dodaj sk\u0142adnik kwadratowy, transformuj X, u\u017cyj GAM")),
          tags$tr(tags$td("Heteroscedastyczno\u015b\u0107"),
                  tags$td("Odporne b\u0142\u0119dy std. (HC), WLS, transformacja log(Y)")),
          tags$tr(tags$td("Nienormalne reszty"),
                  tags$td("Transformacja Y, bootstrap CI, GLM")),
          tags$tr(tags$td("Autokorelacja"),
                  tags$td("Modele szereg\u00f3w czasowych (ARIMA), GLS")),
          tags$tr(tags$td("Wsp\u00f3\u0142liniowo\u015b\u0107"),
                  tags$td("Usu\u0144 skorelowane predyktory, PCA, regularyzacja (ridge/lasso)"))
        )
      )
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: za\u0142o\u017cenia test\u00f3w chi-kwadrat i Fishera"),
      actionButton("ch3_next", "Dalej \u2192 4. Za\u0142o\u017cenia \u03c7\u00b2 i Fishera",
                   class = "btn-primary btn-lg")
    )
  ))
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
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(df, aes(x = x, y = y)) +
        geom_point(color = col_test, alpha = 0.5) +
        geom_smooth(method = "lm", se = TRUE, color = col_ok, fill = col_ok, alpha = 0.1) +
        labs(title = "Dane + linia regresji", x = "X", y = "Y") +
        theme_assumptions()
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
      geom_hline(yintercept = 0, linetype = "dashed", color = col_dark) +
      geom_point(color = col_test, alpha = 0.5) +
      geom_smooth(se = FALSE, color = col_fail, linewidth = 0.8) +
      labs(title = "Reszty vs dopasowane", x = "Dopasowane", y = "Reszty") +
      theme_assumptions()

    p2 <- ggplot(df, aes(sample = std_resid)) +
      stat_qq(color = col_test, alpha = 0.5) +
      stat_qq_line(color = col_ok) +
      labs(title = "Q-Q reszt") +
      theme_assumptions()

    p3 <- ggplot(df, aes(x = fitted, y = sqrt_abs_resid)) +
      geom_point(color = col_test, alpha = 0.5) +
      geom_smooth(se = FALSE, color = col_fail, linewidth = 0.8) +
      labs(title = "Scale-Location", x = "Dopasowane",
           y = expression(sqrt("|Std. reszty|"))) +
      theme_assumptions()

    gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
  })

  # --- Testy formalne ---
  output$ch3_diag_results <- renderUI({
    req(input$ch3_run_tests)
    model <- isolate(ch3_model())
    if (is.null(model)) return(div(class = "callout-warning", "Najpierw wygeneruj dane."))

    # Shapiro-Wilk na resztach
    resid <- residuals(model)
    sw <- shapiro.test(resid)

    # Breusch-Pagan (heteroscedastycznosc)
    bp <- lmtest::bptest(model)

    # Durbin-Watson (autokorelacja)
    dw <- lmtest::dwtest(model)

    results <- list(
      list(name = "Normalno\u015b\u0107 reszt (Shapiro-Wilk)",
           stat = paste0("W = ", round(sw$statistic, 4)),
           p = sw$p.value,
           ok_msg = "Reszty normalne", fail_msg = "Reszty nienormalne!"),
      list(name = "Homoscedastyczno\u015b\u0107 (Breusch-Pagan)",
           stat = paste0("BP = ", round(bp$statistic, 3)),
           p = bp$p.value,
           ok_msg = "Wariancja sta\u0142a", fail_msg = "Heteroscedastyczno\u015b\u0107!"),
      list(name = "Niezale\u017cno\u015b\u0107 reszt (Durbin-Watson)",
           stat = paste0("DW = ", round(dw$statistic, 3)),
           p = dw$p.value,
           ok_msg = "Brak autokorelacji", fail_msg = "Autokorelacja!")
    )

    div(class = "callout-info",
      lapply(results, function(r) {
        color <- if (r$p >= 0.05) col_ok else col_fail
        msg <- if (r$p >= 0.05) r$ok_msg else r$fail_msg
        div(style = "margin-bottom: 10px;",
          p(tags$strong(r$name)),
          p(paste0(r$stat, ", p = ", format.pval(r$p, digits = 4))),
          p(style = paste0("color:", color, "; font-weight: bold;"), msg)
        )
      })
    )
  })
}
