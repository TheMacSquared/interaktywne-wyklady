# ============================================================================
# CHAPTER 1: Regresja liniowa prosta
# ============================================================================

ch1_ui <- list(
  id    = "ch-liniowa",
  num   = "01",
  title = "Regresja liniowa prosta",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 01 · Regresja",
      num    = "01",
      title  = "Regresja liniowa prosta.",
      lead   = "Korelacja mówiła, czy dwie zmienne są powiązane.
                Regresja idzie dalej: modeluje ten związek i pozwala predykować."
    ),

    lc_h2("ch1-od-korelacji", "Od korelacji do regresji"),

    tagList(
      p("Regresja liniowa prosta opisuje związek między jedną zmienną
        objaśniającą (X) a zmienną zależną (Y) za pomocą linii prostej:"),
      lc_formula_box(
        withMathJax(helpText("$$Y = \\beta_0 + \\beta_1 X + \\varepsilon$$")),
        p(withMathJax("\\(\\beta_0\\)"), " — wyraz wolny (intercept): wartość Y gdy X = 0"),
        p(withMathJax("\\(\\beta_1\\)"), " — nachylenie (slope): o ile zmieni się Y, gdy X wzrośnie o 1"),
        p(withMathJax("\\(\\varepsilon\\)"), " — błąd losowy (reszty)")
      )
    ),

    lc_h2("ch1-dopasowanie", "Dopasowanie linii regresji"),

    figure_panel(
      label = "Ryc. 1.1", title = "Regresja liniowa prosta",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch1_scenario", "Scenariusz:",
            choices = c(
              "Wzrost vs waga"      = "height_weight",
              "Nauka vs oceny"      = "study_grade",
              "Temperatura vs lody" = "temp_icecream"
            ),
            selected = "height_weight"
          ),
          sliderInput("ch1_n", "Wielkość próby (n):",
                      min = 20, max = 200, value = 80, step = 10),
          actionButton("ch1_gen", "Generuj dane i dopasuj",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          checkboxInput("ch1_show_residuals", "Pokaż reszty", value = FALSE),
          checkboxInput("ch1_show_ci", "Pokaż pasmo ufności", value = TRUE)
        ),
        column(8,
          plotOutput("ch1_scatter", height = "380px"),
          uiOutput("ch1_model_summary")
        )
      )
    ),

    lc_h2("ch1-reszty", "Reszty (residuals)"),

    tagList(
      p("Reszta to różnica między wartością zaobserwowaną a przewidywaną:"),
      lc_formula_box(
        withMathJax(helpText("$$e_i = y_i - \\hat{y}_i$$"))
      ),
      p("Dobry model ma reszty ", tags$strong("małe"), ", ",
        tags$strong("losowe"), " i ", tags$strong("bez wzorca"), ".")
    ),

    figure_panel(
      label = "Ryc. 1.2", title = "Analiza reszt",
      full_width = TRUE,
      helpText("Używa danych z widgetu powyżej."),
      plotOutput("ch1_resid_plots", height = "300px")
    ),

    lc_h2("ch1-r2", "R² — ile model wyjaśnia?"),

    tagList(
      p("Współczynnik determinacji ", withMathJax("\\(R^2\\)"),
        " mówi, jaki odsetek zmienności Y jest wyjaśniony przez model."),
      lc_formula_box(
        withMathJax(helpText("$$R^2 = 1 - \\frac{SS_{res}}{SS_{tot}} = 1 - \\frac{\\sum(y_i - \\hat{y}_i)^2}{\\sum(y_i - \\bar{y})^2}$$"))
      ),
      p("Zakres [0, 1]: 0 = model nic nie wyjaśnia, 1 = idealne dopasowanie.")
    ),

    figure_panel(
      label = "Ryc. 1.3", title = "Wizualizacja R²",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch1_r2_noise", "Szum (σ):",
                      min = 0.5, max = 20, value = 5, step = 0.5),
          sliderInput("ch1_r2_slope", "Nachylenie (β₁):",
                      min = 0, max = 5, value = 2, step = 0.25),
          actionButton("ch1_r2_gen", "Generuj",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch1_r2_plot", height = "300px"),
          uiOutput("ch1_r2_stats")
        )
      )
    ),

    inline_callout(label = "Uwaga", color = "uwaga",
      "Wysokie R² nie oznacza, że model jest „dobry” — może być przeuczony.
       Niskie R² nie oznacza, że model jest bezwartościowy — w naukach
       społecznych R² = 0.3 jest często bardzo dobre."
    ),

    lc_chapter_next(
      num       = "02",
      title     = "Regresja wieloraka",
      lead      = "wiele zmiennych objaśniających naraz",
      target_id = "ch-wieloraka"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch1_server <- function(input, output, session) {

  # --- Widget 1: Regresja prosta ---
  ch1_data <- reactiveVal(NULL)
  ch1_model <- reactiveVal(NULL)

  observeEvent(input$ch1_gen, {
    df <- generate_regression_data(n = input$ch1_n, scenario = input$ch1_scenario)
    ch1_data(df)
    model <- lm(y ~ x, data = df)
    ch1_model(model)
  })

  output$ch1_scatter <- renderPlot({
    df <- ch1_data()
    model <- ch1_model()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj dane i dopasuj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      df$fitted <- fitted(model)
      df$resid <- residuals(model)

      p <- ggplot(df, aes(x = x, y = y)) +
        geom_point(color = upwr_secondary, alpha = 0.5, size = 2)

      if (input$ch1_show_residuals) {
        p <- p + geom_segment(aes(xend = x, yend = fitted),
                              color = unname(upwr_cat["terakota"]), alpha = 0.3)
      }

      if (input$ch1_show_ci) {
        p <- p + geom_smooth(method = "lm", se = TRUE,
                             color = unname(upwr_cat["niebo"]), fill = unname(upwr_cat["niebo"]), alpha = 0.15)
      } else {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"]))
      }

      p + labs(title = paste0(df$y_label[1], " ~ ", df$x_label[1]),
               x = df$x_label[1], y = df$y_label[1]) +
        theme_upwr()
    }
  })

  output$ch1_model_summary <- renderUI({
    model <- ch1_model()
    if (is.null(model)) return(NULL)

    s <- summary(model)
    coefs <- broom::tidy(model)
    g <- broom::glance(model)

    tagList(
      lc_stat_box("R²", round(g$r.squared, 3), color = unname(upwr_cat["niebo"])),
      lc_stat_box("β₀", round(coefs$estimate[1], 2), color = upwr_secondary),
      lc_stat_box("β₁", round(coefs$estimate[2], 3), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("RMSE", round(sqrt(mean(residuals(model)^2)), 2),
                  color = unname(upwr_cat["terakota"])),
      lc_feedback(type = "info", style = "margin-top: 10px;",
        p(tags$strong("Interpretacja:"),
          paste0(" Gdy ", ch1_data()$x_label[1], " wzrasta o 1, ",
                 ch1_data()$y_label[1], " zmienia się średnio o ",
                 round(coefs$estimate[2], 3), "."))
      )
    )
  })

  # --- Widget 2: Wykresy reszt ---
  output$ch1_resid_plots <- renderPlot({
    model <- ch1_model()
    if (is.null(model)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Najpierw dopasuj model",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      df <- data.frame(
        fitted = fitted(model),
        residuals = residuals(model),
        std_resid = rstandard(model)
      )

      p1 <- ggplot(df, aes(x = fitted, y = residuals)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = upwr_secondary) +
        geom_point(color = unname(upwr_cat["terakota"]), alpha = 0.5) +
        geom_smooth(se = FALSE, color = unname(upwr_cat["niebo"]), linewidth = 0.8) +
        labs(title = "Reszty vs dopasowane", x = "Wartości dopasowane",
             y = "Reszty") +
        theme_upwr()

      p2 <- ggplot(df, aes(sample = std_resid)) +
        stat_qq(color = upwr_secondary, alpha = 0.5) +
        stat_qq_line(color = unname(upwr_cat["niebo"])) +
        labs(title = "Q-Q reszty", x = "Kwantyle teoretyczne",
             y = "Kwantyle próbkowe") +
        theme_upwr()

      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }
  })

  # --- Widget 3: R-kwadrat ---
  ch1_r2_data <- reactiveVal(NULL)

  observeEvent(input$ch1_r2_gen, {
    df <- generate_regression_data(
      n = 100, beta0 = 10, beta1 = input$ch1_r2_slope,
      sigma = input$ch1_r2_noise
    )
    ch1_r2_data(df)
  })

  output$ch1_r2_plot <- renderPlot({
    df <- ch1_r2_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ggplot(df, aes(x = x, y = y)) +
        geom_point(color = upwr_secondary, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
        labs(title = paste0("R² = ",
                            round(summary(lm(y ~ x, data = df))$r.squared, 3)),
             x = "X", y = "Y") +
        theme_upwr()
    }
  })

  output$ch1_r2_stats <- renderUI({
    df <- ch1_r2_data()
    if (is.null(df)) return(NULL)
    model <- lm(y ~ x, data = df)
    r2 <- summary(model)$r.squared
    tagList(
      lc_stat_box("R²", round(r2, 3), color = unname(upwr_cat["niebo"])),
      lc_stat_box("Wyjaśnione", round(r2 * 100, 1), "%",
                  caption = "zmienności",
                  color = upwr_secondary)
    )
  })
}
