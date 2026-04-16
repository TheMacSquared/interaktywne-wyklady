# ============================================================================
# CHAPTER 1: Regresja liniowa prosta
# ============================================================================

ch1_ui <- tabPanel("1. Regresja liniowa prosta",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Korelacja m\u00f3wi\u0142a, czy dwie zmienne s\u0105 powi\u0105zane.
       Regresja idzie dalej: modeluje ten zwi\u0105zek i pozwala predykowa\u0107."
    ),

    div(class = "section-title", "Od korelacji do regresji"),

    div(class = "narrative",
      p("Regresja liniowa prosta opisuje zwi\u0105zek mi\u0119dzy jedn\u0105 zmienn\u0105
        obja\u015bniaj\u0105c\u0105 (X) a zmienn\u0105 zale\u017cn\u0105 (Y) za pomoc\u0105 linii prostej:"),
      div(class = "formula-box",
        withMathJax(helpText("$$Y = \\beta_0 + \\beta_1 X + \\varepsilon$$")),
        p(withMathJax("\\(\\beta_0\\)"), " \u2014 wyraz wolny (intercept): warto\u015b\u0107 Y gdy X = 0"),
        p(withMathJax("\\(\\beta_1\\)"), " \u2014 nachylenie (slope): o ile zmieni si\u0119 Y, gdy X wzro\u015bnie o 1"),
        p(withMathJax("\\(\\varepsilon\\)"), " \u2014 b\u0142\u0105d losowy (reszty)")
      )
    ),

    # ========================================================================
    # WIDGET 1: Interaktywna regresja
    # ========================================================================
    div(class = "section-title", "Dopasowanie linii regresji"),

    div(class = "widget-block",
      h4("Regresja liniowa prosta"),
      fluidRow(
        column(4,
          selectInput("ch1_scenario", "Scenariusz:",
            choices = c(
              "Wzrost vs waga" = "height_weight",
              "Nauka vs oceny" = "study_grade",
              "Temperatura vs lody" = "temp_icecream"
            ),
            selected = "height_weight"
          ),
          sliderInput("ch1_n", "Wielko\u015b\u0107 pr\u00f3by (n):",
                      min = 20, max = 200, value = 80, step = 10),
          actionButton("ch1_gen", "Generuj dane i dopasuj",
                       class = "btn-primary", width = "100%"),
          hr(),
          checkboxInput("ch1_show_residuals", "Poka\u017c reszty", value = FALSE),
          checkboxInput("ch1_show_ci", "Poka\u017c pasmo ufno\u015bci", value = TRUE)
        ),
        column(8,
          plotOutput("ch1_scatter", height = "380px"),
          uiOutput("ch1_model_summary")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Reszty - czym sa?
    # ========================================================================
    div(class = "section-title", "Reszty (residuals)"),

    div(class = "narrative",
      p("Reszta to r\u00f3\u017cnica mi\u0119dzy warto\u015bci\u0105 zaobserwowan\u0105 a przewidywan\u0105:"),
      div(class = "formula-box",
        withMathJax(helpText("$$e_i = y_i - \\hat{y}_i$$"))
      ),
      p("Dobry model ma reszty ", tags$b("ma\u0142e"), ", ",
        tags$b("losowe"), " i ", tags$b("bez wzorca"), ".")
    ),

    div(class = "widget-block",
      h4("Analiza reszt"),
      fluidRow(
        column(12,
          helpText("U\u017cywa danych z widgetu powy\u017cej."),
          plotOutput("ch1_resid_plots", height = "300px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: R-kwadrat
    # ========================================================================
    div(class = "section-title", withMathJax("R\u00b2 \u2014 ile model wyja\u015bnia?")),

    div(class = "narrative",
      p("Wsp\u00f3\u0142czynnik determinacji ", withMathJax("\\(R^2\\)"),
        " m\u00f3wi, jaki odsetek zmienno\u015bci Y jest wyja\u015bniony przez model."),
      div(class = "formula-box",
        withMathJax(helpText("$$R^2 = 1 - \\frac{SS_{res}}{SS_{tot}} = 1 - \\frac{\\sum(y_i - \\hat{y}_i)^2}{\\sum(y_i - \\bar{y})^2}$$"))
      ),
      p("Zakres [0, 1]: 0 = model nic nie wyja\u015bnia, 1 = idealne dopasowanie.")
    ),

    div(class = "widget-block",
      h4("Wizualizacja R\u00b2"),
      fluidRow(
        column(4,
          sliderInput("ch1_r2_noise", "Szum (\u03c3):",
                      min = 0.5, max = 20, value = 5, step = 0.5),
          sliderInput("ch1_r2_slope", "Nachylenie (\u03b2\u2081):",
                      min = 0, max = 5, value = 2, step = 0.25),
          actionButton("ch1_r2_gen", "Generuj",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch1_r2_plot", height = "300px"),
          uiOutput("ch1_r2_stats")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Uwaga:"),
      " Wysokie R\u00b2 nie oznacza, \u017ce model jest \"dobry\" \u2014 mo\u017ce by\u0107 przeuczony.
        Niskie R\u00b2 nie oznacza, \u017ce model jest bezwarto\u015bciowy \u2014 w naukach spo\u0142ecznych
        R\u00b2 = 0.3 jest cz\u0119sto bardzo dobre."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: wiele zmiennych obja\u015bniaj\u0105cych"),
      actionButton("ch1_next", "Dalej \u2192 2. Regresja wieloraka",
                   class = "btn-primary btn-lg")
    )
  ))
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
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      df$fitted <- fitted(model)
      df$resid <- residuals(model)

      p <- ggplot(df, aes(x = x, y = y)) +
        geom_point(color = col_data, alpha = 0.5, size = 2)

      if (input$ch1_show_residuals) {
        p <- p + geom_segment(aes(xend = x, yend = fitted),
                              color = col_residual, alpha = 0.3)
      }

      if (input$ch1_show_ci) {
        p <- p + geom_smooth(method = "lm", se = TRUE,
                             color = col_fit, fill = col_ci_band, alpha = 0.15)
      } else {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = col_fit)
      }

      p + labs(title = paste0(df$y_label[1], " ~ ", df$x_label[1]),
               x = df$x_label[1], y = df$y_label[1]) +
        theme_educational()
    }
  })

  output$ch1_model_summary <- renderUI({
    model <- ch1_model()
    if (is.null(model)) return(NULL)

    s <- summary(model)
    coefs <- broom::tidy(model)
    g <- broom::glance(model)

    tagList(
      div(class = "stat-box", style = paste0("background:", col_fit, ";"),
          paste0("R\u00b2 = ", round(g$r.squared, 3))),
      div(class = "stat-box", style = paste0("background:", col_data, ";"),
          paste0("\u03b2\u2080 = ", round(coefs$estimate[1], 2))),
      div(class = "stat-box", style = paste0("background:", col_predict, ";"),
          paste0("\u03b2\u2081 = ", round(coefs$estimate[2], 3))),
      div(class = "stat-box", style = paste0("background:", col_residual, ";"),
          paste0("RMSE = ", round(sqrt(mean(residuals(model)^2)), 2))),
      div(class = "callout-info", style = "margin-top: 10px;",
        p(tags$strong("Interpretacja:"),
          paste0(" Gdy ", ch1_data()$x_label[1], " wzrasta o 1, ",
                 ch1_data()$y_label[1], " zmienia si\u0119 \u015brednio o ",
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
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      df <- data.frame(
        fitted = fitted(model),
        residuals = residuals(model),
        std_resid = rstandard(model)
      )

      p1 <- ggplot(df, aes(x = fitted, y = residuals)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = col_dark) +
        geom_point(color = col_residual, alpha = 0.5) +
        geom_smooth(se = FALSE, color = col_fit, linewidth = 0.8) +
        labs(title = "Reszty vs dopasowane", x = "Warto\u015bci dopasowane",
             y = "Reszty") +
        theme_educational()

      p2 <- ggplot(df, aes(sample = std_resid)) +
        stat_qq(color = col_data, alpha = 0.5) +
        stat_qq_line(color = col_fit) +
        labs(title = "Q-Q reszty", x = "Kwantyle teoretyczne",
             y = "Kwantyle pr\u00f3bkowe") +
        theme_educational()

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
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(df, aes(x = x, y = y)) +
        geom_point(color = col_data, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = col_fit, linewidth = 1.2) +
        labs(title = paste0("R\u00b2 = ",
                            round(summary(lm(y ~ x, data = df))$r.squared, 3)),
             x = "X", y = "Y") +
        theme_educational()
    }
  })

  output$ch1_r2_stats <- renderUI({
    df <- ch1_r2_data()
    if (is.null(df)) return(NULL)
    model <- lm(y ~ x, data = df)
    r2 <- summary(model)$r.squared
    tagList(
      div(class = "stat-box", style = paste0("background:", col_fit, ";"),
          paste0("R\u00b2 = ", round(r2, 3))),
      div(class = "stat-box", style = paste0("background:", col_data, ";"),
          paste0(round(r2 * 100, 1), "% zmienno\u015bci wyja\u015bnione"))
    )
  })
}
