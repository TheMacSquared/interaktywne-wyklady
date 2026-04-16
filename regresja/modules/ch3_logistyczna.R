# ============================================================================
# CHAPTER 3: Regresja logistyczna
# ============================================================================

ch3_ui <- tabPanel("3. Regresja logistyczna",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Regresja liniowa wymaga ci\u0105g\u0142ej zmiennej zale\u017cnej.
       A co, gdy Y to 0 lub 1 (sukces/pora\u017cka)?"
    ),

    div(class = "section-title", "Dlaczego nie regresja liniowa?"),

    div(class = "narrative",
      p("Gdy zmienna zale\u017cna jest ", tags$b("binarna"),
        " (np. zdany/niezdany egzamin), regresja liniowa daje przewidywania
        spoza zakresu [0, 1]. Rozwi\u0105zanie: ", tags$b("regresja logistyczna"), "."),
      p("Zamiast modelowa\u0107 Y bezpo\u015brednio, modelujemy ",
        tags$b("prawdopodobie\u0144stwo"), " sukcesu:"),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$P(Y=1) = \\frac{1}{1 + e^{-(\\beta_0 + \\beta_1 X_1 + \\ldots + \\beta_k X_k)}}$$"
        )),
        p("Funkcja logistyczna (sigmoida) zamyka wynik w [0, 1].")
      )
    ),

    # ========================================================================
    # WIDGET 1: Wizualizacja krzywej logistycznej
    # ========================================================================
    div(class = "section-title", "Krzywa logistyczna"),

    div(class = "widget-block",
      h4("Sigmoida w akcji"),
      fluidRow(
        column(4,
          sliderInput("ch3_b0", "\u03b2\u2080 (intercept):",
                      min = -10, max = 10, value = -4, step = 0.5),
          sliderInput("ch3_b1", "\u03b2\u2081 (slope):",
                      min = -3, max = 3, value = 0.2, step = 0.05),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch3_preset_steep", "Stromy",
                         class = "btn-outline-primary"),
            actionButton("ch3_preset_flat", "P\u0142aski",
                         class = "btn-outline-secondary"),
            actionButton("ch3_preset_neg", "Odwrotny",
                         class = "btn-outline-danger")
          )
        ),
        column(8,
          plotOutput("ch3_sigmoid_plot", height = "350px")
        )
      )
    ),

    # ========================================================================
    # WIDGET 2: Regresja logistyczna na danych
    # ========================================================================
    div(class = "section-title", "Model logistyczny na danych"),

    div(class = "narrative",
      p("Scenariusz: czy student zda egzamin? Predyktory: godziny nauki i \u015brednia ocen.")
    ),

    div(class = "widget-block",
      h4("Predykcja zdania egzaminu"),
      fluidRow(
        column(4,
          sliderInput("ch3_n", "n:", min = 50, max = 300, value = 150, step = 25),
          selectInput("ch3_predictor", "Prezentowany predyktor:",
            choices = c(
              "Godziny nauki" = "godziny_nauki",
              "\u015arednia ocen" = "srednia_ocen"
            ),
            selected = "godziny_nauki"
          ),
          actionButton("ch3_fit", "Dopasuj model",
                       class = "btn-primary", width = "100%"),
          hr(),
          h5("Predykcja dla nowego studenta:"),
          numericInput("ch3_pred_hours", "Godziny nauki:", value = 20, min = 0, max = 40),
          numericInput("ch3_pred_gpa", "\u015arednia ocen:", value = 3.5, min = 2, max = 5, step = 0.1),
          uiOutput("ch3_prediction")
        ),
        column(8,
          plotOutput("ch3_logit_plot", height = "350px"),
          uiOutput("ch3_model_summary")
        )
      )
    ),

    # ========================================================================
    # WIDGET 3: Interpretacja wspolczynnikow (odds ratio)
    # ========================================================================
    div(class = "section-title", "Interpretacja: iloraz szans"),

    div(class = "narrative",
      p("W regresji logistycznej wsp\u00f3\u0142czynniki interpretujemy przez ",
        tags$b("iloraz szans (odds ratio)"), ":"),
      div(class = "formula-box",
        withMathJax(helpText("$$OR = e^{\\beta_j}$$")),
        p("OR = 1.5 oznacza: wzrost X o 1 zwi\u0119ksza szanse sukcesu 1.5-krotnie.")
      )
    ),

    div(class = "widget-block",
      h4("Odds ratio"),
      fluidRow(
        column(12,
          helpText("U\u017cywa modelu dopasowanego powy\u017cej."),
          uiOutput("ch3_odds_ratios")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Ocena modelu logistycznego:"),
      " Nie u\u017cywamy R\u00b2 w sensie liniowym. Zamiast tego: AIC, BIC, oraz
        macierz pomy\u0142ek (confusion matrix) z dok\u0142adno\u015bci\u0105, czu\u0142o\u015bci\u0105 i swoisto\u015bci\u0105."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: jak por\u00f3wna\u0107 modele i wybra\u0107 najlepszy?"),
      actionButton("ch3_next", "Dalej \u2192 4. Por\u00f3wnanie modeli",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch3_server <- function(input, output, session) {

  # --- Widget 1: Sigmoida ---
  observeEvent(input$ch3_preset_steep, {
    updateSliderInput(session, "ch3_b0", value = -5)
    updateSliderInput(session, "ch3_b1", value = 0.5)
  })
  observeEvent(input$ch3_preset_flat, {
    updateSliderInput(session, "ch3_b0", value = -1)
    updateSliderInput(session, "ch3_b1", value = 0.05)
  })
  observeEvent(input$ch3_preset_neg, {
    updateSliderInput(session, "ch3_b0", value = 5)
    updateSliderInput(session, "ch3_b1", value = -0.3)
  })

  output$ch3_sigmoid_plot <- renderPlot({
    b0 <- input$ch3_b0
    b1 <- input$ch3_b1
    x <- seq(-5, 45, length.out = 500)
    p <- 1 / (1 + exp(-(b0 + b1 * x)))

    ggplot(data.frame(x = x, p = p), aes(x = x, y = p)) +
      geom_line(color = col_logit, linewidth = 1.5) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = col_dark, alpha = 0.5) +
      labs(title = paste0("Sigmoida: \u03b2\u2080 = ", b0, ", \u03b2\u2081 = ", b1),
           x = "X", y = "P(Y = 1)") +
      ylim(0, 1) +
      theme_educational()
  })

  # --- Widget 2: Model logistyczny ---
  ch3_data <- reactiveVal(NULL)
  ch3_model <- reactiveVal(NULL)

  observeEvent(input$ch3_fit, {
    df <- generate_logistic_data(input$ch3_n)
    ch3_data(df)
    model <- glm(zdal_num ~ godziny_nauki + srednia_ocen,
                 data = df, family = binomial)
    ch3_model(model)
  })

  output$ch3_logit_plot <- renderPlot({
    df <- ch3_data()
    model <- ch3_model()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Dopasuj model'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      pred_var <- input$ch3_predictor
      pred_label <- if (pred_var == "godziny_nauki") "Godziny nauki" else "\u015arednia ocen"

      # Predykcja dla wykresu (trzymajac drugi predyktor na sredniej)
      other_var <- setdiff(c("godziny_nauki", "srednia_ocen"), pred_var)
      other_mean <- mean(df[[other_var]])

      x_seq <- seq(min(df[[pred_var]]), max(df[[pred_var]]), length.out = 200)
      newdata <- data.frame(x_seq, other_mean)
      names(newdata) <- c(pred_var, other_var)
      newdata$pred_prob <- predict(model, newdata, type = "response")

      ggplot() +
        geom_jitter(data = df, aes_string(x = pred_var, y = "zdal_num"),
                    height = 0.03, alpha = 0.3, color = col_data) +
        geom_line(data = newdata, aes_string(x = pred_var, y = "pred_prob"),
                  color = col_logit, linewidth = 1.5) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = col_warning) +
        labs(title = paste0("Regresja logistyczna: P(zdanie) ~ ", pred_label),
             x = pred_label, y = "P(zdanie egzaminu)") +
        ylim(-0.05, 1.05) +
        theme_educational()
    }
  })

  output$ch3_model_summary <- renderUI({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    g <- broom::glance(model)
    coefs <- broom::tidy(model)

    # Confusion matrix
    df <- ch3_data()
    pred_class <- ifelse(predict(model, type = "response") >= 0.5, 1, 0)
    accuracy <- mean(pred_class == df$zdal_num) * 100

    tagList(
      div(class = "stat-box", style = paste0("background:", col_logit, ";"),
          paste0("AIC = ", round(g$AIC, 1))),
      div(class = "stat-box", style = paste0("background:", col_data, ";"),
          paste0("BIC = ", round(g$BIC, 1))),
      div(class = "stat-box", style = paste0("background:", col_predict, ";"),
          paste0("Dok\u0142adno\u015b\u0107 = ", round(accuracy, 1), "%"))
    )
  })

  output$ch3_prediction <- renderUI({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    newdata <- data.frame(
      godziny_nauki = input$ch3_pred_hours,
      srednia_ocen = input$ch3_pred_gpa
    )
    prob <- predict(model, newdata, type = "response")

    color <- if (prob >= 0.5) col_predict else col_residual
    decision <- if (prob >= 0.5) "Prawdopodobnie zda" else "Raczej nie zda"

    div(class = "stat-box", style = paste0("background:", color, "; display: block;"),
        paste0("P(zdanie) = ", round(prob, 3), "\n", decision))
  })

  # --- Widget 3: Odds ratios ---
  output$ch3_odds_ratios <- renderUI({
    model <- ch3_model()
    if (is.null(model)) {
      return(div(class = "callout-warning", "Najpierw dopasuj model."))
    }

    coefs <- broom::tidy(model, conf.int = TRUE)
    coefs$or <- exp(coefs$estimate)
    coefs$or_low <- exp(coefs$conf.low)
    coefs$or_high <- exp(coefs$conf.high)

    labels_pl <- c(
      "(Intercept)" = "Wyraz wolny",
      "godziny_nauki" = "Godziny nauki (+1h)",
      "srednia_ocen" = "\u015arednia ocen (+1 pkt)"
    )

    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)

    rows <- lapply(2:nrow(coefs), function(i) {  # pomijamy intercept
      tags$tr(
        tags$td(coefs$term_pl[i]),
        tags$td(round(coefs$estimate[i], 3)),
        tags$td(tags$strong(round(coefs$or[i], 3))),
        tags$td(paste0("[", round(coefs$or_low[i], 3), " ; ",
                        round(coefs$or_high[i], 3), "]")),
        tags$td(format.pval(coefs$p.value[i], digits = 3))
      )
    })

    tagList(
      tags$table(class = "table table-bordered",
        style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Zmienna"), tags$th("\u03b2"), tags$th("OR"),
                  tags$th("95% CI (OR)"), tags$th("p"))
        ),
        tags$tbody(rows)
      ),
      div(class = "callout-info",
        p(tags$strong("Interpretacja OR:"),
          " OR > 1 oznacza, \u017ce wzrost predyktora o 1 zwi\u0119ksza szanse sukcesu.
            OR < 1 \u2014 zmniejsza. OR = 1 \u2014 brak wp\u0142ywu.")
      )
    )
  })
}
