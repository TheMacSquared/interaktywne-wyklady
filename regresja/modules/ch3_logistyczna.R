# ============================================================================
# CHAPTER 3: Regresja logistyczna
# ============================================================================

ch3_ui <- list(
  id    = "ch-logistyczna",
  num   = "03",
  title = "Regresja logistyczna",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Regresja",
      num    = "03",
      title  = "Regresja logistyczna.",
      lead   = "Regresja liniowa wymaga ciągłej zmiennej zależnej.
                A co, gdy Y to 0 lub 1 (sukces/porażka)?"
    ),

    lc_h2("ch3-dlaczego", "Dlaczego nie regresja liniowa?"),

    tagList(
      p("Gdy zmienna zależna jest ", tags$b("binarna"),
        " (np. zdany/niezdany egzamin), regresja liniowa daje przewidywania
        spoza zakresu [0, 1]. Rozwiązanie: ", tags$b("regresja logistyczna"), "."),
      p("Zamiast modelować Y bezpośrednio, modelujemy ",
        tags$b("prawdopodobieństwo"), " sukcesu:"),
      lc_formula_box(
        withMathJax(helpText(
          "$$P(Y=1) = \\frac{1}{1 + e^{-(\\beta_0 + \\beta_1 X_1 + \\ldots + \\beta_k X_k)}}$$"
        )),
        p("Funkcja logistyczna (sigmoida) zamyka wynik w [0, 1].")
      )
    ),

    lc_h2("ch3-krzywa", "Krzywa logistyczna"),

    figure_panel(
      label = "Ryc. 3.1", title = "Sigmoida w akcji",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch3_b0", "β₀ (intercept):",
                      min = -10, max = 10, value = -4, step = 0.5),
          sliderInput("ch3_b1", "β₁ (slope):",
                      min = -3, max = 3, value = 0.2, step = 0.05),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch3_preset_steep", "Stromy",
                         class = "lc-btn-outline"),
            actionButton("ch3_preset_flat", "Płaski",
                         class = "lc-btn-secondary-outline"),
            actionButton("ch3_preset_neg", "Odwrotny",
                         class = "lc-btn-danger-outline")
          )
        ),
        column(8,
          plotOutput("ch3_sigmoid_plot", height = "350px")
        )
      )
    ),

    lc_h2("ch3-model-dane", "Model logistyczny na danych"),

    tagList(
      p("Scenariusz: czy student zda egzamin? Predyktory: godziny nauki
        i średnia ocen.")
    ),

    figure_panel(
      label = "Ryc. 3.2", title = "Predykcja zdania egzaminu",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch3_n", "n:", min = 50, max = 300, value = 150, step = 25),
          selectInput("ch3_predictor", "Prezentowany predyktor:",
            choices = c(
              "Godziny nauki" = "godziny_nauki",
              "Średnia ocen"  = "srednia_ocen"
            ),
            selected = "godziny_nauki"
          ),
          actionButton("ch3_fit", "Dopasuj model",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Predykcja dla nowego studenta:"),
          numericInput("ch3_pred_hours", "Godziny nauki:", value = 20, min = 0, max = 40),
          numericInput("ch3_pred_gpa", "Średnia ocen:", value = 3.5, min = 2, max = 5, step = 0.1),
          uiOutput("ch3_prediction")
        ),
        column(8,
          plotOutput("ch3_logit_plot", height = "350px"),
          uiOutput("ch3_model_summary")
        )
      )
    ),

    lc_h2("ch3-iloraz-szans", "Interpretacja: iloraz szans"),

    tagList(
      p("W regresji logistycznej współczynniki interpretujemy przez ",
        tags$strong("iloraz szans (odds ratio)"), ":"),
      lc_formula_box(
        withMathJax(helpText("$$OR = e^{\\beta_j}$$")),
        p("OR = 1.5 oznacza: wzrost X o 1 zwiększa szanse sukcesu
          1.5-krotnie.")
      )
    ),

    figure_panel(
      label = "Ryc. 3.3", title = "Odds ratio",
      full_width = TRUE,
      helpText("Używa modelu dopasowanego powyżej."),
      uiOutput("ch3_odds_ratios")
    ),

    inline_callout(label = "Ocena modelu", color = "wskazowka",
      "Nie używamy R² w sensie liniowym. Zamiast tego: AIC, BIC, oraz
       macierz pomyłek (confusion matrix) z dokładnością, czułością
       i swoistością."
    ),

    lc_chapter_next(
      num       = "04",
      title     = "Porównanie modeli",
      lead      = "jak porównać modele i wybrać najlepszy",
      target_id = "ch-porownanie"
    )
  )
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
      geom_line(color = unname(upwr_cat["wrzos"]), linewidth = 1.5) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = upwr_secondary, alpha = 0.5) +
      labs(
           x = "X", y = "P(Y = 1)") +
      ylim(0, 1) +
      theme_upwr()
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
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      pred_var <- input$ch3_predictor
      pred_label <- if (pred_var == "godziny_nauki") "Godziny nauki" else "Średnia ocen"

      # Predykcja dla wykresu (trzymajac drugi predyktor na sredniej)
      other_var <- setdiff(c("godziny_nauki", "srednia_ocen"), pred_var)
      other_mean <- mean(df[[other_var]])

      x_seq <- seq(min(df[[pred_var]]), max(df[[pred_var]]), length.out = 200)
      newdata <- data.frame(x_seq, other_mean)
      names(newdata) <- c(pred_var, other_var)
      newdata$pred_prob <- predict(model, newdata, type = "response")

      ggplot() +
        geom_jitter(data = df, aes(x = .data[[pred_var]], y = .data[["zdal_num"]]),
                    height = 0.03, alpha = 0.3, color = upwr_secondary) +
        geom_line(data = newdata, aes(x = .data[[pred_var]], y = .data[["pred_prob"]]),
                  color = unname(upwr_cat["wrzos"]), linewidth = 1.5) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = unname(upwr_cat["bursztyn"])) +
        labs(
             x = pred_label, y = "P(zdanie egzaminu)") +
        ylim(-0.05, 1.05) +
        theme_upwr()
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
      lc_stat_box("AIC", round(g$AIC, 1), color = unname(upwr_cat["wrzos"])),
      lc_stat_box("BIC", round(g$BIC, 1), color = upwr_secondary),
      lc_stat_box("Dokładność", round(accuracy, 1), "%", color = unname(upwr_cat["szalwia"]))
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

    color <- if (prob >= 0.5) unname(upwr_cat["szalwia"]) else unname(upwr_cat["terakota"])
    decision <- if (prob >= 0.5) "Prawdopodobnie zda" else "Raczej nie zda"

    lc_stat_box("P(zdanie)", round(prob, 3), caption = decision, color = color)
  })

  # --- Widget 3: Odds ratios ---
  output$ch3_odds_ratios <- renderUI({
    model <- ch3_model()
    if (is.null(model)) {
      return(lc_feedback(type = "warning", "Najpierw dopasuj model."))
    }

    coefs <- broom::tidy(model, conf.int = TRUE)
    coefs$or <- exp(coefs$estimate)
    coefs$or_low <- exp(coefs$conf.low)
    coefs$or_high <- exp(coefs$conf.high)

    labels_pl <- c(
      "(Intercept)" = "Wyraz wolny",
      "godziny_nauki" = "Godziny nauki (+1h)",
      "srednia_ocen" = "Średnia ocen (+1 pkt)"
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
      tags$table(class = "lc-table lc-table-bordered",
        style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Zmienna"), tags$th("β"), tags$th("OR"),
                  tags$th("95% CI (OR)"), tags$th("p"))
        ),
        tags$tbody(rows)
      ),
      lc_feedback(type = "info",
        p(tags$strong("Interpretacja OR:"),
          " OR > 1 oznacza, że wzrost predyktora o 1 zwiększa szanse sukcesu.
            OR < 1 — zmniejsza. OR = 1 — brak wpływu.")
      )
    )
  })
}
