# ============================================================================
# CHAPTER 4: Porownanie modeli
# ============================================================================

ch4_ui <- tabPanel("4. Por\u00f3wnanie modeli",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Zbudowali\u015bmy r\u00f3\u017cne modele. Jak wybra\u0107 najlepszy?
       Poznajmy metryki por\u00f3wnawcze."
    ),

    div(class = "section-title", "Metryki jako\u015bci modelu"),

    div(class = "narrative",
      p("Nie ma jednej uniwersalnej miary. Ka\u017cda odpowiada na inne pytanie:"),
      tags$table(class = "table table-bordered", style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Metryka"), tags$th("Co mierzy"), tags$th("Lepiej gdy"))
        ),
        tags$tbody(
          tags$tr(tags$td(withMathJax("\\(R^2\\)")),
                  tags$td("Odsetek wyja\u015bnionej zmienno\u015bci"),
                  tags$td("wy\u017csze")),
          tags$tr(tags$td(withMathJax("\\(R^2_{adj}\\)")),
                  tags$td("R\u00b2 skorygowane za liczb\u0119 predyktor\u00f3w"),
                  tags$td("wy\u017csze")),
          tags$tr(tags$td("AIC"),
                  tags$td("Jako\u015b\u0107 + z\u0142o\u017cono\u015b\u0107 (kara za parametry)"),
                  tags$td("ni\u017csze")),
          tags$tr(tags$td("BIC"),
                  tags$td("Jak AIC, ale silniejsza kara za parametry"),
                  tags$td("ni\u017csze")),
          tags$tr(tags$td("RMSE"),
                  tags$td("\u015aredni b\u0142\u0105d predykcji (w jednostkach Y)"),
                  tags$td("ni\u017csze"))
        )
      )
    ),

    div(class = "formula-box",
      p(tags$strong("AIC:"), withMathJax("\\(AIC = -2 \\ln(L) + 2k\\)")),
      p(tags$strong("BIC:"), withMathJax("\\(BIC = -2 \\ln(L) + k \\ln(n)\\)")),
      p(tags$strong("RMSE:"), withMathJax("\\(RMSE = \\sqrt{\\frac{1}{n}\\sum(y_i - \\hat{y}_i)^2}\\)")),
      p("gdzie L = wiarygodno\u015b\u0107, k = liczba parametr\u00f3w, n = liczba obserwacji")
    ),

    # ========================================================================
    # WIDGET 1: Porownanie modeli liniowych
    # ========================================================================
    div(class = "section-title", "Arena modeli liniowych"),

    div(class = "widget-block",
      h4("Por\u00f3wnanie modeli regresji"),
      fluidRow(
        column(4,
          helpText("Generujemy dane i budujemy 4 modele z r\u00f3\u017cn\u0105 liczb\u0105 predyktor\u00f3w."),
          sliderInput("ch4_n", "n:", min = 50, max = 300, value = 150, step = 25),
          actionButton("ch4_compare", "Buduj i por\u00f3wnaj modele",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch4_metrics_plot", height = "350px"),
          uiOutput("ch4_metrics_table")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("AIC vs BIC:"),
      " AIC faworyzuje modele z lepsz\u0105 predykcj\u0105 (nawet z\u0142o\u017cone).
        BIC mocniej karze za z\u0142o\u017cono\u015b\u0107 \u2014 preferuje prostsze modele.
        Gdy si\u0119 nie zgadzaj\u0105, AIC jest lepszy do predykcji, BIC do wyja\u015bnienia."
    ),

    # ========================================================================
    # WIDGET 2: Porownanie liniowy vs logistyczny
    # ========================================================================
    div(class = "section-title", "Liniowy vs logistyczny"),

    div(class = "narrative",
      p("Co si\u0119 stanie, je\u015bli spr\u00f3bujemy u\u017cy\u0107 regresji liniowej
        do predykcji zmiennej binarnej? Por\u00f3wnajmy z logistyczn\u0105.")
    ),

    div(class = "widget-block",
      h4("Liniowy vs logistyczny (dane binarne)"),
      fluidRow(
        column(4,
          actionButton("ch4_lin_vs_log", "Generuj por\u00f3wnanie",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch4_lin_log_plot", height = "300px"),
          uiOutput("ch4_lin_log_stats")
        )
      )
    ),

    div(class = "callout-danger",
      tags$strong("Wniosek:"),
      " Regresja liniowa na danych binarnych daje predykcje poza [0, 1]
        i nie jest poprawnym modelem. Zawsze u\u017cywaj regresji logistycznej
        dla zmiennej zale\u017cnej 0/1."
    ),

    # ========================================================================
    # WIDGET 3: Overfitting
    # ========================================================================
    div(class = "section-title", "Przeucz enie (overfitting)"),

    div(class = "narrative",
      p("Model z wieloma parametrami mo\u017ce idealnie dopasowa\u0107 si\u0119 do danych
        treningowych, ale \u017ale generalizowa\u0107. Zobaczmy to na wielomianach.")
    ),

    div(class = "widget-block",
      h4("Wielomian: dopasowanie vs generalizacja"),
      fluidRow(
        column(4,
          sliderInput("ch4_poly_degree", "Stopie\u0144 wielomianu:",
                      min = 1, max = 15, value = 1, step = 1),
          sliderInput("ch4_poly_n", "n (punkt\u00f3w):",
                      min = 15, max = 100, value = 30, step = 5),
          actionButton("ch4_poly_gen", "Generuj",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch4_poly_plot", height = "300px"),
          uiOutput("ch4_poly_stats")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Z\u0142ota regu\u0142a:"),
      " Najlepszy model to nie ten z najwy\u017cszym R\u00b2, ale ten, kt\u00f3ry
        najlepiej ", tags$b("generalizuje"), " na nowe dane.
        U\u017cywaj AIC/BIC do wyboru z\u0142o\u017cono\u015bci."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: podsumowanie wzor\u00f3w i zasad"),
      actionButton("ch4_next", "Dalej \u2192 5. \u015aci\u0105ga",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch4_server <- function(input, output, session) {

  # --- Widget 1: Porownanie modeli liniowych ---
  ch4_models <- reactiveVal(NULL)

  observeEvent(input$ch4_compare, {
    df <- generate_multi_data(input$ch4_n)

    models <- list(
      "1: nauka" = lm(ocena ~ godziny_nauki, data = df),
      "2: nauka + frekw." = lm(ocena ~ godziny_nauki + frekwencja, data = df),
      "3: nauka + frekw. + stres" = lm(ocena ~ godziny_nauki + frekwencja + stres, data = df),
      "4: wszystkie" = lm(ocena ~ godziny_nauki + frekwencja + stres + sen_h, data = df)
    )

    results <- lapply(names(models), function(name) {
      m <- compute_model_metrics(models[[name]])
      data.frame(
        model = name,
        r_squared = m$r_squared,
        adj_r_squared = m$adj_r_squared,
        aic = m$aic,
        bic = m$bic,
        rmse = m$rmse,
        n_params = m$n_params
      )
    })

    ch4_models(do.call(rbind, results))
  })

  output$ch4_metrics_plot <- renderPlot({
    df <- ch4_models()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Buduj i por\u00f3wnaj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      # Normalize metrics for comparison
      long <- df %>%
        select(model, adj_r_squared, aic, bic, rmse) %>%
        tidyr::pivot_longer(-model, names_to = "metric", values_to = "value") %>%
        mutate(metric = factor(metric,
          levels = c("adj_r_squared", "aic", "bic", "rmse"),
          labels = c("adj. R\u00b2", "AIC", "BIC", "RMSE")))

      ggplot(long, aes(x = model, y = value, fill = model)) +
        geom_col(alpha = 0.8) +
        facet_wrap(~metric, scales = "free_y", ncol = 4) +
        scale_fill_manual(values = c(col_fit, col_predict, col_warning, col_logit)) +
        labs(title = "Metryki 4 modeli", x = NULL, y = "Warto\u015b\u0107") +
        theme_reg() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
    }
  })

  output$ch4_metrics_table <- renderUI({
    df <- ch4_models()
    if (is.null(df)) return(NULL)

    # Zaznacz najlepsze wartosci
    best_adj_r2 <- which.max(df$adj_r_squared)
    best_aic <- which.min(df$aic)
    best_bic <- which.min(df$bic)
    best_rmse <- which.min(df$rmse)

    rows <- lapply(1:nrow(df), function(i) {
      tags$tr(
        tags$td(df$model[i]),
        tags$td(round(df$r_squared[i], 3)),
        tags$td(style = if (i == best_adj_r2) "font-weight:bold; color:#27ae60;" else "",
                round(df$adj_r_squared[i], 3)),
        tags$td(style = if (i == best_aic) "font-weight:bold; color:#27ae60;" else "",
                round(df$aic[i], 1)),
        tags$td(style = if (i == best_bic) "font-weight:bold; color:#27ae60;" else "",
                round(df$bic[i], 1)),
        tags$td(style = if (i == best_rmse) "font-weight:bold; color:#27ae60;" else "",
                round(df$rmse[i], 3))
      )
    })

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Model"), tags$th("R\u00b2"), tags$th("adj.R\u00b2"),
                tags$th("AIC"), tags$th("BIC"), tags$th("RMSE"))
      ),
      tags$tbody(rows)
    )
  })

  # --- Widget 2: Liniowy vs logistyczny ---
  ch4_lin_log_data <- reactiveVal(NULL)

  observeEvent(input$ch4_lin_vs_log, {
    ch4_lin_log_data(generate_logistic_data(200))
  })

  output$ch4_lin_log_plot <- renderPlot({
    df <- ch4_lin_log_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      ggplot(df, aes(x = godziny_nauki, y = zdal_num)) +
        geom_jitter(height = 0.03, alpha = 0.3, color = col_data) +
        geom_smooth(method = "lm", se = FALSE, color = col_model_a,
                    linewidth = 1, linetype = "dashed") +
        geom_smooth(method = "glm", method.args = list(family = "binomial"),
                    se = FALSE, color = col_logit, linewidth = 1.2) +
        geom_hline(yintercept = c(0, 1), linetype = "dotted", color = "#bdc3c7") +
        annotate("text", x = 5, y = 0.85, label = "Logistyczny", color = col_logit,
                 fontface = "bold") +
        annotate("text", x = 35, y = 0.85, label = "Liniowy", color = col_model_a,
                 fontface = "bold") +
        labs(title = "Regresja liniowa vs logistyczna (dane binarne)",
             x = "Godziny nauki", y = "P(zdanie)") +
        ylim(-0.2, 1.2) +
        theme_reg()
    }
  })

  output$ch4_lin_log_stats <- renderUI({
    df <- ch4_lin_log_data()
    if (is.null(df)) return(NULL)

    lin <- lm(zdal_num ~ godziny_nauki, data = df)
    log <- glm(zdal_num ~ godziny_nauki, data = df, family = binomial)

    lin_pred <- ifelse(fitted(lin) >= 0.5, 1, 0)
    log_pred <- ifelse(fitted(log) >= 0.5, 1, 0)
    acc_lin <- mean(lin_pred == df$zdal_num) * 100
    acc_log <- mean(log_pred == df$zdal_num) * 100

    # Procent predykcji poza [0,1]
    outside <- mean(fitted(lin) < 0 | fitted(lin) > 1) * 100

    tagList(
      div(class = "stat-box", style = paste0("background:", col_model_a, ";"),
          paste0("Liniowy: ", round(acc_lin, 1), "%")),
      div(class = "stat-box", style = paste0("background:", col_logit, ";"),
          paste0("Logistyczny: ", round(acc_log, 1), "%")),
      div(class = "stat-box", style = paste0("background:", col_residual, ";"),
          paste0("Liniowy poza [0,1]: ", round(outside, 1), "%"))
    )
  })

  # --- Widget 3: Overfitting ---
  ch4_poly_data <- reactiveVal(NULL)

  observeEvent(input$ch4_poly_gen, {
    n <- input$ch4_poly_n
    x <- sort(runif(n, 0, 10))
    y <- sin(x) * 3 + rnorm(n, 0, 1)
    ch4_poly_data(data.frame(x = x, y = y))
  })

  output$ch4_poly_plot <- renderPlot({
    df <- ch4_poly_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      degree <- input$ch4_poly_degree
      model <- lm(y ~ poly(x, degree), data = df)

      x_pred <- seq(min(df$x), max(df$x), length.out = 200)
      y_pred <- predict(model, newdata = data.frame(x = x_pred))

      pred_df <- data.frame(x = x_pred, y = y_pred)

      ggplot() +
        geom_point(data = df, aes(x = x, y = y), color = col_data, alpha = 0.5) +
        geom_line(data = pred_df, aes(x = x, y = y), color = col_fit, linewidth = 1.2) +
        labs(title = paste0("Wielomian stopnia ", degree),
             x = "X", y = "Y") +
        theme_reg()
    }
  })

  output$ch4_poly_stats <- renderUI({
    df <- ch4_poly_data()
    if (is.null(df)) return(NULL)
    degree <- input$ch4_poly_degree
    model <- lm(y ~ poly(x, degree), data = df)
    metrics <- compute_model_metrics(model)

    tagList(
      div(class = "stat-box", style = paste0("background:", col_fit, ";"),
          paste0("R\u00b2 = ", round(metrics$r_squared, 3))),
      div(class = "stat-box", style = paste0("background:", col_predict, ";"),
          paste0("adj.R\u00b2 = ", round(metrics$adj_r_squared, 3))),
      div(class = "stat-box", style = paste0("background:", col_warning, ";"),
          paste0("AIC = ", round(metrics$aic, 1))),
      div(class = "stat-box", style = paste0("background:", col_data, ";"),
          paste0("BIC = ", round(metrics$bic, 1)))
    )
  })
}
