# ============================================================================
# CHAPTER 4: Porownanie modeli
# ============================================================================

ch4_ui <- list(
  id    = "ch-porownanie",
  num   = "04",
  title = "Porównanie modeli",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 04 · Regresja",
      num    = "04",
      title  = "Porównanie modeli.",
      lead   = "Zbudowaliśmy różne modele. Jak wybrać najlepszy?
                Poznajmy metryki porównawcze."
    ),

    h2(id = "ch4-metryki", class = "section-title",
       "Metryki jakości modelu"),

    div(class = "narrative",
      p("Nie ma jednej uniwersalnej miary. Każda odpowiada na inne pytanie:"),
      tags$table(class = "table table-bordered", style = "font-size: 14px;",
        tags$thead(
          tags$tr(tags$th("Metryka"), tags$th("Co mierzy"), tags$th("Lepiej gdy"))
        ),
        tags$tbody(
          tags$tr(tags$td(withMathJax("\\(R^2\\)")),
                  tags$td("Odsetek wyjaśnionej zmienności"),
                  tags$td("wyższe")),
          tags$tr(tags$td(withMathJax("\\(R^2_{adj}\\)")),
                  tags$td("R² skorygowane za liczbę predyktorów"),
                  tags$td("wyższe")),
          tags$tr(tags$td("AIC"),
                  tags$td("Jakość + złożoność (kara za parametry)"),
                  tags$td("niższe")),
          tags$tr(tags$td("BIC"),
                  tags$td("Jak AIC, ale silniejsza kara za parametry"),
                  tags$td("niższe")),
          tags$tr(tags$td("RMSE"),
                  tags$td("Średni błąd predykcji (w jednostkach Y)"),
                  tags$td("niższe"))
        )
      )
    ),

    div(class = "formula-box",
      p(tags$strong("AIC:"), withMathJax("\\(AIC = -2 \\ln(L) + 2k\\)")),
      p(tags$strong("BIC:"), withMathJax("\\(BIC = -2 \\ln(L) + k \\ln(n)\\)")),
      p(tags$strong("RMSE:"), withMathJax("\\(RMSE = \\sqrt{\\frac{1}{n}\\sum(y_i - \\hat{y}_i)^2}\\)")),
      p("gdzie L = wiarygodność, k = liczba parametrów, n = liczba obserwacji")
    ),

    h2(id = "ch4-arena", class = "section-title",
       "Arena modeli liniowych"),

    figure_panel(
      label = "Ryc. 4.1", title = "Porównanie modeli regresji",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Generujemy dane i budujemy 4 modele z różną
                    liczbą predyktorów."),
          sliderInput("ch4_n", "n:", min = 50, max = 300, value = 150, step = 25),
          actionButton("ch4_compare", "Buduj i porównaj modele",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch4_metrics_plot", height = "350px"),
          uiOutput("ch4_metrics_table")
        )
      )
    ),

    margin_callout(label = "AIC vs BIC", color = "wskazowka",
      "AIC faworyzuje modele z lepszą predykcją (nawet złożone).
       BIC mocniej karze za złożoność — preferuje prostsze modele.
       Gdy się nie zgadzają, AIC jest lepszy do predykcji, BIC do
       wyjaśnienia."
    ),

    h2(id = "ch4-liniowy-vs-logistyczny", class = "section-title",
       "Liniowy vs logistyczny"),

    div(class = "narrative",
      p("Co się stanie, jeśli spróbujemy użyć regresji liniowej
        do predykcji zmiennej binarnej? Porównajmy z logistyczną.")
    ),

    figure_panel(
      label = "Ryc. 4.2", title = "Liniowy vs logistyczny (dane binarne)",
      full_width = TRUE,
      fluidRow(
        column(4,
          actionButton("ch4_lin_vs_log", "Generuj porównanie",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch4_lin_log_plot", height = "300px"),
          uiOutput("ch4_lin_log_stats")
        )
      )
    ),

    margin_callout(label = "Wniosek", color = "uwaga",
      "Regresja liniowa na danych binarnych daje predykcje poza [0, 1]
       i nie jest poprawnym modelem. Zawsze używaj regresji logistycznej
       dla zmiennej zależnej 0/1."
    ),

    h2(id = "ch4-overfitting", class = "section-title",
       "Przeuczenie (overfitting)"),

    div(class = "narrative",
      p("Model z wieloma parametrami może idealnie dopasować się do
        danych treningowych, ale źle generalizować. Zobaczmy to
        na wielomianach.")
    ),

    figure_panel(
      label = "Ryc. 4.3", title = "Wielomian: dopasowanie vs generalizacja",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch4_poly_degree", "Stopień wielomianu:",
                      min = 1, max = 15, value = 1, step = 1),
          sliderInput("ch4_poly_n", "n (punktów):",
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

    margin_callout(label = "Złota reguła", color = "uwaga",
      "Najlepszy model to nie ten z najwyższym R², ale ten, który
       najlepiej generalizuje na nowe dane. Używaj AIC/BIC do wyboru
       złożoności."
    ),

    lc_chapter_next(
      num       = "05",
      title     = "Ściąga",
      lead      = "podsumowanie wzorów i zasad",
      target_id = "ch-sciaga"
    )
  )
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
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Buduj i porównaj'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      # Normalize metrics for comparison
      long <- df %>%
        select(model, adj_r_squared, aic, bic, rmse) %>%
        tidyr::pivot_longer(-model, names_to = "metric", values_to = "value") %>%
        mutate(metric = factor(metric,
          levels = c("adj_r_squared", "aic", "bic", "rmse"),
          labels = c("adj. R²", "AIC", "BIC", "RMSE")))

      ggplot(long, aes(x = model, y = value, fill = model)) +
        geom_col(alpha = 0.8) +
        facet_wrap(~metric, scales = "free_y", ncol = 4) +
        scale_fill_manual(values = c(unname(upwr_cat["niebo"]), unname(upwr_cat["szalwia"]), unname(upwr_cat["bursztyn"]), unname(upwr_cat["wrzos"]))) +
        labs(title = "Metryki 4 modeli", x = NULL, y = "Wartość") +
        theme_upwr() +
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
        tags$tr(tags$th("Model"), tags$th("R²"), tags$th("adj.R²"),
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
        geom_jitter(height = 0.03, alpha = 0.3, color = upwr_secondary) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"]),
                    linewidth = 1, linetype = "dashed") +
        geom_smooth(method = "glm", method.args = list(family = "binomial"),
                    se = FALSE, color = unname(upwr_cat["wrzos"]), linewidth = 1.2) +
        geom_hline(yintercept = c(0, 1), linetype = "dotted", color = "#bdc3c7") +
        annotate("text", x = 5, y = 0.85, label = "Logistyczny", color = unname(upwr_cat["wrzos"]),
                 fontface = "bold") +
        annotate("text", x = 35, y = 0.85, label = "Liniowy", color = unname(upwr_cat["niebo"]),
                 fontface = "bold") +
        labs(title = "Regresja liniowa vs logistyczna (dane binarne)",
             x = "Godziny nauki", y = "P(zdanie)") +
        ylim(-0.2, 1.2) +
        theme_upwr()
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
      div(class = "stat-box", style = paste0("background:", unname(upwr_cat["niebo"]), ";"),
          paste0("Liniowy: ", round(acc_lin, 1), "%")),
      div(class = "stat-box", style = paste0("background:", unname(upwr_cat["wrzos"]), ";"),
          paste0("Logistyczny: ", round(acc_log, 1), "%")),
      div(class = "stat-box", style = paste0("background:", unname(upwr_cat["terakota"]), ";"),
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
        geom_point(data = df, aes(x = x, y = y), color = upwr_secondary, alpha = 0.5) +
        geom_line(data = pred_df, aes(x = x, y = y), color = unname(upwr_cat["niebo"]), linewidth = 1.2) +
        labs(title = paste0("Wielomian stopnia ", degree),
             x = "X", y = "Y") +
        theme_upwr()
    }
  })

  output$ch4_poly_stats <- renderUI({
    df <- ch4_poly_data()
    if (is.null(df)) return(NULL)
    degree <- input$ch4_poly_degree
    model <- lm(y ~ poly(x, degree), data = df)
    metrics <- compute_model_metrics(model)

    tagList(
      div(class = "stat-box", style = paste0("background:", unname(upwr_cat["niebo"]), ";"),
          paste0("R² = ", round(metrics$r_squared, 3))),
      div(class = "stat-box", style = paste0("background:", unname(upwr_cat["szalwia"]), ";"),
          paste0("adj.R² = ", round(metrics$adj_r_squared, 3))),
      div(class = "stat-box", style = paste0("background:", unname(upwr_cat["bursztyn"]), ";"),
          paste0("AIC = ", round(metrics$aic, 1))),
      div(class = "stat-box", style = paste0("background:", upwr_secondary, ";"),
          paste0("BIC = ", round(metrics$bic, 1)))
    )
  })
}
