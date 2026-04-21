# ============================================================================
# CHAPTER 2: Regresja wieloraka
# ============================================================================

ch2_ui <- tabPanel("2. Regresja wieloraka",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Regresja prosta używała jednego predyktora.
       W rzeczywistości na Y wpływa wiele czynników jednocześnie."
    ),

    div(class = "section-title", "Wiele predyktorów naraz"),

    div(class = "narrative",
      p("Regresja wieloraka rozszerza model o ", tags$b("k predyktorów"), ":"),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$Y = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\ldots + \\beta_k X_k + \\varepsilon$$"
        ))
      ),
      p("Każde ", withMathJax("\\(\\beta_j\\)"), " mówi:
        o ile zmieni się Y, gdy ", withMathJax("\\(X_j\\)"),
        " wzrośnie o 1, ", tags$b("przy stałych pozostałych zmiennych"), ".")
    ),

    # ========================================================================
    # WIDGET 1: Regresja wieloraka
    # ========================================================================
    div(class = "section-title", "Budowanie modelu wielorakiego"),

    div(class = "widget-block",
      h4("Predykcja średniej ocen"),
      fluidRow(
        column(4,
          helpText("Dane: 150 studentów. Zmienna zależna: średnia ocen."),
          checkboxGroupInput("ch2_predictors", "Predyktory:",
            choices = c(
              "Godziny nauki/tydz." = "godziny_nauki",
              "Frekwencja (%)" = "frekwencja",
              "Poziom stresu (1-10)" = "stres",
              "Sen (h/dobę)" = "sen_h"
            ),
            selected = c("godziny_nauki", "frekwencja")
          ),
          actionButton("ch2_gen", "Generuj dane i dopasuj",
                       class = "btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch2_model_coefs"),
          plotOutput("ch2_coef_plot", height = "250px"),
          uiOutput("ch2_model_stats")
        )
      )
    ),

    div(class = "callout-info",
      tags$strong("Skorygowane R²:"),
      " Zwykłe R² zawsze rośnie z każdym dodanym predyktorem (nawet bezużytecznym!).
        Adjusted R² koryguje ten efekt — karze za zbędne zmienne."
    ),

    # ========================================================================
    # WIDGET 2: Dodawanie zmiennych krok po kroku
    # ========================================================================
    div(class = "section-title", "Efekt dodawania zmiennych"),

    div(class = "narrative",
      p("Zobaczmy, jak zmieniają się metryki modelu, gdy dodajemy
        kolejne predyktory. Czy każda zmienna poprawia model?")
    ),

    div(class = "widget-block",
      h4("Krok po kroku"),
      fluidRow(
        column(4,
          helpText("Modele z 1, 2, 3 i 4 predyktorami — porównanie metryk."),
          actionButton("ch2_stepwise", "Buduj modele krok po kroku",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch2_step_plot", height = "300px"),
          uiOutput("ch2_step_table")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("Ostrożnie z dodawaniem zmiennych!"),
      " Więcej zmiennych = większe R², ale nie zawsze lepszy model.
        Przeuczone modele słabo generalizują. Używaj adj. R², AIC, BIC."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: gdy zmienna zależna jest binarna"),
      actionButton("ch2_next", "Dalej → 3. Regresja logistyczna",
                   class = "btn-primary btn-lg")
    )
  ))
)

# ============================================================================
# SERVER
# ============================================================================

ch2_server <- function(input, output, session) {

  ch2_data <- reactiveVal(NULL)
  ch2_model <- reactiveVal(NULL)

  observeEvent(input$ch2_gen, {
    df <- generate_multi_data(150)
    ch2_data(df)

    preds <- input$ch2_predictors
    if (length(preds) == 0) preds <- "godziny_nauki"

    formula <- as.formula(paste("ocena ~", paste(preds, collapse = " + ")))
    model <- lm(formula, data = df)
    ch2_model(model)
  })

  output$ch2_model_coefs <- renderUI({
    model <- ch2_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model)

    labels_pl <- c(
      "(Intercept)" = "Wyraz wolny",
      "godziny_nauki" = "Godziny nauki",
      "frekwencja" = "Frekwencja",
      "stres" = "Stres",
      "sen_h" = "Sen (h)"
    )

    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)

    rows <- lapply(1:nrow(coefs), function(i) {
      sig <- if (coefs$p.value[i] < 0.05) " *" else ""
      tags$tr(
        tags$td(coefs$term_pl[i]),
        tags$td(round(coefs$estimate[i], 4)),
        tags$td(round(coefs$std.error[i], 4)),
        tags$td(round(coefs$statistic[i], 3)),
        tags$td(paste0(format.pval(coefs$p.value[i], digits = 3), sig))
      )
    })

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 14px;",
      tags$thead(
        tags$tr(tags$th("Zmienna"), tags$th("Estymata"), tags$th("SE"),
                tags$th("t"), tags$th("p"))
      ),
      tags$tbody(rows)
    )
  })

  output$ch2_coef_plot <- renderPlot({
    model <- ch2_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model, conf.int = TRUE)
    coefs <- coefs[coefs$term != "(Intercept)", ]

    if (nrow(coefs) == 0) return(NULL)

    labels_pl <- c(
      "godziny_nauki" = "Godziny nauki",
      "frekwencja" = "Frekwencja",
      "stres" = "Stres",
      "sen_h" = "Sen (h)"
    )
    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)
    coefs$significant <- coefs$p.value < 0.05

    ggplot(coefs, aes(x = estimate, y = term_pl, color = significant)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = col_dark) +
      scale_color_manual(values = c("TRUE" = col_fit, "FALSE" = col_residual),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                         name = NULL) +
      labs(title = "Współczynniki regresji z 95% CI",
           x = "Estymata β", y = NULL) +
      theme_educational() +
      theme(legend.position = "top")
  })

  output$ch2_model_stats <- renderUI({
    model <- ch2_model()
    if (is.null(model)) return(NULL)
    metrics <- compute_model_metrics(model)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_fit, ";"),
          paste0("R² = ", round(metrics$r_squared, 3))),
      div(class = "stat-box", style = paste0("background:", col_predict, ";"),
          paste0("adj.R² = ", round(metrics$adj_r_squared, 3))),
      div(class = "stat-box", style = paste0("background:", col_warning, ";"),
          paste0("AIC = ", round(metrics$aic, 1))),
      div(class = "stat-box", style = paste0("background:", col_residual, ";"),
          paste0("RMSE = ", round(metrics$rmse, 3)))
    )
  })

  # --- Widget 2: Krok po kroku ---
  ch2_step_data <- reactiveVal(NULL)

  observeEvent(input$ch2_stepwise, {
    df <- generate_multi_data(150)

    pred_sets <- list(
      c("godziny_nauki"),
      c("godziny_nauki", "frekwencja"),
      c("godziny_nauki", "frekwencja", "stres"),
      c("godziny_nauki", "frekwencja", "stres", "sen_h")
    )

    results <- lapply(seq_along(pred_sets), function(i) {
      formula <- as.formula(paste("ocena ~", paste(pred_sets[[i]], collapse = " + ")))
      model <- lm(formula, data = df)
      metrics <- compute_model_metrics(model)
      data.frame(
        k = i,
        predictors = paste(pred_sets[[i]], collapse = " + "),
        r_squared = metrics$r_squared,
        adj_r_squared = metrics$adj_r_squared,
        aic = metrics$aic,
        bic = metrics$bic,
        rmse = metrics$rmse
      )
    })

    ch2_step_data(do.call(rbind, results))
  })

  output$ch2_step_plot <- renderPlot({
    df <- ch2_step_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Buduj modele'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
    } else {
      long <- df %>%
        select(k, r_squared, adj_r_squared) %>%
        tidyr::pivot_longer(cols = c(r_squared, adj_r_squared),
                            names_to = "metric", values_to = "value") %>%
        mutate(metric = ifelse(metric == "r_squared", "R²", "adj. R²"))

      ggplot(long, aes(x = k, y = value, color = metric)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = 1:4,
                           labels = paste0(1:4, " pred.")) +
        scale_color_manual(values = c(col_fit, col_predict), name = NULL) +
        labs(title = "R² vs adj. R² w funkcji liczby predyktorów",
             x = "Liczba predyktorów", y = "Wartość") +
        theme_educational() +
        theme(legend.position = "top")
    }
  })

  output$ch2_step_table <- renderUI({
    df <- ch2_step_data()
    if (is.null(df)) return(NULL)

    rows <- lapply(1:nrow(df), function(i) {
      tags$tr(
        tags$td(df$predictors[i]),
        tags$td(round(df$r_squared[i], 3)),
        tags$td(round(df$adj_r_squared[i], 3)),
        tags$td(round(df$aic[i], 1)),
        tags$td(round(df$bic[i], 1)),
        tags$td(round(df$rmse[i], 3))
      )
    })

    tags$table(class = "table table-bordered table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Predyktory"), tags$th("R²"), tags$th("adj.R²"),
                tags$th("AIC"), tags$th("BIC"), tags$th("RMSE"))
      ),
      tags$tbody(rows)
    )
  })
}
