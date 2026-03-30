# ============================================================================
# CHAPTER 2: Regresja wieloraka
# ============================================================================

ch2_ui <- tabPanel("2. Regresja wieloraka",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Regresja prosta u\u017cywa\u0142a jednego predyktora.
       W rzeczywisto\u015bci na Y wp\u0142ywa wiele czynnik\u00f3w jednocze\u015bnie."
    ),

    div(class = "section-title", "Wiele predyktor\u00f3w naraz"),

    div(class = "narrative",
      p("Regresja wieloraka rozszerza model o ", tags$b("k predyktor\u00f3w"), ":"),
      div(class = "formula-box",
        withMathJax(helpText(
          "$$Y = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\ldots + \\beta_k X_k + \\varepsilon$$"
        ))
      ),
      p("Ka\u017cde ", withMathJax("\\(\\beta_j\\)"), " m\u00f3wi:
        o ile zmieni si\u0119 Y, gdy ", withMathJax("\\(X_j\\)"),
        " wzro\u015bnie o 1, ", tags$b("przy sta\u0142ych pozosta\u0142ych zmiennych"), ".")
    ),

    # ========================================================================
    # WIDGET 1: Regresja wieloraka
    # ========================================================================
    div(class = "section-title", "Budowanie modelu wielorakiego"),

    div(class = "widget-block",
      h4("Predykcja \u015bredniej ocen"),
      fluidRow(
        column(4,
          helpText("Dane: 150 student\u00f3w. Zmienna zale\u017cna: \u015brednia ocen."),
          checkboxGroupInput("ch2_predictors", "Predyktory:",
            choices = c(
              "Godziny nauki/tydz." = "godziny_nauki",
              "Frekwencja (%)" = "frekwencja",
              "Poziom stresu (1-10)" = "stres",
              "Sen (h/dob\u0119)" = "sen_h"
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
      tags$strong("Skorygowane R\u00b2:"),
      " Zwyk\u0142e R\u00b2 zawsze ro\u015bnie z ka\u017cdym dodanym predyktorem (nawet bezu\u017cytecznym!).
        Adjusted R\u00b2 koryguje ten efekt \u2014 karze za zb\u0119dne zmienne."
    ),

    # ========================================================================
    # WIDGET 2: Dodawanie zmiennych krok po kroku
    # ========================================================================
    div(class = "section-title", "Efekt dodawania zmiennych"),

    div(class = "narrative",
      p("Zobaczmy, jak zmieniaj\u0105 si\u0119 metryki modelu, gdy dodajemy
        kolejne predyktory. Czy ka\u017cda zmienna poprawia model?")
    ),

    div(class = "widget-block",
      h4("Krok po kroku"),
      fluidRow(
        column(4,
          helpText("Modele z 1, 2, 3 i 4 predyktorami \u2014 por\u00f3wnanie metryk."),
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
      tags$strong("Ostro\u017cnie z dodawaniem zmiennych!"),
      " Wi\u0119cej zmiennych = wi\u0119ksze R\u00b2, ale nie zawsze lepszy model.
        Przeuczone modele s\u0142abo generalizuj\u0105. U\u017cywaj adj. R\u00b2, AIC, BIC."
    ),

    # Chapter transition
    div(class = "chapter-transition",
      p("Dalej: gdy zmienna zale\u017cna jest binarna"),
      actionButton("ch2_next", "Dalej \u2192 3. Regresja logistyczna",
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
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
                         name = NULL) +
      labs(title = "Wsp\u00f3\u0142czynniki regresji z 95% CI",
           x = "Estymata \u03b2", y = NULL) +
      theme_reg() +
      theme(legend.position = "top")
  })

  output$ch2_model_stats <- renderUI({
    model <- ch2_model()
    if (is.null(model)) return(NULL)
    metrics <- compute_model_metrics(model)
    tagList(
      div(class = "stat-box", style = paste0("background:", col_fit, ";"),
          paste0("R\u00b2 = ", round(metrics$r_squared, 3))),
      div(class = "stat-box", style = paste0("background:", col_predict, ";"),
          paste0("adj.R\u00b2 = ", round(metrics$adj_r_squared, 3))),
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
        mutate(metric = ifelse(metric == "r_squared", "R\u00b2", "adj. R\u00b2"))

      ggplot(long, aes(x = k, y = value, color = metric)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = 1:4,
                           labels = paste0(1:4, " pred.")) +
        scale_color_manual(values = c(col_fit, col_predict), name = NULL) +
        labs(title = "R\u00b2 vs adj. R\u00b2 w funkcji liczby predyktor\u00f3w",
             x = "Liczba predyktor\u00f3w", y = "Warto\u015b\u0107") +
        theme_reg() +
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
        tags$tr(tags$th("Predyktory"), tags$th("R\u00b2"), tags$th("adj.R\u00b2"),
                tags$th("AIC"), tags$th("BIC"), tags$th("RMSE"))
      ),
      tags$tbody(rows)
    )
  })
}
