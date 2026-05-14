# ============================================================================
# CHAPTER 3: Regresja wieloraka
# ============================================================================

ch3_ui <- list(
  id    = "ch-wieloraka",
  num   = "03",
  title = "Regresja wieloraka",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 03 · Regresja",
      num   = "03",
      title  = "Regresja wieloraka.",
      lead   = "Regresja prosta używała jednego predyktora.
                W rzeczywistości na Y wpływa wiele czynników jednocześnie."
    ),

    lc_h2("ch3-wiele-predyktorow", "Wiele predyktorów naraz"),

    tagList(
      p("Regresja wieloraka rozszerza model o ", tags$b("k predyktorów"), ":"),
      lc_formula_box(
        withMathJax(helpText(
          "$$Y = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\ldots + \\beta_k X_k + \\varepsilon$$"
        ))
      ),
      p("Każde ", withMathJax("\\(\\beta_j\\)"), " mówi:
        o ile zmieni się Y, gdy ", withMathJax("\\(X_j\\)"),
        " wzrośnie o 1, ", tags$b("przy stałych pozostałych zmiennych"), ".")
    ),

    lc_h2("ch3-budowanie", "Budowanie modelu wielorakiego"),

    figure_panel(
      label = "Ryc. 2.1", title = "Predykcja średniej ocen",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Dane: 150 studentów. Zmienna zależna: średnia ocen."),
          checkboxGroupInput("ch3_predictors", "Predyktory:",
            choices = c(
              "Godziny nauki/tydz." = "godziny_nauki",
              "Frekwencja (%)"      = "frekwencja",
              "Poziom stresu (1-10)" = "stres",
              "Sen (h/dobę)"        = "sen_h"
            ),
            selected = c("godziny_nauki", "frekwencja")
          ),
          actionButton("ch3_gen", "Generuj dane i dopasuj",
                       class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          plotOutput("ch3_scatter_model_plot", height = "340px"),
          uiOutput("ch3_scatter_model_info"),
          uiOutput("ch3_model_coefs"),
          plotOutput("ch3_coef_plot", height = "250px"),
          uiOutput("ch3_model_stats")
        )
      )
    ),

    lc_h2("ch3-kontrola", "Co znaczy „przy stałych pozostałych zmiennych”?"),

    figure_panel(
      label = "Ryc. 2.1b", title = "Efekt pozorny i kontrola zmiennych",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Porównujemy model prosty i model z kontrolą drugiej zmiennej."),
          actionButton("ch3_control_new", "Generuj dane",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Kroki:"),
          actionButton("ch3_control_step1", "1. Ocena ~ frekwencja",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch3_control_step2", "2. Ocena ~ nauka",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch3_control_step3", "3. Obie zmienne naraz",
                       class = "lc-btn-outline", width = "100%")
        ),
        column(8,
          plotOutput("ch3_control_plot", height = "320px"),
          uiOutput("ch3_control_info")
        )
      )
    ),

    inline_callout(label = "Skorygowane R²", color = "wskazowka",
      "Zwykłe R² zawsze rośnie z każdym dodanym predyktorem (nawet
       bezużytecznym!). Adjusted R² koryguje ten efekt — karze
       za zbędne zmienne."
    ),

    lc_h2("ch3-krok-po-kroku", "Efekt dodawania zmiennych"),

    tagList(
      p("Zobaczmy, jak zmieniają się metryki modelu, gdy dodajemy
        kolejne predyktory. Czy każda zmienna poprawia model?")
    ),

    figure_panel(
      label = "Ryc. 2.2", title = "Krok po kroku",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Modele z 1, 2, 3 i 4 predyktorami — porównanie metryk."),
          actionButton("ch3_stepwise", "Buduj modele krok po kroku",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch3_step_plot", height = "300px"),
          uiOutput("ch3_step_table")
        )
      )
    ),

    lc_h2("ch3-wspolliniowosc", "Multikolinearność"),

    figure_panel(
      label = "Ryc. 2.3", title = "Gdy predyktory mówią prawie to samo",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch3_collin_rho", "Korelacja X₁–X₂:",
                      min = 0, max = 0.98, value = 0.8, step = 0.02),
          actionButton("ch3_collin_new", "Generuj i dopasuj",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch3_collin_plot", height = "300px"),
          uiOutput("ch3_collin_info")
        )
      )
    ),

    inline_callout(label = "Ostrożnie!", color = "uwaga",
      "Więcej zmiennych = większe R², ale nie zawsze lepszy model.
       Przeuczone modele słabo generalizują. Używaj adj. R², AIC, BIC."
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

  ch3_data <- reactiveVal(NULL)
  ch3_model <- reactiveVal(NULL)

  ch3_labels_pl <- c(
    "godziny_nauki" = "Godziny nauki",
    "frekwencja" = "Frekwencja",
    "stres" = "Stres",
    "sen_h" = "Sen (h)"
  )

  ch3_make_three_groups <- function(x) {
    breaks <- unique(stats::quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE))
    if (length(breaks) < 4) {
      breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 4)
    }
    cut(x, breaks = breaks, include.lowest = TRUE,
        labels = c("niski", "średni", "wysoki"))
  }

  observeEvent(input$ch3_gen, {
    df <- generate_multi_data(150)
    ch3_data(df)

    preds <- input$ch3_predictors
    if (length(preds) == 0) preds <- "godziny_nauki"

    formula <- as.formula(paste("ocena ~", paste(preds, collapse = " + ")))
    model <- lm(formula, data = df)
    ch3_model(model)
  })

  output$ch3_scatter_model_plot <- renderPlot({
    df <- ch3_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj dane i dopasuj'",
                 size = 5.5, color = upwr_reference) +
        theme_void()
    } else {
      preds <- input$ch3_predictors
      if (length(preds) == 0) preds <- "godziny_nauki"

      x_var <- preds[1]
      df$kolor_pred <- if (length(preds) >= 2) ch3_make_three_groups(df[[preds[2]]]) else factor("wszyscy")
      df$facet_row <- if (length(preds) >= 3) ch3_make_three_groups(df[[preds[3]]]) else factor("wszyscy")
      df$facet_col <- if (length(preds) >= 4) ch3_make_three_groups(df[[preds[4]]]) else factor("wszyscy")

      color_title <- if (length(preds) >= 2) ch3_labels_pl[[preds[2]]] else NULL

      p <- ggplot(df, aes(x = .data[[x_var]], y = ocena, color = kolor_pred)) +
        geom_point(alpha = 0.55, size = 2) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
        scale_color_manual(
          values = if (length(preds) >= 2) {
            c("niski" = unname(upwr_cat["niebo"]),
              "średni" = unname(upwr_cat["bursztyn"]),
              "wysoki" = unname(upwr_cat["wrzos"]))
          } else {
            c("wszyscy" = upwr_secondary)
          },
          name = color_title
        ) +
        labs(x = ch3_labels_pl[[x_var]], y = "Średnia ocen") +
        theme_upwr() +
        theme(legend.position = if (length(preds) >= 2) "top" else "none")

      if (length(preds) == 3) {
        p <- p + facet_grid(rows = vars(facet_row), labeller = labeller(
          facet_row = function(x) paste(ch3_labels_pl[[preds[3]]], x)
        ))
      } else if (length(preds) >= 4) {
        p <- p + facet_grid(rows = vars(facet_row), cols = vars(facet_col),
          labeller = labeller(
            facet_row = function(x) paste(ch3_labels_pl[[preds[3]]], x),
            facet_col = function(x) paste(ch3_labels_pl[[preds[4]]], x)
          )
        )
      }

      p
    }
  })

  output$ch3_scatter_model_info <- renderUI({
    df <- ch3_data()
    if (is.null(df)) return(NULL)
    preds <- input$ch3_predictors
    if (length(preds) == 0) preds <- "godziny_nauki"
    x_var <- ch3_labels_pl[[preds[1]]]
    layers <- c(paste("oś X:", x_var))
    if (length(preds) >= 2) layers <- c(layers, paste("kolor:", ch3_labels_pl[[preds[2]]], "w 3 grupach"))
    if (length(preds) >= 3) layers <- c(layers, paste("wiersze:", ch3_labels_pl[[preds[3]]], "w 3 grupach"))
    if (length(preds) >= 4) layers <- c(layers, paste("kolumny:", ch3_labels_pl[[preds[4]]], "w 3 grupach"))

    lc_feedback(type = "info",
      p(tags$strong("Wizualizacja modelu: "), paste(layers, collapse = "; "),
        ". Linie są poglądowymi prostymi na przekrojach danych; tabela niżej pokazuje współczynniki pełnego modelu.")
    )
  })

  output$ch3_model_coefs <- renderUI({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model)

    labels_pl <- c("(Intercept)" = "Wyraz wolny", ch3_labels_pl)

    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)

    rows <- lapply(1:nrow(coefs), function(i) {
      sig <- if (coefs$p.value[i] < 0.05) " *" else ""
      tags$tr(
        tags$td(coefs$term_pl[i]),
        tags$td(round(coefs$estimate[i], 4)),
        tags$td(round(coefs$std.error[i], 4)),
        tags$td(round(coefs$statistic[i], 3)),
        tags$td(paste0(format_p_value(coefs$p.value[i]), sig))
      )
    })

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 14px;",
      tags$thead(
        tags$tr(tags$th("Zmienna"), tags$th("Estymata"), tags$th("SE"),
                tags$th("t"), tags$th("p"))
      ),
      tags$tbody(rows)
    )
  })

  output$ch3_coef_plot <- renderPlot({
    model <- ch3_model()
    if (is.null(model)) return(NULL)

    coefs <- broom::tidy(model, conf.int = TRUE)
    coefs <- coefs[coefs$term != "(Intercept)", ]

    if (nrow(coefs) == 0) return(NULL)

    labels_pl <- ch3_labels_pl
    coefs$term_pl <- ifelse(coefs$term %in% names(labels_pl),
                             labels_pl[coefs$term], coefs$term)
    coefs$significant <- coefs$p.value < 0.05

    ggplot(coefs, aes(x = estimate, y = term_pl, color = significant)) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = upwr_secondary) +
      scale_color_manual(values = c("TRUE" = unname(upwr_cat["niebo"]), "FALSE" = unname(upwr_cat["terakota"])),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                         name = NULL) +
      labs(
           x = "Estymata β", y = NULL) +
      theme_upwr() +
      theme(legend.position = "top")
  })

  output$ch3_model_stats <- renderUI({
    model <- ch3_model()
    if (is.null(model)) return(NULL)
    metrics <- compute_model_metrics(model)
    tagList(
      lc_stat_box("R²", round(metrics$r_squared, 3), color = unname(upwr_cat["niebo"])),
      lc_stat_box("adj.R²", round(metrics$adj_r_squared, 3), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("AIC", round(metrics$aic, 1), color = unname(upwr_cat["bursztyn"])),
      lc_stat_box("RMSE", round(metrics$rmse, 3), color = unname(upwr_cat["terakota"]))
    )
  })

  # --- Widget: kontrola zmiennych ---
  ch3_control_data <- reactiveVal(NULL)
  ch3_control_step <- reactiveVal(0)

  observeEvent(input$ch3_control_new, {
    ch3_control_data(generate_confounding_data(160))
    ch3_control_step(0)
  })
  observeEvent(input$ch3_control_step1, ch3_control_step(1))
  observeEvent(input$ch3_control_step2, ch3_control_step(2))
  observeEvent(input$ch3_control_step3, ch3_control_step(3))

  output$ch3_control_plot <- renderPlot({
    df <- ch3_control_data()
    step <- ch3_control_step()
    if (is.null(df) || step == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj dane', potem kroki",
                 size = 5.5, color = upwr_reference) +
        theme_void()
    } else if (step == 1) {
      ggplot(df, aes(x = frekwencja, y = ocena)) +
        geom_point(color = upwr_secondary, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"])) +
        labs(x = "Frekwencja (%)", y = "Ocena") +
        theme_upwr()
    } else if (step == 2) {
      ggplot(df, aes(x = godziny_nauki, y = ocena)) +
        geom_point(color = upwr_secondary, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["szalwia"])) +
        labs(x = "Godziny nauki", y = "Ocena") +
        theme_upwr()
    } else {
      model <- lm(ocena ~ godziny_nauki + frekwencja, data = df)
      coefs <- broom::tidy(model)
      coefs <- coefs[coefs$term != "(Intercept)", ]
      labels <- c(godziny_nauki = "Godziny nauki", frekwencja = "Frekwencja")
      coefs$term <- labels[coefs$term]
      ggplot(coefs, aes(x = estimate, y = term)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = upwr_secondary) +
        geom_point(color = unname(upwr_cat["wrzos"]), size = 3) +
        geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                           xmax = estimate + 1.96 * std.error),
                       height = 0.2, color = unname(upwr_cat["wrzos"])) +
        labs(x = "β w modelu wielorakim", y = NULL) +
        theme_upwr()
    }
  })

  output$ch3_control_info <- renderUI({
    df <- ch3_control_data()
    step <- ch3_control_step()
    if (is.null(df) || step == 0) return(NULL)
    m_freq <- lm(ocena ~ frekwencja, data = df)
    m_study <- lm(ocena ~ godziny_nauki, data = df)
    m_both <- lm(ocena ~ godziny_nauki + frekwencja, data = df)
    tf <- broom::tidy(m_freq)
    ts <- broom::tidy(m_study)
    tb <- broom::tidy(m_both)
    if (step == 1) {
      tagList(
        lc_stat_box("β frekw.", round(tf$estimate[2], 3), color = unname(upwr_cat["niebo"])),
        lc_stat_box("p", format_p_value(tf$p.value[2]), color = upwr_secondary),
        lc_feedback(type = "info", p("W modelu prostym frekwencja wygląda na ważną, ale może nieść informację o przygotowaniu studenta."))
      )
    } else if (step == 2) {
      tagList(
        lc_stat_box("β nauka", round(ts$estimate[2], 3), color = unname(upwr_cat["szalwia"])),
        lc_stat_box("p", format_p_value(ts$p.value[2]), color = upwr_secondary),
        lc_feedback(type = "info", p("Godziny nauki też są powiązane z oceną. Teraz sprawdzimy, co zostaje po kontroli obu naraz."))
      )
    } else {
      rows <- lapply(2:nrow(tb), function(i) {
        tags$tr(tags$td(tb$term[i]), tags$td(round(tb$estimate[i], 4)),
                tags$td(round(tb$std.error[i], 4)), tags$td(format_p_value(tb$p.value[i])))
      })
      tagList(
        tags$table(class = "lc-table lc-table-bordered lc-table-striped",
          tags$thead(tags$tr(tags$th("Zmienna"), tags$th("β"), tags$th("SE"), tags$th("p"))),
          tags$tbody(rows)
        ),
        lc_feedback(type = "warning", p("Współczynnik oznacza efekt danej zmiennej po odjęciu informacji wspólnej z pozostałymi predyktorami."))
      )
    }
  })

  # --- Widget 2: Krok po kroku ---
  ch3_step_data <- reactiveVal(NULL)

  observeEvent(input$ch3_stepwise, {
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

    ch3_step_data(do.call(rbind, results))
  })

  output$ch3_step_plot <- renderPlot({
    df <- ch3_step_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Buduj modele'",
                 size = 6, color = upwr_reference) +
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
        scale_color_manual(values = c(unname(upwr_cat["niebo"]), unname(upwr_cat["szalwia"])), name = NULL) +
        labs(
             x = "Liczba predyktorów", y = "Wartość") +
        theme_upwr() +
        theme(legend.position = "top")
    }
  })

  output$ch3_step_table <- renderUI({
    df <- ch3_step_data()
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

    tags$table(class = "lc-table lc-table-bordered lc-table-striped",
      style = "font-size: 13px;",
      tags$thead(
        tags$tr(tags$th("Predyktory"), tags$th("R²"), tags$th("adj.R²"),
                tags$th("AIC"), tags$th("BIC"), tags$th("RMSE"))
      ),
      tags$tbody(rows)
    )
  })

  # --- Widget: multikolinearnosc ---
  ch3_collin_data <- reactiveVal(NULL)

  observeEvent(input$ch3_collin_new, {
    ch3_collin_data(generate_collinearity_data(140, input$ch3_collin_rho))
  })

  output$ch3_collin_plot <- renderPlot({
    df <- ch3_collin_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i dopasuj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ggplot(df, aes(x = x1, y = x2)) +
        geom_point(color = upwr_secondary, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"])) +
        labs(x = "X₁", y = "X₂") +
        theme_upwr()
    }
  })

  output$ch3_collin_info <- renderUI({
    df <- ch3_collin_data()
    if (is.null(df)) return(NULL)
    model <- lm(y ~ x1 + x2, data = df)
    coefs <- broom::tidy(model)
    vifs <- compute_vif_simple(df, c("x1", "x2"))
    rows <- lapply(2:nrow(coefs), function(i) {
      term <- coefs$term[i]
      tags$tr(
        tags$td(term),
        tags$td(round(coefs$estimate[i], 3)),
        tags$td(round(coefs$std.error[i], 3)),
        tags$td(format_p_value(coefs$p.value[i])),
        tags$td(round(vifs[[term]], 2))
      )
    })
    tagList(
      lc_stat_box("corr(X₁,X₂)", round(cor(df$x1, df$x2), 2), color = unname(upwr_cat["niebo"])),
      lc_stat_box("R² modelu", round(summary(model)$r.squared, 3), color = unname(upwr_cat["szalwia"])),
      tags$table(class = "lc-table lc-table-bordered lc-table-striped",
        style = "font-size: 13px;",
        tags$thead(tags$tr(tags$th("Zmienna"), tags$th("β"), tags$th("SE"), tags$th("p"), tags$th("VIF"))),
        tags$tbody(rows)
      ),
      lc_feedback(type = "warning",
        p("Im bardziej X₁ i X₂ są podobne, tym trudniej modelowi stabilnie przypisać osobny efekt każdej zmiennej."))
    )
  })
}
