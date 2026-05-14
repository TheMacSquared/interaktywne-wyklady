# ============================================================================
# CHAPTER 5: Regresja logistyczna
# ============================================================================

ch5_ui <- list(
  id    = "ch-logistyczna",
  num   = "05",
  title = "Regresja logistyczna",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Regresja",
      num   = "05",
      title  = "Regresja logistyczna.",
      lead   = "Regresja liniowa wymaga ciągłej zmiennej zależnej.
                A co, gdy Y to 0 lub 1 (sukces/porażka)?"
    ),

    tagList(
      p("W rozdziale 4 spotkaliśmy się z porównywaniem modeli liniowych —
        ale wszystkie zakładały, że Y jest ciągłe. Co, gdy Y to ",
        tags$em("zdał albo nie zdał"), "? Albo ",
        tags$em("kliknął albo nie kliknął"), "? Albo ",
        tags$em("przeżył albo nie przeżył"), "? Wtedy regresja liniowa nie
        odmawia odpowiedzi — ale ta odpowiedź jest bezsensowna."),
      p("Zaraz zobaczymy, dlaczego — a potem poznamy alternatywę.")
    ),

    lc_h2("ch5-dlaczego", "Dlaczego nie regresja liniowa?"),

    tagList(
      p("Zacznijmy od eksperymentu: nałóżmy zwykłą regresję liniową na
        dane binarne (zdał = 1, nie zdał = 0) i zobaczmy, co się stanie.")
    ),

    figure_panel(
      label = "Ryc. 5.1", title = "Liniowy vs logistyczny na danych binarnych",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Symulujemy dane studentów: czy zdają egzamin w zależności od godzin nauki?"),
          actionButton("ch5_lin_vs_log", "Generuj porównanie",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch5_lin_log_plot", height = "300px"),
          uiOutput("ch5_lin_log_stats")
        )
      )
    ),

    inline_callout(label = "Wniosek", color = "uwaga",
      "Linia liniowa wychodzi poza [0, 1] — przy małej liczbie godzin daje
       prawdopodobieństwa ujemne, przy dużej powyżej 100%. To nie są
       prawdopodobieństwa, to absurd. Potrzebujemy modelu, który z definicji
       zwraca wartości w przedziale [0, 1]."
    ),

    tagList(
      p("Rozwiązaniem jest ", tags$strong("regresja logistyczna"),
        ". Zamiast modelować Y bezpośrednio, modelujemy ",
        tags$strong("prawdopodobieństwo"), " sukcesu:"),
      lc_formula_box(
        withMathJax(helpText(
          "$$P(Y=1) = \\frac{1}{1 + e^{-(\\beta_0 + \\beta_1 X_1 + \\ldots + \\beta_k X_k)}}$$"
        )),
        p("Funkcja logistyczna (sigmoida) zamyka wynik w [0, 1] —
          niezależnie od tego, jak duże albo małe są X-y.")
      )
    ),

    lc_h2("ch5-krzywa", "Krzywa logistyczna"),

    figure_panel(
      label = "Ryc. 3.1", title = "Sigmoida w akcji",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch5_b0", "β₀ (intercept):",
                      min = -10, max = 10, value = -4, step = 0.5),
          sliderInput("ch5_b1", "β₁ (slope):",
                      min = -3, max = 3, value = 0.2, step = 0.05),
          hr(),
          div(class = "preset-buttons",
            actionButton("ch5_preset_steep", "Stromy",
                         class = "lc-btn-outline"),
            actionButton("ch5_preset_flat", "Płaski",
                         class = "lc-btn-secondary-outline"),
            actionButton("ch5_preset_neg", "Odwrotny",
                         class = "lc-btn-danger-outline")
          )
        ),
        column(8,
          plotOutput("ch5_sigmoid_plot", height = "350px")
        )
      )
    ),

    figure_panel(
      label = "Ryc. 3.1b", title = "Logit → prawdopodobieństwo → decyzja",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch5_logit_x", "X (np. godziny nauki):",
                      min = 0, max = 40, value = 20, step = 1),
          sliderInput("ch5_logit_b0", "β₀:",
                      min = -8, max = 4, value = -4, step = 0.5),
          sliderInput("ch5_logit_b1", "β₁:",
                      min = -0.2, max = 0.5, value = 0.16, step = 0.02),
          sliderInput("ch5_logit_threshold", "Próg decyzji:",
                      min = 0.1, max = 0.9, value = 0.5, step = 0.05),
          hr(),
          h5("Kroki:"),
          actionButton("ch5_logit_step1", "1. Policz η",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch5_logit_step2", "2. Zamień na p",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch5_logit_step3", "3. Podejmij decyzję",
                       class = "lc-btn-outline", width = "100%")
        ),
        column(8,
          plotOutput("ch5_logit_step_plot", height = "320px"),
          uiOutput("ch5_logit_step_info")
        )
      )
    ),

    lc_h2("ch5-model-dane", "Model logistyczny na danych"),

    tagList(
      p("Scenariusz: czy student zda egzamin? Predyktory: godziny nauki
        i średnia ocen.")
    ),

    figure_panel(
      label = "Ryc. 3.2", title = "Predykcja zdania egzaminu",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch5_n", "n:", min = 50, max = 300, value = 150, step = 25),
          selectInput("ch5_predictor", "Prezentowany predyktor:",
            choices = c(
              "Godziny nauki" = "godziny_nauki",
              "Średnia ocen"  = "srednia_ocen"
            ),
            selected = "godziny_nauki"
          ),
          actionButton("ch5_fit", "Dopasuj model",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Predykcja dla nowego studenta:"),
          numericInput("ch5_pred_hours", "Godziny nauki:", value = 20, min = 0, max = 40),
          numericInput("ch5_pred_gpa", "Średnia ocen:", value = 3.5, min = 2, max = 5, step = 0.1),
          uiOutput("ch5_prediction")
        ),
        column(8,
          plotOutput("ch5_logit_plot", height = "350px"),
          uiOutput("ch5_model_summary")
        )
      )
    ),

    lc_h2("ch5-iloraz-szans", "Interpretacja: iloraz szans"),

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
      uiOutput("ch5_odds_ratios")
    ),

    figure_panel(
      label = "Ryc. 3.4", title = "Próg klasyfikacji i macierz pomyłek",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Używa modelu dopasowanego w Ryc. 3.2."),
          sliderInput("ch5_threshold", "Próg decyzji:",
                      min = 0.1, max = 0.9, value = 0.5, step = 0.05)
        ),
        column(8,
          plotOutput("ch5_threshold_plot", height = "280px"),
          uiOutput("ch5_threshold_info")
        )
      )
    ),

    inline_callout(label = "Ocena modelu", color = "wskazowka",
      "Nie używamy R² w sensie liniowym. Zamiast tego: AIC, BIC, oraz
       macierz pomyłek (confusion matrix) z dokładnością, czułością
       i swoistością."
    ),

    lc_chapter_next(
      num       = "06",
      title     = "Ściąga",
      lead      = "podsumowanie wzorów i zasad",
      target_id = "ch-sciaga"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  # --- Widget: Liniowy vs logistyczny (przeniesiony z ch4) ---
  ch5_lin_log_data <- reactiveVal(NULL)

  observeEvent(input$ch5_lin_vs_log, {
    ch5_lin_log_data(generate_logistic_data(200))
  })

  output$ch5_lin_log_plot <- renderPlot({
    df <- ch5_lin_log_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      ggplot(df, aes(x = godziny_nauki, y = zdal_num)) +
        geom_jitter(height = 0.03, alpha = 0.3, color = upwr_secondary) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"]),
                    linewidth = 1, linetype = "dashed") +
        geom_smooth(method = "glm", method.args = list(family = "binomial"),
                    se = FALSE, color = unname(upwr_cat["wrzos"]), linewidth = 1.2) +
        geom_hline(yintercept = c(0, 1), linetype = "dotted", color = upwr_rule) +
        annotate("text", x = 5, y = 0.85, label = "Logistyczny", color = unname(upwr_cat["wrzos"]),
                 fontface = "bold") +
        annotate("text", x = 35, y = 0.85, label = "Liniowy", color = unname(upwr_cat["niebo"]),
                 fontface = "bold") +
        labs(
             x = "Godziny nauki", y = "P(zdanie)") +
        ylim(-0.2, 1.2) +
        theme_upwr()
    }
  })

  output$ch5_lin_log_stats <- renderUI({
    df <- ch5_lin_log_data()
    if (is.null(df)) return(NULL)

    lin <- lm(zdal_num ~ godziny_nauki, data = df)
    log <- glm(zdal_num ~ godziny_nauki, data = df, family = binomial)

    lin_pred <- ifelse(fitted(lin) >= 0.5, 1, 0)
    log_pred <- ifelse(fitted(log) >= 0.5, 1, 0)
    acc_lin <- mean(lin_pred == df$zdal_num) * 100
    acc_log <- mean(log_pred == df$zdal_num) * 100

    outside <- mean(fitted(lin) < 0 | fitted(lin) > 1) * 100

    tagList(
      lc_stat_box("Liniowy", round(acc_lin, 1), "%", color = unname(upwr_cat["niebo"])),
      lc_stat_box("Logistyczny", round(acc_log, 1), "%", color = unname(upwr_cat["wrzos"])),
      lc_stat_box("Liniowy poza [0,1]", round(outside, 1), "%", color = unname(upwr_cat["terakota"]))
    )
  })

  # --- Widget 1: Sigmoida ---
  observeEvent(input$ch5_preset_steep, {
    updateSliderInput(session, "ch5_b0", value = -5)
    updateSliderInput(session, "ch5_b1", value = 0.5)
  })
  observeEvent(input$ch5_preset_flat, {
    updateSliderInput(session, "ch5_b0", value = -1)
    updateSliderInput(session, "ch5_b1", value = 0.05)
  })
  observeEvent(input$ch5_preset_neg, {
    updateSliderInput(session, "ch5_b0", value = 5)
    updateSliderInput(session, "ch5_b1", value = -0.3)
  })

  output$ch5_sigmoid_plot <- renderPlot({
    b0 <- input$ch5_b0
    b1 <- input$ch5_b1
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

  # --- Widget: logit krok po kroku ---
  ch5_logit_step <- reactiveVal(0)
  observeEvent(input$ch5_logit_step1, ch5_logit_step(1))
  observeEvent(input$ch5_logit_step2, ch5_logit_step(2))
  observeEvent(input$ch5_logit_step3, ch5_logit_step(3))

  output$ch5_logit_step_plot <- renderPlot({
    x <- seq(0, 40, length.out = 300)
    eta <- input$ch5_logit_b0 + input$ch5_logit_b1 * x
    p <- 1 / (1 + exp(-eta))
    x0 <- input$ch5_logit_x
    eta0 <- input$ch5_logit_b0 + input$ch5_logit_b1 * x0
    p0 <- 1 / (1 + exp(-eta0))
    step <- ch5_logit_step()

    df <- data.frame(x = x, p = p)
    plot <- ggplot(df, aes(x = x, y = p)) +
      geom_line(color = unname(upwr_cat["wrzos"]), linewidth = 1.4) +
      geom_hline(yintercept = input$ch5_logit_threshold, linetype = "dashed",
                 color = unname(upwr_cat["bursztyn"])) +
      labs(x = "X", y = "P(Y = 1)") +
      ylim(0, 1) +
      theme_upwr()
    if (step >= 2) {
      plot <- plot +
        geom_segment(aes(x = x0, xend = x0, y = 0, yend = p0),
                     color = upwr_secondary, linetype = "dotted") +
        geom_point(data = data.frame(x = x0, p = p0), aes(x = x, y = p),
                   color = upwr_secondary, size = 3)
    }
    plot
  })

  output$ch5_logit_step_info <- renderUI({
    step <- ch5_logit_step()
    if (step == 0) return(NULL)
    eta <- input$ch5_logit_b0 + input$ch5_logit_b1 * input$ch5_logit_x
    prob <- 1 / (1 + exp(-eta))
    decision <- if (prob >= input$ch5_logit_threshold) "klasa 1" else "klasa 0"
    tagList(
      if (step >= 1) lc_stat_box("η", round(eta, 3), caption = "β₀ + β₁X", color = unname(upwr_cat["niebo"])),
      if (step >= 2) lc_stat_box("p", round(prob, 3), caption = "sigmoid(η)", color = unname(upwr_cat["wrzos"])),
      if (step >= 3) lc_stat_box("Decyzja", decision, caption = paste("próg", input$ch5_logit_threshold), color = if (prob >= input$ch5_logit_threshold) unname(upwr_cat["szalwia"]) else unname(upwr_cat["terakota"])),
      if (step >= 3) lc_feedback(type = "info", p("Regresja logistyczna najpierw zwraca prawdopodobieństwo. Klasa 0/1 pojawia się dopiero po wybraniu progu."))
    )
  })

  # --- Widget 2: Model logistyczny ---
  ch5_data <- reactiveVal(NULL)
  ch5_model <- reactiveVal(NULL)

  observeEvent(input$ch5_fit, {
    df <- generate_logistic_data(input$ch5_n)
    ch5_data(df)
    model <- glm(zdal_num ~ godziny_nauki + srednia_ocen,
                 data = df, family = binomial)
    ch5_model(model)
  })

  output$ch5_logit_plot <- renderPlot({
    df <- ch5_data()
    model <- ch5_model()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Dopasuj model'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      pred_var <- input$ch5_predictor
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

  output$ch5_model_summary <- renderUI({
    model <- ch5_model()
    if (is.null(model)) return(NULL)

    g <- broom::glance(model)
    coefs <- broom::tidy(model)

    # Confusion matrix
    df <- ch5_data()
    pred_class <- ifelse(predict(model, type = "response") >= 0.5, 1, 0)
    accuracy <- mean(pred_class == df$zdal_num) * 100

    tagList(
      lc_stat_box("AIC", round(g$AIC, 1), color = unname(upwr_cat["wrzos"])),
      lc_stat_box("BIC", round(g$BIC, 1), color = upwr_secondary),
      lc_stat_box("Dokładność", round(accuracy, 1), "%", color = unname(upwr_cat["szalwia"]))
    )
  })

  output$ch5_prediction <- renderUI({
    model <- ch5_model()
    if (is.null(model)) return(NULL)

    newdata <- data.frame(
      godziny_nauki = input$ch5_pred_hours,
      srednia_ocen = input$ch5_pred_gpa
    )
    prob <- predict(model, newdata, type = "response")

    color <- if (prob >= 0.5) unname(upwr_cat["szalwia"]) else unname(upwr_cat["terakota"])
    decision <- if (prob >= 0.5) "Prawdopodobnie zda" else "Raczej nie zda"

    lc_stat_box("P(zdanie)", round(prob, 3), caption = decision, color = color)
  })

  # --- Widget 3: Odds ratios ---
  output$ch5_odds_ratios <- renderUI({
    model <- ch5_model()
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
        tags$td(format_p_value(coefs$p.value[i]))
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

  # --- Widget: prog klasyfikacji ---
  output$ch5_threshold_plot <- renderPlot({
    model <- ch5_model()
    df <- ch5_data()
    if (is.null(model) || is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Najpierw dopasuj model w Ryc. 3.2",
                 size = 5.5, color = upwr_reference) +
        theme_void()
    } else {
      probs <- predict(model, type = "response")
      pred <- ifelse(probs >= input$ch5_threshold, 1, 0)
      cm <- as.data.frame(table(
        Rzeczywiste = factor(df$zdal_num, levels = c(0, 1), labels = c("Nie", "Tak")),
        Predykcja = factor(pred, levels = c(0, 1), labels = c("Nie", "Tak"))
      ))
      ggplot(cm, aes(x = Predykcja, y = Rzeczywiste, fill = Freq)) +
        geom_tile(color = "white", linewidth = 1) +
        geom_text(aes(label = Freq), size = 7, fontface = "bold", color = "white") +
        scale_fill_gradient(low = unname(upwr_cat["niebo"]), high = upwr_secondary) +
        labs(x = "Predykcja modelu", y = "Rzeczywistość") +
        theme_upwr() +
        theme(legend.position = "none")
    }
  })

  output$ch5_threshold_info <- renderUI({
    model <- ch5_model()
    df <- ch5_data()
    if (is.null(model) || is.null(df)) return(NULL)
    probs <- predict(model, type = "response")
    pred <- ifelse(probs >= input$ch5_threshold, 1, 0)
    tp <- sum(pred == 1 & df$zdal_num == 1)
    tn <- sum(pred == 0 & df$zdal_num == 0)
    fp <- sum(pred == 1 & df$zdal_num == 0)
    fn <- sum(pred == 0 & df$zdal_num == 1)
    accuracy <- (tp + tn) / length(pred)
    sensitivity <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
    specificity <- ifelse(tn + fp == 0, NA, tn / (tn + fp))
    tagList(
      lc_stat_box("Accuracy", paste0(round(accuracy * 100, 1), "%"), color = unname(upwr_cat["szalwia"])),
      lc_stat_box("Czułość", paste0(round(sensitivity * 100, 1), "%"), caption = "wykrywa Tak", color = unname(upwr_cat["niebo"])),
      lc_stat_box("Swoistość", paste0(round(specificity * 100, 1), "%"), caption = "wykrywa Nie", color = unname(upwr_cat["bursztyn"])),
      lc_feedback(type = "warning", p("Obniżenie progu zwykle zwiększa czułość, ale może obniżyć swoistość."))
    )
  })
}
