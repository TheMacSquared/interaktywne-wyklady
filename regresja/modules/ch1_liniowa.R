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

    figure_panel(
      label = "Ryc. 1.0", title = "Co robią β₀, β₁ i szum?",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch1_beta_b0", "β₀ (punkt startu):",
                      min = -10, max = 20, value = 5, step = 1),
          sliderInput("ch1_beta_b1", "β₁ (nachylenie):",
                      min = -3, max = 3, value = 1, step = 0.25),
          sliderInput("ch1_beta_sigma", "Szum σ:",
                      min = 0, max = 8, value = 2, step = 0.5)
        ),
        column(8,
          plotOutput("ch1_beta_plot", height = "320px"),
          uiOutput("ch1_beta_info")
        )
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

    lc_h2("ch1-ols-krok", "Najmniejsze kwadraty — krok po kroku"),

    figure_panel(
      label = "Ryc. 1.1b", title = "Jak linia staje się modelem",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Ta sama próba, kolejne warstwy interpretacji."),
          actionButton("ch1_ols_new", "Nowa próba",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Kroki:"),
          actionButton("ch1_ols_step1", "1. Dane",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_ols_step2", "2. Średnia Y",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_ols_step3", "3. Linia regresji",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_ols_step4", "4. Reszty",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_ols_step5", "5. Wynik modelu",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_ols_step6", "6. Inna prosta?",
                       class = "lc-btn-outline", width = "100%")
        ),
        column(8,
          plotOutput("ch1_ols_plot", height = "360px"),
          uiOutput("ch1_ols_info")
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

    figure_panel(
      label = "Ryc. 1.4", title = "R² jako SST, SSE i SSR",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Rozbijamy zmienność Y na część wyjaśnioną i niewyjaśnioną."),
          actionButton("ch1_r2_parts_new", "Nowe dane",
                       class = "lc-btn-primary", width = "100%"),
          hr(),
          h5("Warstwa:"),
          actionButton("ch1_r2_part1", "1. Całkowita zmienność",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_r2_part2", "2. Model i reszty",
                       class = "lc-btn-outline", width = "100%"),
          actionButton("ch1_r2_part3", "3. R²",
                       class = "lc-btn-outline", width = "100%")
        ),
        column(8,
          plotOutput("ch1_r2_parts_plot", height = "330px"),
          uiOutput("ch1_r2_parts_info")
        )
      )
    ),

    lc_h2("ch1-zalozenia", "Diagnostyka założeń"),

    figure_panel(
      label = "Ryc. 1.5", title = "Co może pójść nie tak?",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch1_assumption_scenario", "Scenariusz:",
            choices = c(
              "Model liniowy OK" = "good",
              "Nieliniowość" = "nonlinear",
              "Niejednorodna wariancja" = "hetero",
              "Punkt odstający" = "outlier",
              "Ciężkie ogony reszt" = "nonnormal"
            )
          ),
          actionButton("ch1_assumption_new", "Generuj i diagnozuj",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch1_assumption_plot", height = "360px"),
          uiOutput("ch1_assumption_info")
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

  output$ch1_beta_plot <- renderPlot({
    set.seed(101)
    x <- seq(0, 10, length.out = 80)
    y_true <- input$ch1_beta_b0 + input$ch1_beta_b1 * x
    y <- y_true + rnorm(length(x), 0, input$ch1_beta_sigma)
    df <- data.frame(x = x, y = y, y_true = y_true)

    ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.45) +
      geom_line(aes(y = y_true), color = unname(upwr_cat["niebo"]), linewidth = 1.3) +
      geom_segment(aes(x = 0, xend = 0, y = 0, yend = input$ch1_beta_b0),
                   color = unname(upwr_cat["bursztyn"]), linewidth = 1.1) +
      annotate("text", x = 0.6, y = input$ch1_beta_b0,
               label = paste0("β₀ = ", input$ch1_beta_b0),
               hjust = 0, color = unname(upwr_cat["bursztyn"]), fontface = "bold") +
      labs(x = "X", y = "Y") +
      theme_upwr()
  })

  output$ch1_beta_info <- renderUI({
    direction <- if (input$ch1_beta_b1 > 0) "rośnie" else if (input$ch1_beta_b1 < 0) "maleje" else "nie zmienia się"
    lc_feedback(type = "info",
      p(tags$strong("Interpretacja:"),
        paste0(" gdy X wzrasta o 1, oczekiwane Y ", direction,
               " o ", abs(input$ch1_beta_b1), ". Szum σ = ",
               input$ch1_beta_sigma, " rozprasza punkty wokół linii."))
    )
  })

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

      p + labs(
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

  # --- Widget: OLS krok po kroku ---
  ch1_ols_data <- reactiveVal(generate_regression_data(n = 70, beta0 = 4, beta1 = 1.4, sigma = 4))
  ch1_ols_step <- reactiveVal(0)

  observeEvent(input$ch1_ols_new, {
    ch1_ols_data(generate_regression_data(n = 70, beta0 = 4, beta1 = 1.4, sigma = 4))
    ch1_ols_step(0)
  })
  observeEvent(input$ch1_ols_step1, ch1_ols_step(1))
  observeEvent(input$ch1_ols_step2, ch1_ols_step(2))
  observeEvent(input$ch1_ols_step3, ch1_ols_step(3))
  observeEvent(input$ch1_ols_step4, ch1_ols_step(4))
  observeEvent(input$ch1_ols_step5, ch1_ols_step(5))
  observeEvent(input$ch1_ols_step6, ch1_ols_step(6))

  output$ch1_ols_plot <- renderPlot({
    df <- ch1_ols_data()
    step <- ch1_ols_step()
    model <- lm(y ~ x, data = df)
    coefs <- coef(model)
    df$fitted <- fitted(model)
    df$resid <- residuals(model)
    mean_y <- mean(df$y)
    alt_b1 <- coefs[2] * 0.45
    alt_b0 <- mean_y - alt_b1 * mean(df$x)
    df$alt_fitted <- alt_b0 + alt_b1 * df$x

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.55, size = 2) +
      labs(x = "X", y = "Y") +
      theme_upwr()

    if (step >= 2) {
      p <- p + geom_hline(yintercept = mean_y, linetype = "dashed",
                          color = unname(upwr_cat["bursztyn"]), linewidth = 1)
    }
    if (step >= 3) {
      p <- p + geom_smooth(method = "lm", se = FALSE,
                           color = unname(upwr_cat["niebo"]), linewidth = 1.2)
    }
    if (step >= 4) {
      p <- p + geom_segment(aes(xend = x, yend = fitted),
                            color = unname(upwr_cat["terakota"]), alpha = 0.35)
    }
    if (step >= 6) {
      p <- p +
        geom_abline(intercept = alt_b0, slope = alt_b1,
                    color = unname(upwr_cat["bursztyn"]), linewidth = 1.1,
                    linetype = "longdash") +
        geom_segment(aes(xend = x, yend = alt_fitted),
                     color = unname(upwr_cat["bursztyn"]), alpha = 0.22) +
        annotate("text", x = min(df$x), y = max(df$y),
                 label = "inna prosta", hjust = 0, vjust = 1,
                 color = unname(upwr_cat["bursztyn"]), fontface = "bold") +
        annotate("text", x = min(df$x), y = max(df$y) - 0.1 * diff(range(df$y)),
                 label = "OLS", hjust = 0, vjust = 1,
                 color = unname(upwr_cat["niebo"]), fontface = "bold")
    }
    if (step == 0) {
      p <- p + annotate("text", x = mean(df$x), y = mean(df$y),
                        label = "Klikaj kroki po lewej", color = upwr_reference, size = 5)
    }
    p
  })

  output$ch1_ols_info <- renderUI({
    df <- ch1_ols_data()
    step <- ch1_ols_step()
    if (step == 0) return(NULL)
    model <- lm(y ~ x, data = df)
    coefs <- coef(model)
    sse <- sum(residuals(model)^2)
    alt_b1 <- coefs[2] * 0.45
    alt_b0 <- mean(df$y) - alt_b1 * mean(df$x)
    alt_sse <- sum((df$y - (alt_b0 + alt_b1 * df$x))^2)
    if (step == 6) {
      return(tagList(
        lc_stat_box("SSE OLS", round(sse, 1), color = unname(upwr_cat["niebo"])),
        lc_stat_box("SSE innej prostej", round(alt_sse, 1),
                    caption = paste0("+", round((alt_sse / sse - 1) * 100, 1), "%"),
                    color = unname(upwr_cat["bursztyn"])),
        lc_feedback(type = "warning",
          p("Ta przerywana linia też jest prostym modelem regresyjnym: dla każdego X daje przewidywane Ŷ. Nie jest jednak linią OLS, bo ma większą sumę kwadratów reszt. OLS wygrywa nie dlatego, że jest jedyną prostą, tylko dlatego, że minimalizuje SSE.")
        )
      ))
    }
    info <- switch(as.character(step),
      "1" = "Najpierw mamy tylko punkty: pary obserwacji X i Y.",
      "2" = "Pozioma linia to średnia Y. To najprostszy model bez predyktora.",
      "3" = "Linia regresji przechodzi tak, aby suma kwadratów pionowych błędów była możliwie mała.",
      "4" = "Każdy odcinek to reszta: obserwacja minus predykcja.",
      "5" = paste0("Model: Ŷ = ", round(coefs[1], 2), " + ",
                   round(coefs[2], 2), "X; SSE = ", round(sse, 1),
                   "; R² = ", round(summary(model)$r.squared, 3), ".")
    )
    lc_feedback(type = "info", p(info))
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
        labs(x = "Wartości dopasowane",
             y = "Reszty") +
        theme_upwr()

      p2 <- ggplot(df, aes(sample = std_resid)) +
        stat_qq(color = upwr_secondary, alpha = 0.5) +
        stat_qq_line(color = unname(upwr_cat["niebo"])) +
        labs(x = "Kwantyle teoretyczne",
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
        labs(
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

  # --- Widget: R2 decomposition ---
  ch1_r2_parts_data <- reactiveVal(generate_regression_data(n = 55, beta0 = 8, beta1 = 1.7, sigma = 4))
  ch1_r2_parts_step <- reactiveVal(0)

  observeEvent(input$ch1_r2_parts_new, {
    ch1_r2_parts_data(generate_regression_data(n = 55, beta0 = 8, beta1 = 1.7, sigma = 4))
    ch1_r2_parts_step(0)
  })
  observeEvent(input$ch1_r2_part1, ch1_r2_parts_step(1))
  observeEvent(input$ch1_r2_part2, ch1_r2_parts_step(2))
  observeEvent(input$ch1_r2_part3, ch1_r2_parts_step(3))

  output$ch1_r2_parts_plot <- renderPlot({
    df <- ch1_r2_parts_data()
    step <- ch1_r2_parts_step()
    model <- lm(y ~ x, data = df)
    df$fitted <- fitted(model)
    mean_y <- mean(df$y)

    p <- ggplot(df, aes(x = x, y = y)) +
      geom_point(color = upwr_secondary, alpha = 0.55) +
      labs(x = "X", y = "Y") +
      theme_upwr()
    if (step >= 1) {
      p <- p +
        geom_hline(yintercept = mean_y, color = unname(upwr_cat["bursztyn"]),
                   linetype = "dashed") +
        geom_segment(aes(xend = x, yend = mean_y),
                     color = unname(upwr_cat["bursztyn"]), alpha = 0.25)
    }
    if (step >= 2) {
      p <- p +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"]),
                    linewidth = 1.2) +
        geom_segment(aes(xend = x, yend = fitted),
                     color = unname(upwr_cat["terakota"]), alpha = 0.35)
    }
    if (step == 0) {
      p <- p + annotate("text", x = mean(df$x), y = mean(df$y),
                        label = "Kliknij warstwę po lewej", color = upwr_reference, size = 5)
    }
    p
  })

  output$ch1_r2_parts_info <- renderUI({
    df <- ch1_r2_parts_data()
    step <- ch1_r2_parts_step()
    if (step == 0) return(NULL)
    model <- lm(y ~ x, data = df)
    sst <- sum((df$y - mean(df$y))^2)
    sse <- sum(residuals(model)^2)
    ssr <- sst - sse
    r2 <- 1 - sse / sst
    tagList(
      lc_stat_box("SST", round(sst, 1), caption = "całkowita zmienność", color = unname(upwr_cat["bursztyn"])),
      if (step >= 2) lc_stat_box("SSE", round(sse, 1), caption = "niewyjaśnione", color = unname(upwr_cat["terakota"])),
      if (step >= 3) lc_stat_box("SSR", round(ssr, 1), caption = "wyjaśnione", color = unname(upwr_cat["niebo"])),
      if (step >= 3) lc_stat_box("R²", round(r2, 3), caption = "SSR / SST", color = unname(upwr_cat["szalwia"]))
    )
  })

  # --- Widget: diagnostics ---
  ch1_assumption_data <- reactiveVal(NULL)

  observeEvent(input$ch1_assumption_new, {
    ch1_assumption_data(generate_assumption_data(120, input$ch1_assumption_scenario))
  })

  output$ch1_assumption_plot <- renderPlot({
    df <- ch1_assumption_data()
    if (is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Generuj i diagnozuj'",
                 size = 6, color = upwr_reference) +
        theme_void()
    } else {
      model <- lm(y ~ x, data = df)
      diag <- data.frame(
        x = df$x,
        y = df$y,
        fitted = fitted(model),
        residuals = residuals(model),
        std_resid = rstandard(model)
      )
      p1 <- ggplot(diag, aes(x = x, y = y)) +
        geom_point(color = upwr_secondary, alpha = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = unname(upwr_cat["niebo"])) +
        labs(x = "X", y = "Y") +
        theme_upwr()
      p2 <- ggplot(diag, aes(x = fitted, y = residuals)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = upwr_secondary) +
        geom_point(color = unname(upwr_cat["terakota"]), alpha = 0.5) +
        geom_smooth(se = FALSE, color = unname(upwr_cat["niebo"]), linewidth = 0.8) +
        labs(x = "Dopasowane", y = "Reszty") +
        theme_upwr()
      p3 <- ggplot(diag, aes(sample = std_resid)) +
        stat_qq(color = upwr_secondary, alpha = 0.5) +
        stat_qq_line(color = unname(upwr_cat["niebo"])) +
        labs(x = "Kwantyle teoret.", y = "Kwantyle próby") +
        theme_upwr()
      gridExtra::grid.arrange(p1, p2, p3, ncol = 3)
    }
  })

  output$ch1_assumption_info <- renderUI({
    scenario <- input$ch1_assumption_scenario
    msg <- switch(scenario,
      "good" = "Brak wyraźnego wzorca w resztach: model liniowy jest rozsądnym opisem danych.",
      "nonlinear" = "Reszty układają się łukiem: prosta nie łapie krzywizny związku.",
      "hetero" = "Rozrzut reszt rośnie wraz z predykcją: wariancja błędu nie jest stała.",
      "outlier" = "Pojedyncza obserwacja mocno ciągnie linię i może zmienić wnioski.",
      "nonnormal" = "QQ-plot odchyla się na końcach: reszty mają ciężkie ogony."
    )
    lc_feedback(type = if (scenario == "good") "ok" else "warning", p(msg))
  })
}
