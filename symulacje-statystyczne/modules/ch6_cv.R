# ============================================================================
# CHAPTER 6: Cross-validation
# ============================================================================

ch6_ui <- lecture_chapter(
  id = "ch-cv",
  num = "06",
  title = "Cross-validation",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 06 · Symulacje statystyczne",
      num    = "06",
      title  = "Cross-validation",
      lead   = "Walidacja krzyżowa pomaga ocenić, jak model działa poza danymi treningowymi."
    ),

    lc_feedback(type = "info",
      "Bootstrap i jackknife mierzyły niepewność statystyk z próby.
       Cross-validation mierzy coś innego: jak dobry jest model
       predykcyjny na ", tags$em("nowych"), " danych?"
    ),

    lc_h2("ch6-sec-01", "Po co cross-validation?"),

    tagList(
      p("Mamy model regresji. MSE treningowy (na tych samych danych,
         na których model był uczony) jest zawsze zbyt optymistyczny —
         model „zapamiętał‟ dane."),
      p("Pytanie: jak dobrze model przewiduje ", tags$b("nowe obserwacje"),
        "? CV odpowiada na to pytanie bez potrzeby posiadania osobnego
         zbioru testowego.")
    ),

    lc_feedback(type = "info",
      tags$strong("Algorytm K-Fold CV:"),
      tags$ol(
        tags$li("Podziel dane losowo na k równych części (foldów)"),
        tags$li("Dla każdego foldu: wyucz model na pozostałych k-1 foldach,
                oblicz błąd predykcji na wybranym"),
        tags$li("CV MSE = średnia błędów z k foldów"),
        tags$li("Powtarzaj dla różnych złożoności modelu — wybierz najlepszą")
      )
    ),

    # ========================================================================
    # WIDGET 1: K-Fold CV demo
    # ========================================================================
    lc_h2("ch6-sec-02", "K-Fold CV w praktyce"),

    tagList(
      p("Poniżej dane wygenerowane z kwadratowej zależności + szum.
         Dopasujemy wielomiany różnych stopni i sprawdzimy,
         który ma najniższe CV MSE.")
    ),

    figure_panel(label = "Ryc. 6.1", title = "K-Fold CV — demo",
      fluidRow(
        column(4,
          selectInput("ch6_degree", "Stopień wielomianu:",
            choices = c(
              "1 — liniowy"             = "1",
              "2 — kwadratowy (prawdziwy)" = "2",
              "3 — sześcian"          = "3",
              "5 — prze-fitowany"      = "5",
              "10 — silnie prze-fitowany" = "10"
            ),
            selected = "2"
          ),
          sliderInput("ch6_k", "k (liczba foldów):",
                      min = 2, max = 20, value = 5, step = 1),
          sliderInput("ch6_n", "n (rozmiar zbioru):",
                      min = 30, max = 200, value = 80, step = 10),
          sliderInput("ch6_sigma", "Szum (σ):",
                      min = 1, max = 20, value = 8, step = 1),
          hr(),
          actionButton("ch6_run", "Uruchom CV",
                       class = "lc-btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch6_cv_stats")
        ),
        column(8,
          plotOutput("ch6_cv_plot", height = "360px")
        )
      )
    ),

    lc_feedback(type = "ok",
      tags$strong("Aha-moment:"),
      " Model 10. stopnia ma niski MSE treningowy, ale wysoki CV MSE —
       to właśnie ", tags$b("prze-uczenie (overfitting)"),
      ". Model „zapamiętał‟ szum zamiast wzorca."
    ),

    # ========================================================================
    # WIDGET 2: Porownanie wszystkich stopni
    # ========================================================================
    lc_h2("ch6-sec-03", "Optymalny stopień wielomianu"),

    tagList(
      p("Uruchom CV dla wszystkich stopni naraz i porównaj MSE treningowe
         i CV na jednym wykresie. Optymalny model ma najniższe CV MSE.")
    ),

    figure_panel(label = "Ryc. 6.2", title = "MSE treningowy vs CV MSE",
      fluidRow(
        column(4,
          sliderInput("ch6_cmp_n",     "n:", min = 40, max = 150, value = 80, step = 10),
          sliderInput("ch6_cmp_sigma", "σ:", min = 2, max = 20, value = 8, step = 1),
          sliderInput("ch6_cmp_k",     "k (folds):", min = 3, max = 15, value = 5, step = 1),
          actionButton("ch6_cmp_run", "Porównaj wszystkie stopnie",
                       class = "lc-btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch6_cmp_plot", height = "300px")
        )
      )
    ),

    lc_feedback(type = "warning",
      tags$strong("LOOCV (Leave-One-Out CV):"),
      " szczególny przypadek k = n.
       Daje prawie nieprzychylne (unbiased) szacunki błędu, ale:
       (1) wysoka wariancja estymaty, (2) długie obliczenia.
       W praktyce k = 5 lub k = 10 jest zwykle lepszym kompromisem."
    ),

    lc_chapter_next(
      num = "07",
      title = "Monte Carlo",
      lead = "symulacje mocy testu i rozkładu pod hipotezą zerową.",
      target_id = "ch-monte-carlo"
    )

  )
)
# ============================================================================
# SERVER
# ============================================================================

ch6_server <- function(input, output, session) {

  ch6_data_rv   <- reactiveVal(NULL)
  ch6_cv_result <- reactiveVal(NULL)

  observeEvent(list(input$ch6_n, input$ch6_sigma), {
    ch6_data_rv(NULL); ch6_cv_result(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$ch6_run, {
    if (is.null(ch6_data_rv())) {
      ch6_data_rv(generate_regression_data(n = input$ch6_n,
                                            degree_true = 2,
                                            sigma = input$ch6_sigma))
    }
    df     <- ch6_data_rv()
    degree <- as.integer(input$ch6_degree)
    k_val  <- if (input$ch6_k >= nrow(df)) nrow(df) else input$ch6_k
    result <- run_kfold_cv(df, degree = degree, k = k_val)
    ch6_cv_result(list(result = result, degree = degree))
  })

  output$ch6_cv_plot <- renderPlot({
    cv_res <- ch6_cv_result()
    df     <- ch6_data_rv()

    if (is.null(cv_res) || is.null(df)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Uruchom CV'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }

    degree <- cv_res$degree
    result <- cv_res$result

    # Lewy panel: dane + dopasowany model
    x_seq  <- seq(min(df$x), max(df$x), length.out = 200)
    fit    <- lm(y ~ poly(x, degree, raw = TRUE), data = df)
    preds  <- predict(fit, newdata = data.frame(x = x_seq))
    df_fit <- data.frame(x = x_seq, y = preds)

    p1 <- ggplot(df, aes(x = x, y = y)) +
      geom_point(color = sim_bootstrap, size = 2, alpha = 0.7) +
      geom_line(data = df_fit, aes(x = x, y = y),
                color = sim_observed, linewidth = 1.5) +
      labs(title = paste0("Model: wielomian stopnia ", degree),
           x = "x", y = "y") +
      theme_upwr()

    # Prawy panel: bledy per fold
    n_folds <- length(result$fold_errors)
    df_err  <- data.frame(
      fold = factor(seq_len(n_folds)),
      err  = result$fold_errors
    )
    p2 <- ggplot(df_err, aes(x = fold, y = err)) +
      geom_col(fill = sim_cv_test, alpha = 0.8) +
      geom_hline(yintercept = result$cv_mse, color = sim_cv_test,
                 linewidth = 1.2, linetype = "dashed") +
      geom_hline(yintercept = result$train_mse, color = sim_cv_train,
                 linewidth = 1.2, linetype = "solid") +
      annotate("text", x = 0.6, y = result$cv_mse,
               label = paste0("CV MSE = ", round(result$cv_mse, 1)),
               hjust = 0, vjust = -0.4, color = sim_cv_test, size = 3.5) +
      annotate("text", x = 0.6, y = result$train_mse,
               label = paste0("Train MSE = ", round(result$train_mse, 1)),
               hjust = 0, vjust = -0.4, color = sim_cv_train, size = 3.5) +
      labs(title = paste0(n_folds, "-Fold CV: błąd per fold"),
           x = "Fold", y = "MSE") +
      theme_upwr()

    gridExtra::grid.arrange(p1, p2, ncol = 2)
  })

  output$ch6_cv_stats <- renderUI({
    cv_res <- ch6_cv_result()
    if (is.null(cv_res)) return(NULL)
    res  <- cv_res$result
    tagList(
      div(class = "lc-stat-box", style = paste0("background:", sim_cv_train, ";"),
          paste0("MSE train = ", round(res$train_mse, 2))),
      div(class = "lc-stat-box", style = paste0("background:", sim_cv_test, ";"),
          paste0("CV MSE = ", round(res$cv_mse, 2))),
      div(class = "lc-stat-box", style = paste0("background:", sim_secondary, ";"),
          paste0("k = ", res$k))
    )
  })

  # --- Widget 2: Porownanie wszystkich stopni ---
  ch6_cmp_data   <- reactiveVal(NULL)
  ch6_cmp_result <- reactiveVal(NULL)

  observeEvent(input$ch6_cmp_run, {
    df <- generate_regression_data(n     = input$ch6_cmp_n,
                                    degree_true = 2,
                                    sigma = input$ch6_cmp_sigma)
    ch6_cmp_data(df)
    degrees <- c(1, 2, 3, 5, 7, 10)
    k_val   <- min(input$ch6_cmp_k, nrow(df))
    results <- lapply(degrees, function(d) {
      run_kfold_cv(df, degree = d, k = k_val)
    })
    ch6_cmp_result(results)
  })

  output$ch6_cmp_plot <- renderPlot({
    results <- ch6_cmp_result()
    if (is.null(results)) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Porównaj wszystkie stopnie'",
                 size = 6, color = upwr_reference) +
        theme_void()
      return()
    }
    plot_cv_results(results, sim_cv_train = sim_cv_train, sim_cv_test = sim_cv_test)
  })

}
