# ============================================================================
# CHAPTER 6: Cross-validation
# ============================================================================

ch6_ui <- tabPanel("6. Cross-validation",
  fluidRow(column(8, offset = 2,

    div(class = "chapter-recap",
      "Bootstrap i jackknife mierzy\u0142y niepewno\u015b\u0107 statystyk z pr\u00f3by.
       Cross-validation mierzy co\u015b innego: jak dobry jest model
       predykcyjny na ", tags$em("nowych"), " danych?"
    ),

    div(class = "section-title", "Po co cross-validation?"),

    div(class = "narrative",
      p("Mamy model regresji. MSE treningowy (na tych samych danych,
         na kt\u00f3rych model by\u0142 uczony) jest zawsze zbyt optymistyczny \u2014
         model \u201ezapami\u0119ta\u0142\u201f dane."),
      p("Pytanie: jak dobrze model przewiduje ", tags$b("nowe obserwacje"),
        "? CV odpowiada na to pytanie bez potrzeby posiadania osobnego
         zbioru testowego.")
    ),

    div(class = "callout-info",
      tags$strong("Algorytm K-Fold CV:"),
      tags$ol(
        tags$li("Podziel dane losowo na k r\u00f3wnych cz\u0119\u015bci (fold\u00f3w)"),
        tags$li("Dla ka\u017cdego foldu: wyucz model na pozosta\u0142ych k-1 foldach,
                oblicz b\u0142\u0105d predykcji na wybranym"),
        tags$li("CV MSE = \u015brednia b\u0142\u0119d\u00f3w z k fold\u00f3w"),
        tags$li("Powtarzaj dla r\u00f3\u017cnych z\u0142o\u017cono\u015bci modelu \u2014 wybierz najlepsz\u0105")
      )
    ),

    # ========================================================================
    # WIDGET 1: K-Fold CV demo
    # ========================================================================
    div(class = "section-title", "K-Fold CV w praktyce"),

    div(class = "narrative",
      p("Poni\u017cej dane wygenerowane z kwadratowej zale\u017cno\u015bci + szum.
         Dopasujemy wielomiany r\u00f3\u017cnych stopni i sprawdzimy,
         kt\u00f3ry ma najni\u017csze CV MSE.")
    ),

    div(class = "widget-block",
      h4("K-Fold CV \u2014 demo"),
      fluidRow(
        column(4,
          selectInput("ch6_degree", "Stopie\u0144 wielomianu:",
            choices = c(
              "1 \u2014 liniowy"             = "1",
              "2 \u2014 kwadratowy (prawdziwy)" = "2",
              "3 \u2014 sze\u015bcian"          = "3",
              "5 \u2014 prze-fitowany"      = "5",
              "10 \u2014 silnie prze-fitowany" = "10"
            ),
            selected = "2"
          ),
          sliderInput("ch6_k", "k (liczba fold\u00f3w):",
                      min = 2, max = 20, value = 5, step = 1),
          sliderInput("ch6_n", "n (rozmiar zbioru):",
                      min = 30, max = 200, value = 80, step = 10),
          sliderInput("ch6_sigma", "Szum (\u03c3):",
                      min = 1, max = 20, value = 8, step = 1),
          hr(),
          actionButton("ch6_run", "Uruchom CV",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          uiOutput("ch6_cv_stats")
        ),
        column(8,
          plotOutput("ch6_cv_plot", height = "360px")
        )
      )
    ),

    div(class = "callout-success",
      tags$strong("Aha-moment:"),
      " Model 10. stopnia ma niski MSE treningowy, ale wysoki CV MSE \u2014
       to w\u0142a\u015bnie ", tags$b("prze-uczenie (overfitting)"),
      ". Model \u201ezapami\u0119ta\u0142\u201f szum zamiast wzorca."
    ),

    # ========================================================================
    # WIDGET 2: Porownanie wszystkich stopni
    # ========================================================================
    div(class = "section-title", "Optymalny stopie\u0144 wielomianu"),

    div(class = "narrative",
      p("Uruchom CV dla wszystkich stopni naraz i por\u00f3wnaj MSE treningowe
         i CV na jednym wykresie. Optymalny model ma najni\u017csze CV MSE.")
    ),

    div(class = "widget-block",
      h4("MSE treningowy vs CV MSE"),
      fluidRow(
        column(4,
          sliderInput("ch6_cmp_n",     "n:", min = 40, max = 150, value = 80, step = 10),
          sliderInput("ch6_cmp_sigma", "\u03c3:", min = 2, max = 20, value = 8, step = 1),
          sliderInput("ch6_cmp_k",     "k (folds):", min = 3, max = 15, value = 5, step = 1),
          actionButton("ch6_cmp_run", "Por\u00f3wnaj wszystkie stopnie",
                       class = "btn-warning", width = "100%")
        ),
        column(8,
          plotOutput("ch6_cmp_plot", height = "300px")
        )
      )
    ),

    div(class = "callout-warning",
      tags$strong("LOOCV (Leave-One-Out CV):"),
      " szczeg\u00f3lny przypadek k = n.
       Daje prawie nieprzychylne (unbiased) szacunki b\u0142\u0119du, ale:
       (1) wysoka wariancja estymaty, (2) d\u0142ugie obliczenia.
       W praktyce k = 5 lub k = 10 jest zwykle lepszym kompromisem."
    ),

    div(class = "chapter-transition",
      p("Dalej: Monte Carlo \u2014 symulacja mocy testu i rozk\u0142ad\u00f3w pod H\u2080"),
      actionButton("ch6_next",
                   "Dalej \u2192 7. Monte Carlo",
                   class = "btn-primary btn-lg")
    )

  ))
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
                 size = 6, color = "#7f8c8d") +
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
      geom_point(color = col_primary, size = 2, alpha = 0.7) +
      geom_line(data = df_fit, aes(x = x, y = y),
                color = col_secondary, linewidth = 1.5) +
      labs(title = paste0("Model: wielomian stopnia ", degree),
           x = "x", y = "y") +
      theme_sim()

    # Prawy panel: bledy per fold
    n_folds <- length(result$fold_errors)
    df_err  <- data.frame(
      fold = factor(seq_len(n_folds)),
      err  = result$fold_errors
    )
    p2 <- ggplot(df_err, aes(x = fold, y = err)) +
      geom_col(fill = col_cv_test, alpha = 0.8) +
      geom_hline(yintercept = result$cv_mse, color = col_cv_test,
                 linewidth = 1.2, linetype = "dashed") +
      geom_hline(yintercept = result$train_mse, color = col_cv_train,
                 linewidth = 1.2, linetype = "solid") +
      annotate("text", x = 0.6, y = result$cv_mse,
               label = paste0("CV MSE = ", round(result$cv_mse, 1)),
               hjust = 0, vjust = -0.4, color = col_cv_test, size = 3.5) +
      annotate("text", x = 0.6, y = result$train_mse,
               label = paste0("Train MSE = ", round(result$train_mse, 1)),
               hjust = 0, vjust = -0.4, color = col_cv_train, size = 3.5) +
      labs(title = paste0(n_folds, "-Fold CV: b\u0142\u0105d per fold"),
           x = "Fold", y = "MSE") +
      theme_sim()

    gridExtra::grid.arrange(p1, p2, ncol = 2)
  })

  output$ch6_cv_stats <- renderUI({
    cv_res <- ch6_cv_result()
    if (is.null(cv_res)) return(NULL)
    res  <- cv_res$result
    tagList(
      div(class = "stat-box", style = paste0("background:", col_cv_train, ";"),
          paste0("MSE train = ", round(res$train_mse, 2))),
      div(class = "stat-box", style = paste0("background:", col_cv_test, ";"),
          paste0("CV MSE = ", round(res$cv_mse, 2))),
      div(class = "stat-box", style = paste0("background:", col_dark, ";"),
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
                 label = "Kliknij 'Por\u00f3wnaj wszystkie stopnie'",
                 size = 6, color = "#7f8c8d") +
        theme_void()
      return()
    }
    plot_cv_results(results, col_cv_train = col_cv_train, col_cv_test = col_cv_test)
  })

}
