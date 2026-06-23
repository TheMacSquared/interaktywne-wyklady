# ============================================================================
# CHAPTER 13: Ocena dokładności prognozy
# ============================================================================

ch13_ui <- list(
  id    = "ch-dokladnosc",
  num   = "13",
  title = "Ocena dokładności prognozy",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 13 · Szeregi czasowe",
      num    = "13",
      title  = "Ocena dokładności.",
      lead   = "MAE, RMSE, MAPE, MASE — cztery miary, cztery różne pytania.
                Żadna nie jest idealna. Heatmap metoda × metryka ujawnia, kto naprawdę wygrywa."
    ),

    lc_h2("ch13-metryki", "Cztery miary dokładności"),

    tagList(
      lc_formula_box(
        tags$table(class = "lc-table lc-table-bordered lc-table-sm",
          tags$thead(tags$tr(
            tags$th("Metryka"), tags$th("Wzór"), tags$th("Interpretacja"), tags$th("Słabość")
          )),
          tags$tbody(
            tags$tr(
              tags$td("MAE"), tags$td(withMathJax("\\(\\frac{1}{n}\\sum|e_t|\\)")),
              tags$td("Średni błąd bezwzględny"), tags$td("Jednostkowy, trudno porównywać szeregi")
            ),
            tags$tr(
              tags$td("RMSE"), tags$td(withMathJax("\\(\\sqrt{\\frac{1}{n}\\sum e_t^2}\\)")),
              tags$td("Karze za duże błędy"), tags$td("Wrażliwy na outlier")
            ),
            tags$tr(
              tags$td("MAPE"), tags$td(withMathJax("\\(\\frac{100}{n}\\sum\\left|\\frac{e_t}{y_t}\\right|\\)")),
              tags$td("Procentowy — porównuje szeregi"), tags$td("Nieskończony gdy y_t ≈ 0")
            ),
            tags$tr(
              tags$td("MASE"), tags$td(withMathJax("\\(\\frac{MAE}{MAE_{\\text{naïve}}}\\)")),
              tags$td("Relative vs. naïve — bez problemu z zerami"), tags$td("Wymaga interpretacji skalowej")
            )
          )
        )
      ),
      margin_callout(label = "Uwaga o MAPE", color = "uwaga",
        "W czasie pandemii COVID-19 liczba noclegów spadła niemal do zera. MAPE eksplodowała — nie z powodu złej prognozy, ale z powodu małego mianownika. Wtedy używaj MAE lub MASE."
      )
    ),

    lc_h2("ch13-widget", "Widget train/test — tabela metryk na żywo"),

    figure_panel(
      label = "Ryc. 13.1", title = "Suwak podziału → tabela metryk dla 4 metod",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch13_data", "Szereg:",
                      choices = .ts_dataset_choices[c("noclegi", "bezrobocie", "sprzedaz", "warszawa")],
                      selected = "noclegi"),
          sliderInput("ch13_test_pct", "Procent danych testowych:", min = 10, max = 30, value = 20, step = 5),
          actionButton("ch13_run", "Oblicz metryki", class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch13_metrics_table")
        )
      )
    ),

    lc_h2("ch13-heatmap", "Heatmap metoda × metryka"),

    tagList(
      lc_p("Heatmap pozwala jednym rzutem oka zobaczyć, która metoda wygrywa według której metryki.
        Im ciemniejszy kolor, tym wyższy (gorszy) błąd.")
    ),

    figure_panel(
      label = "Ryc. 13.2", title = "Heatmap — kolor = znormalizowany błąd (jasny = lepszy)",
      full_width = TRUE,
      fluidRow(
        column(12,
          zoom_plot_ui("ch13_heatmap", height = "260px")
        )
      )
    ),

    lc_h2("ch13-interpretacja", "Jak czytać wyniki?"),

    tagList(
      lc_p("Kilka praktycznych wskazówek:"),
      tags$ul(
        tags$li(tags$strong("MASE < 1:"), " metoda bije naiwną sezonową — minimum oczekiwane."),
        tags$li(tags$strong("RMSE / MAE ≫ 1:"), " model duże błędy penalizuje mocniej — sprawdź, czy są outliery."),
        tags$li(tags$strong("MAPE > 20%:"), " prognoza jest słaba; szukaj dodatkowych zmiennych lub zmień model."),
        tags$li(tags$strong("Różne metryki, różni zwycięzcy:"), " zwykle wybieramy metodę najlepszą dla metryki najważniejszej w danym zastosowaniu.")
      )
    ),

    lc_chapter_next(
      num       = "14",
      title     = "Diagnostyka modelu",
      lead      = "residua, ACF, Ljung-Box, quad-plot",
      target_id = "ch-diagnostyka"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch13_server <- function(input, output, session) {

  ch13_results <- reactiveVal(NULL)

  observeEvent(input$ch13_run, {
    key    <- input$ch13_data
    pct    <- input$ch13_test_pct / 100
    ts_obj <- .ts_datasets[[key]]$get_ts()
    n      <- length(ts_obj)
    n_test <- round(n * pct)
    n_train <- n - n_test
    train  <- ts(as.numeric(ts_obj)[seq_len(n_train)],
                 start = start(ts_obj), frequency = frequency(ts_obj))
    actual <- as.numeric(ts_obj)[(n_train + 1):n]
    h      <- n_test

    methods <- c("naive", "snaive", "ets", "arima")
    results <- lapply(methods, function(m) {
      fit <- tryCatch({
        switch(m,
          naive  = forecast::naive(train,  h = h),
          snaive = forecast::snaive(train, h = h),
          ets    = forecast::forecast(forecast::ets(train),             h = h),
          arima  = forecast::forecast(forecast::auto.arima(train),      h = h)
        )
      }, error = function(e) forecast::naive(train, h = h))
      pred <- as.numeric(fit$mean)
      met  <- compute_accuracy_metrics(actual, pred)
      naive_mae <- mean(abs(diff(actual, lag = frequency(ts_obj))), na.rm = TRUE)
      mase <- if (!is.na(naive_mae) && naive_mae > 0) met$mae / naive_mae else NA
      list(method = m, mae = met$mae, rmse = met$rmse, mape = met$mape, mase = mase)
    })
    ch13_results(results)
  })

  output$ch13_metrics_table <- renderUI({
    results <- ch13_results()
    if (is.null(results)) return(lc_feedback(type = "info", p("Kliknij 'Oblicz metryki'.")))
    labels <- c(naive = "Naiwna", snaive = "Naiwna sezonowa", ets = "ETS", arima = "ARIMA")
    best_mae  <- which.min(sapply(results, `[[`, "mae"))
    best_rmse <- which.min(sapply(results, `[[`, "rmse"))
    best_mape <- which.min(sapply(results, `[[`, "mape"))
    best_mase <- which.min(sapply(results, `[[`, "mase"))
    rows <- lapply(seq_along(results), function(i) {
      r  <- results[[i]]
      hl <- function(j, best) if (i == best) "font-weight:bold; color:var(--upwr-accent);" else ""
      tags$tr(
        tags$td(labels[r$method]),
        tags$td(style = hl(i, best_mae),  round(r$mae,  2)),
        tags$td(style = hl(i, best_rmse), round(r$rmse, 2)),
        tags$td(style = hl(i, best_mape), paste0(round(r$mape, 1), "%")),
        tags$td(style = hl(i, best_mase), round(r$mase, 2))
      )
    })
    tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
      tags$thead(tags$tr(
        tags$th("Metoda"), tags$th("MAE"), tags$th("RMSE"), tags$th("MAPE"), tags$th("MASE")
      )),
      tags$tbody(rows)
    )
  })

  zoom_plot_server("ch13_heatmap", reactive({
    results <- ch13_results()
    if (is.null(results)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
               label = "Kliknij 'Oblicz metryki'", color = upwr_reference, size = 5) + theme_upwr())
    }
    labels <- c(naive = "Naiwna", snaive = "Naiwna sezonowa", ets = "ETS", arima = "ARIMA")
    rows <- lapply(results, function(r) {
      data.frame(
        Metoda  = labels[r$method],
        Metryka = c("MAE", "RMSE", "MAPE", "MASE"),
        Wartosc = c(r$mae, r$rmse, r$mape, if (!is.na(r$mase)) r$mase else 0),
        stringsAsFactors = FALSE
      )
    })
    df <- do.call(rbind, rows)
    df <- df |>
      dplyr::group_by(Metryka) |>
      dplyr::mutate(Znorm = (Wartosc - min(Wartosc, na.rm = TRUE)) /
                              (max(Wartosc, na.rm = TRUE) - min(Wartosc, na.rm = TRUE) + 1e-9)) |>
      dplyr::ungroup()

    ggplot(df, aes(x = Metryka, y = Metoda, fill = Znorm)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = round(Wartosc, 1)), color = "white", size = 3.5) +
      scale_fill_gradient(low = upwr_cat["niebo"], high = upwr_accent, name = "Znorm.") +
      labs(x = NULL, y = NULL,
           title = "Heatmap dokładności — jasny = lepszy") +
      theme_upwr() +
      theme(legend.position = "right")
  }))
}
