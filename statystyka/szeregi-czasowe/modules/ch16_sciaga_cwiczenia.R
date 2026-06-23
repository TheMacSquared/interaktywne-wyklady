# ============================================================================
# CHAPTER 16: Ściąga i ćwiczenia
# ============================================================================

ch16_ui <- list(
  id    = "ch-sciaga",
  num   = "16",
  title = "Ściąga i ćwiczenia",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 16 · Szeregi czasowe",
      num    = "16",
      title  = "Ściąga i ćwiczenia.",
      lead   = "Drzewo decyzyjne, tabela metod, tabela metryk.
                Trzy ćwiczenia dla rolnictwa, bezpieczeństwa i technologii żywności."
    ),

    lc_h2("ch16-drzewo", "Drzewo decyzyjne: jaki model?"),

    tagList(
      lc_p("Skorzystaj z tego schematu, gdy nie wiesz od czego zacząć:"),
      tags$div(class = "lc-decision-tree",
        tags$div(class = "lc-dt-root",
          tags$strong("Szereg czasowy"), " — od czego zacząć?"
        ),
        tags$div(class = "lc-dt-row",
          tags$div(class = "lc-dt-branch",
            tags$div(class = "lc-dt-node lc-dt-q", "Czy dane mają wyraźną sezonowość?"),
            tags$div(class = "lc-dt-children",
              tags$div(class = "lc-dt-yes",
                tags$div(class = "lc-dt-node lc-dt-q", "Czy wariancja rośnie z poziomem?"),
                tags$div(class = "lc-dt-children",
                  tags$div(class = "lc-dt-yes",
                    tags$div(class = "lc-dt-node lc-dt-a", "ETS(M,A,M) lub SARIMA + Box-Cox")
                  ),
                  tags$div(class = "lc-dt-no",
                    tags$div(class = "lc-dt-node lc-dt-a", "ETS(A,A,A) lub SARIMA(p,d,q)(P,D,Q)[12]")
                  )
                )
              ),
              tags$div(class = "lc-dt-no",
                tags$div(class = "lc-dt-node lc-dt-q", "Czy szereg jest niestacjonarny?"),
                tags$div(class = "lc-dt-children",
                  tags$div(class = "lc-dt-yes",
                    tags$div(class = "lc-dt-node lc-dt-a", "ARIMA(p,d,q) z d≥1")
                  ),
                  tags$div(class = "lc-dt-no",
                    tags$div(class = "lc-dt-node lc-dt-a", "ARMA(p,q) lub ETS(A,N,N)")
                  )
                )
              )
            )
          )
        )
      )
    ),

    lc_h2("ch16-tabela-metod", "Tabela metod"),

    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
        tags$thead(tags$tr(
          tags$th("Metoda"), tags$th("Kiedy stosować"), tags$th("Zalety"), tags$th("Wady")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Naiwna sezonowa"), tags$td("Punkt odniesienia, silna sezonowość"),
            tags$td("Szybka, bez parametrów"), tags$td("Brak trendu, ignoruje historię poza sezonem")
          ),
          tags$tr(
            tags$td("ETS"), tags$td("Dane z trendem i/lub sezonowością, brak outlierów"),
            tags$td("Intuicyjne parametry α, β, γ"), tags$td("Słabszy przy złożonej zależności AR")
          ),
          tags$tr(
            tags$td("ARIMA"), tags$td("Stacjonarne lub różnicowalne szeregi"),
            tags$td("Elastyczne p,d,q; dobre PI"), tags$td("Wymaga stacjonarności, trudna identyfikacja")
          ),
          tags$tr(
            tags$td("SARIMA"), tags$td("ARIMA + sezonowość"),
            tags$td("Pełna kontrola nad składnikami"), tags$td("Dużo parametrów, wolne dopasowanie")
          ),
          tags$tr(
            tags$td("ARIMAX"), tags$td("Zdarzenia strukturalne, zewnętrzne regresory"),
            tags$td("Uwzględnia anomalie explicite"), tags$td("Wymaga znajomości terminów zdarzeń")
          )
        )
      )
    ),

    lc_h2("ch16-tabela-metryk", "Tabela metryk dokładności"),

    tagList(
      tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
        tags$thead(tags$tr(
          tags$th("Metryka"), tags$th("Jednostka"), tags$th("Kiedy używać"), tags$th("Unikaj gdy")
        )),
        tags$tbody(
          tags$tr(tags$td("MAE"), tags$td("Oryginalna"),
            tags$td("Baseline porównawczy, robustny"), tags$td("Porównujesz szeregi o różnych skalach")),
          tags$tr(tags$td("RMSE"), tags$td("Oryginalna"),
            tags$td("Chcesz karać duże błędy"), tags$td("Dane mają outliety (RMSE eksploduje)")),
          tags$tr(tags$td("MAPE"), tags$td("%"),
            tags$td("Porównanie szeregów o różnych skalach"), tags$td("Wartości bliskie zera")),
          tags$tr(tags$td("MASE"), tags$td("Bezwymiarowa"),
            tags$td("Zawsze bezpieczna alternatywa dla MAPE"), tags$td("Naiwna sezonowa jest idealna (MASE=1)")),
          tags$tr(tags$td("AIC/BIC"), tags$td("Bezwymiarowa"),
            tags$td("Wybór modelu na danych uczących"), tags$td("Porównanie prognozy out-of-sample (użyj MAE/RMSE)"))
        )
      )
    ),

    lc_h2("ch16-cwiczenia", "Ćwiczenia"),

    tabsetPanel(id = "ch16_tabs",
      tabPanel("Rolnictwo — pszenica",
        lc_spacer("md"),
        tagList(
          lc_p(tags$strong("Zadanie: "), "Zidentyfikuj model ARIMA dla cen skupu pszenicy i prognozuj na 12 miesięcy."),
          tags$ol(
            tags$li("Narysuj szereg. Czy widzisz trend, sezonowość, zmieniającą się wariancję?"),
            tags$li("Czy szereg jest stacjonarny? Sprawdź testem ADF i różnicowaniem."),
            tags$li("Narysuj ACF i PACF. Wstępnie zaproponuj p, d, q."),
            tags$li("Dopasuj kilka kandydatów i porównaj AIC."),
            tags$li("Sprawdź diagnostykę residuów (quad-plot + Ljung-Box)."),
            tags$li("Sporządź prognozę na 12 miesięcy z przedziałami ufności.")
          ),
          lc_spacer("md"),
          figure_panel(
            label = "Ryc. 16.1", title = "Ćwiczenie: ceny pszenicy → ARIMA",
            full_width = TRUE,
            fluidRow(
              column(4,
                div(style = "display: flex; gap: 8px;",
                  numericInput("ch16_r_p", "p:", value = 1, min = 0, max = 3, step = 1, width = "70px"),
                  numericInput("ch16_r_d", "d:", value = 1, min = 0, max = 2, step = 1, width = "70px"),
                  numericInput("ch16_r_q", "q:", value = 1, min = 0, max = 3, step = 1, width = "70px")
                ),
                numericInput("ch16_r_h", "Horyzont:", value = 12, min = 6, max = 24, step = 6),
                actionButton("ch16_r_run", "Dopasuj i prognozuj", class = "lc-btn-primary", width = "100%"),
                uiOutput("ch16_r_result")
              ),
              column(8,
                zoom_plot_ui("ch16_r_plot", height = "300px")
              )
            )
          )
        )
      ),

      tabPanel("Bezpieczeństwo — wypadki",
        lc_spacer("md"),
        tagList(
          lc_p(tags$strong("Zadanie: "), "Przeanalizuj sezonowość wypadków przy pracy (dane syntetyczne) i zbuduj model SARIMA."),
          tags$ol(
            tags$li("Narysuj dekompozycję STL. Kiedy jest więcej wypadków?"),
            tags$li("Sprawdź stacjonarność i wykonaj odpowiednie różnicowanie."),
            tags$li("Użyj auto.arima() z wykryciem sezonowości (frequency = 12)."),
            tags$li("Oceń model diagnostycznie."),
            tags$li("Prognozuj na 2 lata i zinterpretuj wachlarz niepewności.")
          ),
          lc_spacer("md"),
          figure_panel(
            label = "Ryc. 16.2", title = "Ćwiczenie: wypadki przy pracy → SARIMA",
            full_width = TRUE,
            fluidRow(
              column(4,
                numericInput("ch16_b_h", "Horyzont (miesiące):", value = 24, min = 12, max = 36, step = 6),
                actionButton("ch16_b_run", "auto.arima + prognoza", class = "lc-btn-primary", width = "100%"),
                uiOutput("ch16_b_result")
              ),
              column(8,
                zoom_plot_ui("ch16_b_plot", height = "300px")
              )
            )
          )
        )
      ),

      tabPanel("Technologia żywności — fermentacja",
        lc_spacer("md"),
        tagList(
          lc_p(tags$strong("Zadanie: "), "Przeanalizuj temperaturę fermentacji (dane syntetyczne). Zbuduj ETS i wykryj anomalię."),
          tags$ol(
            tags$li("Narysuj szereg. Czy widzisz anomalię?"),
            tags$li("Dopasuj ETS metodą auto i sprawdź residua."),
            tags$li("Narysuj ACF residuów. Czy anomalia zostawiła ślad?"),
            tags$li("Opisz, jak poradziłbyś sobie z anomalią w modelu produkcyjnym.")
          ),
          lc_spacer("md"),
          figure_panel(
            label = "Ryc. 16.3", title = "Ćwiczenie: temperatura fermentacji → ETS + anomalia",
            full_width = TRUE,
            fluidRow(
              column(4,
                actionButton("ch16_f_run", "Dopasuj ETS", class = "lc-btn-primary", width = "100%"),
                uiOutput("ch16_f_result")
              ),
              column(8,
                fluidRow(
                  column(6, zoom_plot_ui("ch16_f_ts",   height = "200px")),
                  column(6, zoom_plot_ui("ch16_f_resid", height = "200px"))
                )
              )
            )
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch16_server <- function(input, output, session) {

  # ---- Ćwiczenie Rolnictwo --------------------------------------------------

  ch16_r_fit <- reactiveVal(NULL)

  observeEvent(input$ch16_r_run, {
    ts_obj <- .ts_datasets[["pszenica"]]$get_ts()
    p  <- as.integer(input$ch16_r_p)
    d  <- as.integer(input$ch16_r_d)
    q  <- as.integer(input$ch16_r_q)
    h  <- input$ch16_r_h
    fit <- tryCatch(
      forecast::forecast(forecast::Arima(ts_obj, order = c(p, d, q)), h = h),
      error = function(e) forecast::forecast(forecast::auto.arima(ts_obj), h = h)
    )
    ch16_r_fit(list(fit = fit, key = "pszenica"))
  })

  output$ch16_r_result <- renderUI({
    res <- ch16_r_fit()
    if (is.null(res)) return(lc_feedback(type = "info", p("Kliknij 'Dopasuj i prognozuj'.")))
    fit <- res$fit
    acc <- forecast::accuracy(fit)
    lb  <- tryCatch(Box.test(residuals(fit), lag = 12, type = "Ljung-Box"), error = function(e) NULL)
    lc_stack(
      lc_stat_grid(
        lc_stat_box("RMSE", round(acc[1, "RMSE"], 1), color = upwr_accent),
        lc_stat_box("MAPE", paste0(round(acc[1, "MAPE"], 1), "%"), color = upwr_secondary),
        columns = 2
      ),
      if (!is.null(lb)) lc_feedback(
        type = if (lb$p.value >= 0.05) "ok" else "warning",
        paste0("Ljung-Box(12): p = ", format_p_value(lb$p.value))
      )
    )
  })

  zoom_plot_server("ch16_r_plot", reactive({
    res <- ch16_r_fit()
    if (is.null(res)) {
      ts_obj <- .ts_datasets[["pszenica"]]$get_ts()
      df <- ts_to_df(ts_obj, "cena")
      return(ggplot(df, aes(x = date, y = cena)) +
        geom_line(color = upwr_secondary, linewidth = 0.9) +
        labs(x = NULL, y = "PLN/dt", title = "Ceny pszenicy skupu") + theme_upwr())
    }
    ts_obj <- .ts_datasets[[res$key]]$get_ts()
    plot_forecast_gg(ts_obj, res$fit, value_label = "PLN/dt") +
      labs(title = "Prognoza cen pszenicy")
  }))

  # ---- Ćwiczenie Bezpieczeństwo ---------------------------------------------

  ch16_b_fit <- reactiveVal(NULL)
  ch16_b_ts  <- local({
    set.seed(123)
    n  <- 10 * 12
    t  <- seq_len(n)
    x  <- 80 + 0.3 * t + 20 * cos(2 * pi * t / 12) + rnorm(n, 0, 5)
    ts(round(x), start = c(2013, 1), frequency = 12)
  })

  observeEvent(input$ch16_b_run, {
    h   <- input$ch16_b_h
    fit <- tryCatch(
      forecast::forecast(forecast::auto.arima(ch16_b_ts), h = h),
      error = function(e) forecast::snaive(ch16_b_ts, h = h)
    )
    ch16_b_fit(fit)
  })

  output$ch16_b_result <- renderUI({
    fit <- ch16_b_fit()
    if (is.null(fit)) return(lc_feedback(type = "info", p("Kliknij 'auto.arima + prognoza'.")))
    acc <- forecast::accuracy(fit)
    lc_feedback(type = "ok",
      tags$strong("Model: "), fit$method,
      tags$br(),
      paste0("RMSE = ", round(acc[1, "RMSE"], 1), ", MAPE = ", round(acc[1, "MAPE"], 1), "%")
    )
  })

  zoom_plot_server("ch16_b_plot", reactive({
    fit <- ch16_b_fit()
    if (is.null(fit)) {
      df <- ts_to_df(ch16_b_ts, "wypadki")
      return(ggplot(df, aes(x = date, y = wypadki)) +
        geom_line(color = upwr_secondary, linewidth = 0.9) +
        labs(x = NULL, y = "liczba wypadków", title = "Wypadki przy pracy (syntetyczne)") + theme_upwr())
    }
    plot_forecast_gg(ch16_b_ts, fit, value_label = "liczba") +
      labs(title = "Wypadki przy pracy — prognoza SARIMA")
  }))

  # ---- Ćwiczenie Technologia żywności ----------------------------------------

  ch16_f_fit <- reactiveVal(NULL)
  ch16_f_ts  <- local({
    as.ts(ts_gen_fermentation(n = 120, seed = 42))
  })

  observeEvent(input$ch16_f_run, {
    fit <- tryCatch(forecast::ets(ch16_f_ts), error = function(e) NULL)
    ch16_f_fit(fit)
  })

  output$ch16_f_result <- renderUI({
    fit <- ch16_f_fit()
    if (is.null(fit)) return(lc_feedback(type = "info", p("Kliknij 'Dopasuj ETS'.")))
    lb  <- tryCatch(Box.test(residuals(fit), lag = 12, type = "Ljung-Box"), error = function(e) NULL)
    ok  <- if (!is.null(lb)) lb$p.value >= 0.05 else TRUE
    lc_stack(
      lc_feedback(type = "ok", tags$strong("Model ETS: "), fit$method),
      if (!is.null(lb)) lc_feedback(
        type = if (ok) "ok" else "warning",
        paste0("Ljung-Box: p = ", format_p_value(lb$p.value)),
        tags$br(),
        if (!ok) "Residua mają wzorzec — anomalia przy t=45-47 pozostawiła ślad." else "Residua OK."
      )
    )
  })

  zoom_plot_server("ch16_f_ts", reactive({
    x  <- as.numeric(ch16_f_ts)
    df <- data.frame(t = seq_along(x), x = x)
    ggplot(df, aes(x = t, y = x)) +
      geom_line(color = upwr_secondary, linewidth = 0.8) +
      annotate("rect", xmin = 44.5, xmax = 47.5, ymin = -Inf, ymax = Inf,
               fill = upwr_accent, alpha = 0.2) +
      labs(x = "Krok (godziny)", y = "Temperatura (°C)",
           title = "Temperatura fermentacji") +
      theme_upwr()
  }))

  zoom_plot_server("ch16_f_resid", reactive({
    fit <- ch16_f_fit()
    if (is.null(fit)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
               label = "Kliknij 'Dopasuj ETS'", color = upwr_reference, size = 4) + theme_upwr())
    }
    e  <- as.numeric(residuals(fit))
    plot_acf_gg(e, lag.max = 24, title = "ACF residuów ETS")
  }))
}
