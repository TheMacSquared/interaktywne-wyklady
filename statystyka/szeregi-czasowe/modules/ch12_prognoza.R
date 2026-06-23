# ============================================================================
# CHAPTER 12: Prognozowanie — horyzont i niepewność
# ============================================================================

ch12_ui <- list(
  id    = "ch-prognoza",
  num   = "12",
  title = "Prognozowanie: horyzont i niepewność",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 12 · Szeregi czasowe",
      num    = "12",
      title  = "Prognozowanie.",
      lead   = "Każda prognoza to twierdzenie o przyszłości z pewnym marginesem błędu.
                Wachlarz rośnie z horyzontem — to nieuniknione."
    ),

    lc_h2("ch12-wachlarz", "Główny widget — porównanie metod"),

    tagList(
      lc_p("Wybierz metodę prognozowania, dane i horyzont. Obserwuj, jak rośnie wachlarz przedziałów ufności z każdym krokiem w przyszłość.")
    ),

    figure_panel(
      label = "Ryc. 12.1", title = "Prognoza z wachlarzem przedziałów ufności",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch12_data", "Szereg:",
                      choices = .ts_choices_for("warszawa", "noclegi", "bezrobocie", "sprzedaz"),
                      selected = "noclegi"),
          selectInput("ch12_method", "Metoda:",
                      choices = c(
                        "Naiwna"             = "naive",
                        "Naiwna sezonowa"    = "snaive",
                        "ETS (auto)"         = "ets",
                        "ARIMA (auto)"       = "arima"
                      ),
                      selected = "ets"),
          sliderInput("ch12_horizon", "Horyzont (miesiące):", min = 1, max = 36, value = 12, step = 1),
          radioButtons("ch12_ci", "Przedziały ufności:",
                       choices = c("80% i 95%" = "both", "Tylko 95%" = "only95", "Brak" = "none"),
                       selected = "both"),
          actionButton("ch12_run", "Prognozuj", class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch12_forecast_plot", height = "320px")
        )
      )
    ),

    lc_h2("ch12-wachlarz-animacja", "Jak rośnie niepewność z horyzontem?"),

    tagList(
      lc_p("Przedziały ufności rosną z horyzontem proporcjonalnie do ", withMathJax("\\(\\sqrt{h}\\)"),
        " (dla ARIMA z niezależnymi błędami). Dlatego prognoza na 12 miesięcy
         jest istotnie mniej pewna niż na 3 miesiące."),
      lc_formula_box(
        withMathJax(helpText("$$\\text{Szerokość PI} \\propto \\hat{\\sigma} \\cdot z_{\\alpha/2} \\cdot \\sqrt{h}$$")),
        p("gdzie ", withMathJax("\\(z_{\\alpha/2}\\)"), " to kwantyl normalny (≈ 1,96 dla 95%),
          h to horyzont, σ̂ to oszacowane odchylenie standardowe reszt.")
      )
    ),

    figure_panel(
      label = "Ryc. 12.2", title = "Szerokość przedziału ufności vs. horyzont",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch12_fan_data", "Szereg:",
                      choices = .ts_choices_for("noclegi", "warszawa", "bezrobocie"),
                      selected = "warszawa"),
          actionButton("ch12_fan_run", "Przelicz", class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch12_fan_plot", height = "280px")
        )
      )
    ),

    lc_h2("ch12-odslon", "'Odsłoń przyszłość' — ocena trafności"),

    tagList(
      lc_p("Jedna z najprostszych form walidacji: ukryj ostatnie obserwacje i sprawdź,
        czy prognoza 'trafiała' w ukryte dane. Suwak split_point dzieli dane na uczący i testowy.")
    ),

    figure_panel(
      label = "Ryc. 12.3", title = "Prognoza vs. ukryte dane rzeczywiste",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch12_reveal_data", "Szereg:",
                      choices = .ts_choices_for("noclegi", "bezrobocie", "sprzedaz", "warszawa"),
                      selected = "bezrobocie"),
          selectInput("ch12_reveal_method", "Metoda:",
                      choices = c(
                        "Naiwna sezonowa" = "snaive",
                        "ETS (auto)"      = "ets",
                        "ARIMA (auto)"    = "arima"
                      ),
                      selected = "ets"),
          sliderInput("ch12_test_pct", "Procent danych testowych:", min = 10, max = 30, value = 20, step = 5),
          actionButton("ch12_reveal_run", "Odsłoń", class = "lc-btn-primary", width = "100%"),
          uiOutput("ch12_reveal_result")
        ),
        column(8,
          zoom_plot_ui("ch12_reveal_plot", height = "300px")
        )
      )
    ),

    lc_chapter_next(
      num       = "13",
      title     = "Ocena dokładności prognozy",
      lead      = "MAE, RMSE, MAPE, MASE — tabela i heatmap",
      target_id = "ch-dokladnosc"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch12_server <- function(input, output, session) {

  ch12_fit     <- reactiveVal(NULL)
  ch12_fit_key <- reactiveVal(NULL)

  observeEvent(input$ch12_run, {
    key    <- input$ch12_data
    method <- input$ch12_method
    h      <- input$ch12_horizon
    ts_obj <- .ts_datasets[[key]]$get_ts()
    fit <- tryCatch({
      switch(method,
        naive  = forecast::naive(ts_obj,  h = h),
        snaive = forecast::snaive(ts_obj, h = h),
        ets    = forecast::forecast(forecast::ets(ts_obj), h = h),
        arima  = forecast::forecast(forecast::auto.arima(ts_obj), h = h)
      )
    }, error = function(e) forecast::naive(ts_obj, h = h))
    ch12_fit(fit)
    ch12_fit_key(key)
  })

  zoom_plot_server("ch12_forecast_plot", reactive({
    fit <- ch12_fit()
    key <- ch12_fit_key()
    ci  <- if (!is.null(input$ch12_ci)) input$ch12_ci else "both"
    if (is.null(fit)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
               label = "Wybierz parametry i kliknij 'Prognozuj'",
               color = upwr_reference, size = 4.5) + theme_upwr())
    }
    ts_obj <- .ts_datasets[[key]]$get_ts()
    method_lbl <- forecast_method_label(input$ch12_method)

    n     <- length(ts_obj)
    t_obs <- seq_len(n)
    t_fc  <- n + seq_along(fit$mean)
    df_obs <- data.frame(t = t_obs, value = as.numeric(ts_obj), typ = "Dane historyczne")
    df_fc  <- data.frame(t = t_fc, value = as.numeric(fit$mean), typ = "Prognoza")
    df_all <- rbind(df_obs, df_fc)

    p <- ggplot(df_all, aes(x = t, y = value, color = typ, linetype = typ)) +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = c("Dane historyczne" = upwr_secondary, "Prognoza" = upwr_accent), name = NULL) +
      scale_linetype_manual(values = c("Dane historyczne" = "solid", "Prognoza" = "dashed"), name = NULL) +
      labs(x = "Czas", y = .ts_datasets[[key]]$unit,
           title = paste0("Prognoza — ", .ts_datasets[[key]]$label, " (", method_lbl, ")")) +
      theme_upwr() +
      theme(legend.position = "bottom")

    if (ci != "none" && !is.null(fit$lower)) {
      if (ci %in% c("both", "only95")) {
        ci95 <- data.frame(t = t_fc, lo = as.numeric(fit$lower[, 2]), hi = as.numeric(fit$upper[, 2]))
        p <- p + geom_ribbon(data = ci95, aes(x = t, ymin = lo, ymax = hi),
                             inherit.aes = FALSE, fill = upwr_accent, alpha = 0.12)
      }
      if (ci == "both") {
        ci80 <- data.frame(t = t_fc, lo = as.numeric(fit$lower[, 1]), hi = as.numeric(fit$upper[, 1]))
        p <- p + geom_ribbon(data = ci80, aes(x = t, ymin = lo, ymax = hi),
                             inherit.aes = FALSE, fill = upwr_accent, alpha = 0.20)
      }
    }
    p
  }))

  ch12_fan_fit <- reactiveVal(NULL)
  ch12_fan_key <- reactiveVal(NULL)

  observeEvent(input$ch12_fan_run, {
    key    <- input$ch12_fan_data
    ts_obj <- .ts_datasets[[key]]$get_ts()
    fit    <- tryCatch(
      forecast::forecast(forecast::ets(ts_obj), h = 24),
      error = function(e) forecast::naive(ts_obj, h = 24)
    )
    ch12_fan_fit(fit)
    ch12_fan_key(key)
  })

  zoom_plot_server("ch12_fan_plot", reactive({
    fit <- ch12_fan_fit()
    if (is.null(fit)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
               label = "Kliknij 'Przelicz'", color = upwr_reference, size = 5) + theme_upwr())
    }
    sigma_est <- sd(fit$residuals, na.rm = TRUE)
    h <- length(fit$mean)
    df <- data.frame(
      h = seq_len(h),
      szerokosc = 2 * 1.96 * sigma_est * sqrt(seq_len(h))
    )
    ggplot(df, aes(x = h, y = szerokosc)) +
      geom_line(color = upwr_accent, linewidth = 1.2) +
      geom_area(fill = upwr_accent, alpha = 0.15) +
      labs(x = "Horyzont prognozy (miesiące)",
           y = "Szerokość 95% PI",
           title = "Niepewność rośnie z horyzontem ~ √h") +
      theme_upwr()
  }))

  ch12_reveal_fit    <- reactiveVal(NULL)
  ch12_reveal_actual <- reactiveVal(NULL)
  ch12_reveal_train  <- reactiveVal(NULL)
  ch12_reveal_key    <- reactiveVal(NULL)

  observeEvent(input$ch12_reveal_run, {
    key    <- input$ch12_reveal_data
    method <- input$ch12_reveal_method
    pct    <- input$ch12_test_pct / 100
    ts_obj <- .ts_datasets[[key]]$get_ts()
    n      <- length(ts_obj)
    n_test <- round(n * pct)
    n_train <- n - n_test
    train <- ts(as.numeric(ts_obj)[seq_len(n_train)],
                start = start(ts_obj), frequency = frequency(ts_obj))
    actual <- as.numeric(ts_obj)[(n_train + 1):n]
    h <- n_test
    fit <- tryCatch({
      switch(method,
        snaive = forecast::snaive(train, h = h),
        ets    = forecast::forecast(forecast::ets(train), h = h),
        arima  = forecast::forecast(forecast::auto.arima(train), h = h)
      )
    }, error = function(e) forecast::snaive(train, h = h))
    ch12_reveal_fit(fit)
    ch12_reveal_actual(actual)
    ch12_reveal_train(train)
    ch12_reveal_key(key)
  })

  output$ch12_reveal_result <- renderUI({
    fit    <- ch12_reveal_fit()
    actual <- ch12_reveal_actual()
    if (is.null(fit)) return(lc_feedback(type = "info", p("Kliknij 'Odsłoń'.")))
    pred <- as.numeric(fit$mean)
    m    <- compute_accuracy_metrics(actual, pred)
    lc_stat_grid(
      lc_stat_box("MAE",  round(m$mae,  2), color = upwr_accent),
      lc_stat_box("RMSE", round(m$rmse, 2), color = upwr_secondary),
      lc_stat_box("MAPE", paste0(round(m$mape, 1), "%"), color = upwr_secondary),
      columns = 3
    )
  })

  zoom_plot_server("ch12_reveal_plot", reactive({
    fit    <- ch12_reveal_fit()
    actual <- ch12_reveal_actual()
    train  <- ch12_reveal_train()
    key    <- ch12_reveal_key()
    if (is.null(fit)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
               label = "Kliknij 'Odsłoń'", color = upwr_reference, size = 5) + theme_upwr())
    }
    n_train <- length(train)
    n_test  <- length(actual)
    df_train  <- data.frame(t = seq_len(n_train), value = as.numeric(train),  typ = "Dane uczące")
    df_actual <- data.frame(t = n_train + seq_len(n_test), value = actual,     typ = "Dane rzeczywiste (ukryte)")
    df_pred   <- data.frame(t = n_train + seq_len(n_test), value = as.numeric(fit$mean), typ = "Prognoza")
    df_all    <- rbind(df_train, df_actual, df_pred)
    kolory    <- c("Dane uczące" = upwr_secondary,
                   "Dane rzeczywiste (ukryte)" = upwr_cat["szalwia"],
                   "Prognoza" = upwr_accent)
    linie     <- c("Dane uczące" = "solid",
                   "Dane rzeczywiste (ukryte)" = "solid",
                   "Prognoza" = "dashed")
    ggplot(df_all, aes(x = t, y = value, color = typ, linetype = typ)) +
      geom_line(linewidth = 0.9) +
      scale_color_manual(values = kolory, name = NULL) +
      scale_linetype_manual(values = linie, name = NULL) +
      labs(x = "Czas", y = .ts_datasets[[key]]$unit,
           title = paste0("Odsłonięte dane — ", .ts_datasets[[key]]$label)) +
      theme_upwr() +
      theme(legend.position = "bottom")
  }))
}
