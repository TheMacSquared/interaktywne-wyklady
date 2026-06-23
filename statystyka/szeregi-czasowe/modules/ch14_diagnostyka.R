# ============================================================================
# CHAPTER 14: Diagnostyka modelu
# ============================================================================

ch14_ui <- list(
  id    = "ch-diagnostyka",
  num   = "14",
  title = "Diagnostyka modelu",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 14 · Szeregi czasowe",
      num    = "14",
      title  = "Diagnostyka modelu.",
      lead   = "Dobrze dopasowany model zostawia residua wyglądające jak biały szum.
                Jeśli w residuach nadal widać wzorzec — model czegoś nie wychwycił."
    ),

    lc_h2("ch14-workflow", "Krok po kroku: workflow diagnostyczny"),

    tagList(
      tags$ol(
        tags$li("Dopasuj model (ARIMA lub ETS)."),
        tags$li("Wyodrębnij residua: ", withMathJax("\\(\\hat{e}_t = x_t - \\hat{x}_t\\)"), "."),
        tags$li("Narysuj residua w czasie — szukaj trendów, skupisk, zmieniającej się wariancji."),
        tags$li("Narysuj ACF residuów — czy lagi mieszczą się w przedziale ufności?"),
        tags$li("Wykonaj test Ljung-Box (p-value < 0,05 → problem)."),
        tags$li("Narysuj QQ-plot i histogram residuów — ocena normalności.")
      ),
      margin_callout(label = "Dlaczego to ważne?", color = "wskazowka",
        "Jeśli residua nie są białym szumem, model systematycznie się myli —
         pozostały wzorzec, którego nie uchwycił. Przedziały ufności prognozy będą za wąskie."
      )
    ),

    lc_h2("ch14-quad-plot", "Quad-plot diagnostyczny"),

    figure_panel(
      label = "Ryc. 14.1", title = "Cztery wykresy diagnostyczne naraz",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch14_data", "Szereg:",
                      choices = .ts_choices_for("bezrobocie", "noclegi", "sprzedaz", "warszawa"),
                      selected = "bezrobocie"),
          selectInput("ch14_method", "Model:",
                      choices = c("ETS (auto)" = "ets", "ARIMA (auto)" = "arima"),
                      selected = "arima"),
          actionButton("ch14_run", "Dopasuj i diagnozuj", class = "lc-btn-primary", width = "100%"),
          uiOutput("ch14_verdict")
        ),
        column(8,
          fluidRow(
            column(6, zoom_plot_ui("ch14_resid_ts",   height = "180px")),
            column(6, zoom_plot_ui("ch14_resid_acf",  height = "180px"))
          ),
          fluidRow(
            column(6, zoom_plot_ui("ch14_resid_qq",   height = "180px")),
            column(6, zoom_plot_ui("ch14_resid_hist", height = "180px"))
          )
        )
      )
    ),

    lc_h2("ch14-ljung-box", "Test Ljung-Box — ile lagów?"),

    tagList(
      lc_p("Test Ljung-Box sprawdza zbiorowo, czy autokorelacje residuów przy lagach 1, …, K
        są istotnie różne od zera. Suwak K pozwala obserwować, jak p-value zmienia się z liczbą testowanych lagów."),
      lc_formula_box(
        withMathJax(helpText("$$Q(K) = n(n+2) \\sum_{k=1}^{K} \\frac{\\hat{\\rho}_k^2}{n-k}$$")),
        p(tags$strong("H₀:"), " residua są białym szumem (brak autokorelacji do lagu K)."),
        p("Odrzucamy H₀ jeśli Q(K) > χ²(K) lub p-value < 0,05.")
      )
    ),

    figure_panel(
      label = "Ryc. 14.2", title = "p-value testu Ljung-Box vs. liczba testowanych lagów",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch14_lb_lag", "Maksymalna liczba lagów K:", min = 5, max = 30, value = 12, step = 1),
          uiOutput("ch14_lb_result")
        ),
        column(8,
          zoom_plot_ui("ch14_lb_plot", height = "260px")
        )
      )
    ),

    lc_h2("ch14-porownanie", "Za prosty vs. dobry vs. przefiltrowany"),

    tagList(
      lc_p("Trzy różne modele na tych samych danych — jak wygląda ACF residuów?"),
      tags$ul(
        tags$li(tags$strong("Za prosty"), " (np. naiwna sezonowa): wyraźny wzorzec w ACF residuów."),
        tags$li(tags$strong("Dobry"), " (ARIMA/ETS): residua ≈ biały szum."),
        tags$li(tags$strong("Przefiltrowany"), " (zbyt wiele parametrów): AIC gorszy, residua OK, ale prognoza niestabilna.")
      )
    ),

    figure_panel(
      label = "Ryc. 14.3", title = "Porównanie ACF residuów: trzy modele",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch14_cmp_data", "Szereg:",
                      choices = .ts_choices_for("bezrobocie", "noclegi"),
                      selected = "bezrobocie"),
          actionButton("ch14_cmp_run", "Porównaj", class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          fluidRow(
            column(4, h6("Za prosty", style = "text-align:center"),
              zoom_plot_ui("ch14_cmp_simple", height = "200px")),
            column(4, h6("Dobry", style = "text-align:center"),
              zoom_plot_ui("ch14_cmp_good",   height = "200px")),
            column(4, h6("Przefiltrowany", style = "text-align:center"),
              zoom_plot_ui("ch14_cmp_over",   height = "200px"))
          )
        )
      )
    ),

    lc_chapter_next(
      num       = "15",
      title     = "Anomalie i zdarzenia strukturalne",
      lead      = "COVID w noclegach, pszenica 2022, ARIMAX z dummy",
      target_id = "ch-anomalie"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch14_server <- function(input, output, session) {

  ch14_resid <- reactiveVal(NULL)
  ch14_ts    <- reactiveVal(NULL)

  observeEvent(input$ch14_run, {
    key    <- input$ch14_data
    method <- input$ch14_method
    ts_obj <- .ts_datasets[[key]]$get_ts()
    fit <- tryCatch({
      switch(method,
        ets   = forecast::ets(ts_obj),
        arima = forecast::auto.arima(ts_obj)
      )
    }, error = function(e) forecast::ets(ts_obj))
    ch14_resid(as.numeric(residuals(fit)))
    ch14_ts(ts_obj)
  })

  zoom_plot_server("ch14_resid_ts", reactive({
    e <- ch14_resid()
    if (is.null(e)) return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Kliknij 'Dopasuj'",
                                                color = upwr_reference, size = 4) + theme_upwr())
    df <- data.frame(t = seq_along(e), e = e)
    ggplot(df, aes(x = t, y = e)) +
      geom_line(color = upwr_secondary, linewidth = 0.7) +
      geom_hline(yintercept = 0, color = upwr_reference, linetype = "dashed") +
      labs(x = "Czas", y = "Residuum", title = "Residua w czasie") +
      theme_upwr()
  }))

  zoom_plot_server("ch14_resid_acf", reactive({
    e <- ch14_resid()
    if (is.null(e)) return(ggplot() + theme_upwr())
    plot_acf_gg(e, lag.max = 24, title = "ACF residuów")
  }))

  zoom_plot_server("ch14_resid_qq", reactive({
    e <- ch14_resid()
    if (is.null(e)) return(ggplot() + theme_upwr())
    df <- data.frame(sample = sort(e))
    ggplot(df, aes(sample = sample)) +
      stat_qq(color = upwr_accent, size = 1.5, alpha = 0.7) +
      stat_qq_line(color = upwr_secondary) +
      labs(x = "Kwantyle teoretyczne", y = "Kwantyle próbkowe", title = "QQ-plot") +
      theme_upwr()
  }))

  zoom_plot_server("ch14_resid_hist", reactive({
    e <- ch14_resid()
    if (is.null(e)) return(ggplot() + theme_upwr())
    df <- data.frame(e = e)
    ggplot(df, aes(x = e)) +
      geom_histogram(aes(y = after_stat(density)), bins = 20,
                     fill = upwr_accent, alpha = 0.7, color = "white") +
      stat_function(fun = dnorm, args = list(mean = 0, sd = sd(e, na.rm = TRUE)),
                    color = upwr_secondary, linewidth = 1.1) +
      labs(x = "Residuum", y = "Gęstość", title = "Histogram + N(0,σ²)") +
      theme_upwr()
  }))

  output$ch14_verdict <- renderUI({
    e <- ch14_resid()
    if (is.null(e)) return(lc_feedback(type = "info", p("Kliknij 'Dopasuj i diagnozuj'.")))
    lb <- tryCatch(Box.test(e, lag = 12, type = "Ljung-Box"), error = function(err) NULL)
    if (is.null(lb)) return(NULL)
    ok <- lb$p.value >= 0.05
    lc_feedback(type = if (ok) "ok" else "warning",
      tags$strong(if (ok) "✓ Residua: biały szum" else "✗ Residua: wzorzec pozostał"),
      tags$br(),
      paste0("Ljung-Box Q(12): p = ", format_p_value(lb$p.value))
    )
  })

  ch14_lb_e <- reactive({
    ch14_resid()
  })

  zoom_plot_server("ch14_lb_plot", reactive({
    e <- ch14_lb_e()
    k_max <- if (!is.null(input$ch14_lb_lag)) input$ch14_lb_lag else 12
    if (is.null(e)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
               label = "Najpierw dopasuj model w górnej sekcji", color = upwr_reference, size = 4) + theme_upwr())
    }
    pvals <- sapply(seq_len(k_max), function(k) {
      tryCatch(Box.test(e, lag = k, type = "Ljung-Box")$p.value, error = function(e2) NA)
    })
    df <- data.frame(k = seq_len(k_max), p = pvals)
    ggplot(df, aes(x = k, y = p)) +
      geom_hline(yintercept = 0.05, color = upwr_accent, linetype = "dashed", linewidth = 0.8) +
      geom_line(color = upwr_secondary, linewidth = 0.8) +
      geom_point(aes(color = p < 0.05), size = 3) +
      scale_color_manual(values = c("FALSE" = upwr_cat["szalwia"], "TRUE" = upwr_accent),
                         labels = c("FALSE" = "OK (p≥0,05)", "TRUE" = "Problem (p<0,05)"),
                         name = NULL) +
      scale_x_continuous(breaks = seq(1, k_max, by = 2)) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "Lag K", y = "p-value Ljung-Box",
           title = "p-value testu Ljung-Box (linia przerywana = 0,05)") +
      theme_upwr()
  }))

  output$ch14_lb_result <- renderUI({
    e <- ch14_lb_e()
    k_max <- if (!is.null(input$ch14_lb_lag)) input$ch14_lb_lag else 12
    if (is.null(e)) return(NULL)
    lb <- tryCatch(Box.test(e, lag = k_max, type = "Ljung-Box"), error = function(e2) NULL)
    if (is.null(lb)) return(NULL)
    ok <- lb$p.value >= 0.05
    lc_feedback(type = if (ok) "ok" else "warning",
      tags$strong(paste0("Q(", k_max, "): ")),
      paste0("p = ", format_p_value(lb$p.value), if (ok) " — OK" else " — zostały zależności")
    )
  })

  ch14_cmp_resids <- reactiveVal(NULL)

  observeEvent(input$ch14_cmp_run, {
    key    <- input$ch14_cmp_data
    ts_obj <- .ts_datasets[[key]]$get_ts()
    simple_fit <- forecast::snaive(ts_obj, h = 1)
    good_fit   <- tryCatch(forecast::auto.arima(ts_obj), error = function(e) forecast::ets(ts_obj))
    over_fit   <- tryCatch(
      forecast::Arima(ts_obj, order = c(3, 1, 3), seasonal = c(1, 1, 1)),
      error = function(e) good_fit
    )
    simple_r <- as.numeric(na.omit(simple_fit$residuals))
    good_r   <- as.numeric(residuals(good_fit))
    over_r   <- as.numeric(residuals(over_fit))
    ch14_cmp_resids(list(simple = simple_r, good = good_r, over = over_r))
  })

  .make_cmp_acf <- function(key) {
    reactive({
      resids <- ch14_cmp_resids()
      if (is.null(resids)) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                 label = "Kliknij 'Porównaj'", color = upwr_reference, size = 3.5) + theme_upwr())
      }
      e <- resids[[key]]
      plot_acf_gg(e, lag.max = 24, title = NULL)
    })
  }

  zoom_plot_server("ch14_cmp_simple", .make_cmp_acf("simple"))
  zoom_plot_server("ch14_cmp_good",   .make_cmp_acf("good"))
  zoom_plot_server("ch14_cmp_over",   .make_cmp_acf("over"))
}
