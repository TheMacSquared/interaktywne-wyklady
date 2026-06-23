# ============================================================================
# CHAPTER 10: ARIMA i SARIMA
# ============================================================================

ch10_ui <- list(
  id    = "ch-arima",
  num   = "10",
  title = "ARIMA i SARIMA",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 10 · Szeregi czasowe",
      num    = "10",
      title  = "ARIMA i SARIMA.",
      lead   = "AR + I (różnicowanie) + MA = ARIMA. Dodaj sezonowość → SARIMA.
                To jest standardowy model szeregów niestacjonarnych."
    ),

    lc_h2("ch10-skladanie", "Składamy ARIMA(p, d, q)"),

    tagList(
      lc_p("ARIMA(p, d, q) to rozszerzenie ARMA o różnicowanie d-rzędu:"),
      tags$ul(
        tags$li(withMathJax("\\(p\\)"), " — rząd AR (ile przeszłych wartości)"),
        tags$li(withMathJax("\\(d\\)"), " — rząd różnicowania (ile razy różnicujemy)"),
        tags$li(withMathJax("\\(q\\)"), " — rząd MA (ile przeszłych błędów)")
      ),
      lc_formula_box(
        withMathJax(helpText("$$\\nabla^d x_t = \\phi_1 \\nabla^d x_{t-1} + \\cdots + \\phi_p \\nabla^d x_{t-p} + \\varepsilon_t + \\theta_1 \\varepsilon_{t-1} + \\cdots + \\theta_q \\varepsilon_{t-q}$$")),
        p("ARIMA(1,1,0): pierwszy raz różnicujemy, potem AR(1) na różnicach."),
        p("ARIMA(0,1,1): pierwszy raz różnicujemy, potem MA(1) na różnicach.")
      ),
      inline_callout(label = "Chcesz więcej matematyki?", color = "wskazowka", open = FALSE,
        p("ARIMA(1,1,1) explicite: ", withMathJax("\\((1-\\phi_1 B)(1-B)x_t = (1+\\theta_1 B)\\varepsilon_t\\)")),
        p("gdzie B to operator opóźnienia: ", withMathJax("\\(B x_t = x_{t-1}\\)"), "."),
        p("Szczególne przypadki: ARIMA(0,1,0) = random walk; ARIMA(0,0,0) = biały szum; ARIMA(p,0,0) = AR(p); ARIMA(0,0,q) = MA(q).")
      )
    ),

    lc_h2("ch10-picker", "Picker ARIMA(p,d,q)"),

    figure_panel(
      label = "Ryc. 10.1", title = "Wybierz parametry i obserwuj symulowany szereg",
      full_width = TRUE,
      fluidRow(
        column(4,
          div(style = "display: flex; gap: 8px;",
            numericInput("ch10_p", "p:", value = 1, min = 0, max = 3, step = 1, width = "70px"),
            numericInput("ch10_d", "d:", value = 1, min = 0, max = 2, step = 1, width = "70px"),
            numericInput("ch10_q", "q:", value = 0, min = 0, max = 3, step = 1, width = "70px")
          ),
          actionButton("ch10_sim_new", "Nowa realizacja", class = "lc-btn-primary", width = "100%"),
          lc_spacer("md"),
          uiOutput("ch10_model_desc")
        ),
        column(8,
          zoom_plot_ui("ch10_ts_plot",   height = "200px"),
          fluidRow(
            column(6, zoom_plot_ui("ch10_acf_plot",  height = "160px")),
            column(6, zoom_plot_ui("ch10_pacf_plot", height = "160px"))
          )
        )
      )
    ),

    lc_h2("ch10-auto-arima", "auto.arima: automatyczny wybór modelu"),

    tagList(
      lc_p("Zamiast ręcznie próbować dziesiątek kombinacji p/d/q, pakiet ",
        tags$code("forecast"), " oferuje funkcję ", tags$code("auto.arima()"),
        ", która przeszukuje przestrzeń modeli i wybiera najlepszy według kryterium AIC."),
      margin_code_note(
        code = 'library(forecast)\nauto.arima(x)',
        description = "Automatycznie dobiera p, d, q i sezonowe P, D, Q."
      )
    ),

    figure_panel(
      label = "Ryc. 10.2", title = "auto.arima na wybranych danych",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch10_auto_data", "Szereg:",
                      choices = .ts_dataset_choices[c("warszawa", "bezrobocie", "noclegi", "pszenica")],
                      selected = "bezrobocie"),
          actionButton("ch10_auto_run", "Uruchom auto.arima", class = "lc-btn-primary", width = "100%"),
          uiOutput("ch10_auto_result")
        ),
        column(8,
          zoom_plot_ui("ch10_auto_plot", height = "280px")
        )
      )
    ),

    lc_h2("ch10-sarima", "SARIMA: sezonowe rozszerzenie"),

    tagList(
      lc_p("Dla danych z sezonowością (miesięczne, kwartalne) używamy SARIMA(p,d,q)(P,D,Q)[s]:"),
      tags$ul(
        tags$li("(p,d,q) — parametry niesezonowe (jak w ARIMA)"),
        tags$li("(P,D,Q) — sezonowe odpowiedniki p,d,q"),
        tags$li("[s] — okres sezonowości (12 dla danych miesięcznych)")
      ),
      lc_formula_box(
        p(tags$strong("Przykład:"), " bezrobocie PL → SARIMA(1,1,1)(0,1,1)[12]"),
        p("Znaczy: AR(1) + różnicowanie + MA(1) + sezonowe MA(1) z różnicowaniem rocznym.")
      )
    ),

    figure_panel(
      label = "Ryc. 10.3", title = "Porównanie kandydatów — tabela AIC/BIC",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch10_aic_data", "Szereg:",
                      choices = .ts_dataset_choices[c("bezrobocie", "noclegi", "sprzedaz")],
                      selected = "bezrobocie"),
          actionButton("ch10_aic_run", "Porównaj modele", class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          uiOutput("ch10_aic_table")
        )
      )
    ),

    lc_chapter_next(
      num       = "11",
      title     = "Wygładzanie wykładnicze: ETS",
      lead      = "alternatywa dla ARIMA — intuicja α, Holt, Holt-Winters",
      target_id = "ch-ets"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch10_server <- function(input, output, session) {

  ch10_sim_seed <- reactiveVal(42)
  observeEvent(input$ch10_sim_new, ch10_sim_seed(ch10_sim_seed() + 1))

  ch10_sim_ts <- reactive({
    p <- if (!is.null(input$ch10_p)) as.integer(input$ch10_p) else 1L
    d <- if (!is.null(input$ch10_d)) as.integer(input$ch10_d) else 1L
    q <- if (!is.null(input$ch10_q)) as.integer(input$ch10_q) else 0L
    set.seed(ch10_sim_seed())
    ar_c <- if (p > 0) rep(0.6 / p, p) else NULL
    ma_c <- if (q > 0) rep(0.5 / q, q) else NULL
    model <- list()
    if (!is.null(ar_c)) model$ar <- ar_c
    if (!is.null(ma_c)) model$ma <- ma_c
    tryCatch({
      base <- if (length(model) > 0) arima.sim(model, n = 200 + d) else rnorm(200 + d)
      if (d > 0) for (i in seq_len(d)) base <- cumsum(base)
      head(as.numeric(base), 200)
    }, error = function(e) rnorm(200))
  })

  zoom_plot_server("ch10_ts_plot", reactive({
    x  <- ch10_sim_ts()
    p  <- if (!is.null(input$ch10_p)) input$ch10_p else 1
    d  <- if (!is.null(input$ch10_d)) input$ch10_d else 1
    q  <- if (!is.null(input$ch10_q)) input$ch10_q else 0
    df <- data.frame(t = seq_along(x), x = x)
    ggplot(df, aes(x = t, y = x)) +
      geom_line(color = upwr_secondary, linewidth = 0.7) +
      labs(x = "Czas", y = "x_t",
           title = paste0("ARIMA(", p, ",", d, ",", q, ") — symulacja")) +
      theme_upwr()
  }))

  zoom_plot_server("ch10_acf_plot",  reactive(plot_acf_gg(ch10_sim_ts(), lag.max = 20, title = "ACF")))
  zoom_plot_server("ch10_pacf_plot", reactive(plot_acf_gg(ch10_sim_ts(), lag.max = 20, type = "PACF", title = "PACF")))

  output$ch10_model_desc <- renderUI({
    p <- if (!is.null(input$ch10_p)) input$ch10_p else 1
    d <- if (!is.null(input$ch10_d)) input$ch10_d else 1
    q <- if (!is.null(input$ch10_q)) input$ch10_q else 0
    special <- switch(paste0(p, d, q),
      "010" = "Random walk — ceny akcji, kurs walutowy.",
      "000" = "Biały szum — czysto losowy.",
      "011" = "IMA(1,1) — ważony EWMA. Często naturalnie pojawia się przy niezakłóconym trendzie.",
      "110" = "ARI(1,1) — AR(1) na pierwszych różnicach.",
      NULL
    )
    msg <- paste0("ARIMA(", p, ",", d, ",", q, ")")
    if (!is.null(special)) {
      lc_feedback(type = "info", tags$strong(msg), tags$br(), special)
    } else {
      lc_feedback(type = "info", tags$strong(msg))
    }
  })

  ch10_auto_model <- reactiveVal(NULL)
  ch10_auto_key   <- reactiveVal(NULL)

  observeEvent(input$ch10_auto_run, {
    key    <- input$ch10_auto_data
    ts_obj <- .ts_datasets[[key]]$get_ts()
    fit    <- tryCatch(forecast::auto.arima(ts_obj, stepwise = FALSE, approximation = FALSE),
                       error = function(e) forecast::auto.arima(ts_obj))
    ch10_auto_model(fit)
    ch10_auto_key(key)
  })

  output$ch10_auto_result <- renderUI({
    fit <- ch10_auto_model()
    if (is.null(fit)) {
      return(lc_feedback(type = "info", p("Kliknij 'Uruchom auto.arima', żeby dopasować model.")))
    }
    coefs <- broom::tidy(fit)
    lc_stack(
      lc_feedback(type = "ok",
        tags$strong("Wybrany model: "),
        fit$method
      ),
      lc_stat_grid(
        lc_stat_box("AIC",  round(fit$aic,  2), color = upwr_accent),
        lc_stat_box("BIC",  round(fit$bic,  2), color = upwr_secondary),
        lc_stat_box("Log L", round(fit$loglik, 1), color = upwr_secondary),
        columns = 3
      )
    )
  })

  zoom_plot_server("ch10_auto_plot", reactive({
    fit <- ch10_auto_model()
    key <- ch10_auto_key()
    if (is.null(fit)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "Kliknij 'Uruchom auto.arima'",
                                  color = upwr_reference, size = 5) + theme_upwr())
    }
    fc   <- forecast::forecast(fit, h = 24)
    info <- .ts_datasets[[key]]
    plot_forecast_gg(info$get_ts(), fc, value_label = info$unit) +
      labs(title = paste0("Prognoza ARIMA — ", info$label))
  }))

  ch10_aic_results <- reactiveVal(NULL)

  observeEvent(input$ch10_aic_run, {
    key    <- input$ch10_aic_data
    ts_obj <- .ts_datasets[[key]]$get_ts()
    candidates <- list(
      list(p=0, d=1, q=0, P=0, D=1, Q=0),
      list(p=1, d=1, q=0, P=0, D=1, Q=0),
      list(p=0, d=1, q=1, P=0, D=1, Q=0),
      list(p=1, d=1, q=1, P=0, D=1, Q=0),
      list(p=1, d=1, q=0, P=0, D=1, Q=1),
      list(p=1, d=1, q=1, P=0, D=1, Q=1)
    )
    results <- lapply(candidates, function(c) {
      tryCatch({
        fit <- forecast::Arima(ts_obj,
                               order  = c(c$p, c$d, c$q),
                               seasonal = c(c$P, c$D, c$Q))
        data.frame(
          Model = paste0("SARIMA(", c$p, ",", c$d, ",", c$q,
                         ")(", c$P, ",", c$D, ",", c$Q, ")[12]"),
          AIC = round(fit$aic, 2),
          BIC = round(fit$bic, 2)
        )
      }, error = function(e) NULL)
    })
    df_aic <- do.call(rbind, Filter(Negate(is.null), results))
    df_aic <- df_aic[order(df_aic$AIC), ]
    df_aic$Ranking <- seq_len(nrow(df_aic))
    ch10_aic_results(df_aic)
  })

  output$ch10_aic_table <- renderUI({
    df <- ch10_aic_results()
    if (is.null(df)) {
      return(lc_feedback(type = "info", p("Kliknij 'Porównaj modele', żeby zobaczyć ranking AIC/BIC.")))
    }
    tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
      tags$thead(tags$tr(
        tags$th("#"), tags$th("Model"), tags$th("AIC"), tags$th("BIC")
      )),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          bold <- if (i == 1) "font-weight:bold; color:var(--upwr-accent);" else ""
          tags$tr(
            tags$td(style = bold, df$Ranking[i]),
            tags$td(style = bold, df$Model[i]),
            tags$td(style = bold, df$AIC[i]),
            tags$td(style = bold, df$BIC[i])
          )
        })
      )
    )
  })
}
