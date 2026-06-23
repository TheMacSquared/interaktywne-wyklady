# ============================================================================
# CHAPTER 11: Wygładzanie wykładnicze — ETS
# ============================================================================

ch11_ui <- list(
  id    = "ch-ets",
  num   = "11",
  title = "Wygładzanie wykładnicze: ETS",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 11 · Szeregi czasowe",
      num    = "11",
      title  = "Wygładzanie wykładnicze.",
      lead   = "Stare obserwacje ważymy mniej, nowe — bardziej.
                α kontroluje tempo zapominania. Holt dodaje trend, Holt-Winters sezonowość."
    ),

    lc_h2("ch11-alpha", "Suwak α — tempo zapominania"),

    tagList(
      lc_p("Prosta metoda ETS (błąd–trend–sezon) przypisuje każdej obserwacji wagę malejącą geometrycznie:"),
      lc_formula_box(
        withMathJax(helpText("$$\\hat{x}_{t+1} = \\alpha x_t + \\alpha(1-\\alpha) x_{t-1} + \\alpha(1-\\alpha)^2 x_{t-2} + \\cdots$$")),
        p(withMathJax("\\(\\alpha \\in (0,1)\\)"), " — parametr wygładzania. Im bliżej 1, tym szybciej zapominamy historię.")
      ),
      inline_callout(label = "Chcesz więcej matematyki?", color = "wskazowka", open = FALSE,
        p("Równanie rekurencyjne (iteracyjne wyznaczanie prognozy):"),
        withMathJax(helpText("$$L_t = \\alpha x_t + (1-\\alpha) L_{t-1}$$")),
        p("Prognoza na horyzont h: ", withMathJax("\\(\\hat{x}_{t+h} = L_t\\)"), " (stała dla prostego ETS, bo brak trendu)."),
        p("Przy α → 0: niemal równe wagi (długa pamięć — stabilna prognoza). Przy α → 1: prawie tylko ostatnia obserwacja (krótka pamięć — szybka reakcja).")
      )
    ),

    figure_panel(
      label = "Ryc. 11.1", title = "Wagi historyczne dla różnych α",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch11_alpha", "α (tempo wygładzania):",
                      min = 0.05, max = 0.95, value = 0.3, step = 0.05),
          uiOutput("ch11_alpha_info")
        ),
        column(8,
          zoom_plot_ui("ch11_weights_plot", height = "280px")
        )
      )
    ),

    lc_h2("ch11-holt", "Holt: ETS z trendem"),

    tagList(
      lc_p("Model Holta dodaje składnik trendu z własnym parametrem wygładzania β:"),
      lc_formula_box(
        withMathJax(helpText("$$L_t = \\alpha x_t + (1-\\alpha)(L_{t-1} + B_{t-1})$$")),
        withMathJax(helpText("$$B_t = \\beta(L_t - L_{t-1}) + (1-\\beta) B_{t-1}$$")),
        p("Prognoza: ", withMathJax("\\(\\hat{x}_{t+h} = L_t + h \\cdot B_t\\)"))
      )
    ),

    figure_panel(
      label = "Ryc. 11.2", title = "Holt — suwaki α i β",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch11_holt_alpha", "α (poziom):",  min = 0.05, max = 0.95, value = 0.4, step = 0.05),
          sliderInput("ch11_holt_beta",  "β (trend):",   min = 0.01, max = 0.50, value = 0.1, step = 0.01),
          selectInput("ch11_holt_data", "Szereg:",
                      choices = .ts_dataset_choices[c("bezrobocie", "sprzedaz", "pszenica")],
                      selected = "bezrobocie")
        ),
        column(8,
          zoom_plot_ui("ch11_holt_plot", height = "280px")
        )
      )
    ),

    lc_h2("ch11-hw", "Holt-Winters: ETS z trendem i sezonowością"),

    tagList(
      lc_p("Model Holta-Wintersa dorzuca jeszcze parametr γ odpowiadający za sezonowość.
        To najczęściej stosowany ETS dla danych miesięcznych z wyraźnym sezonowym wzorcem."),
      lc_formula_box(
        p(tags$strong("Trzy równania rekurencyjne:"), " poziom L_t, trend B_t, sezonowość S_t."),
        p("Prognoza: ", withMathJax("\\(\\hat{x}_{t+h} = (L_t + h B_t) + S_{t+h-s}\\)"),
          " (addytywny) lub ", withMathJax("\\((L_t + h B_t) \\cdot S_{t+h-s}\\)"), " (multiplikatywny).")
      )
    ),

    figure_panel(
      label = "Ryc. 11.3", title = "Holt-Winters na danych sezonowych",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch11_hw_data", "Szereg:",
                      choices = .ts_dataset_choices[c("noclegi", "sprzedaz", "warszawa")],
                      selected = "noclegi"),
          radioButtons("ch11_hw_type", "Typ modelu:",
                       choices = c("Addytywny" = "additive", "Multiplikatywny" = "multiplicative"),
                       selected = "additive"),
          numericInput("ch11_hw_h", "Horyzont prognozy (miesiące):", value = 24, min = 6, max = 48, step = 6),
          actionButton("ch11_hw_run", "Dopasuj model", class = "lc-btn-primary", width = "100%"),
          uiOutput("ch11_hw_result")
        ),
        column(8,
          zoom_plot_ui("ch11_hw_plot", height = "280px")
        )
      )
    ),

    lc_h2("ch11-ets-table", "Rodzina ETS"),

    tagList(
      lc_p("ETS to ogólny framework: ", tags$strong("E"), "rror × ",
        tags$strong("T"), "rend × ", tags$strong("S"), "easonal. Każdy z nich może przyjąć jedną z kilku form:"),
      tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
        tags$thead(tags$tr(
          tags$th("Składnik"), tags$th("Skrót"), tags$th("Znaczenie")
        )),
        tags$tbody(
          tags$tr(tags$td("Error"), tags$td("A / M"), tags$td("Addytywny / multiplikatywny błąd")),
          tags$tr(tags$td("Trend"), tags$td("N / A / Ad"), tags$td("Brak / addytywny / tłumiony")),
          tags$tr(tags$td("Seasonal"), tags$td("N / A / M"), tags$td("Brak / addytywny / multiplikatywny"))
        )
      ),
      lc_p("Prosta ETS = ETS(A,N,N). Holt = ETS(A,A,N). Holt-Winters addytywny = ETS(A,A,A). Razem 30 kombinacji — funkcja ", tags$code("ets()"), " wybiera najlepszą wg AIC.")
    ),

    lc_chapter_next(
      num       = "12",
      title     = "Prognozowanie: horyzont i niepewność",
      lead      = "wachlarz przedziałów, 'odsłoń przyszłość', 4 metody",
      target_id = "ch-prognoza"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch11_server <- function(input, output, session) {

  zoom_plot_server("ch11_weights_plot", reactive({
    alpha <- if (!is.null(input$ch11_alpha)) input$ch11_alpha else 0.3
    k     <- 20
    wagi  <- alpha * (1 - alpha)^(0:(k-1))
    df <- data.frame(
      lag   = factor(0:(k-1), levels = (k-1):0),
      waga  = wagi,
      period = paste0("t-", 0:(k-1))
    )
    ggplot(df, aes(x = lag, y = waga)) +
      geom_col(fill = upwr_accent, alpha = 0.85, width = 0.7) +
      coord_flip() +
      scale_x_discrete(labels = rev(paste0("t-", 0:(k-1)))) +
      labs(x = NULL, y = "Waga",
           title = paste0("Wagi przy α = ", alpha, " — malejące geometrycznie")) +
      theme_upwr()
  }))

  output$ch11_alpha_info <- renderUI({
    alpha <- if (!is.null(input$ch11_alpha)) input$ch11_alpha else 0.3
    interp <- if (alpha < 0.2) {
      "Długa pamięć — historia ważona równomiernie, powolna reakcja na zmiany."
    } else if (alpha < 0.5) {
      "Umiarkowana pamięć — dobra równowaga między reagowaniem a stabilnością."
    } else {
      "Krótka pamięć — szybka reakcja, ale prognoza bardzo czuła na ostatnią obserwację."
    }
    lc_feedback(type = "info", p(tags$strong(paste0("α = ", alpha, ":")), " ", interp))
  })

  zoom_plot_server("ch11_holt_plot", reactive({
    alpha <- if (!is.null(input$ch11_holt_alpha)) input$ch11_holt_alpha else 0.4
    beta  <- if (!is.null(input$ch11_holt_beta))  input$ch11_holt_beta  else 0.1
    key   <- if (!is.null(input$ch11_holt_data))  input$ch11_holt_data  else "bezrobocie"
    ts_obj <- .ts_datasets[[key]]$get_ts()
    fit   <- tryCatch(
      forecast::holt(ts_obj, alpha = alpha, beta = beta, h = 24),
      error = function(e) forecast::holt(ts_obj, h = 24)
    )
    plot_forecast_gg(ts_obj, fit, value_label = .ts_datasets[[key]]$unit) +
      labs(title = paste0("Model Holta (α=", alpha, ", β=", beta, ") — ", .ts_datasets[[key]]$label))
  }))

  ch11_hw_fit <- reactiveVal(NULL)
  ch11_hw_key <- reactiveVal(NULL)

  observeEvent(input$ch11_hw_run, {
    key   <- input$ch11_hw_data
    type  <- input$ch11_hw_type
    h     <- input$ch11_hw_h
    ts_obj <- .ts_datasets[[key]]$get_ts()
    fit <- tryCatch(
      forecast::hw(ts_obj, seasonal = type, h = h),
      error = function(e) forecast::hw(ts_obj, h = h)
    )
    ch11_hw_fit(fit)
    ch11_hw_key(key)
  })

  output$ch11_hw_result <- renderUI({
    fit <- ch11_hw_fit()
    if (is.null(fit)) return(lc_feedback(type = "info", p("Kliknij 'Dopasuj model'.")))
    acc  <- forecast::accuracy(fit)
    lc_feedback(type = "ok",
      tags$strong("Model Holta-Wintersa"),
      tags$br(),
      paste0("MAE: ", round(acc[1, "MAE"], 2), " | RMSE: ", round(acc[1, "RMSE"], 2))
    )
  })

  zoom_plot_server("ch11_hw_plot", reactive({
    fit <- ch11_hw_fit()
    key <- ch11_hw_key()
    if (is.null(fit)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
               label = "Kliknij 'Dopasuj model'", color = upwr_reference, size = 5) + theme_upwr())
    }
    ts_obj <- .ts_datasets[[key]]$get_ts()
    plot_forecast_gg(ts_obj, fit, value_label = .ts_datasets[[key]]$unit) +
      labs(title = paste0("Holt-Winters — ", .ts_datasets[[key]]$label))
  }))
}
