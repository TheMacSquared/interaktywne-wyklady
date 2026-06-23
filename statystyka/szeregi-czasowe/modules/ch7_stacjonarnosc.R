# ============================================================================
# CHAPTER 7: Stacjonarność i przekształcenia
# ============================================================================

ch7_ui <- list(
  id    = "ch-stacjonarnosc",
  num   = "07",
  title = "Stacjonarność i przekształcenia",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 07 · Szeregi czasowe",
      num    = "07",
      title  = "Stacjonarność.",
      lead   = "Większość modeli szeregów czasowych wymaga stacjonarności.
                Naucz się ją rozpoznawać i wymusić — przez różnicowanie lub transformację."
    ),

    lc_h2("ch7-definicja", "Czym jest stacjonarność?"),

    tagList(
      lc_p("Szereg jest ", tags$strong("stacjonarny"), " jeśli jego własności statystyczne
        nie zmieniają się w czasie. Formalnie: rozkład prawdopodobieństwa jest taki sam
        dla każdego przesuniętego odcinka szeregu."),
      lc_p("W praktyce sprawdzamy trzy warunki:"),
      tags$ol(
        tags$li(tags$strong("Stała średnia:"), " E[x_t] = μ (nie zmienia się w czasie)"),
        tags$li(tags$strong("Stała wariancja:"), " Var(x_t) = σ² (brak rosnącej amplitudy)"),
        tags$li(tags$strong("Autokorelacja zależy tylko od lagu,"), " nie od czasu: Cov(x_t, x_{t+k}) = γ(k)")
      ),
      lc_formula_box(
        withMathJax(helpText("$$\\text{Stacjonarny: } E[x_t] = \\mu, \\quad \\text{Var}(x_t) = \\sigma^2 < \\infty, \\quad \\text{Cov}(x_t, x_{t+k}) = \\gamma(k)$$"))
      )
    ),

    lc_h2("ch7-visual-test", "Test wzrokowy: stacjonarny czy nie?"),

    tagList(
      lc_p("Kliknij każdy szereg i zanim sprawdzisz wynik — oceń sam: czy widzisz trend, rosnącą wariancję lub sezonowość?")
    ),

    figure_panel(
      label = "Ryc. 7.1", title = "Czy to szereg stacjonarny?",
      full_width = TRUE,
      fluidRow(
        column(4,
          div(
            style = "display: flex; flex-direction: column; gap: 6px;",
            actionButton("ch7_vt_wig20",   "WIG20 (tygodniowy)",       class = "lc-btn-outline", width = "100%"),
            actionButton("ch7_vt_returns", "WIG20 log-zwroty",         class = "lc-btn-outline", width = "100%"),
            actionButton("ch7_vt_temp",    "Temperatura (po różnicow.)", class = "lc-btn-outline", width = "100%"),
            actionButton("ch7_vt_wn",      "Biały szum (symulacja)",   class = "lc-btn-outline", width = "100%")
          ),
          uiOutput("ch7_vt_verdict")
        ),
        column(8,
          zoom_plot_ui("ch7_vt_plot", height = "280px")
        )
      )
    ),

    lc_h2("ch7-roznicowanie", "Różnicowanie: jak zrobić szereg stacjonarnym"),

    tagList(
      lc_p("Najpopularniejsza metoda usuwania niestacjonarności to ",
        tags$strong("różnicowanie"), ": liczymy różnice między kolejnymi wartościami."),
      lc_formula_box(
        withMathJax(helpText("$$\\nabla x_t = x_t - x_{t-1} \\quad \\text{(pierwsza różnica)}$$")),
        withMathJax(helpText("$$\\nabla^2 x_t = \\nabla x_t - \\nabla x_{t-1} \\quad \\text{(druga różnica)}$$"))
      ),
      lc_p("Różnicowanie sezonowe usuwa sezonowość: ",
        withMathJax("\\(\\nabla_{12} x_t = x_t - x_{t-12}\\)"),
        " (dla danych miesięcznych).")
    ),

    figure_panel(
      label = "Ryc. 7.2", title = "Widget różnicowania — live transform",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch7_diff_data", "Szereg:",
                      choices = .ts_choices_for("warszawa", "bezrobocie", "pszenica", "pm10"),
                      selected = "bezrobocie"),
          div(
            style = "display: flex; flex-direction: column; gap: 6px; margin: 12px 0;",
            actionButton("ch7_diff_none", "Oryginał",            class = "lc-btn-outline", width = "100%"),
            actionButton("ch7_diff_d1",   "∇¹ (pierwsza różnica)", class = "lc-btn-outline", width = "100%"),
            actionButton("ch7_diff_d12",  "∇¹² (sezonowa ∆)",     class = "lc-btn-outline", width = "100%"),
            actionButton("ch7_diff_d1d12","∇¹ + ∇¹²",             class = "lc-btn-outline", width = "100%")
          ),
          uiOutput("ch7_adf_result")
        ),
        column(8,
          zoom_plot_ui("ch7_diff_plot", height = "260px"),
          zoom_plot_ui("ch7_diff_acf_plot", height = "160px")
        )
      )
    ),

    lc_h2("ch7-boxcox", "Transformacja Box-Cox — stabilizacja wariancji"),

    tagList(
      lc_p("Jeśli problem to rosnąca wariancja (nie trend), różnicowanie nie wystarczy.
        Potrzebna jest transformacja, która ją ustabilizuje."),
      lc_formula_box(
        withMathJax(helpText(
          "$$x_t^{(\\lambda)} = \\begin{cases} \\log(x_t) & \\lambda = 0 \\\\ (x_t^\\lambda - 1)/\\lambda & \\lambda \\neq 0 \\end{cases}$$"
        )),
        p("λ = 0 → logarytm (najczęściej stosowany dla danych ekonomicznych), λ = 0.5 → pierwiastek.")
      )
    ),

    figure_panel(
      label = "Ryc. 7.3", title = "Box-Cox — suwak λ, obserwuj zmianę wariancji",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch7_bc_data", "Szereg:",
                      choices = .ts_choices_for("noclegi", "sprzedaz", "pszenica"),
                      selected = "noclegi"),
          sliderInput("ch7_bc_lambda", "λ:",
                      min = -1, max = 1, value = 0, step = 0.1),
          uiOutput("ch7_bc_info")
        ),
        column(8,
          zoom_plot_ui("ch7_bc_plot", height = "280px")
        )
      )
    ),

    lc_h2("ch7-adf-test", "Test ADF — formalny test stacjonarności"),

    tagList(
      lc_p("Test ADF (Augmented Dickey-Fuller) formalnie weryfikuje hipotezę zerową,
        że szereg ", tags$em("nie jest"), " stacjonarny (ma pierwiastek jednostkowy)."),
      lc_formula_box(
        p(tags$strong("H₀:"), " szereg ma pierwiastek jednostkowy (jest niestacjonarny)"),
        p(tags$strong("H₁:"), " szereg jest stacjonarny"),
        p("Małe p-value (< 0,05) → odrzucamy H₀ → szereg stacjonarny.")
      ),
      margin_callout(label = "Uwaga", color = "uwaga",
        "Nie ufaj ślepo testowi ADF. Małe próby mogą nie wykrywać niestacjonarności.
         Zawsze patrz też na wykres i ACF."
      )
    ),

    lc_chapter_next(
      num       = "08",
      title     = "Modele AR: autoregresja",
      lead      = "φ₁, AR(p) — pierwsza rodzina modeli ARIMA",
      target_id = "ch-ar"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch7_server <- function(input, output, session) {

  .ch7_vt_series <- list(
    wig20   = list(label = "WIG20 (tygodniowy)", stat = FALSE,
                   desc  = "Niestacjonarny: wyraźny trend stochastyczny. Wariancja rośnie w czasie."),
    returns = list(label = "WIG20 log-zwroty",  stat = TRUE,
                   desc  = "Stacjonarny: wahania wokół zera, stała wariancja. Klasyczny przykład stacjonarności."),
    temp    = list(label = "Temperatura (∇¹²)", stat = TRUE,
                   desc  = "Po różnicowaniu sezonowym: stacjonarny. Oryginalna temperatura jest niestacjonarna (trend + sezonowość)."),
    wn      = list(label = "Biały szum",         stat = TRUE,
                   desc  = "Stacjonarny: czysto losowy, brak pamięci, stała wariancja i średnia.")
  )

  ch7_vt_selected <- reactiveVal("wig20")
  observeEvent(input$ch7_vt_wig20,   ch7_vt_selected("wig20"))
  observeEvent(input$ch7_vt_returns, ch7_vt_selected("returns"))
  observeEvent(input$ch7_vt_temp,    ch7_vt_selected("temp"))
  observeEvent(input$ch7_vt_wn,      ch7_vt_selected("wn"))

  ch7_vt_get <- function(sel) {
    switch(sel,
      wig20   = as.numeric(.ts_wig20$wig20),
      returns = as.numeric(na.omit(.ts_wig20$log_return)),
      temp    = diff(as.numeric(warszawa_ts()), lag = 12),
      wn      = { set.seed(99); rnorm(200) }
    )
  }

  zoom_plot_server("ch7_vt_plot", reactive({
    sel <- ch7_vt_selected()
    x   <- ch7_vt_get(sel)
    info <- .ch7_vt_series[[sel]]
    df   <- data.frame(t = seq_along(x), x = x)
    ggplot(df, aes(x = t, y = x)) +
      geom_line(color = upwr_secondary, linewidth = 0.7) +
      labs(x = "Czas", y = NULL, title = info$label) +
      theme_upwr()
  }))

  output$ch7_vt_verdict <- renderUI({
    sel  <- ch7_vt_selected()
    info <- .ch7_vt_series[[sel]]
    if (info$stat) {
      lc_feedback(type = "ok",  tags$strong("✓ Stacjonarny"), tags$br(), info$desc)
    } else {
      lc_feedback(type = "warning", tags$strong("✗ Niestacjonarny"), tags$br(), info$desc)
    }
  })

  ch7_diff_mode <- reactiveVal("none")
  observeEvent(input$ch7_diff_none,  ch7_diff_mode("none"))
  observeEvent(input$ch7_diff_d1,    ch7_diff_mode("d1"))
  observeEvent(input$ch7_diff_d12,   ch7_diff_mode("d12"))
  observeEvent(input$ch7_diff_d1d12, ch7_diff_mode("d1d12"))

  ch7_diff_series <- reactive({
    key  <- input$ch7_diff_data
    mode <- ch7_diff_mode()
    x    <- as.numeric(.ts_datasets[[key]]$get_ts())
    switch(mode,
      none  = x,
      d1    = diff(x, differences = 1),
      d12   = diff(x, lag = 12),
      d1d12 = diff(diff(x, lag = 12), differences = 1)
    )
  })

  zoom_plot_server("ch7_diff_plot", reactive({
    x    <- ch7_diff_series()
    mode <- ch7_diff_mode()
    key  <- input$ch7_diff_data
    unit <- .ts_datasets[[key]]$unit
    title <- switch(mode,
      none  = paste0("Oryginał — ", .ts_datasets[[key]]$label),
      d1    = "Po pierwszym różnicowaniu (∇¹)",
      d12   = "Po różnicowaniu sezonowym (∇¹²)",
      d1d12 = "Po ∇¹ i ∇¹² (podwójne różnicowanie)"
    )
    df <- data.frame(t = seq_along(x), x = x)
    ggplot(df, aes(x = t, y = x)) +
      geom_line(color = upwr_accent, linewidth = 0.8) +
      geom_hline(yintercept = mean(x), color = upwr_reference, linetype = "dashed") +
      labs(x = "Czas", y = if (mode == "none") unit else paste0("∆", unit), title = title) +
      theme_upwr()
  }))

  zoom_plot_server("ch7_diff_acf_plot", reactive({
    x <- ch7_diff_series()
    plot_acf_gg(x, lag.max = 24, title = "ACF po transformacji")
  }))

  output$ch7_adf_result <- renderUI({
    x <- ch7_diff_series()
    if (length(x) < 10) return(lc_feedback(type = "warning", p("Za mało obserwacji po różnicowaniu.")))
    adf <- tryCatch(tseries::adf.test(x), error = function(e) NULL)
    if (is.null(adf)) return(NULL)
    pv <- adf$p.value
    type <- if (pv < 0.05) "ok" else "warning"
    verdict <- if (pv < 0.05) "Stacjonarny (p < 0,05)" else "Niestacjonarny (p ≥ 0,05)"
    lc_feedback(type = type,
      tags$strong("Test ADF: "), verdict,
      tags$br(),
      paste0("p = ", format_p_value(pv), " (statystyka: ", round(adf$statistic, 3), ")")
    )
  })

  zoom_plot_server("ch7_bc_plot", reactive({
    key    <- input$ch7_bc_data
    lambda <- if (!is.null(input$ch7_bc_lambda)) input$ch7_bc_lambda else 0
    x      <- as.numeric(.ts_datasets[[key]]$get_ts())

    x_tr <- if (abs(lambda) < 0.01) {
      log(pmax(x, 0.001))
    } else {
      (pmax(x, 0.001)^lambda - 1) / lambda
    }

    df <- data.frame(
      date = .ts_datasets[[key]]$get_df()$date,
      orig = x,
      trans = x_tr
    )
    df_long <- data.frame(
      date  = rep(df$date, 2),
      value = c(df$orig, df$trans),
      panel = rep(c("Oryginał", paste0("Box-Cox (λ=", lambda, ")")), each = nrow(df))
    )

    ggplot(df_long, aes(x = date, y = value)) +
      geom_line(color = upwr_accent, linewidth = 0.8) +
      facet_wrap(~ panel, ncol = 1, scales = "free_y") +
      labs(x = NULL, y = NULL) +
      theme_upwr() +
      theme(strip.text = element_text(face = "bold"))
  }))

  output$ch7_bc_info <- renderUI({
    lambda <- if (!is.null(input$ch7_bc_lambda)) input$ch7_bc_lambda else 0
    msg <- switch(as.character(round(lambda, 1)),
      "0"    = "λ = 0: transformacja logarytmiczna. Eliminuje liniowe skalowanie wariancji.",
      "0.5"  = "λ = 0.5: transformacja pierwiastkowa. Umiarkowana stabilizacja wariancji.",
      "-1"   = "λ = −1: transformacja odwrotna (1/x).",
      paste0("λ = ", lambda)
    )
    lc_feedback(type = "info", p(msg))
  })
}
