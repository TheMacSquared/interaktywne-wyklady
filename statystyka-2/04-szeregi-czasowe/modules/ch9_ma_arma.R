# ============================================================================
# CHAPTER 9: Modele MA i ARMA
# ============================================================================

ch9_ui <- list(
  id    = "ch-ma-arma",
  num   = "09",
  title = "Modele MA i ARMA",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 09 · Szeregi czasowe",
      num    = "09",
      title  = "Modele MA i ARMA.",
      lead   = "AR pamięta przeszłe wartości. MA pamięta przeszłe błędy.
                ARMA łączy oba mechanizmy. Trzy bardzo różne typy pamięci."
    ),

    lc_h2("ch9-ma-intuicja", "MA: pamięć na błędy"),

    tagList(
      lc_p("Model MA(q) — średnia krocząca na błędach — mówi: bieżąca wartość zależy
        od bieżącego i q poprzednich błędów losowych (innowacji), nie od poprzednich wartości."),
      lc_formula_box(
        withMathJax(helpText("$$x_t = \\varepsilon_t + \\theta_1 \\varepsilon_{t-1} \\quad \\text{MA(1)}$$")),
        withMathJax(helpText("$$x_t = \\varepsilon_t + \\theta_1 \\varepsilon_{t-1} + \\cdots + \\theta_q \\varepsilon_{t-q} \\quad \\text{MA(q)}$$"))
      ),
      lc_p("Kluczowa różnica vs. AR: wstrząs w MA(q) wpływa na szereg ",
        tags$em("przez dokładnie q+1 kroków"),
        " i zanika. W AR(p) wstrząs zanika geometrycznie, trwa nieskończenie długo.")
    ),

    lc_h2("ch9-ar-vs-ma", "AR vs. MA — side-by-side"),

    figure_panel(
      label = "Ryc. 9.1", title = "AR(1) vs. MA(1) — takie same parametry, zupełnie inne ACF/PACF",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch9_phi",   "AR: φ₁:",   min = -0.9, max = 0.9, value = 0.7, step = 0.05),
          sliderInput("ch9_theta", "MA: θ₁:",   min = -0.9, max = 0.9, value = 0.7, step = 0.05),
          actionButton("ch9_ar_ma_new", "Nowa realizacja", class = "lc-btn-primary", width = "100%"),
          uiOutput("ch9_ar_ma_info")
        ),
        column(8,
          fluidRow(
            column(6, h6("AR(1)", style = "text-align:center;"),
              zoom_plot_ui("ch9_ar_ts",   height = "150px"),
              zoom_plot_ui("ch9_ar_acf",  height = "130px"),
              zoom_plot_ui("ch9_ar_pacf", height = "130px")
            ),
            column(6, h6("MA(1)", style = "text-align:center;"),
              zoom_plot_ui("ch9_ma_ts",   height = "150px"),
              zoom_plot_ui("ch9_ma_acf",  height = "130px"),
              zoom_plot_ui("ch9_ma_pacf", height = "130px")
            )
          )
        )
      )
    ),

    lc_h2("ch9-theta-intuicja", "Co robi θ₁?"),

    tagList(
      lc_p("Suwak θ₁ sterowuje tym, jak długo 'wstrząs' pozostaje w szeregu. Porównaj:"),
      tags$ul(
        tags$li(tags$strong("θ₁ = 0:"),
          " biały szum — każda obserwacja to czysty szum"),
        tags$li(tags$strong("θ₁ = 0.9:"),
          " mocny efekt MA — wstrząs z poprzedniego okresu silnie wpływa na bieżący"),
        tags$li(tags$strong("θ₁ = −0.9:"),
          " oscylacje — każde duże 'zdarzenie' jest natychmiast kompensowane")
      )
    ),

    figure_panel(
      label = "Ryc. 9.2", title = "Suwak θ₁ — długość i charakter wstrząsu w MA(1)",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch9_theta_suw", "θ₁:", min = -0.99, max = 0.99, value = 0.8, step = 0.05),
          actionButton("ch9_theta_new", "Nowa realizacja", class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch9_theta_plot", height = "280px")
        )
      )
    ),

    lc_h2("ch9-arma", "ARMA(p,q): połączenie obu mechanizmów"),

    tagList(
      lc_p("ARMA łączy AR i MA: bieżąca wartość zależy od poprzednich wartości ",
        tags$em("i"), " poprzednich błędów."),
      lc_formula_box(
        withMathJax(helpText("$$x_t = \\phi_1 x_{t-1} + \\cdots + \\phi_p x_{t-p} + \\varepsilon_t + \\theta_1 \\varepsilon_{t-1} + \\cdots + \\theta_q \\varepsilon_{t-q}$$")),
        p("ACF i PACF obu zanikają geometrycznie — brak wyraźnego ucięcia.",
          " Patrz tabelę identyfikacji w ch6.")
      ),
      inline_callout(label = "Chcesz więcej matematyki?", color = "wskazowka", open = FALSE,
        p("ARMA(p,q) można zapisać jako AR(∞) lub MA(∞) (przy odpowiednich warunkach stacjonarności i odwracalności):"),
        p("AR to MA(∞): nieskończona suma błędów."),
        p("MA to AR(∞): nieskończona regresja na własnych poprzednich wartościach."),
        p("Odwracalność MA: |θᵢ| < 1 (dla MA(1)).")
      )
    ),

    figure_panel(
      label = "Ryc. 9.3", title = "ARMA(1,1) — dwa suwaki, mieszany wzorzec",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch9_arma_phi",   "φ₁ (AR):", min = -0.9, max = 0.9, value = 0.5, step = 0.05),
          sliderInput("ch9_arma_theta", "θ₁ (MA):", min = -0.9, max = 0.9, value = 0.5, step = 0.05),
          actionButton("ch9_arma_new", "Nowa realizacja", class = "lc-btn-primary", width = "100%")
        ),
        column(8,
          zoom_plot_ui("ch9_arma_ts_plot",   height = "200px"),
          fluidRow(
            column(6, zoom_plot_ui("ch9_arma_acf_plot",  height = "180px")),
            column(6, zoom_plot_ui("ch9_arma_pacf_plot", height = "180px"))
          )
        )
      )
    ),

    lc_chapter_next(
      num       = "10",
      title     = "ARIMA i SARIMA",
      lead      = "składamy AR + I + MA, dodajemy sezonowość",
      target_id = "ch-arima"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch9_server <- function(input, output, session) {

  ch9_ar_ma_seed <- reactiveVal(42)
  observeEvent(input$ch9_ar_ma_new, ch9_ar_ma_seed(ch9_ar_ma_seed() + 1))

  ch9_ar_data <- reactive({
    set.seed(ch9_ar_ma_seed())
    phi <- if (!is.null(input$ch9_phi)) input$ch9_phi else 0.7
    as.numeric(arima.sim(list(ar = phi), n = 200))
  })
  ch9_ma_data <- reactive({
    set.seed(ch9_ar_ma_seed() + 100)
    theta <- if (!is.null(input$ch9_theta)) input$ch9_theta else 0.7
    as.numeric(arima.sim(list(ma = theta), n = 200))
  })

  .ts_line <- function(x, title) {
    df <- data.frame(t = seq_along(x), x = x)
    ggplot(df, aes(x = t, y = x)) +
      geom_line(color = upwr_secondary, linewidth = 0.7) +
      labs(x = NULL, y = NULL, title = title) +
      theme_upwr()
  }

  zoom_plot_server("ch9_ar_ts",   reactive(.ts_line(ch9_ar_data(),
                                                     paste0("AR(1) φ=", input$ch9_phi))))
  zoom_plot_server("ch9_ma_ts",   reactive(.ts_line(ch9_ma_data(),
                                                     paste0("MA(1) θ=", input$ch9_theta))))
  zoom_plot_server("ch9_ar_acf",  reactive(plot_acf_gg(ch9_ar_data(),  lag.max = 20, title = "ACF")))
  zoom_plot_server("ch9_ma_acf",  reactive(plot_acf_gg(ch9_ma_data(),  lag.max = 20, title = "ACF")))
  zoom_plot_server("ch9_ar_pacf", reactive(plot_acf_gg(ch9_ar_data(),  lag.max = 20, type = "PACF", title = "PACF")))
  zoom_plot_server("ch9_ma_pacf", reactive(plot_acf_gg(ch9_ma_data(),  lag.max = 20, type = "PACF", title = "PACF")))

  output$ch9_ar_ma_info <- renderUI({
    phi   <- if (!is.null(input$ch9_phi))   input$ch9_phi   else 0.7
    theta <- if (!is.null(input$ch9_theta)) input$ch9_theta else 0.7
    lc_feedback(type = "info",
      p("Przy φ₁ = θ₁ = ", phi, ": wykresy szeregów wyglądają podobnie,
         ale ACF i PACF są zupełnie inne — to właśnie sposób identyfikacji modelu.")
    )
  })

  ch9_theta_seed <- reactiveVal(55)
  observeEvent(input$ch9_theta_new, ch9_theta_seed(ch9_theta_seed() + 1))

  zoom_plot_server("ch9_theta_plot", reactive({
    theta <- if (!is.null(input$ch9_theta_suw)) input$ch9_theta_suw else 0.8
    set.seed(ch9_theta_seed())
    eps <- rnorm(200)
    x   <- eps[-1] + theta * eps[-200]
    df  <- data.frame(t = seq_along(x), x = x)
    ggplot(df, aes(x = t, y = x)) +
      geom_line(color = upwr_secondary, linewidth = 0.7) +
      labs(x = "Czas", y = "x_t",
           title = paste0("MA(1) z θ₁ = ", theta)) +
      theme_upwr()
  }))

  ch9_arma_seed <- reactiveVal(77)
  observeEvent(input$ch9_arma_new, ch9_arma_seed(ch9_arma_seed() + 1))

  ch9_arma_data <- reactive({
    phi   <- if (!is.null(input$ch9_arma_phi))   input$ch9_arma_phi   else 0.5
    theta <- if (!is.null(input$ch9_arma_theta)) input$ch9_arma_theta else 0.5
    set.seed(ch9_arma_seed())
    tryCatch(
      as.numeric(arima.sim(list(ar = phi, ma = theta), n = 200)),
      error = function(e) rnorm(200)
    )
  })

  zoom_plot_server("ch9_arma_ts_plot", reactive({
    x <- ch9_arma_data()
    phi   <- if (!is.null(input$ch9_arma_phi))   input$ch9_arma_phi   else 0.5
    theta <- if (!is.null(input$ch9_arma_theta)) input$ch9_arma_theta else 0.5
    df <- data.frame(t = seq_along(x), x = x)
    ggplot(df, aes(x = t, y = x)) +
      geom_line(color = upwr_secondary, linewidth = 0.7) +
      labs(x = "Czas", y = "x_t",
           title = paste0("ARMA(1,1) φ=", phi, ", θ=", theta)) +
      theme_upwr()
  }))

  zoom_plot_server("ch9_arma_acf_plot", reactive({
    plot_acf_gg(ch9_arma_data(), lag.max = 20, title = "ACF — obydwa zanikają")
  }))
  zoom_plot_server("ch9_arma_pacf_plot", reactive({
    plot_acf_gg(ch9_arma_data(), lag.max = 20, type = "PACF", title = "PACF — obydwa zanikają")
  }))
}
