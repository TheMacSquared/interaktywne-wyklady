# ============================================================================
# CHAPTER 6: PACF i identyfikacja modelu
# ============================================================================

ch6_ui <- list(
  id    = "ch-pacf",
  num   = "06",
  title = "PACF i identyfikacja modelu",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 06 · Szeregi czasowe",
      num    = "06",
      title  = "PACF i identyfikacja modelu.",
      lead   = "Cząstkowa autokorelacja (PACF) oczyszcza ACF z efektów pośrednich.
                Razem ACF i PACF tworzą mapę do identyfikacji rzędu modelu ARIMA."
    ),

    lc_h2("ch6-intuicja", "Intuicja: korelacja 'bezpośrednia'"),

    tagList(
      lc_p("ACF mierzy korelację między x_t a x_{t−k}, ale jest w niej ukryta
        część wyjaśniana przez wartości pośrednie (x_{t−1}, …, x_{t−k+1}).
        Analogia z regresji: korelacja częściowa Pearsona kontroluje efekty pośrednie."),
      lc_p(tags$strong("PACF(k)"), " to korelacja między x_t a x_{t−k} po usunięciu
        liniowego wpływu x_{t−1}, …, x_{t−k+1} — czyli korelacja 'bezpośrednia',
        niezakłócona pośrednikami."),
      inline_callout(label = "Chcesz więcej matematyki?", color = "wskazowka", open = FALSE,
        p("PACF(k) oblicza się jako ostatni współczynnik autoregresji AR(k):"),
        withMathJax(helpText(
          "$$x_t = \\phi_{k1}x_{t-1} + \\phi_{k2}x_{t-2} + \\cdots + \\phi_{kk}x_{t-k} + \\varepsilon_t$$"
        )),
        p("PACF(k) = φ_{kk}. To efekt lagging k na x_t 'po wyczyszczeniu' lagów 1, …, k−1.")
      )
    ),

    lc_h2("ch6-reguly", "Reguły identyfikacji"),

    tagList(
      lc_p("Para ACF + PACF pozwala wstępnie zidentyfikować typ modelu:"),
      tags$table(class = "lc-table lc-table-bordered lc-table-striped lc-table-sm",
        tags$thead(tags$tr(
          tags$th("Model"),
          tags$th("Wzorzec ACF"),
          tags$th("Wzorzec PACF")
        )),
        tags$tbody(
          tags$tr(
            tags$td("AR(p)"),
            tags$td("Geometryczne/sinusoidalne zanikanie (nieskońcone)"),
            tags$td(tags$strong("Urywa się po lagging p"))
          ),
          tags$tr(
            tags$td("MA(q)"),
            tags$td(tags$strong("Urywa się po lagging q")),
            tags$td("Geometryczne/sinusoidalne zanikanie (nieskońcone)")
          ),
          tags$tr(
            tags$td("ARMA(p,q)"),
            tags$td("Geometryczne zanikanie"),
            tags$td("Geometryczne zanikanie")
          ),
          tags$tr(
            tags$td("Biały szum"),
            tags$td("Wszystko w przedziale ufności"),
            tags$td("Wszystko w przedziale ufności")
          )
        )
      )
    ),

    lc_h2("ch6-symulator", "Symulator: wybierz model → zobacze ACF i PACF"),

    tagList(
      lc_p("Wygeneruj szereg z wybranego modelu i odczytaj wzorzec ACF i PACF.
        Sprawdź, czy reguły z tabeli rzeczywiście działają.")
    ),

    figure_panel(
      label = "Ryc. 6.1", title = "Symulator modeli AR/MA — ACF i PACF",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch6_sim_type", "Typ modelu:",
                      choices = c("AR(1)" = "ar1", "AR(2)" = "ar2",
                                  "MA(1)" = "ma1", "MA(2)" = "ma2",
                                  "ARMA(1,1)" = "arma11",
                                  "Biały szum" = "wn"),
                      selected = "ar1"),
          uiOutput("ch6_sim_params"),
          numericInput("ch6_sim_n", "Liczba obserwacji n:", value = 300, min = 100, max = 1000, step = 50),
          actionButton("ch6_sim_new", "Nowa realizacja", class = "lc-btn-primary", width = "100%"),
          uiOutput("ch6_sim_rule")
        ),
        column(8,
          fluidRow(
            column(12, zoom_plot_ui("ch6_sim_ts_plot", height = "180px"))
          ),
          fluidRow(
            column(6,  zoom_plot_ui("ch6_sim_acf_plot",  height = "200px")),
            column(6,  zoom_plot_ui("ch6_sim_pacf_plot", height = "200px"))
          )
        )
      )
    ),

    lc_h2("ch6-krok-po-kroku", "Jak używać ACF i PACF w praktyce?"),

    tagList(
      lc_p("W praktycznej identyfikacji ARIMA stosujemy następujące kroki:"),
      tags$ol(
        tags$li("Upewnij się, że szereg jest ", tags$strong("stacjonarny"),
          " (jeśli nie — różnicuj; to temat ch7)."),
        tags$li("Narysuj ACF i PACF dla stacjonarnego szeregu."),
        tags$li("Użyj reguł z tabeli, żeby wstępnie określić p i q."),
        tags$li("Dopasuj kilka kandydatów i porównaj AIC/BIC."),
        tags$li("Sprawdź residua (ch14) — powinny być białym szumem.")
      ),
      lc_p("Pamiętaj, że reguły identyfikacji to tylko punkt startowy. Zazwyczaj
        warto sprawdzić kilka modeli i wybrać ten z najlepszym kryterium informacyjnym.
        Pakiet ", tags$code("forecast"), " ma funkcję ", tags$code("auto.arima()"),
        " która robi to automatycznie — omówimy ją w ch10.")
    ),

    lc_chapter_next(
      num       = "07",
      title     = "Stacjonarność i przekształcenia",
      lead      = "test ADF, różnicowanie, transformacja Box-Cox",
      target_id = "ch-stacjonarnosc"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch6_server <- function(input, output, session) {

  ch6_sim_seed <- reactiveVal(42)
  observeEvent(input$ch6_sim_new, {
    ch6_sim_seed(ch6_sim_seed() + 1)
  })

  output$ch6_sim_params <- renderUI({
    type <- input$ch6_sim_type
    switch(type,
      ar1    = sliderInput("ch6_phi1", "φ₁ (AR lag 1):", min = -0.99, max = 0.99, value = 0.8, step = 0.05),
      ar2    = tagList(
        sliderInput("ch6_phi1", "φ₁ (AR lag 1):", min = -0.99, max = 0.99, value = 0.6, step = 0.05),
        sliderInput("ch6_phi2", "φ₂ (AR lag 2):", min = -0.99, max = 0.99, value = 0.2, step = 0.05)
      ),
      ma1    = sliderInput("ch6_theta1", "θ₁ (MA lag 1):", min = -0.99, max = 0.99, value = 0.7, step = 0.05),
      ma2    = tagList(
        sliderInput("ch6_theta1", "θ₁ (MA lag 1):", min = -0.99, max = 0.99, value = 0.6, step = 0.05),
        sliderInput("ch6_theta2", "θ₂ (MA lag 2):", min = -0.99, max = 0.99, value = 0.3, step = 0.05)
      ),
      arma11 = tagList(
        sliderInput("ch6_phi1",   "φ₁ (AR):", min = -0.99, max = 0.99, value = 0.5, step = 0.05),
        sliderInput("ch6_theta1", "θ₁ (MA):", min = -0.99, max = 0.99, value = 0.5, step = 0.05)
      ),
      wn     = NULL
    )
  })

  ch6_sim_data <- reactive({
    type  <- input$ch6_sim_type
    n     <- if (!is.null(input$ch6_sim_n)) input$ch6_sim_n else 300
    seed  <- ch6_sim_seed()
    set.seed(seed)

    phi1   <- if (!is.null(input$ch6_phi1))   input$ch6_phi1   else 0.8
    phi2   <- if (!is.null(input$ch6_phi2))   input$ch6_phi2   else 0.2
    theta1 <- if (!is.null(input$ch6_theta1)) input$ch6_theta1 else 0.7
    theta2 <- if (!is.null(input$ch6_theta2)) input$ch6_theta2 else 0.3

    tryCatch({
      switch(type,
        ar1    = arima.sim(list(ar = phi1), n = n),
        ar2    = arima.sim(list(ar = c(phi1, phi2)), n = n),
        ma1    = arima.sim(list(ma = theta1), n = n),
        ma2    = arima.sim(list(ma = c(theta1, theta2)), n = n),
        arma11 = arima.sim(list(ar = phi1, ma = theta1), n = n),
        wn     = rnorm(n)
      )
    }, error = function(e) rnorm(n))
  })

  zoom_plot_server("ch6_sim_ts_plot", reactive({
    x  <- as.numeric(ch6_sim_data())
    df <- data.frame(t = seq_along(x), x = x)
    ggplot(df, aes(x = t, y = x)) +
      geom_line(color = upwr_secondary, linewidth = 0.7) +
      labs(x = "Czas", y = "x_t") +
      theme_upwr()
  }))

  zoom_plot_server("ch6_sim_acf_plot", reactive({
    x <- as.numeric(ch6_sim_data())
    plot_acf_gg(x, lag.max = 24, title = "ACF")
  }))

  zoom_plot_server("ch6_sim_pacf_plot", reactive({
    x <- as.numeric(ch6_sim_data())
    plot_acf_gg(x, lag.max = 24, type = "PACF", title = "PACF")
  }))

  output$ch6_sim_rule <- renderUI({
    type <- input$ch6_sim_type
    msg  <- switch(type,
      ar1    = "AR(1): ACF zanika geometrycznie, PACF ma jeden istotny lag (lag 1) i się urywa.",
      ar2    = "AR(2): ACF zanika wolno lub sinusoidalnie; PACF ucina się po 2 lagach.",
      ma1    = "MA(1): ACF ma tylko 1 istotny lag (lag 1); PACF zanika geometrycznie.",
      ma2    = "MA(2): ACF ucina się po 2 lagach; PACF zanika geometrycznie.",
      arma11 = "ARMA(1,1): oba wykresy zanikają geometrycznie — brak wyraźnego ucięcia.",
      wn     = "Biały szum: wszystkie lagi w ACF i PACF mieszczą się w przedziale ufności."
    )
    lc_feedback(type = "info", p(tags$strong("Reguła: "), msg))
  })
}
