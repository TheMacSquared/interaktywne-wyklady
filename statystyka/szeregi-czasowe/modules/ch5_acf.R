# ============================================================================
# CHAPTER 5: ACF — pamięć szeregu
# ============================================================================

ch5_ui <- list(
  id    = "ch-acf",
  num   = "05",
  title = "ACF: pamięć szeregu",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 05 · Szeregi czasowe",
      num    = "05",
      title  = "ACF: pamięć szeregu.",
      lead   = "Funkcja autokorelacji (ACF) to rentgen szeregu — mówi, jak daleko w przeszłość
                sięga 'pamięć' danych. To kluczowe narzędzie do identyfikacji modelu."
    ),

    lc_h2("ch5-lag", "Lag plot — zanim zobaczymy ACF"),

    tagList(
      lc_p("Zanim narysujemy ACF, zrozumiejmy, co mierzy. ",
        tags$strong("Lag plot"), " to wykres rozrzutu: wartość w czasie ",
        tags$em("t"), " na osi Y, wartość ",
        tags$em("k"), " kroków wcześniej (", tags$em("t−k"), ") na osi X."),
      lc_p("Jeśli punkty układają się wzdłuż przekątnej — obserwacje oddalone o ",
        tags$em("k"),
        " kroków są ze sobą powiązane. Korelacja Pearsona między ",
        tags$em("x_t"),
        " i ",
        tags$em("x_{t−k}"),
        " to właśnie wartość autokorelacji przy lagging ", tags$em("k"), ".")
    ),

    figure_panel(
      label = "Ryc. 5.1", title = "Lag plot — ustaw opóźnienie i wybierz szereg",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch5_lag_data", "Szereg:",
                      choices = .ts_dataset_choices,
                      selected = "warszawa"),
          sliderInput("ch5_lag_k", "Opóźnienie k (lag):",
                      min = 1, max = 24, value = 1, step = 1),
          uiOutput("ch5_lag_r")
        ),
        column(8,
          zoom_plot_ui("ch5_lag_plot", height = "300px")
        )
      )
    ),

    lc_h2("ch5-acf-def", "Funkcja autokorelacji (ACF)"),

    tagList(
      lc_p("Zamiast rysować lag plot dla każdego k osobno, ACF zbiera korelacje
        dla wszystkich opóźnień naraz i rysuje je jako wykres słupkowy."),
      lc_formula_box(
        withMathJax(helpText(
          "$$r(k) = \\frac{\\text{Cov}(x_t, x_{t-k})}{\\text{Var}(x_t)}$$"
        )),
        p("Wartość ", withMathJax("\\(r(k)\\)"), " zawiera się w [−1, 1].
          Przerywane linie to przedziały ufności 95%: słupki poza nimi wskazują
          statystycznie istotną autokorelację.")
      ),
      inline_callout(label = "Chcesz więcej matematyki?", color = "wskazowka", open = FALSE,
        p("Pełna definicja funkcji autokowariancji:"),
        withMathJax(helpText("$$\\gamma(k) = \\frac{1}{n} \\sum_{t=k+1}^{n} (x_t - \\bar{x})(x_{t-k} - \\bar{x})$$")),
        p("Autokorelacja: ",
          withMathJax("\\(r(k) = \\gamma(k) / \\gamma(0)\\)"),
          ", gdzie ",
          withMathJax("\\(\\gamma(0) = \\text{Var}(x_t)\\)."))
      )
    ),

    lc_h2("ch5-acf-porownanie", "Porównanie ACF dla różnych szeregów"),

    tagList(
      lc_p("Różne typy szeregów dają charakterystyczne wzorce ACF. Kliknij szereg,
        żeby zobaczyć jego ACF i interpretację.")
    ),

    figure_panel(
      label = "Ryc. 5.2", title = "ACF — wybierz szereg i przeczytaj wzorzec",
      full_width = TRUE,
      fluidRow(
        column(4,
          div(
            style = "display: flex; flex-direction: column; gap: 6px; margin-bottom: 12px;",
            actionButton("ch5_acf_warszawa",   "Temperatura Warszawa",    class = "lc-btn-outline", width = "100%"),
            actionButton("ch5_acf_pszenica",   "Ceny pszenicy (AR-like)", class = "lc-btn-outline", width = "100%"),
            actionButton("ch5_acf_wn",         "Biały szum (symulacja)",  class = "lc-btn-outline", width = "100%")
          ),
          uiOutput("ch5_acf_desc")
        ),
        column(8,
          zoom_plot_ui("ch5_acf_plot", height = "280px")
        )
      )
    ),

    lc_h2("ch5-lag-plot-acf", "Od lag plot do ACF"),

    tagList(
      lc_p("Interaktywny widget łączy lag plot z ACF. Kliknij słupek w ACF — po lewej
        pojawi się odpowiadający lag plot, a r(k) zaświeci na tym samym słupku.")
    ),

    figure_panel(
      label = "Ryc. 5.3", title = "Kliknij lag w ACF → zobacz odpowiadający lag plot",
      full_width = TRUE,
      fluidRow(
        column(4,
          selectInput("ch5_both_data", "Szereg:",
                      choices = .ts_dataset_choices[c("warszawa", "bezrobocie", "pszenica")],
                      selected = "pszenica"),
          sliderInput("ch5_both_k", "Wybrany lag k:",
                      min = 1, max = 24, value = 1, step = 1),
          uiOutput("ch5_both_r")
        ),
        column(4,
          zoom_plot_ui("ch5_both_lag_plot", height = "260px")
        ),
        column(4,
          zoom_plot_ui("ch5_both_acf_plot", height = "260px")
        )
      )
    ),

    margin_callout(label = "Zapamiętaj", color = "wskazowka",
      p(tags$strong("Wzorce ACF:"),
        tags$ul(
          tags$li("Sinusoida → sezonowość (temp)"),
          tags$li("Powolne zanikanie → AR (ceny)"),
          tags$li("Nagłe ucięcie po k lagach → MA(k)"),
          tags$li("Wszystko w przedziale ufności → biały szum")
        )
      )
    ),

    lc_chapter_next(
      num       = "06",
      title     = "PACF i identyfikacja modelu",
      lead      = "cząstkowa autokorelacja — jak odróżnić AR od MA",
      target_id = "ch-pacf"
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

ch5_server <- function(input, output, session) {

  ch5_lag_series <- reactive({
    key <- input$ch5_lag_data
    df  <- .ts_datasets[[key]]$get_df()
    as.numeric(df[[2]])
  })

  zoom_plot_server("ch5_lag_plot", reactive({
    x   <- ch5_lag_series()
    k   <- if (!is.null(input$ch5_lag_k)) input$ch5_lag_k else 1
    key <- input$ch5_lag_data
    n   <- length(x)

    if (k >= n) return(ggplot() + theme_upwr())

    x_t   <- x[(k+1):n]
    x_lag <- x[1:(n-k)]
    r_val <- cor(x_t, x_lag, use = "complete.obs")

    unit <- .ts_datasets[[key]]$unit
    df_lag <- data.frame(x_lag = x_lag, x_t = x_t)

    ggplot(df_lag, aes(x = x_lag, y = x_t)) +
      geom_point(color = upwr_secondary, alpha = 0.35, size = 1.5) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1.2) +
      labs(x = paste0("x(t−", k, ")"), y = "x(t)",
           title = paste0("Lag plot k=", k, ",  r = ", round(r_val, 3))) +
      theme_upwr()
  }))

  output$ch5_lag_r <- renderUI({
    x <- ch5_lag_series()
    k <- if (!is.null(input$ch5_lag_k)) input$ch5_lag_k else 1
    n <- length(x)
    if (k >= n) return(NULL)
    x_t <- x[(k+1):n]; x_lag <- x[1:(n-k)]
    r   <- cor(x_t, x_lag, use = "complete.obs")
    lc_stat_grid(
      lc_stat_box(paste0("r(", k, ")"), round(r, 3),
                  color = if (abs(r) > 0.2) upwr_accent else upwr_reference),
      columns = 1
    )
  })

  ch5_acf_selected <- reactiveVal("warszawa")
  observeEvent(input$ch5_acf_warszawa, ch5_acf_selected("warszawa"))
  observeEvent(input$ch5_acf_pszenica, ch5_acf_selected("pszenica"))
  observeEvent(input$ch5_acf_wn,       ch5_acf_selected("wn"))

  .ch5_acf_descs <- list(
    warszawa  = "Sinusoidalny wzorzec ACF z lagem 12 (i 24): temperatura ma silną, roczną sezonowość. Autokorelacje są statystycznie istotne przez dziesiątki lagów.",
    pszenica  = "Powoli zanikająca, monotonicznie malejąca ACF — charakterystyczna dla procesu AR. Ceny pszenicy 'pamiętają' przeszłość, ale efekt gaszony jest z każdym lagiem.",
    wn        = "Biały szum: wszystkie autokorelacje w granicach przedziału ufności (poza losowymi wyjątkami ~5%). Brak struktury → nie da się prognozować."
  )

  zoom_plot_server("ch5_acf_plot", reactive({
    sel <- ch5_acf_selected()
    x   <- if (sel == "wn") {
      set.seed(42); rnorm(200)
    } else {
      as.numeric(.ts_datasets[[sel]]$get_ts())
    }
    title <- switch(sel,
      warszawa = "ACF — Temperatura Warszawa",
      pszenica = "ACF — Ceny pszenicy skupu",
      wn       = "ACF — Biały szum (symulacja)"
    )
    plot_acf_gg(x, lag.max = 30, title = title)
  }))

  output$ch5_acf_desc <- renderUI({
    sel <- ch5_acf_selected()
    lc_feedback(type = "info", p(.ch5_acf_descs[[sel]]))
  })

  ch5_both_series <- reactive({
    key <- input$ch5_both_data
    as.numeric(.ts_datasets[[key]]$get_ts())
  })

  zoom_plot_server("ch5_both_lag_plot", reactive({
    x <- ch5_both_series()
    k <- if (!is.null(input$ch5_both_k)) input$ch5_both_k else 1
    n <- length(x)
    x_t   <- x[(k+1):n]
    x_lag <- x[1:(n-k)]
    r_val <- cor(x_t, x_lag, use = "complete.obs")
    df_l <- data.frame(x_lag = x_lag, x_t = x_t)
    ggplot(df_l, aes(x = x_lag, y = x_t)) +
      geom_point(color = upwr_secondary, alpha = 0.3, size = 1.2) +
      geom_smooth(method = "lm", se = FALSE, color = upwr_accent, linewidth = 1.1) +
      labs(x = paste0("x(t−", k, ")"), y = "x(t)",
           subtitle = paste0("r = ", round(r_val, 3))) +
      theme_upwr()
  }))

  zoom_plot_server("ch5_both_acf_plot", reactive({
    x  <- ch5_both_series()
    k  <- if (!is.null(input$ch5_both_k)) input$ch5_both_k else 1
    df_acf <- acf_df(x, lag.max = 24)
    df_acf <- df_acf[df_acf$lag > 0, ]
    ci     <- ci_acf(length(x), lag.max = 24)

    df_acf$highlight <- df_acf$lag == k

    ggplot(df_acf, aes(x = lag, y = acf)) +
      geom_hline(yintercept = 0, color = upwr_reference) +
      geom_hline(yintercept =  ci, linetype = "dashed", color = upwr_secondary) +
      geom_hline(yintercept = -ci, linetype = "dashed", color = upwr_secondary) +
      geom_segment(aes(xend = lag, yend = 0,
                       color = highlight,
                       linewidth = highlight)) +
      geom_point(aes(color = highlight), size = 2) +
      scale_color_manual(values = c("FALSE" = upwr_accent, "TRUE" = unname(upwr_cat["terakota"])),
                         guide = "none") +
      scale_linewidth_manual(values = c("FALSE" = 0.9, "TRUE" = 1.8), guide = "none") +
      scale_x_continuous(breaks = seq(0, 24, by = 4)) +
      labs(x = "Lag k", y = "r(k)", title = "ACF") +
      theme_upwr()
  }))

  output$ch5_both_r <- renderUI({
    x <- ch5_both_series()
    k <- if (!is.null(input$ch5_both_k)) input$ch5_both_k else 1
    n <- length(x)
    x_t <- x[(k+1):n]; x_lag <- x[1:(n-k)]
    r   <- cor(x_t, x_lag, use = "complete.obs")
    lc_stat_grid(
      lc_stat_box(paste0("r(", k, ")"), round(r, 3),
                  color = if (abs(r) > ci_acf(n)) upwr_accent else upwr_reference),
      columns = 1
    )
  })
}
