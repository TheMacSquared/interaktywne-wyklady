# ============================================================================
# CHAPTER 5: Gospodarka Wodna
# ============================================================================

ch5_ui <- lecture_chapter(
  id = "ch-wodna",
  num = "05",
  title = "Gospodarka Wodna",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 05 · Materiał kierunkowy",
      num    = "05",
      title  = "Analiza częstości i okres powrotu.",
      lead   = "Przepływ stuletni nie pojawia się grzecznie raz na sto lat. To zdarzenie z prawdopodobieństwem około 1% w każdym roku."
    ),

    lc_h2("ch5-return", "Okres powrotu jako prawdopodobieństwo"),
    p("W analizie częstości dopasowujemy rozkład do serii maksimów rocznych. Potem T lat oznacza prawdopodobieństwo przekroczenia 1/T w jednym roku, a nie kalendarzową obietnicę."),

    figure_panel(
      label = "Ryc. 5.1", title = "Maksima roczne i pozycja wykreślna Weibulla",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch5_t", "Okres powrotu T:", min = 2, max = 500, value = 100, step = 1),
          sliderInput("ch5_life", "Okres eksploatacji obiektu:", min = 1, max = 100, value = 50, step = 1),
          uiOutput("ch5_risk_info")
        ),
        column(8, zoom_plot_ui("ch5_frequency_plot", height = "350px"))
      )
    ),

    lc_h2("ch5-ostroznosc", "Ekstrapolacja nie jest darmowa"),
    p("GEV i Pearson III są standardowymi rodzinami rozkładów dla ekstremów, ale najtrudniejsza część nie polega na nazwie rozkładu. Ryzyko zaczyna się wtedy, gdy projektujemy daleko poza zakres obserwowanych lat."),

    figure_panel(
      label = "Ryc. 5.2", title = "Ile lat danych potrzebuje Q100?",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch5_record_len", "Długość serii:", min = 15, max = 80, value = 35, step = 5),
          sliderInput("ch5_boot_n", "Liczba powtórzeń bootstrap:", min = 50, max = 400, value = 150, step = 50),
          uiOutput("ch5_uncertainty_info")
        ),
        column(8, zoom_plot_ui("ch5_uncertainty_plot", height = "320px"))
      )
    ),

    lc_feedback(type = "warning",
      tags$strong("Uwaga projektowa:"),
      " Q1% z 30-letniej serii jest w dużej części ekstrapolacją. Regionalizacja pomaga, bo pożycza informację z podobnych zlewni."
    )
  )
)

ch5_server <- function(input, output, session) {
  floods <- reactive(generate_floods())

  zoom_plot_server("ch5_frequency_plot", reactive({
    d <- floods() |>
      arrange(desc(qmax)) |>
      mutate(
        rank = row_number(),
        p_exceed = rank / (n() + 1),
        T = 1 / p_exceed
      )
    ggplot(d, aes(T, qmax)) +
      geom_point(size = 2.6, alpha = 0.8) +
      geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = upwr_accent) +
      geom_vline(xintercept = input$ch5_t, linetype = "dashed", color = upwr_secondary) +
      scale_x_log10() +
      labs(x = "Okres powrotu T (lata, skala log)", y = "Maksymalny przepływ roczny Qmax")
  }))

  output$ch5_risk_info <- renderUI({
    annual_p <- 1 / input$ch5_t
    life_risk <- 1 - (1 - annual_p)^input$ch5_life
    lc_stat_grid(
      lc_stat_box("Szansa w jednym roku", fmt_pct(annual_p, 2), color = upwr_secondary),
      lc_stat_box("Ryzyko w okresie eksploatacji", fmt_pct(life_risk, 1), color = upwr_accent),
      columns = 1
    )
  })

  q_estimates <- reactive({
    set.seed(2026)
    d <- generate_floods(input$ch5_record_len)
    qfun <- function(x, target_t = 100) {
      dd <- data.frame(qmax = x) |>
        arrange(desc(qmax)) |>
        mutate(rank = row_number(), T = (n() + 1) / rank)
      fit <- lm(qmax ~ log(T), data = dd)
      unname(predict(fit, newdata = data.frame(T = target_t)))
    }
    qhat <- qfun(d$qmax)
    boot <- replicate(input$ch5_boot_n, qfun(sample(d$qmax, replace = TRUE)))
    data.frame(q100 = c(qhat, boot), typ = c("Seria oryginalna", rep("Bootstrap", length(boot))))
  })

  zoom_plot_server("ch5_uncertainty_plot", reactive({
    qs <- q_estimates()
    qhat <- qs$q100[qs$typ == "Seria oryginalna"][1]
    ggplot(qs |> filter(typ == "Bootstrap"), aes(q100)) +
      geom_histogram(bins = 24, fill = upwr_seq_burgundy[3], color = "white") +
      geom_vline(xintercept = qhat, color = upwr_accent, linewidth = 1.2) +
      labs(x = "Szacowany Q100", y = "Liczba powtórzeń")
  }))

  output$ch5_uncertainty_info <- renderUI({
    boot <- q_estimates() |> filter(typ == "Bootstrap")
    ci <- quantile(boot$q100, c(0.05, 0.95), na.rm = TRUE)
    lc_stat_grid(
      lc_stat_box("Przedział 90%", paste(fmt(ci[1], 0), "-", fmt(ci[2], 0)), color = upwr_secondary),
      lc_stat_box("Szerokość", fmt(diff(ci), 0), caption = "maleje zwykle wraz z długością serii", color = upwr_accent),
      columns = 1
    )
  })
}
