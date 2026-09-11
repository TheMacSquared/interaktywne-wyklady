# ============================================================================
# CHAPTER 2: Technologia Zywnosci
# ============================================================================

ch2_ui <- lecture_chapter(
  id = "ch-zywnosc",
  num = "02",
  title = "Technologia Żywności",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 02 · Materiał kierunkowy",
      num    = "02",
      title  = "Sensoryka i karty kontrolne.",
      lead   = "Ocena panelowa mierzy produkt przez ludzi, a SPC pilnuje, czy proces produkcyjny nie odpływa od stabilnego rytmu."
    ),

    lc_h2("ch2-sensoryka", "Zgodność panelu"),
    p("Skale sensoryczne są zwykle porządkowe: pięć punktów na teksturę sera nie musi oznaczać równych odległości psychologicznych. Dlatego naturalnym rozszerzeniem ANOVA jest tu test Friedmana, czyli ANOVA na rangach dla tych samych oceniających."),
    p("Kendall W mówi, czy panel układa produkty w podobnej kolejności. W bliskie 0 oznacza chaos, W bliskie 1 oznacza niemal pełną zgodę."),

    figure_panel(
      label = "Ryc. 2.1", title = "Panel oceny tekstury sera",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch2_agreement", "Zgodność oceniających:", min = 0, max = 1, value = 0.65, step = 0.05),
          uiOutput("ch2_kendall_info")
        ),
        column(8, zoom_plot_ui("ch2_sensory_plot", height = "330px"))
      )
    ),

    figure_panel(
      label = "Ryc. 2.1b", title = "Czy panel ma wspólny ranking?",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("Po rangowaniu każdy oceniający ma własną kolejność produktów. W zgodnym panelu kolumny układają się podobnie u większości osób."),
          radioButtons("ch2_rank_view", "Widok:", choices = c("rangi" = "rank", "oceny surowe" = "score"), selected = "rank"),
          uiOutput("ch2_rank_comment")
        ),
        column(8, zoom_plot_ui("ch2_rank_heatmap", height = "330px"))
      )
    ),

    lc_h2("ch2-spc", "Proces pod kontrolą"),
    p("Karta X-bar nie jest testem „czy produkt spełnia specyfikację”. Ona pyta wcześniej: czy proces zachowuje się jak stabilny proces z losowymi odchyleniami wokół swojej średniej."),
    p("Granice kontrolne wynikają z danych procesu. Granice specyfikacji pochodzą z technologii, prawa albo wymagań klienta. To dwie różne rozmowy."),

    figure_panel(
      label = "Ryc. 2.2", title = "Karta X-bar dla lepkości sosu",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch2_shift", "Skok procesu od próbki 17:", min = 0, max = 3, value = 1.2, step = 0.1),
          sliderInput("ch2_trend", "Trend na próbkę:", min = -0.08, max = 0.08, value = 0, step = 0.01),
          uiOutput("ch2_spc_info")
        ),
        column(8, zoom_plot_ui("ch2_spc_plot", height = "330px"))
      )
    )
  )
)

ch2_server <- function(input, output, session) {
  sensory <- reactive(generate_sensory(input$ch2_agreement))

  zoom_plot_server("ch2_sensory_plot", reactive({
    d <- sensory()
    ggplot(d, aes(product, score, fill = product)) +
      geom_boxplot(width = 0.55, alpha = 0.75, outlier.shape = NA) +
      geom_jitter(width = 0.12, alpha = 0.4) +
      scale_fill_manual(values = upwr_cat_n(4), guide = "none") +
      labs(x = "Produkt", y = "Ocena tekstury (1-5)")
  }))

  output$ch2_kendall_info <- renderUI({
    d <- sensory()
    w <- kendall_w(d)
    pval <- friedman_p(d)
    lc_stat_grid(
      lc_stat_box("Kendall W", fmt(w, 2), caption = "0 = chaos, 1 = zgoda", color = upwr_secondary),
      lc_stat_box("Friedman", format_p(pval), caption = "czy produkty mają różne rangi", color = upwr_accent),
      columns = 1
    )
  })

  zoom_plot_server("ch2_rank_heatmap", reactive({
    d <- sensory() |>
      group_by(judge) |>
      mutate(rank = rank(score, ties.method = "average")) |>
      ungroup() |>
      mutate(value = if (input$ch2_rank_view == "rank") rank else score)
    ggplot(d, aes(product, judge, fill = value)) +
      geom_tile(color = "white", linewidth = 0.7) +
      geom_text(aes(label = fmt(value, 0)), size = 3.4) +
      scale_fill_gradient(low = upwr_seq_gold[2], high = upwr_accent, name = if (input$ch2_rank_view == "rank") "Ranga" else "Ocena") +
      labs(x = "Produkt", y = NULL)
  }))

  output$ch2_rank_comment <- renderUI({
    w <- kendall_w(sensory())
    msg <- if (w > 0.7) "Panel ma wyraźny wspólny porządek produktów."
      else if (w > 0.4) "Widać częściową zgodę, ale pojedynczy oceniający nadal potrafią zmienić ranking."
      else "Ranking jest niestabilny: średnia ocena może ukrywać brak wspólnego języka panelu."
    lc_feedback(type = if (w > 0.7) "ok" else if (w > 0.4) "info" else "warning", msg)
  })

  spc <- reactive(generate_spc(input$ch2_shift, input$ch2_trend))

  zoom_plot_server("ch2_spc_plot", reactive({
    d <- spc() |>
      group_by(sample_id) |>
      summarise(xbar = mean(value), r = diff(range(value)), .groups = "drop")
    center <- mean(d$xbar[1:15])
    sigma_xbar <- sd(d$xbar[1:15])
    ucl <- center + 3 * sigma_xbar
    lcl <- center - 3 * sigma_xbar
    ggplot(d, aes(sample_id, xbar)) +
      geom_hline(yintercept = center, color = upwr_secondary, linewidth = 1) +
      geom_hline(yintercept = c(lcl, ucl), color = upwr_accent, linetype = "dashed") +
      geom_line(linewidth = 0.9) +
      geom_point(aes(color = xbar > ucl | xbar < lcl), size = 2.6) +
      scale_color_manual(values = c("FALSE" = upwr_single, "TRUE" = upwr_accent), guide = "none") +
      labs(x = "Numer próbki", y = "Średnia lepkość", caption = "Linie przerywane: granice kontrolne, nie specyfikacja produktu")
  }))

  output$ch2_spc_info <- renderUI({
    d <- spc() |>
      group_by(sample_id) |>
      summarise(xbar = mean(value), .groups = "drop")
    center <- mean(d$xbar[1:15])
    sigma_xbar <- sd(d$xbar[1:15])
    signal <- any(d$xbar > center + 3 * sigma_xbar | d$xbar < center - 3 * sigma_xbar)
    lc_feedback(
      type = if (signal) "warning" else "ok",
      if (signal) "Pojawia się sygnał braku kontroli: proces warto zatrzymać i znaleźć przyczynę."
      else "Brak punktów poza granicami: na tym poziomie wykres wygląda stabilnie."
    )
  })
}
