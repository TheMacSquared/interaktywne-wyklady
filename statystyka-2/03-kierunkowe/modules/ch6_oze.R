# ============================================================================
# CHAPTER 6: Odnawialne Zrodla Energii
# ============================================================================

ch6_ui <- lecture_chapter(
  id = "ch-oze",
  num = "06",
  title = "Odnawialne Źródła Energii",
  content = tagList(
    lc_chapter_hero(
      kicker = "Rozdział 06 · Materiał kierunkowy",
      num    = "06",
      title  = "Wiatr Weibulla i szeregi czasowe.",
      lead   = "Średnia prędkość wiatru to za mało, bo moc rośnie jak v³, a obserwacje w czasie nie są niezależne."
    ),

    lc_h2("ch6-weibull", "Zasób wiatru nie jest normalny"),
    p("Prędkość wiatru jest dodatnia, skośna i często dobrze opisywana Weibullem. Parametr skali c mówi o charakterystycznej prędkości, a kształt k o jednorodności warunków."),

    figure_panel(
      label = "Ryc. 6.1", title = "Rozkład prędkości wiatru i konsekwencja v³",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch6_shape", "Kształt k:", min = 1.1, max = 3.5, value = 2.0, step = 0.1),
          sliderInput("ch6_scale", "Skala c (m/s):", min = 3, max = 12, value = 7, step = 0.2),
          uiOutput("ch6_wind_info")
        ),
        column(8, zoom_plot_ui("ch6_wind_plot", height = "350px"))
      )
    ),

    figure_panel(
      label = "Ryc. 6.2", title = "Ta sama średnia wiatru, inna produkcja",
      full_width = TRUE,
      fluidRow(
        column(4,
          sliderInput("ch6_mean_compare", "Wspólna średnia prędkość (m/s):", min = 4, max = 10, value = 7, step = 0.2),
          sliderInput("ch6_k_low", "Lokalizacja A: k:", min = 1.1, max = 2.2, value = 1.5, step = 0.1),
          sliderInput("ch6_k_high", "Lokalizacja B: k:", min = 2.0, max = 4.0, value = 3.0, step = 0.1),
          uiOutput("ch6_compare_info")
        ),
        column(8, zoom_plot_ui("ch6_compare_plot", height = "330px"))
      )
    ),

    lc_h2("ch6-time", "Dane zależne w czasie"),
    p("Szereg czasowy różni się od zwykłej tabeli tym, że kolejność wierszy niesie informację. Dzisiejsza produkcja często jest podobna do wczorajszej, więc zwykła regresja może zaniżyć niepewność."),

    figure_panel(
      label = "Dla ambitnych", title = "Autokorelacja dobowej produkcji",
      full_width = TRUE,
      fluidRow(
        column(4,
          helpText("ACF pokazuje korelację szeregu z samym sobą po przesunięciu o 1, 2, 3... dni."),
          uiOutput("ch6_acf_info")
        ),
        column(8, zoom_plot_ui("ch6_acf_plot", height = "330px"))
      )
    )
  )
)

ch6_server <- function(input, output, session) {
  zoom_plot_server("ch6_wind_plot", reactive({
    k <- input$ch6_shape
    c <- input$ch6_scale
    d <- data.frame(v = seq(0.1, 25, length.out = 300)) |>
      mutate(
        density = dweibull(v, shape = k, scale = c),
        power = pmin(1, (v / 12)^3)
      )
    ggplot(d, aes(v)) +
      geom_area(aes(y = density), fill = upwr_seq_burgundy[3], alpha = 0.65) +
      geom_line(aes(y = power / 12, color = "Moc turbiny (skalowana)"), linewidth = 1.1) +
      scale_color_manual(values = c("Moc turbiny (skalowana)" = upwr_secondary), name = NULL) +
      labs(x = "Prędkość wiatru (m/s)", y = "Gęstość / moc względna")
  }))

  output$ch6_wind_info <- renderUI({
    k <- input$ch6_shape
    c <- input$ch6_scale
    expected_power <- integrate(function(v) pmin(1, (v / 12)^3) * dweibull(v, k, c), 0, 40)$value
    lc_stat_grid(
      lc_stat_box("Średnia prędkość", paste(fmt(c * gamma(1 + 1 / k), 1), "m/s"), color = upwr_secondary),
      lc_stat_box("Capacity factor", fmt_pct(expected_power, 1), caption = "uproszczona krzywa mocy", color = upwr_accent),
      columns = 1
    )
  })

  zoom_plot_server("ch6_compare_plot", reactive({
    mean_v <- input$ch6_mean_compare
    k_vals <- c("A: bardziej zmienny wiatr" = input$ch6_k_low,
                "B: bardziej równy wiatr" = input$ch6_k_high)
    d <- bind_rows(lapply(names(k_vals), function(site) {
      k <- k_vals[[site]]
      c_scale <- mean_v / gamma(1 + 1 / k)
      data.frame(
        site = site,
        v = seq(0.1, 24, length.out = 280),
        k = k,
        c_scale = c_scale
      ) |>
        mutate(density = dweibull(v, shape = k, scale = c_scale))
    }))
    ggplot(d, aes(v, density, color = site, fill = site)) +
      geom_area(alpha = 0.18, position = "identity") +
      geom_line(linewidth = 1.1) +
      scale_color_manual(values = upwr_cat_n(2), name = NULL) +
      scale_fill_manual(values = upwr_cat_n(2), name = NULL) +
      labs(x = "Prędkość wiatru (m/s)", y = "Gęstość")
  }))

  output$ch6_compare_info <- renderUI({
    mean_v <- input$ch6_mean_compare
    cf <- sapply(c(input$ch6_k_low, input$ch6_k_high), function(k) {
      c_scale <- mean_v / gamma(1 + 1 / k)
      integrate(function(v) pmin(1, (v / 12)^3) * dweibull(v, k, c_scale), 0, 40)$value
    })
    lc_stat_grid(
      lc_stat_box("Capacity factor A", fmt_pct(cf[1], 1), color = upwr_reference),
      lc_stat_box("Capacity factor B", fmt_pct(cf[2], 1), color = upwr_secondary),
      lc_stat_box("Różnica", fmt_pct(cf[1] - cf[2], 1), caption = "v³ premiuje ogon rozkładu", color = upwr_accent),
      columns = 1
    )
  })

  zoom_plot_server("ch6_acf_plot", reactive({
    d <- generate_oze_series()
    ac <- acf(d$power, plot = FALSE, lag.max = 40)
    ac_df <- data.frame(lag = as.numeric(ac$lag), acf = as.numeric(ac$acf))
    ggplot(ac_df[-1, ], aes(lag, acf)) +
      geom_hline(yintercept = 0, color = upwr_rule) +
      geom_segment(aes(xend = lag, y = 0, yend = acf), linewidth = 0.8, color = upwr_single) +
      geom_point(size = 2, color = upwr_accent) +
      labs(x = "Opóźnienie (dni)", y = "Autokorelacja")
  }))

  output$ch6_acf_info <- renderUI({
    d <- generate_oze_series()
    lag1 <- acf(d$power, plot = FALSE, lag.max = 1)$acf[2]
    lc_stat_box("ACF dla 1 dnia", fmt(lag1, 2), caption = "wysoka wartość = produkcja pamięta wczoraj", color = upwr_secondary)
  })
}
