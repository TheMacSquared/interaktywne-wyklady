# PUNKTY STOP — rozdział 4 (i Ryc. 5.13) ---------------------------------------
#   s3p_4a_pop — populacja dla Ryc. 4.2 „Ile osób ma dokładnie 170 cm?” (s3p_4aa_ui w stops_ec.R)
#   s3p_4b_ui  — Ryc. 4.5 „Kwantyl: odwrotne pytanie” (ECharts, quantile_chart.js)
#   s3p_4c_ui  — Ryc. 4.10 „Paradoks autobusu”
#   s3p_4f_ui  — Ryc. 5.13 „Losowe podwyżki” (w rozdziale 5)
# Serwer: s3p_ch4_server(input, output, session).

# ============================================================================
# (4a) PIASKOWNICA „ILE OSÓB MA DOKŁADNIE 170 CM?”
# ============================================================================

s3p_4a_pop <- local({ set.seed(3401); rnorm(1e6, 170, 8) })

# ============================================================================
# (4b) „KWANTYL: ODWROTNE PYTANIE” — ECharts (quantile_chart.js), wzrost N(170, 8)
# ============================================================================

# Przyciski segmentowe sterowane przez quantile_chart.js (bez wejść Shiny): atrybut attr = wartość.
s3p_4b_seg <- function(attr, choices, selected) {
  tags$div(class = "lc-seg", role = "group",
    lapply(seq_along(choices), function(i) {
      v <- unname(choices[[i]])
      do.call(tags$button, c(list(type = "button", `aria-pressed` = if (v == selected) "true" else "false",
                                  names(choices)[[i]]), setNames(list(v), attr)))
    }))
}

s3p_4b_ui <- s3p_panel("Ryc. 4.5", "Kwantyl: odwrotne pytanie",
  tags$div(class = "lc-quant", `data-mu` = "170", `data-sd` = "8", `data-x` = "178",
    `data-lo` = "140", `data-hi` = "200",
    lc_toolbar(
      lc_group("Znam", s3p_4b_seg("data-q-mode", c("x" = "x", "p" = "p"), "x")),
      tags$button(type = "button", class = "lc-action is-solid", `data-q-reveal` = "1", tags$span("Odsłoń")),
      lc_readouts(
        lc_readout("x", tags$span(`data-role` = "x", "178.0")),
        lc_readout("P(X ≤ x)", tags$span(`data-role` = "p", "?"), color = "var(--upwr-accent)")
      ),
      lc_chart_zoom_button()
    ),
    tags$div(class = "lc-area-canvas",
      tags$div(class = "lc-ec", role = "img", `data-role` = "pdf", `aria-label` = "Gęstość wzrostu N(170, 8) z polem na lewo od x"))
  )
)

# ============================================================================
# (4c) SYMULACJA „PARADOKS AUTOBUSU”
# ============================================================================

s3p_4c_ui <- s3p_panel("Ryc. 4.10", "Paradoks autobusu",
  lc_toolbar(
    lc_segmented("s3p_4c_mode", "Autobusy", choices = c("Co 10 min, punktualnie" = "regular",
      "Średnio co 10 min, losowo" = "random")),
    lc_action_group(label = "Pasażerowie", s3p_4c_p1 = "+1", s3p_4c_p100 = "+100", s3p_4c_p1000 = "+1000"),
    lc_action("s3p_4c_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
    lc_readouts(uiOutput("s3p_4c_read"))
  ),
  lc_plot("s3p_4c_line", ratio = "5/1"),
  lc_plot("s3p_4c_hist", ratio = "2.6/1"),
  uiOutput("s3p_4c_caption")
)

# ============================================================================
# (4f) SYMULACJA „LOSOWE PODWYŻKI”
# ============================================================================

s3p_4f_ui <- s3p_panel("Ryc. 5.13", "Losowe podwyżki",
  lc_toolbar(
    lc_action_group(label = "Czas", s3p_4f_y1 = "+1 rok", s3p_4f_y10 = "+10 lat"),
    lc_action("s3p_4f_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
    lc_segmented("s3p_4f_scale", "Oś", choices = c("Pensja (zł)" = "raw", "ln(pensji)" = "log")),
    lc_readouts(uiOutput("s3p_4f_read"))
  ),
  lc_plot("s3p_4f_plot", ratio = "2.4/1"),
  uiOutput("s3p_4f_caption")
)

# ============================================================================
# SERWER
# ============================================================================

s3p_ch4_server <- function(input, output, session) {

  # --- 4c -------------------------------------------------------------------
  bus_times <- reactive({
    if (identical(input$s3p_4c_mode, "random")) cumsum(rexp(400, 1 / 10)) else seq(10, 4000, 10)
  })
  pax <- reactiveVal(data.frame(t = numeric(0), wait = numeric(0), gap = numeric(0)))
  observeEvent(input$s3p_4c_mode, pax(pax()[0, ]), ignoreInit = TRUE)
  add_pax <- function(n) {
    b <- bus_times()
    t <- runif(n, 0, max(b) - 50)
    nxt <- findInterval(t, b) + 1
    prev <- c(0, b)[nxt]
    pax(rbind(pax(), data.frame(t = t, wait = b[nxt] - t, gap = b[nxt] - prev)))
  }
  observeEvent(input$s3p_4c_p1, add_pax(1))
  observeEvent(input$s3p_4c_p100, add_pax(100))
  observeEvent(input$s3p_4c_p1000, add_pax(1000))
  observeEvent(input$s3p_4c_reset, pax(pax()[0, ]))

  output$s3p_4c_read <- renderUI({
    p <- pax()
    tagList(lc_readout("Pasażerowie", nrow(p)),
            lc_readout("Średnie czekanie", if (nrow(p)) paste(lc_fmt(mean(p$wait), 1), "min") else "–", color = upwr_accent),
            lc_readout("Średnia trafiona przerwa", if (nrow(p)) paste(lc_fmt(mean(p$gap), 1), "min") else "–"))
  })

  zoom_plot_server("s3p_4c_line", reactive({
    b <- bus_times(); p <- pax()
    c0 <- if (nrow(p)) tail(p$t, 1) else 60
    win <- c(max(0, c0 - 60), max(0, c0 - 60) + 120)
    bb <- b[b >= win[1] & b <= win[2]]
    g <- ggplot() +
      annotate("segment", x = win[1], xend = win[2], y = 0, yend = 0, colour = upwr_rule, linewidth = 1.2) +
      geom_point(data = data.frame(x = bb), aes(x, 0), shape = 15, size = 4, colour = upwr_secondary) +
      coord_cartesian(xlim = win, ylim = c(-1, 1)) +
      labs(x = "Czas (min)", y = NULL, subtitle = "Kwadraty: odjazdy autobusów") +
      theme_upwr(grid = "none") + theme(axis.text.y = element_blank())
    if (nrow(p)) {
      last <- tail(p, 1)
      g <- g + annotate("segment", x = last$t, xend = last$t + last$wait, y = 0.4, yend = 0.4,
                        colour = upwr_accent, linewidth = 1.4, arrow = arrow(length = unit(0.2, "cm"))) +
        annotate("point", x = last$t, y = 0.4, colour = upwr_accent, size = 3.5) +
        annotate("text", x = last$t, y = 0.85, label = sprintf("czeka %s min", lc_fmt(last$wait, 1)),
                 colour = upwr_accent, hjust = 0, fontface = "bold")
    }
    g
  }))

  zoom_plot_server("s3p_4c_hist", reactive({
    p <- pax()
    if (!nrow(p)) return(ggplot() + annotate("text", 0, 0, label = "Wpuść pasażerów", colour = upwr_reference, size = 5) + theme_void())
    ggplot(p, aes(wait)) +
      geom_histogram(binwidth = 1, boundary = 0, fill = upwr_secondary, colour = "white", alpha = 0.85) +
      geom_vline(xintercept = mean(p$wait), colour = upwr_accent, linewidth = 1) +
      annotate("text", x = mean(p$wait), y = Inf, vjust = 1.5, hjust = -0.05, colour = upwr_accent, fontface = "bold",
               label = sprintf("średnio %s min", lc_fmt(mean(p$wait), 1))) +
      coord_cartesian(xlim = c(0, 45)) +
      labs(x = "Czas czekania na przystanku (min)", y = "Liczba pasażerów") + theme_upwr(grid = "y")
  }))

  output$s3p_4c_caption <- renderUI({
    lc_caption(if (identical(input$s3p_4c_mode, "random"))
      "Autobusy odjeżdżają średnio co 10 minut, ale losowo. Pasażer częściej trafia w długą przerwę niż w krótką, bo długa zajmuje więcej czasu. Średnio czeka więc 10 minut, a nie 5. To bezpamięciowość rozkładu wykładniczego w praktyce."
    else "Autobus co 10 minut, pasażer przychodzi o losowej porze: czeka od 0 do 10 minut, średnio 5. To rozkład jednostajny U(0, 10).")
  })

  # --- 4f -------------------------------------------------------------------
  pay <- reactiveVal(list(year = 0, x = rep(5000, 1000)))
  grow <- function(years) {
    s <- pay(); x <- s$x
    for (i in seq_len(years)) x <- x * (1 + rnorm(length(x), 0.04, 0.12))
    pay(list(year = s$year + years, x = pmax(x, 100)))
  }
  observeEvent(input$s3p_4f_y1, grow(1))
  observeEvent(input$s3p_4f_y10, grow(10))
  observeEvent(input$s3p_4f_reset, pay(list(year = 0, x = rep(5000, 1000))))

  output$s3p_4f_read <- renderUI({
    s <- pay()
    tagList(lc_readout("Rok", s$year), lc_readout("Średnia", paste(lc_fmt(mean(s$x), 0, " "), "zł"), color = upwr_accent),
            lc_readout("Mediana", paste(lc_fmt(median(s$x), 0, " "), "zł")),
            lc_readout("Poniżej średniej", s3p_pct(mean(s$x < mean(s$x)), 0)))
  })

  zoom_plot_server("s3p_4f_plot", reactive({
    s <- pay(); lg <- identical(input$s3p_4f_scale, "log")
    v <- if (lg) log(s$x) else s$x
    p <- ggplot(data.frame(v = v), aes(v))
    if (s$year == 0) {
      p <- p + geom_vline(xintercept = v[1], colour = upwr_secondary, linewidth = 2)
    } else {
      p <- p + geom_histogram(bins = 40, fill = upwr_secondary, alpha = 0.8, colour = "white") +
        geom_vline(xintercept = if (lg) log(mean(s$x)) else mean(s$x), colour = upwr_accent, linewidth = 1, linetype = "solid") +
        geom_vline(xintercept = if (lg) log(median(s$x)) else median(s$x), colour = upwr_ink, linetype = "22", linewidth = 1)
    }
    if (!lg) p <- p + scale_x_continuous(labels = function(v) lc_fmt(v, 0, " "))
    p + labs(x = if (lg) "ln(pensja)" else "Pensja (zł)", y = "Liczba osób",
             subtitle = "Linia ciągła: średnia · przerywana: mediana") +
      theme_upwr(grid = "y")
  }))

  output$s3p_4f_caption <- renderUI({
    s <- pay()
    lc_caption(if (s$year == 0) "Tysiąc osób zaczyna z pensją 5000 zł. Co rok każdy dostaje losową podwyżkę: średnio 4%, ale raz 15%, raz −8%."
    else if (identical(input$s3p_4f_scale, "log")) "Po zlogarytmowaniu ogon znika i zostaje symetryczny dzwon: logarytm zamienia mnożenie w dodawanie, a suma wielu wpływów daje rozkład normalny."
    else "Procent od dużej pensji to więcej złotówek niż procent od małej, więc bogaci odjeżdżają: pojawia się długi prawy ogon, a średnia ucieka od mediany. Przełącz oś na ln(pensji).")
  })
}
