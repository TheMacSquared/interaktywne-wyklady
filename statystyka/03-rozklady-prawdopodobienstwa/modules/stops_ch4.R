# PUNKTY STOP — rozdział 4 (i Ryc. 5.13) ---------------------------------------
#   s3p_4a_pop — populacja dla Ryc. 4.2 „Ile osób ma dokładnie 170 cm?” (s3p_4aa_ui w stops_ec.R)
#   s3p_4b_ui  — Ryc. 4.5 „Kwantyl: odwrotne pytanie” (ECharts, quantile_chart.js)
#   s3p_4c_ui  — Ryc. 4.10 „Paradoks autobusu” (w przeglądarce, stops34_charts.js)
#   s3p_4f_ui  — Ryc. 5.13 „Losowe podwyżki” (w rozdziale 5; stops34_charts.js)
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

# Rycina w przeglądarce (stops34_charts.js, "s3p-bus"): tryb, pasażerowie i odczyty w JS.
s3p_4c_ui <- s3p_panel("Ryc. 4.10", "Paradoks autobusu",
  lc_sf_figure("s3p-bus", legend = TRUE, selected = "regular",
    control = lc_segmented("s3p_4c_mode", "Autobusy", choices = c("Co 10 min, punktualnie" = "regular",
      "Średnio co 10 min, losowo" = "random")),
    toolbar = tagList(
      lc_sf_action_group("Pasażerowie", p1 = "+1", p100 = "+100", p1000 = "+1000"),
      lc_sf_action("reset", "s3p_4c_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
      lc_readouts(
        lc_readout("Pasażerowie", lc_sf_value("bus_n", "0")),
        lc_readout("Średnie czekanie", lc_sf_value("bus_wait"), color = upwr_accent),
        lc_readout("Średnia trafiona przerwa", lc_sf_value("bus_gap")))),
    body = tagList(
      lc_sf_chart(0, ratio = "5/1", max_height = "200px"),
      lc_sf_chart(1, ratio = "2.6/1", max_height = "400px")),
    notes = list(
      regular = lc_caption("Autobus co 10 minut, pasażer przychodzi o losowej porze: czeka od 0 do 10 minut, średnio 5. To rozkład jednostajny U(0, 10)."),
      random = lc_caption("Autobusy odjeżdżają średnio co 10 minut, ale losowo. Pasażer częściej trafia w długą przerwę niż w krótką, bo długa zajmuje więcej czasu. Średnio czeka więc 10 minut, a nie 5. To bezpamięciowość rozkładu wykładniczego w praktyce.")))
)

# ============================================================================
# (4f) SYMULACJA „LOSOWE PODWYŻKI”
# ============================================================================

# Rycina w przeglądarce (stops34_charts.js, "s3p-raises"): lata, pensje i odczyty w JS.
s3p_4f_ui <- s3p_panel("Ryc. 5.13", "Losowe podwyżki",
  lc_sf_figure("s3p-raises", legend = TRUE, selected = "start",
    control = lc_segmented("s3p_4f_scale", "Oś", choices = c("Pensja (zł)" = "raw", "ln(pensji)" = "log")),
    toolbar = tagList(
      lc_sf_action_group("Czas", y1 = "+1 rok", y10 = "+10 lat"),
      lc_sf_action("reset", "s3p_4f_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
      lc_readouts(
        lc_readout("Rok", lc_sf_value("pay_year", "0")),
        lc_readout("Średnia", lc_sf_value("pay_mean", "5 000 zł"), color = upwr_accent),
        lc_readout("Mediana", lc_sf_value("pay_median", "5 000 zł")),
        lc_readout("Poniżej średniej", lc_sf_value("pay_below", "0%")))),
    ratio = "2.4/1", max_height = "420px",
    notes = list(
      start = lc_caption("Tysiąc osób zaczyna z pensją 5000 zł. Co rok każdy dostaje losową podwyżkę: średnio 4%, ale raz 15%, raz −8%."),
      raw = lc_caption("Procent od dużej pensji to więcej złotówek niż procent od małej, więc bogaci odjeżdżają: pojawia się długi prawy ogon, a średnia ucieka od mediany. Przełącz oś na ln(pensji)."),
      log = lc_caption("Po zlogarytmowaniu ogon znika i zostaje symetryczny dzwon: logarytm zamienia mnożenie w dodawanie, a suma wielu wpływów daje rozkład normalny.")))
)

# ============================================================================
# SERWER
# ============================================================================

# Ryc. 4.10 i 5.13 liczy przeglądarka (stops34_charts.js); serwer zostaje, bo woła go app.R.
s3p_ch4_server <- function(input, output, session) {
  # Ryc. 4.10 i 5.13 liczy przeglądarka (stops34_charts.js); app.R wywołuje tę funkcję.
}
