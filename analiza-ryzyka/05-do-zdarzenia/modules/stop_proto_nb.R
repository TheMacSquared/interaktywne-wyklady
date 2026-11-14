# PUNKTY STOP (2026-11-04) — rozkład ujemny dwumianowy na tle geometrycznego (P7) ---
#
# Zastępuje dawny widget ggplot „Łączna liczba kontroli”. ECharts
# (modules/nb_chart.js, wzorzec jak „Dwie krzywe normalne” w statystyce):
# jedno pole pod krzywą, stała oś Y (szczyt r = 1), blady kontur rozkładu
# geometrycznego. r zmienia się, przeciągając kropkę na średniej (albo
# strzałkami), p z przycisków nad wykresem. Wszystko liczy przeglądarka.
#   d5p7d_ui — kontener .lc-nb[data-r][data-p]
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css), w nagłówku ECharts,
# R/lc_charts.js i modules/nb_chart.js (app.R).

d5p7_p_values <- c("0.33", "0.10", "0.03")

d5p7_css <- tags$style(HTML("
  .lc-nb .lc-area-canvas { position: relative; aspect-ratio: 2.2 / 1; touch-action: none; }
  .lc-nb .lc-area-canvas > .lc-ec { position: absolute; inset: 0; }
  .lc-nb[data-lc-chart-large='true'] .lc-area-canvas { aspect-ratio: 1.65 / 1; }
  .lc-nb .lc-chart-handle { cursor: ew-resize; }
  .lc-nb .lc-area-canvas > .lc-chart-zoom { position: absolute; top: 0; left: 0; z-index: 1; }
  @media (max-width: 640px) {
    .lc-nb .lc-area-canvas { aspect-ratio: 1.05 / 1; }
    .lc-nb[data-lc-chart-large='true'] .lc-area-canvas { aspect-ratio: 1 / 1; }
  }
"))

d5p7d_ui <- d5p_panel("Piaskownica", "Rozkład i jego geometryczny cień",
  d5p_css, d5p7_css,
  tags$div(class = "lc-nb", `data-r` = "3", `data-p` = "0.10",
    lc_toolbar(
      lc_group("Wady na kontrolę, p", tags$div(class = "lc-seg", role = "group",
        lapply(d5p7_p_values, function(v) tags$button(type = "button", `data-nb-p` = v,
          `aria-label` = paste("p =", v),
          `aria-pressed` = if (v == "0.10") "true" else "false", v)))),
      tags$button(type = "button", class = "lc-action is-ghost is-icon", `data-nb-reset` = "1",
        `aria-label` = "Od nowa", title = "Od nowa", lc_icon("reset")),
      lc_readouts(
        lc_readout("Cel", tags$span(`data-role` = "r", "r = 3"), color = "var(--upwr-accent)", swatch = TRUE),
        lc_readout("Średnia r/p", tags$span(`data-role` = "mean", "30")),
        lc_readout("Mediana", tags$span(`data-role` = "median", "27")),
        lc_readout("Szczyt", tags$span(`data-role` = "peak", "0.029"))
      )
    ),
    tags$div(class = "lc-area-canvas",
      lc_chart_zoom_button(),
      tags$div(class = "lc-ec", role = "img", `data-role` = "pmf",
        `aria-label` = "Rozkład łącznej liczby kontroli do r-tej wady na tle rozkładu geometrycznego; parametry w odczytach nad wykresem"))
  ),
  tags$p(class = "d5p-caption",
    "Kropka stoi na średniej r/p. Przeciągnij ją w prawo, żeby zwiększyć r, czyli liczbę wad, na które czeka audytor; strzałki na klawiaturze robią to samo. Przerywany kontur to r = 1, czyli rozkład geometryczny. Oś pionowa jest stała: im większe r, tym niższy i szerszy rozkład, a jego szczyt wędruje w prawo.")
)
