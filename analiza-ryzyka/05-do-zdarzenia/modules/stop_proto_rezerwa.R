# PUNKTY STOP (2026-11-04) — limit planistyczny, średnia i rezerwa na jednym widoku --
#
# ECharts (modules/plan_chart.js): rozkład łącznej liczby kontroli do trzeciej
# wady (r = 3, p = 0.10), pole do limitu n to P(X ≤ n), kreski średniej i limitu,
# klamra rezerwy (n − średnia). Limit przeciąga się po wykresie albo wybiera
# poziomem pewności α; „wszystkie” pokazuje 90/95/99% naraz (scalony P10
# „Koszt pewności”). Stoi za definicją 5.4.
#   d5p_plan_ui — kontener .lc-plan[data-r][data-p][data-n]
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css), w nagłówku ECharts,
# R/lc_charts.js i modules/plan_chart.js (app.R).

d5p_plan_alphas <- c("50%" = "0.5", "90%" = "0.9", "95%" = "0.95", "99%" = "0.99")

d5p_plan_css <- tags$style(HTML("
  .lc-plan .lc-area-canvas { position: relative; aspect-ratio: 2.2 / 1; touch-action: none; }
  .lc-plan .lc-area-canvas > .lc-ec { position: absolute; inset: 0; }
  .lc-plan .lc-area-canvas > .lc-chart-zoom { position: absolute; top: 0; left: 0; z-index: 1; }
  .lc-plan[data-lc-chart-large='true'] .lc-area-canvas { aspect-ratio: 1.65 / 1; }
  .lc-plan .lc-chart-handle { cursor: ew-resize; }
  .lc-plan.is-all .lc-read-value { font-size: .72em; }
  @media (max-width: 640px) {
    .lc-plan .lc-area-canvas { aspect-ratio: 1.05 / 1; }
    .lc-plan[data-lc-chart-large='true'] .lc-area-canvas { aspect-ratio: 1 / 1; }
  }
"))

d5p_plan_ui <- d5p_panel("Pojęcie", "Limit, średnia i rezerwa",
  d5p_css, d5p_plan_css,
  tags$div(class = "lc-plan", `data-r` = "3", `data-p` = "0.10", `data-n` = "30",
    lc_toolbar(
      lc_group("Poziom pewności α", tags$div(class = "lc-seg", role = "group",
        lapply(seq_along(d5p_plan_alphas), function(i) tags$button(type = "button",
          `data-plan-alpha` = d5p_plan_alphas[[i]], `aria-pressed` = "false", names(d5p_plan_alphas)[i])),
        tags$button(type = "button", `data-plan-all` = "1", `aria-pressed` = "false", "wszystkie"))),
      tags$button(type = "button", class = "lc-action is-ghost is-icon", `data-plan-reset` = "1",
        `aria-label` = "Limit równy średniej", title = "Limit równy średniej", lc_icon("reset")),
      lc_readouts(
        lc_readout("Limit n", tags$span(`data-role` = "limit", "30"), color = "var(--upwr-accent)", swatch = TRUE),
        lc_readout("P(X ≤ n)", tags$span(`data-role` = "alpha", "0.589")),
        lc_readout("Rezerwa", tags$span(`data-role` = "reserve", "0"), color = "var(--upwr-cat-bursztyn)", swatch = TRUE)
      )
    ),
    tags$div(class = "lc-area-canvas",
      lc_chart_zoom_button(),
      tags$div(class = "lc-ec", role = "img", `data-role` = "pmf",
        `aria-label` = "Rozkład liczby kontroli do trzeciej wady z kreską średniej, kreską limitu i klamrą rezerwy; wartości w odczytach nad wykresem"))
  ),
  tags$p(class = "d5p-caption",
    "Zaczynamy od limitu równego średniej: mieści się w nim tylko 58.9% audytów. Przesuń kreskę limitu albo wybierz poziom pewności. Pole pod krzywą do limitu to P(X ≤ n), a klamra między średnią a limitem to rezerwa planistyczna. Mediana (50%) leży przed średnią, więc limit „na pół audytów” daje ujemną rezerwę. „Wszystkie” pokazuje koszt pewności: z 90% na 95% rezerwa rośnie o 9 kontroli, a z 95% na 99% już o 20.")
)
