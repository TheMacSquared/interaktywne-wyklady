# PUNKTY STOP (2026-11-04) — plan na stałym p a dwaj dostawcy (scalone P11 i P11B) ------
#
# Chart.js (modules/mix_chart.js). Połowa partii ma p = 0.10 − d, połowa 0.10 + d.
# Pole: rozkład łącznej liczby kontroli do trzeciej wady w mieszance; przerywany
# kontur: stałe p = 0.10. Kreska planu (domyślnie 61, czyli plan 95% przy stałym p)
# przeciąga się po wykresie; druga kreska to prawdziwy kwantyl 95% mieszanki.
# Pod wykresem rozbicie szansy ukończenia na dostawców (dawny P11B). Kreska 95%
# i rozbicie są schowane za przełącznikami, żeby sala najpierw zgadywała.
# Zastępuje widget ggplot „Stałe p kontra partie o różnej jakości” i P11/P11B.
#   d5p_mix_ui — kontener .lc-mix[data-r][data-p][data-d][data-n]
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css), w nagłówku Chart.js,
# R/lc_charts.js i modules/mix_chart.js (app.R).

d5p_mix_ds <- c("0.10/0.10" = "0", "0.08/0.12" = "0.02", "0.05/0.15" = "0.05", "0.03/0.17" = "0.07")

d5p_mix_css <- tags$style(HTML("
  .lc-mix .lc-area-canvas { position: relative; aspect-ratio: 2.2 / 1; touch-action: none; }
  .lc-mix .lc-area-canvas > canvas { position: absolute; inset: 0; }
  .lc-mix .lc-area-canvas > .lc-chart-zoom { position: absolute; top: 0; left: 0; z-index: 1; }
  .lc-mix[data-lc-chart-large='true'] .lc-area-canvas { aspect-ratio: 1.65 / 1; }
  .lc-mix .lc-chart-handle { cursor: ew-resize; }
  .lc-mix .lc-seg > * { padding: 0 .65em; }
  @media (max-width: 640px) {
    .lc-mix .lc-area-canvas { aspect-ratio: 1.4 / 1; }
    .lc-mix[data-lc-chart-large='true'] .lc-area-canvas { aspect-ratio: 1 / 1; }
  }
"))

d5p_mix_ui <- d5p_panel("Piaskownica", "Plan na 61 a dwóch dostawców",
  d5p_css, d5p_mix_css,
  tags$div(class = "lc-mix", `data-r` = "3", `data-p` = "0.10", `data-d` = "0.05", `data-n` = "61",
    lc_toolbar(
      lc_group("Dostawcy A / B: p − d / p + d", tags$div(class = "lc-seg", role = "group",
        lapply(seq_along(d5p_mix_ds), function(i) tags$button(type = "button", `data-mix-d` = d5p_mix_ds[[i]],
          `aria-pressed` = if (d5p_mix_ds[[i]] == "0.05") "true" else "false", names(d5p_mix_ds)[i])))),
      tags$button(type = "button", class = "lc-action is-ghost is-icon", `data-mix-reset` = "1",
        `aria-label` = "Od nowa", title = "Od nowa", lc_icon("reset")),
      lc_readouts(
        lc_readout("Dwaj dostawcy", tags$span(`data-role` = "pmix", "0.80"), color = "var(--upwr-accent)", swatch = TRUE),
        lc_readout("95% naprawdę", tags$span(`data-role` = "q95", "?"))
      )
    ),
    tags$div(class = "lc-area-canvas",
      lc_chart_zoom_button(),
      tags$canvas(role = "img", `data-role` = "pmf",
        `aria-label` = "Rozkład liczby kontroli do trzeciej wady przy dwóch dostawcach na tle stałego p, z kreską planu i kreską prawdziwego kwantyla 95%")),
    tags$div(class = "lc-chips", style = "margin: .5em 0 .2em",
      tags$button(type = "button", class = "lc-chip", `data-mix-show` = "showQ", `aria-pressed` = "false", "Pokaż prawdziwe 95%"),
      tags$button(type = "button", class = "lc-chip", `data-mix-show` = "showSplit", `aria-pressed` = "false", "Kto psuje plan?")),
    tags$div(class = "d5p-read", `data-role` = "split", hidden = NA, "")
  ),
  tags$p(class = "d5p-caption",
    "Plan 61 kontroli liczono przy stałym p = 0.10. Wybierz parę dostawców: średnie p zawsze wynosi 0.10, a im większy rozrzut d, tym rzadziej plan wystarcza. Zanim odsłonisz „Pokaż prawdziwe 95%”, przeciągnij kreskę planu tam, gdzie twoim zdaniem wypada uczciwy limit. „Kto psuje plan?” rozbija szansę ukończenia na dostawców.")
)
