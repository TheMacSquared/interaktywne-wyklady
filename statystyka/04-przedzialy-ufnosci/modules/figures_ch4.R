# RYCINY — rozdział 4 -----------------------------------------------------------
#   fig_4a_ui — Ryc. 4.2 · piaskownica „Zero braków” (ECharts, fig_ch4.js;
#       w sekcji „Kiedy przybliżenie zawodzi”).
#       Puenta: przy kilku brakach przedział Walda wychodzi poniżej zera,
#       a przy zerze braków ma zerową szerokość; Wilson zostaje rozsądny.
# Bez serwera: przedziały liczy fig_ch4.js (wzory zamknięte, z = 1.96).

# Przyciski segmentowe sterowane przez fig_ch4.js (bez wejść Shiny).
fig_4a_seg <- function(choices, selected) {
  tags$div(class = "lc-seg", role = "group",
    lapply(choices, function(v) {
      tags$button(type = "button", `data-zero-n` = v,
                  `aria-pressed` = if (v == selected) "true" else "false", v)
    }))
}

fig_4a_ui <- fig_panel("4a", "Ryc. 4.2", "Zero braków",
  tags$div(class = "fig-zero", `data-z` = "1.96", `data-n` = "50", `data-x` = "0",
    `data-xmax` = "15",
    lc_toolbar(
      lc_group("Sprawdzone sztuki n", fig_4a_seg(c("20", "50", "200"), "50")),
      tags$div(class = "lc-grp lc-grow lc-slider",
        tags$div(class = "lc-slider-label",
          tags$label(`for` = "fig_4a_x", "Wadliwe sztuki x"),
          tags$output(`data-role` = "x-out", "0")
        ),
        tags$input(id = "fig_4a_x", type = "range", min = 0, max = 15, value = 0,
                   step = 1, class = "fig-zero-range", `data-role` = "x"),
        tags$div(class = "lc-slider-ends", `aria-hidden` = "true",
          tags$span("0"), tags$span("15"))
      ),
      lc_readouts(
        lc_readout("p̂", tags$span(`data-role` = "phat", "0.0%"), color = col_estimate),
        lc_readout("Wald", tags$span(`data-role` = "wald", "od 0.0% do 0.0%")),
        lc_readout("Wilson", tags$span(`data-role` = "wilson", "od 0.0% do 7.1%"))
      ),
      lc_chart_zoom_button()
    ),
    tags$div(class = "fig-zero-canvas",
      tags$div(class = "lc-ec", role = "img", `data-role` = "ci",
        `aria-label` = "Dwa przedziały ufności dla odsetka wadliwych sztuk, Walda i Wilsona, na osi od -10%")),
    lc_caption(tags$span(`data-role` = "caption",
      "Wald: przedział o zerowej szerokości — jakby braków na pewno nie było."))
  )
)
