# Ryciny statystyk opisowych rysowane w przeglądarce (stat_charts.js).
# Kontrolka zostaje zwykłym wejściem (natywny <select>, lc_segmented albo
# checkboxInput), ale wykres czyta ją sam, więc działa także offline. Opisy
# zależne od wyboru liczymy tu dla każdej opcji z góry (dane ankiety są stałe);
# JS pokazuje ten, którego data-when odpowiada wybranej wartości
# (dla pola wyboru: "on" / "off").

# Pojedynczy wykres; k to numer wykresu w rycinie (od 0).
lc01_stat_chart <- function(k = 0, ratio = "2/1", max_height = "340px") {
  tags$div(class = "lc01-stat-chart", `data-k` = k,
    style = paste0("aspect-ratio:", ratio, ";max-height:", max_height),
    tags$div(class = "lc-ec", `data-chart` = k, role = "img", `aria-label` = "Wykres"))
}

# Przycisk obsługiwany w przeglądarce (np. losowanie): data-stat-act = akcja.
lc01_stat_action <- function(act, ...) {
  htmltools::tagAppendAttributes(lc_action(...), `data-stat-act` = act)
}

# Wartość wypełniana przez JS (odczyt, komórka tabeli).
lc01_stat_value <- function(key, text = "–") tags$span(`data-v` = key, text)

lc01_stat_figure <- function(fig, control = NULL, ratio = "2/1", max_height = "340px",
                             charts = 1, headings = NULL, notes = NULL, legend = FALSE,
                             selected = NULL, toolbar = NULL, body = NULL, after = NULL,
                             bare = FALSE) {
  if (is.null(body)) {
    boxes <- lapply(seq_len(charts), function(k) {
      box <- lc01_stat_chart(k - 1, ratio, max_height)
      if (is.null(headings)) box else tags$div(tags$h4(headings[[k]]), box)
    })
    body <- if (charts > 1) lc_plots(boxes) else boxes[[1]]
  }
  # bare: rycina krokowa — kontrolki i lupa są w pasku lc_step_widget.
  bar <- if (bare) NULL else if (is.null(control) && is.null(toolbar)) {
    # Bez kontrolek: sama lupa w rogu, bez pustego paska.
    tags$div(class = "lc01-stat-zoom", lc_chart_zoom_button())
  } else {
    lc_toolbar(
      # Klasa na samej kontrolce (bez opakowania), żeby pasek układał ją jak inne.
      if (!is.null(control)) htmltools::tagAppendAttributes(control, class = "lc01-stat-ctl"),
      toolbar,
      lc_chart_zoom_button()
    )
  }
  tags$div(class = "lc01-stat", `data-fig` = fig,
    bar,
    if (legend) tags$div(class = "lc01-legend", hidden = NA),
    body,
    if (!is.null(notes)) lapply(names(notes), function(v)
      tags$div(`data-when` = v, hidden = if (!identical(v, selected)) NA, notes[[v]])),
    after
  )
}

lc01_stat_styles <- function() tags$style(HTML("
  .lc01-stat { position: relative; }
  .lc01-stat-zoom { position: absolute; right: .3em; top: .3em; z-index: 3; }
  .lc01-stat-chart { position: relative; width: 100%; min-height: 14em; }
  .lc01-stat[data-lc-chart-large='true'] .lc01-stat-chart { max-height: none !important; min-height: 24em; }
  .lc01-legend { display: flex; flex-wrap: wrap; gap: .4em 1.4em; margin: .4em 0 0;
    font-size: .9em; color: var(--upwr-ink-soft); }
  .lc01-legend-item { display: inline-flex; align-items: center; }
  .lc01-stat-slider { min-width: min(100%, 14em); flex: 1 1 14em; }
  .lc01-stat-slider input[type='range'] { width: 100%; accent-color: var(--upwr-accent); cursor: pointer; }
  .lc01-stat-slider output { font-family: var(--upwr-mono); color: var(--upwr-ink); }
  .lc01-box-sw { display: inline-block; width: .9em; height: .9em; margin-right: .4em;
    background: var(--lc-sw); vertical-align: middle; }
"))

# Kontrolka w pasku ryciny krokowej (kontrolki odczytuje stat_charts.js).
lc01_stat_ctl <- function(tag) htmltools::tagAppendAttributes(tag, class = "lc01-stat-ctl")

# Natywny suwak z odczytem wartości (wartość i zakres ustawia przeglądarka).
lc01_stat_slider <- function(id, label, min, max, value, step) {
  tags$div(class = "lc-grp lc01-stat-ctl lc01-stat-slider",
    tags$label(`for` = id, class = "lc-grp-l", label, " ",
               tags$output(`data-value` = id, value)),
    tags$input(id = id, type = "range", min = min, max = max, value = value, step = step))
}
