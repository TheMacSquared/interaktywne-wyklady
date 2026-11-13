# Ryciny rysowane w przeglądarce (lc_stat_figures.js, window.lcSF).
# Wykład rejestruje własne ryciny w JS (lcSF.register({nazwa: figura})), a w R
# stawia kontener lc_sf_figure("nazwa", ...). Kontrolka zostaje zwykłym wejściem
# (natywny <select>, lc_segmented, checkboxInput, checkboxGroupInput albo
# lc_sf_slider), ale wykres czyta ją sam, więc działa także offline.
# Opisy zależne od wyboru można policzyć w R z góry: JS pokazuje ten, którego
# data-when odpowiada wybranej wartości (dla pola wyboru: "on" / "off").
# Dane dla ryciny (np. scenariusze) przechodzą w config jako JSON (data-config).

# Pojedynczy wykres; k to numer wykresu w rycinie (od 0).
lc_sf_chart <- function(k = 0, ratio = "2/1", max_height = "340px") {
  tags$div(class = "lc-sf-chart", `data-k` = k,
    style = paste0("aspect-ratio:", ratio, ";max-height:", max_height),
    tags$div(class = "lc-ec", `data-chart` = k, role = "img", `aria-label` = "Wykres"))
}

# Przycisk obsługiwany w przeglądarce (np. losowanie): data-stat-act = akcja.
lc_sf_action <- function(act, ...) {
  htmltools::tagAppendAttributes(lc_action(...), `data-stat-act` = act)
}

# Wartość wypełniana przez JS (odczyt, komórka tabeli).
lc_sf_value <- function(key, text = "–") tags$span(`data-v` = key, text)

lc_sf_figure <- function(fig, control = NULL, ratio = "2/1", max_height = "340px",
                         charts = 1, headings = NULL, notes = NULL, legend = FALSE,
                         selected = NULL, toolbar = NULL, body = NULL, after = NULL,
                         bare = FALSE, config = NULL) {
  if (is.null(body)) {
    boxes <- lapply(seq_len(charts), function(k) {
      box <- lc_sf_chart(k - 1, ratio, max_height)
      if (is.null(headings)) box else tags$div(tags$h4(headings[[k]]), box)
    })
    body <- if (charts > 1) lc_plots(boxes) else boxes[[1]]
  }
  # Kilka kontrolek: zwykła lista tagów; każda dostaje klasę lc-sf-ctl.
  if (is.list(control) && !inherits(control, c("shiny.tag", "shiny.tag.list"))) {
    control <- do.call(tagList, lapply(control, lc_sf_ctl))
  } else if (!is.null(control)) {
    control <- lc_sf_ctl(control)
  }
  # bare: rycina krokowa — kontrolki i lupa są w pasku lc_step_widget.
  bar <- if (bare) NULL else if (is.null(control) && is.null(toolbar)) {
    # Bez kontrolek: sama lupa w rogu, bez pustego paska.
    tags$div(class = "lc-sf-zoom", lc_chart_zoom_button())
  } else {
    lc_toolbar(control, toolbar, lc_chart_zoom_button())
  }
  tags$div(class = "lc-sf", `data-fig` = fig,
    `data-config` = if (!is.null(config)) as.character(jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA)),
    bar,
    if (legend) tags$div(class = "lc-sf-legend", hidden = NA),
    body,
    if (!is.null(notes)) lapply(names(notes), function(v)
      tags$div(`data-when` = v, hidden = if (!identical(v, selected)) NA, notes[[v]])),
    after
  )
}

# Kontrolka czytana przez lc_stat_figures.js (klasa na samej kontrolce, bez
# opakowania, żeby pasek układał ją jak inne).
lc_sf_ctl <- function(tag) {
  cls <- unlist(strsplit(paste(tag$attribs$class, collapse = " "), " "))
  if ("lc-sf-ctl" %in% cls) tag else htmltools::tagAppendAttributes(tag, class = "lc-sf-ctl")
}

# Natywny suwak z odczytem wartości (wartość i zakres ustawia przeglądarka).
# suffix: jednostka za odczytem (np. " min").
lc_sf_slider <- function(id, label, min, max, value, step, suffix = NULL) {
  tags$div(class = "lc-grp lc-sf-ctl lc-sf-slider",
    tags$label(`for` = id, class = "lc-grp-l", label, " ",
               tags$output(`data-value` = id, value), suffix),
    tags$input(id = id, type = "range", min = min, max = max, value = value, step = step))
}

lc_sf_styles <- function() tags$style(HTML("
  .lc-sf { position: relative; }
  .lc-sf-zoom { position: absolute; right: .3em; top: .3em; z-index: 3; }
  .lc-sf-chart { position: relative; width: 100%; min-height: 14em; }
  .lc-sf[data-lc-chart-large='true'] .lc-sf-chart { max-height: none !important; min-height: 24em; }
  .lc-sf-legend { display: flex; flex-wrap: wrap; gap: .4em 1.4em; margin: .4em 0 0;
    font-size: .9em; color: var(--upwr-ink-soft); }
  .lc-sf-legend-item { display: inline-flex; align-items: center; }
  .lc-sf-slider { min-width: min(100%, 14em); flex: 1 1 14em; }
  .lc-sf-slider input[type='range'] { width: 100%; accent-color: var(--upwr-accent); cursor: pointer; }
  .lc-sf-slider output { font-family: var(--upwr-mono); color: var(--upwr-ink); }
  .lc-sf-colorbar { gap: .5em; }
  .lc-sf-colorbar-bar { display: inline-block; width: 9em; height: .7em; border: 1px solid var(--upwr-rule); }
  .lc-sf-box-sw { display: inline-block; width: .9em; height: .9em; margin-right: .4em;
    background: var(--lc-sw); vertical-align: middle; }
  .lc-sf-line-sw { display: inline-block; width: 1.5em; height: 0; margin-right: .4em;
    border-top: 3px solid var(--lc-sw); vertical-align: middle; }
  .lc-sf-line-sw.is-dashed { border-top-style: dashed; }
  .lc-sf-line-sw.is-dotted { border-top-style: dotted; }
"))
