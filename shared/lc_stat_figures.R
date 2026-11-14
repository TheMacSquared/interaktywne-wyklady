# Ryciny rysowane w przeglądarce (lc_stat_figures.js, window.lcSF).
# Wykład rejestruje własne ryciny w JS (lcSF.register({nazwa: figura})), a w R
# stawia kontener lc_sf_figure("nazwa", ...). Kontrolka zostaje zwykłym wejściem
# (natywny <select>, lc_segmented, checkboxInput, checkboxGroupInput albo
# lc_sf_slider), ale wykres czyta ją sam, więc działa także offline.
# Opisy zależne od wyboru można policzyć w R z góry: JS pokazuje ten, którego
# data-when odpowiada wybranej wartości (dla pola wyboru: "on" / "off").
# Dane dla ryciny (np. scenariusze) przechodzą w config jako JSON (data-config).

# Pojedynczy wykres; k to numer wykresu w rycinie (od 0).
# Niskie paski (ratio szersze niż 3/1, np. oś czasu 5/1) dostają mniejszą
# minimalną wysokość, żeby min-height nie przebijał max_height.
lc_sf_chart <- function(k = 0, ratio = "2/1", max_height = "340px") {
  r <- vapply(strsplit(ratio, "/", fixed = TRUE), function(p) as.numeric(p[1]) / as.numeric(p[2]), 0)
  tags$div(class = if (isTRUE(r > 3)) "lc-sf-chart is-strip" else "lc-sf-chart", `data-k` = k,
    style = paste0("aspect-ratio:", ratio, ";max-height:", max_height),
    tags$div(class = "lc-ec", `data-chart` = k, role = "img", `aria-label` = "Wykres"))
}

# Przycisk obsługiwany w przeglądarce (np. losowanie): data-stat-act = akcja.
lc_sf_action <- function(act, ...) {
  htmltools::tagAppendAttributes(lc_action(...), `data-stat-act` = act)
}

# Grupa przycisków obsługiwanych w przeglądarce, jak lc_action_group() bez Shiny:
# lc_sf_action_group("Pobierz próby", take1 = "1", take100 = "100").
lc_sf_action_group <- function(label, ...) {
  acts <- list(...)
  stopifnot(length(acts) > 0, !is.null(names(acts)), all(nzchar(names(acts))))
  lc_group(label,
    tags$div(class = "lc-seg", role = "group", `aria-label` = label,
      lapply(names(acts), function(a) tags$button(type = "button", `data-stat-act` = a, acts[[a]]))))
}

# Wartość wypełniana przez JS (odczyt, komórka tabeli).
lc_sf_value <- function(key, text = "–") tags$span(`data-v` = key, text)

lc_sf_figure <- function(fig, control = NULL, ratio = "2/1", max_height = "340px",
                         charts = 1, headings = NULL, notes = NULL, legend = FALSE,
                         selected = NULL, toolbar = NULL, body = NULL, after = NULL,
                         bare = FALSE, config = NULL, chartless = FALSE, pair = FALSE) {
  if (is.null(body)) {
    boxes <- lapply(seq_len(charts), function(k) {
      box <- lc_sf_chart(k - 1, ratio, max_height)
      if (is.null(headings)) box else tags$div(tags$h4(headings[[k]]), box)
    })
    # pair: dwa wykresy obok siebie już w kolumnie tekstu (lc_plots(pair = TRUE)).
    body <- if (charts > 1) lc_plots(boxes, pair = pair) else boxes[[1]]
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
  # chartless: rycina bez wykresu (same odczyty albo rysunek z R w body, liczby w lc_sf_value()).
  tags$div(class = "lc-sf", `data-fig` = fig, `data-lc-nochart` = if (isTRUE(chartless)) NA,
    `data-config` = if (!is.null(config)) as.character(jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA)),
    bar,
    if (legend) tags$div(class = "lc-sf-legend", hidden = NA),
    body,
    if (!is.null(notes)) lapply(names(notes), function(v)
      tags$div(`data-when` = v, hidden = if (!identical(v, selected)) NA, notes[[v]])),
    after
  )
}

# ---- Rycina „opis z R” --------------------------------------------------------
# Wykres na stałych danych: R składa opis rysunku (zakresy, warstwy, osie) przy
# budowaniu strony, przeglądarka go rysuje (rycina lc_spec w lc_stat_figures.js).
# Odpowiada dawnemu ggplot + zoom_plot_server() bez wejść.
#   spec      sf_spec(…) albo lista wykresów list(charts = list(sf_spec(…), …))
#   variants  nazwana lista opisów wybieranych kontrolką `control` (nazwa = wartość)
#   reads     odczyty lc_sf_value(): nazwana lista tekstów (dla wariantów: lista list)
# Barwy w opisie: "@niebo", "@accent", "@ref", "@wrzos", …; z przezroczystością "@niebo/0.6".
#   html      gotowe fragmenty HTML do elementów [data-role = nazwa] (np. tabela wariantu)
lc_sf_spec <- function(spec = NULL, variants = NULL, reads = NULL, charts = NULL, html = NULL, ...) {
  config <- if (!is.null(variants)) list(variants = variants) else spec
  if (!is.null(reads)) config$reads <- reads
  if (!is.null(html)) config$html <- html
  first <- if (!is.null(variants)) variants[[1]] else spec
  if (is.null(charts)) charts <- if (!is.null(first$charts)) length(first$charts) else 1
  lc_sf_figure("lc_spec", config = config, charts = charts, ...)
}

# Opis jednego wykresu. x, y: zakresy osi; layers: lista warstw sf_*().
# Pozostałe argumenty trafiają do opisu (xLabels, yLabels, legend, noYGrid, xTicks, …).
sf_spec <- function(x, y, layers, x_name = "", y_name = "", aria = "Wykres", ...) {
  # Argumenty NULL (np. brak legendy) nie trafiają do opisu: w JSON byłyby pustym obiektem.
  extra <- Filter(Negate(is.null), list(...))
  # Oś Y schodząca poniżej zera pokazuje podpisy ujemne (chyba że ma własne yLabels).
  if (y[1] < 0 && is.null(extra$yLabels) && is.null(extra$yNegative)) extra$yNegative <- TRUE
  c(list(x = x, y = y, layers = unname(Filter(Negate(is.null), layers)),
         xName = x_name, yName = y_name, aria = aria), extra)
}
# Zakres danych z marginesem jak domyślna oś ggplot (5% z każdej strony).
sf_range <- function(v, pad = 0.05) {
  r <- range(v, na.rm = TRUE)
  d <- diff(r) * pad
  if (d == 0) d <- 1
  c(r[1] - d, r[2] + d)
}
# Pary [x, y] jako lista (jsonlite nie spłaszcza pojedynczego punktu do liczby).
sf_xy <- function(x, y, digits = 4) {
  x <- unname(x); y <- unname(y)
  lapply(seq_along(x), function(i) c(round(x[i], digits), round(y[i], digits)))
}
# Podpisy osi kategorii: pozycje 1, 2, … → napisy.
sf_labels <- function(labels, at = seq_along(labels)) {
  stats::setNames(as.list(as.character(labels)), at)
}
sf_points <- function(x, y, color = "@accent", size = 6, alpha = NULL, symbol = NULL) {
  Filter(Negate(is.null), list(type = "points", data = sf_xy(x, y), color = color, size = size, alpha = alpha, symbol = symbol))
}
sf_line <- function(x, y, color = "@accent", width = 2, dash = NULL, fill = NULL, fill_alpha = NULL) {
  Filter(Negate(is.null), list(type = "line", data = sf_xy(x, y), color = color, width = width,
                               dash = dash, fill = fill, fillAlpha = fill_alpha))
}
sf_dash <- c(6, 5)   # linia przerywana jak linetype = "dashed"
sf_dot <- c(2, 4)    # linia kropkowana jak linetype = "dotted"
sf_vline <- function(x, color = "@ref", width = 1.4, dash = sf_dash, from = NULL, to = NULL) {
  Filter(Negate(is.null), list(type = "vline", x = x, color = color, width = width, dash = dash, from = from, to = to))
}
sf_hline <- function(y, x, color = "@ref", width = 1.4, dash = sf_dash) {
  sf_line(x, c(y, y), color = color, width = width, dash = dash)
}
# Słupki: jeden kolor (color) albo po kolorze na słupek (colors). horizontal: słupki poziome,
# wtedy lo/hi to zakres na osi Y, a value — długość.
sf_bars <- function(lo, hi, value, color = "@secondary", colors = NULL, alpha = NULL,
                    stroke = "@surface", horizontal = FALSE) {
  Filter(Negate(is.null), list(type = if (horizontal) "hbins" else "bins",
    data = lapply(seq_along(value), function(i) c(lo[i], hi[i], value[i])),
    color = color, colors = if (!is.null(colors)) as.list(colors), alpha = alpha, stroke = stroke))
}
# Słupki kategorii o szerokości `width` wokół pozycji 1, 2, …
sf_cols <- function(value, width = 0.9, at = seq_along(value), ...) {
  sf_bars(at - width / 2, at + width / 2, value, ...)
}
# Histogram jak geom_histogram(): granice przedziałów i wysokości (liczebność albo gęstość).
sf_hist <- function(v, breaks = NULL, bins = 30, density = FALSE, ...) {
  if (is.null(breaks)) {
    b <- ggplot2::ggplot_build(ggplot2::ggplot(data.frame(v), ggplot2::aes(v)) + ggplot2::geom_histogram(bins = bins))$data[[1]]
    lo <- b$xmin; hi <- b$xmax; h <- if (density) b$density else b$count
  } else {
    hh <- graphics::hist(v[v >= min(breaks) & v <= max(breaks)], breaks = breaks, plot = FALSE)
    lo <- utils::head(breaks, -1); hi <- breaks[-1]; h <- if (density) hh$density else hh$counts
  }
  sf_bars(round(lo, 5), round(hi, 5), h, ...)
}
# Statystyki pudełka jak geom_boxplot(): kwartyle typu 7, wąsy do 1.5 IQR.
sf_box_stats <- function(v) {
  q <- unname(stats::quantile(v, c(.25, .5, .75), na.rm = TRUE))
  inside <- v[!is.na(v) & v >= q[1] - 1.5 * (q[3] - q[1]) & v <= q[3] + 1.5 * (q[3] - q[1])]
  list(q1 = q[1], med = q[2], q3 = q[3], lo = min(inside), hi = max(inside),
       out = v[!is.na(v) & (v < min(inside) | v > max(inside))])
}
# Pudełka grup: values to lista wektorów; fills — barwy wypełnienia (z przezroczystością).
sf_boxes <- function(values, fills, width = 0.75, stroke = "@accent", at = seq_along(values), horizontal = FALSE) {
  fills <- rep(fills, length.out = length(values))
  Filter(Negate(is.null), list(type = "box", stroke = stroke, horizontal = if (horizontal) TRUE,
    boxes = lapply(seq_along(values), function(i) {
      s <- sf_box_stats(values[[i]])
      list(pos = at[i], width = width, q1 = s$q1, med = s$med, q3 = s$q3, lo = s$lo, hi = s$hi, fill = fills[[i]])
    })))
}
# Odcinki (np. słupki błędów): from/to to macierze albo listy par [x, y].
sf_segs <- function(x0, y0, x1, y1, color = "@ref", width = 1.6, colors = NULL, alpha = NULL) {
  Filter(Negate(is.null), list(type = "segs", color = color, width = width, alpha = alpha,
    items = lapply(seq_along(x0), function(i) {
      Filter(Negate(is.null), list(from = c(x0[i], y0[i]), to = c(x1[i], y1[i]), color = if (!is.null(colors)) colors[[i]]))
    })))
}
# Poziome słupki błędów z poprzeczkami (geom_errorbar(orientation = "y", width = cap)).
sf_errorbars_h <- function(lo, hi, y, cap = 0.2, ...) {
  sf_segs(c(lo, lo, hi), c(y, y - cap / 2, y - cap / 2), c(hi, lo, hi), c(y, y + cap / 2, y + cap / 2), ...)
}
# Pionowe słupki błędów.
sf_errorbars_v <- function(lo, hi, x, cap = 0.2, ...) {
  sf_segs(c(x, x - cap / 2, x - cap / 2), c(lo, lo, hi), c(x, x + cap / 2, x + cap / 2), c(hi, lo, hi), ...)
}
sf_text <- function(x, y, text, color = "@ink", align = NULL, valign = NULL, bold = NULL, mono = NULL, size = NULL, halo = NULL) {
  Filter(Negate(is.null), list(type = "text", x = x, y = y, text = text, color = color, align = align,
                               valign = valign, bold = bold, mono = mono, size = size, halo = halo))
}
# Kilka napisów naraz: lista warstw do wklejenia w layers przez c(…).
sf_texts <- function(x, y, text, ...) lapply(seq_along(text), function(i) sf_text(x[i], y[i], text[i], ...))
sf_polygon <- function(x, y, fill) list(type = "polygon", points = sf_xy(x, y), fill = fill)
# Pas ufności między dwiema krzywymi (geom_ribbon / geom_smooth(se = TRUE)).
sf_ribbon <- function(x, lo, hi, fill) sf_polygon(c(x, rev(x)), c(hi, rev(lo)), fill)
# Prosta regresji z pasem ufności jak geom_smooth(method = "lm"): lista dwóch warstw.
sf_smooth_lm <- function(x, y, color = "@accent", fill_alpha = 0.15, width = 2.4, n = 80, se = TRUE) {
  grid <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n)
  pred <- stats::predict(stats::lm(y ~ x), data.frame(x = grid), interval = "confidence")
  Filter(Negate(is.null), list(
    if (se) sf_ribbon(grid, pred[, "lwr"], pred[, "upr"], paste0(color, "/", fill_alpha)),
    sf_line(grid, pred[, "fit"], color = color, width = width)
  ))
}
# Wykres Q-Q jak stat_qq() + stat_qq_line(): lista dwóch warstw (prosta, punkty).
sf_qq <- function(v, color = "@niebo", line_color = "@ref", size = 6, alpha = 0.6, line_dash = NULL) {
  v <- sort(v[!is.na(v)])
  q <- stats::qnorm(stats::ppoints(length(v)))
  qy <- unname(stats::quantile(v, c(.25, .75))); qx <- stats::qnorm(c(.25, .75))
  slope <- diff(qy) / diff(qx); int <- qy[1] - slope * qx[1]
  list(sf_line(range(q), int + slope * range(q), color = line_color, width = 1.8, dash = line_dash),
       sf_points(q, v, color = color, size = size, alpha = alpha))
}
# Pozycja w legendzie nad wykresem: linia albo kwadrat (box).
sf_legend <- function(text, color, box = FALSE, dash = FALSE) {
  lapply(seq_along(text), function(i) Filter(Negate(is.null), list(text = text[i], color = color[[i]],
    box = if (box) TRUE, dash = if (dash) TRUE)))
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
  /* Rycina chowa elementy atrybutem hidden; style przycisków (display) by go przebiły. */
  .lc-sf [hidden] { display: none !important; }
  .lc-sf-zoom { position: absolute; right: .3em; top: .3em; z-index: 3; }
  .lc-sf-chart { position: relative; width: 100%; min-height: 14em; }
  .lc-sf-chart.is-strip { min-height: 7em; }
  .lc-sf[data-lc-chart-large='true'] .lc-sf-chart.is-strip { min-height: 9em; }
  .lc-sf-chart > .lc-ec { position: absolute; inset: 0; }
  .lc-sf[data-lc-chart-large='true'] .lc-sf-chart { max-height: none !important; min-height: 24em; }
  .lc-sf-legend { display: flex; flex-wrap: wrap; gap: .4em 1.4em; margin: .4em 0 0;
    font-size: .9em; color: var(--upwr-ink-soft); }
  .lc-sf-legend-item { display: inline-flex; align-items: center; }
  .lc-sf-slider { min-width: min(100%, 14em); flex: 1 1 14em; }
  .lc-sf-slider input[type='range'] { width: 100%; accent-color: var(--upwr-accent); cursor: pointer; }
  .lc-sf-slider output { display: inline; }
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
