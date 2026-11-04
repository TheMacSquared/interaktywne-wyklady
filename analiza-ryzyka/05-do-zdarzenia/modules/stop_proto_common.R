# PUNKTY STOP (2026-11-03) — wspólne narzędzia punktów stop wykładu 05 ----------
#
# Punkty stop rysowane po stronie serwera (SVG z R + renderUI), jak w wykładzie 04.
# Numery jak w PUNKTY-STOP-PROPOZYCJA.md. Każdy plik stop_proto_*.R definiuje
# obiekty UI d5p_*_ui i funkcję serwera; wpięcie w block.R i app.R.
#
# Wspólny język wizualny: TAŚMA KONTROLI. Jedna kratka = kontrola jednej palety;
# kratka z wadą w kolorze akcentu, bez wady kurkumowa (jasna, „bananowa”), niewykonana pusta.
# Funkcja d5p_tape() rysuje jeden wiersz taśmy jako fragment SVG (string).

# Panel punktu stop. Wszystkie panele wykładu 05 są zaakceptowane i renderują
# się jako zwykła figura (figure_panel). Gałąź proto = TRUE (plakietka „Prototyp ·”)
# jest zachowana tylko na wypadek ponownego użycia; domyślnie proto = FALSE.
d5p_panel <- function(kind, title, ..., width_mode = "wide", proto = FALSE) {
  if (!proto) return(figure_panel(label = kind, title = title, width_mode = width_mode, ...))
  figure_panel(label = paste("Prototyp ·", kind), title = title,
    color = upwr_single_alt, width_mode = width_mode, ...)
}

d5p_css <- tags$style(HTML("
  .d5p-svg { display: block; width: 100%; height: auto; margin: .2em auto .4em; }
  .d5p-svg text { font-family: var(--upwr-sans); }
  .d5p-svg text:not([fill]):not([style*='fill']) { fill: var(--upwr-ink); }
  .d5p-mono { font-family: var(--upwr-mono) !important; }
  .d5p-subtle { fill: var(--upwr-ink-subtle) !important; }
  .d5p-cell-ok { fill: color-mix(in srgb, var(--upwr-cat-kurkuma) 60%, var(--upwr-surface)); stroke: var(--upwr-cat-kurkuma); stroke-width: 1; }
  .d5p-cell-bad { fill: var(--upwr-accent); stroke: var(--upwr-accent); stroke-width: 1; }
  .d5p-cell-idle { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1; }
  .d5p-cell-dim { opacity: .3; }
  .d5p-axis { stroke: var(--upwr-ink-subtle); stroke-width: 1.2; }
  .d5p-tick { font-family: var(--upwr-mono) !important; font-size: 12px; fill: var(--upwr-ink-subtle) !important; text-anchor: middle; }
  .d5p-limit { stroke: var(--upwr-ink); stroke-width: 1.6; stroke-dasharray: 6 4; }
  .d5p-ok { fill: var(--upwr-sage); }
  .d5p-bad { fill: var(--upwr-accent); }
  .d5p-head { display: flex; justify-content: space-between; align-items: baseline; gap: 1em; margin-bottom: .3em; }
  .d5p-name { font-weight: 700; font-size: 1.05em; }
  .d5p-step { font-family: var(--upwr-mono); font-size: .9em; color: var(--upwr-ink-subtle); }
  .d5p-caption { min-height: 3em; margin: .4em 0; }
  .d5p-read { font-family: var(--upwr-mono); font-weight: 600; margin: .2em 0 .6em; }
  .d5p-nav { display: flex; gap: .6em; flex-wrap: wrap; }
"))

# Owija fragmenty SVG w <svg> o szerokości w (viewBox w × h).
d5p_svg <- function(w, h, aria, ...) {
  HTML(paste0(sprintf('<svg class="d5p-svg" style="max-width:%dpx" viewBox="0 0 %d %d" role="img" aria-label="%s">', w, w, h, aria),
    paste(unlist(list(...)), collapse = ""), "</svg>"))
}

# Nagłówek sceny: nazwa kadru po lewej, licznik/stan po prawej.
d5p_head <- function(name, step) {
  tags$div(class = "d5p-head", tags$span(class = "d5p-name", name), tags$span(class = "d5p-step", step))
}

# Jeden wiersz taśmy kontroli.
#   outcomes — wektor logiczny: TRUE = wada w tej kontroli, FALSE = brak wady
#   x0, y    — lewy górny róg pierwszej kratki
#   pitch    — odstęp kratek, size — bok kratki
#   n_cells  — ile kratek narysować (dopełnienie pustymi „idle”); domyślnie length(outcomes)
#   dim      — wektor logiczny (długości outcomes) kratek przygaszonych
#   max_cells — ucięcie: dłuższa taśma kończy się strzałką „→”
d5p_tape <- function(outcomes, x0, y, pitch = 12, size = 10, n_cells = length(outcomes),
                     dim = NULL, max_cells = Inf) {
  n_draw <- min(n_cells, max_cells)
  cells <- vapply(seq_len(n_draw), function(i) {
    cls <- if (i > length(outcomes)) "d5p-cell-idle" else if (outcomes[i]) "d5p-cell-bad" else "d5p-cell-ok"
    if (!is.null(dim) && i <= length(dim) && dim[i]) cls <- paste(cls, "d5p-cell-dim")
    sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g" rx="2"/>', cls, x0 + (i - 1) * pitch, y, size, size)
  }, character(1))
  arrow <- if (n_cells > max_cells) sprintf('<text class="d5p-mono d5p-subtle" x="%g" y="%g" font-size="%g">→</text>',
    x0 + n_draw * pitch + 2, y + size - 1, size + 3) else ""
  paste0(paste(cells, collapse = ""), arrow)
}

# Losuje jedną serię kontroli aż do r-tej wady (wektor logiczny, ostatni TRUE).
d5p_series <- function(r, p) {
  x <- logical(0)
  while (sum(x) < r) x <- c(x, runif(1) < p)
  x
}

# Pozioma oś liczby kontroli z podziałką: zwraca fragment SVG.
d5p_axis <- function(x_of, y, ticks, label = NULL, x_label = NULL) {
  paste0(
    sprintf('<line class="d5p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x_of(min(ticks)), x_of(max(ticks)), y, y),
    paste(vapply(ticks, function(t) sprintf('<line class="d5p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="d5p-tick" x="%g" y="%g">%s</text>',
      x_of(t), x_of(t), y, y + 5, x_of(t), y + 18, format(t)), character(1)), collapse = ""),
    if (!is.null(label)) sprintf('<text class="d5p-subtle" x="%g" y="%g" font-size="12" text-anchor="end">%s</text>',
      if (is.null(x_label)) x_of(max(ticks)) else x_label, y + 34, label) else ""
  )
}
