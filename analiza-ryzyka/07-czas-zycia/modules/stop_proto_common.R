# PROTOTYPY PUNKTÓW STOP (2026-11-14) — wspólne narzędzia wykładu 07 -------------
#
# Numery jak w PUNKTY-STOP-PROPOZYCJA.md. Prototypy działają w przeglądarce, bez
# serwera R (także offline):
#   - sceny z kadrami: R rysuje SVG każdego kadru z góry (c7p_frames_ui),
#     modules/sceny.js tylko przełącza kadry;
#   - sceny losowe i ruchome: rysunek i stan w modules/proto_*.js
#     (window.lcC7.register), R podaje dane i teksty w data-config (c7p_scene_ui);
#   - wykresy: ryciny lcSF (R/lc_stat_figures.js, ECharts) rejestrowane w
#     modules/proto_*.js, kontener lc_sf_figure() w R.
# Każdy plik stop_proto_r*.R definiuje obiekty UI c7p_<nr>_ui; wpięcie w block.R
# przez c7p_get("c7p_<nr>_ui") — brakujący obiekt daje panel „w budowie”.
#
# Wspólny język wizualny: oś czasu pracy w godzinach, kreska misji 1000 h,
# wentylator jako pozioma linia życia zakończona kreską awarii (akcent) albo
# trójkątem „działa” (szałwia). Park wentylatorów to linie życia ułożone rosnąco:
# obwiednia jest krzywą R(t). Linie parku nie są losowane — wentylator i ma czas
# życia równy kwantylowi (i − 0.5)/n modelu (c7p_park), więc liczby na obrazie
# zgadzają się z tekstem. Oferta A (wykładnicza) ma kolor „niebo”, oferta B
# (Weibull) „bursztyn”; wczesne defekty (β < 1) „terakota”.

# Karty katalogowe Bananpolu (R/bananpol.R, lifetime.*; decyzja 2026-10-02).
c7p_mttf <- 1500     # oferta A: wykładniczy, MTTF (h)
c7p_beta <- 2        # oferta B: Weibull, kształt
c7p_eta <- 1700      # oferta B: Weibull, skala (h)
c7p_mission <- 1000  # czas misji (h)

# Niezawodność i kwantyl Weibulla (β = 1: wykładniczy z MTTF = η).
c7p_R <- function(t, beta, eta) exp(-(t / eta)^beta)
c7p_q <- function(u, beta, eta) eta * (-log(1 - u))^(1 / beta)
# Park n wentylatorów: czasy życia = kwantyle (i − 0.5)/n, rosnąco.
c7p_park <- function(n, beta, eta) c7p_q((seq_len(n) - 0.5) / n, beta, eta)

# Panel prototypu: złota plakietka „Prototyp P4 · piaskownica”. Po akceptacji
# proto = FALSE daje zwykłą figurę z etykietą rodzaju.
c7p_panel <- function(num, kind, title, ..., width_mode = "wide", proto = TRUE) {
  body <- tags$div(`data-proto` = num, ...)
  if (!proto) return(figure_panel(label = kind, title = title, width_mode = width_mode, body))
  figure_panel(label = paste0("Prototyp ", num, " · ", kind), title = title,
    color = upwr_single_alt, width_mode = width_mode, body)
}

# Obiekt prototypu po nazwie; gdy pliku jeszcze nie ma, panel zastępczy.
c7p_get <- function(name) {
  obj <- get0(name, inherits = TRUE)
  if (!is.null(obj)) return(obj)
  figure_panel(label = "Prototyp · w budowie", title = name, color = upwr_single_alt,
    width_mode = "text", lc_caption("Ten punkt stop nie jest jeszcze zbudowany."))
}

# Podpis wariantu nad panelem (np. to samo miejsce w dwóch wersjach do porównania).
c7p_variant <- function(text) lc_caption(tags$strong("Wariant:"), " ", text, tone = "info")

c7p_css <- tags$style(HTML("
  .c7p-svg { display: block; width: 100%; height: auto; margin: .2em auto .4em; }
  .c7p-svg text { font-family: var(--upwr-sans); }
  .c7p-svg text:not([fill]):not([style*='fill']) { fill: var(--upwr-ink); }
  .c7p-mono { font-family: var(--upwr-mono) !important; }
  .c7p-subtle { fill: var(--upwr-ink-subtle) !important; }
  .c7p-cell-ok { fill: color-mix(in srgb, var(--upwr-cat-kurkuma) 60%, var(--upwr-surface)); stroke: var(--upwr-cat-kurkuma); stroke-width: 1; }
  .c7p-cell-bad { fill: var(--upwr-accent); stroke: var(--upwr-accent); stroke-width: 1; }
  .c7p-cell-idle { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1; }
  .c7p-cell-dim { opacity: .3; }
  .c7p-axis { stroke: var(--upwr-ink-subtle); stroke-width: 1.2; fill: none; }
  .c7p-grid { stroke: var(--upwr-rule); stroke-width: 1; fill: none; }
  .c7p-tick { font-family: var(--upwr-mono) !important; font-size: 12px; fill: var(--upwr-ink-subtle) !important; text-anchor: middle; }
  .c7p-limit { stroke: var(--upwr-ink); stroke-width: 1.6; stroke-dasharray: 6 4; fill: none; }
  .c7p-mean { stroke: var(--upwr-ink-subtle); stroke-width: 1.4; stroke-dasharray: 3 3; fill: none; }
  .c7p-curve { stroke: var(--upwr-secondary); stroke-width: 2.2; fill: none; }
  .c7p-curve-ghost { stroke: var(--upwr-ink-subtle); stroke-width: 1.4; stroke-dasharray: 4 4; fill: none; }
  .c7p-area { fill: color-mix(in srgb, var(--upwr-accent) 45%, transparent); stroke: none; }
  .c7p-area-ok { fill: color-mix(in srgb, var(--upwr-sage) 40%, transparent); stroke: none; }
  .c7p-dot { fill: var(--upwr-secondary); opacity: .55; }
  .c7p-dot-bad { fill: var(--upwr-accent); opacity: .9; }
  /* !important: reguła .c7p-svg text:not([fill]) ma wyższą specyficzność niż sama klasa. */
  .c7p-ok { fill: var(--upwr-sage) !important; }
  .c7p-bad { fill: var(--upwr-accent) !important; }
  .c7p-ink { fill: var(--upwr-ink) !important; }
  .c7p-life { stroke: var(--upwr-secondary); stroke-width: 2; fill: none; stroke-linecap: butt; }
  .c7p-life-a { stroke: var(--upwr-cat-niebo); }
  .c7p-life-b { stroke: var(--upwr-cat-bursztyn); }
  .c7p-life-c { stroke: var(--upwr-cat-terakota); }
  .c7p-life-dim { opacity: .28; }
  .c7p-life-ghost { stroke: var(--upwr-ink-subtle); stroke-width: 1.4; stroke-dasharray: 3 4; fill: none; }
  .c7p-fail { stroke: var(--upwr-accent); stroke-width: 2.4; fill: none; }
  .c7p-alive { fill: var(--upwr-sage); stroke: none; }
  .c7p-fill-a { fill: var(--upwr-cat-niebo) !important; }
  .c7p-fill-b { fill: var(--upwr-cat-bursztyn) !important; }
  .c7p-fill-c { fill: var(--upwr-cat-terakota) !important; }
  .c7p-band { fill: color-mix(in srgb, var(--upwr-accent) 14%, transparent); stroke: none; }
  .c7p-pair { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 1em 1.6em; }
  .c7p-paper { fill: var(--upwr-surface); stroke: var(--upwr-rule); stroke-width: 1.2; }
  .c7p-head { display: flex; justify-content: space-between; align-items: baseline; gap: 1em; margin-bottom: .3em; }
  .c7p-name { font-weight: 700; font-size: 1.05em; }
  .c7p-step { font-family: var(--upwr-mono); font-size: .9em; color: var(--upwr-ink-subtle); }
  .c7p-caption { min-height: 3em; margin: .4em 0; }
  .c7p-read { font-family: var(--upwr-mono); font-weight: 600; margin: .2em 0 .6em; }
  .c7p-nav { display: flex; gap: .6em; flex-wrap: wrap; align-items: center; }
  .c7p-nav .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
"))

# Owija fragmenty SVG w <svg> o szerokości w (viewBox w × h).
c7p_svg <- function(w, h, aria, ...) {
  HTML(paste0(sprintf('<svg class="c7p-svg" style="max-width:%dpx" viewBox="0 0 %d %d" role="img" aria-label="%s">', w, w, h, aria),
    paste(unlist(list(...)), collapse = ""), "</svg>"))
}

# Liczba z kropką dziesiętną, bez końcowych zer (do tekstów i SVG).
c7p_num <- function(x, digits = 3) format(round(x, digits), nsmall = 0, scientific = FALSE, trim = TRUE)

# Pozioma oś z podziałką: zwraca fragment SVG. x_of: funkcja wartość → piksel.
c7p_axis <- function(x_of, y, ticks, label = NULL, labels = format(ticks)) {
  paste0(
    sprintf('<line class="c7p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x_of(min(ticks)), x_of(max(ticks)), y, y),
    paste(sprintf('<line class="c7p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="c7p-tick" x="%g" y="%g">%s</text>',
      x_of(ticks), x_of(ticks), y, y + 5, x_of(ticks), y + 18, labels), collapse = ""),
    if (!is.null(label)) sprintf('<text class="c7p-subtle" x="%g" y="%g" font-size="12" text-anchor="end">%s</text>',
      x_of(max(ticks)), y + 34, label) else ""
  )
}

# Ścieżka SVG krzywej y = f(x) między from i to. x_of, y_of: wartość → piksel.
# close = TRUE domyka pole do osi (y_of(0)).
c7p_path <- function(f, x_of, y_of, from, to, n = 120, close = FALSE) {
  x <- seq(from, to, length.out = n)
  pts <- paste(sprintf("%.1f,%.1f", x_of(x), y_of(f(x))), collapse = " L")
  if (!close) return(paste0("M", pts))
  sprintf("M%.1f,%.1f L%s L%.1f,%.1f Z", x_of(from), y_of(0), pts, x_of(to), y_of(0))
}

# Linie życia: poziome kreski od x_of(0) do x_of(min(t, cut)) w wierszach y;
# awaria (t <= cut) kończy się pionową kreską w kolorze akcentu, działający
# (t > cut) trójkątem. cls: klasa koloru linii (c7p-life-a / -b / -c), wektor
# albo jedna wartość. tmax: linie dłuższe niż oś są ucinane na krawędzi.
c7p_lifelines <- function(t, y, x_of, cut = Inf, cls = "", tmax = Inf, tick = 4, width = 2, marks = TRUE) {
  end <- pmin(t, cut, tmax)
  failed <- t <= cut & t <= tmax
  alive <- t > cut
  paste0(
    paste(sprintf('<line class="c7p-life %s" style="stroke-width:%g" x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f"/>',
      cls, width, x_of(0), x_of(end), y, y), collapse = ""),
    if (marks && any(failed)) sprintf('<path class="c7p-fail" d="%s"/>',
      paste(sprintf("M%.1f,%.1fv%.1f", x_of(end[failed]), y[failed] - tick, 2 * tick), collapse = "")) else "",
    if (marks && any(alive)) sprintf('<path class="c7p-alive" d="%s"/>',
      paste(sprintf("M%.1f,%.1fl-7,-4.5v9z", x_of(end[alive]) + 8, y[alive]), collapse = "")) else ""
  )
}

# Pionowa kreska (misja, koniec badania) z podpisem u góry.
c7p_vmark <- function(x, y0, y1, label = NULL, cls = "c7p-limit", anchor = "start") {
  paste0(sprintf('<line class="%s" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>', cls, x, x, y0, y1),
    if (!is.null(label)) sprintf('<text class="c7p-mono" x="%.1f" y="%g" font-size="13" text-anchor="%s">%s</text>',
      x + if (anchor == "start") 5 else -5, y0 + 12, anchor, label) else "")
}

# Wykresy ryciny lcSF jeden pod drugim (wspólna oś czasu), z nagłówkami; do
# lc_sf_figure(body = c7p_stack(...)). W opisie ryciny alignLeft: true.
c7p_stack <- function(headings, ratio = "3.6/1", max_height = "230px") {
  tagList(lapply(seq_along(headings), function(k)
    tags$div(tags$h4(headings[[k]]), lc_sf_chart(k - 1, ratio, max_height))))
}

# Odczyt ryciny lcSF wypełniany przez JS: S.setValue(root, key, tekst).
c7p_read <- function(key, label, color = NULL, text = "–") {
  lc_readout(label, lc_sf_value(key, text), color = color, swatch = !is.null(color))
}

# ============================================================================
# SCENY W PRZEGLĄDARCE (modules/sceny.js, window.lcC7)
# ============================================================================

# Przycisk sceny: wygląd lc_action(), bez wejścia Shiny — klik obsługuje scena
# po atrybucie data-c7p (scene.on[act]).
c7p_button <- function(act, label = NULL, icon = NULL, variant = "outline", aria_label = NULL) {
  b <- lc_action(paste0("c7p-", act), label, icon = icon, variant = variant, aria_label = aria_label)
  b$attribs$id <- NULL
  b$attribs$class <- sub("action-button ", "", b$attribs$class, fixed = TRUE)
  b$attribs[["data-c7p"]] <- act
  b
}

c7p_prev_next <- function() tagList(c7p_button("prev", "Wstecz", icon = "prev"),
  c7p_button("next", "Dalej", icon = "next", variant = "solid"))

# Przełącznik sceny: przyciski data-c7p = act, data-value = wartość; scena dostaje
# kliknięty przycisk w scene.on[act](btn) i sama ustawia aria-pressed
# (lcC7.util.press(btn)). choices: c("Etykieta" = "wartość").
c7p_seg <- function(act, label, choices, selected = choices[[1]]) {
  lc_group(label, tags$div(class = "lc-seg", role = "group", `aria-label` = label,
    lapply(seq_along(choices), function(i) tags$button(type = "button", `data-c7p` = act,
      `data-value` = unname(choices[[i]]),
      `aria-pressed` = if (identical(unname(choices[[i]]), unname(selected))) "true" else "false",
      names(choices)[[i]]))))
}

# Kontener sceny: ... to treść w kolejności (sloty c7p_out() i zwykłe tagi).
c7p_scene_ui <- function(num, kind, title, scene, config, ..., width_mode = "wide") {
  c7p_panel(num, kind, title, width_mode = width_mode, c7p_css,
    tags$div(class = "c7p-scene", `data-scene` = scene,
      `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA), ...))
}
c7p_out <- function(name, ...) tags$div(`data-c7p-out` = name, ...)

# Scena z kadrami policzonymi w R. frames: lista list(name, text, read); svg(i)
# zwraca rysunek kadru i (c7p_svg).
c7p_frames_ui <- function(num, kind, title, frames, svg, body = c7p_frame_body, width_mode = "wide") {
  cfg <- list(frames = lapply(seq_along(frames), function(i) list(
    name = frames[[i]]$name,
    svg = as.character(svg(i)),
    body = as.character(body(frames[[i]]))
  )))
  c7p_scene_ui(num, kind, title, "kadry", cfg,
    c7p_out("head"), c7p_out("stage"), c7p_out("body"),
    tags$div(class = "c7p-nav", c7p_prev_next()), width_mode = width_mode)
}

# Treść kadru: opis i (gdy jest) odczyt.
c7p_frame_body <- function(f) {
  tagList(tags$p(class = "c7p-caption", f$text),
    if (!is.null(f$read) && !(is.character(f$read) && !nzchar(f$read))) tags$p(class = "c7p-read", f$read))
}
