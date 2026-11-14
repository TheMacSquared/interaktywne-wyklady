# PROTOTYPY PUNKTÓW STOP (2026-11-14) — wspólne narzędzia wykładu 06 -------------
#
# Numery jak w PUNKTY-STOP-PROPOZYCJA.md. Prototypy działają w przeglądarce, bez
# serwera R (także offline):
#   - sceny z kadrami: R rysuje SVG każdego kadru z góry (z6p_frames_ui),
#     modules/sceny.js tylko przełącza kadry;
#   - sceny losowe: rysunek i stan w modules/proto_*.js (window.lcZ6.register),
#     R podaje dane i teksty w data-config (z6p_scene_ui);
#   - wykresy: ryciny lcSF (R/lc_stat_figures.js, ECharts) rejestrowane w
#     modules/proto_*.js, kontener lc_sf_figure() w R.
# Każdy plik stop_proto_r*.R definiuje obiekty UI z6p_<nr>_ui; wpięcie w block.R
# przez z6p_get("z6p_<nr>_ui") — brakujący obiekt daje panel „w budowie”.
#
# Wspólny język wizualny: stała oś 70–95°C, blady kontur stanu bazowego N(82, 3),
# próg 85°C, linijka σ pod osią, dziennik pomiarów w kratkach (kratka ponad progiem
# w kolorze akcentu).

# Stan bazowy łożyska (dane Bananpolu z karty rozdziału 1).
z6p_mu <- 82
z6p_sd <- 3
z6p_c <- 85

# Panel prototypu: złota plakietka „Prototyp P4 · piaskownica”. Po akceptacji
# proto = FALSE daje zwykłą figurę z etykietą rodzaju.
z6p_panel <- function(num, kind, title, ..., width_mode = "wide", proto = TRUE) {
  body <- tags$div(`data-proto` = num, ...)
  if (!proto) return(figure_panel(label = kind, title = title, width_mode = width_mode, body))
  figure_panel(label = paste0("Prototyp ", num, " · ", kind), title = title,
    color = upwr_single_alt, width_mode = width_mode, body)
}

# Obiekt prototypu po nazwie; gdy pliku jeszcze nie ma, panel zastępczy.
z6p_get <- function(name) {
  obj <- get0(name, inherits = TRUE)
  if (!is.null(obj)) return(obj)
  figure_panel(label = "Prototyp · w budowie", title = name, color = upwr_single_alt,
    width_mode = "text", lc_caption("Ten punkt stop nie jest jeszcze zbudowany."))
}

# Podpis wariantu nad panelem (np. to samo miejsce w dwóch wersjach do porównania).
z6p_variant <- function(text) lc_caption(tags$strong("Wariant:"), " ", text, tone = "info")

z6p_css <- tags$style(HTML("
  .z6p-svg { display: block; width: 100%; height: auto; margin: .2em auto .4em; }
  .z6p-svg text { font-family: var(--upwr-sans); }
  .z6p-svg text:not([fill]):not([style*='fill']) { fill: var(--upwr-ink); }
  .z6p-mono { font-family: var(--upwr-mono) !important; }
  .z6p-subtle { fill: var(--upwr-ink-subtle) !important; }
  .z6p-cell-ok { fill: color-mix(in srgb, var(--upwr-cat-kurkuma) 60%, var(--upwr-surface)); stroke: var(--upwr-cat-kurkuma); stroke-width: 1; }
  .z6p-cell-bad { fill: var(--upwr-accent); stroke: var(--upwr-accent); stroke-width: 1; }
  .z6p-cell-idle { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1; }
  .z6p-cell-dim { opacity: .3; }
  .z6p-axis { stroke: var(--upwr-ink-subtle); stroke-width: 1.2; fill: none; }
  .z6p-grid { stroke: var(--upwr-rule); stroke-width: 1; fill: none; }
  .z6p-tick { font-family: var(--upwr-mono) !important; font-size: 12px; fill: var(--upwr-ink-subtle) !important; text-anchor: middle; }
  .z6p-limit { stroke: var(--upwr-ink); stroke-width: 1.6; stroke-dasharray: 6 4; fill: none; }
  .z6p-mean { stroke: var(--upwr-ink-subtle); stroke-width: 1.4; stroke-dasharray: 3 3; fill: none; }
  .z6p-curve { stroke: var(--upwr-secondary); stroke-width: 2.2; fill: none; }
  .z6p-curve-ghost { stroke: var(--upwr-ink-subtle); stroke-width: 1.4; stroke-dasharray: 4 4; fill: none; }
  .z6p-area { fill: color-mix(in srgb, var(--upwr-accent) 45%, transparent); stroke: none; }
  .z6p-area-ok { fill: color-mix(in srgb, var(--upwr-sage) 40%, transparent); stroke: none; }
  .z6p-dot { fill: var(--upwr-secondary); opacity: .55; }
  .z6p-dot-bad { fill: var(--upwr-accent); opacity: .9; }
  /* !important: reguła .z6p-svg text:not([fill]) ma wyższą specyficzność niż sama klasa. */
  .z6p-ok { fill: var(--upwr-sage) !important; }
  .z6p-bad { fill: var(--upwr-accent) !important; }
  .z6p-ink { fill: var(--upwr-ink) !important; }
  .z6p-paper { fill: var(--upwr-surface); stroke: var(--upwr-rule); stroke-width: 1.2; }
  .z6p-head { display: flex; justify-content: space-between; align-items: baseline; gap: 1em; margin-bottom: .3em; }
  .z6p-name { font-weight: 700; font-size: 1.05em; }
  .z6p-step { font-family: var(--upwr-mono); font-size: .9em; color: var(--upwr-ink-subtle); }
  .z6p-caption { min-height: 3em; margin: .4em 0; }
  .z6p-read { font-family: var(--upwr-mono); font-weight: 600; margin: .2em 0 .6em; }
  .z6p-nav { display: flex; gap: .6em; flex-wrap: wrap; align-items: center; }
  .z6p-nav .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
"))

# Owija fragmenty SVG w <svg> o szerokości w (viewBox w × h).
z6p_svg <- function(w, h, aria, ...) {
  HTML(paste0(sprintf('<svg class="z6p-svg" style="max-width:%dpx" viewBox="0 0 %d %d" role="img" aria-label="%s">', w, w, h, aria),
    paste(unlist(list(...)), collapse = ""), "</svg>"))
}

# Liczba z kropką dziesiętną, bez końcowych zer (do tekstów i SVG).
z6p_num <- function(x, digits = 3) format(round(x, digits), nsmall = 0, scientific = FALSE, trim = TRUE)

# Pozioma oś z podziałką: zwraca fragment SVG. x_of: funkcja wartość → piksel.
z6p_axis <- function(x_of, y, ticks, label = NULL, labels = format(ticks)) {
  paste0(
    sprintf('<line class="z6p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x_of(min(ticks)), x_of(max(ticks)), y, y),
    paste(sprintf('<line class="z6p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="z6p-tick" x="%g" y="%g">%s</text>',
      x_of(ticks), x_of(ticks), y, y + 5, x_of(ticks), y + 18, labels), collapse = ""),
    if (!is.null(label)) sprintf('<text class="z6p-subtle" x="%g" y="%g" font-size="12" text-anchor="end">%s</text>',
      x_of(max(ticks)), y + 34, label) else ""
  )
}

# Ścieżka SVG krzywej normalnej N(mu, sd) między from i to. x_of, y_of: wartość →
# piksel (y_of dostaje gęstość). close = TRUE domyka pole do osi (y_of(0)).
z6p_norm_path <- function(mu, sd, x_of, y_of, from, to, n = 120, close = FALSE) {
  x <- seq(from, to, length.out = n)
  pts <- paste(sprintf("%.1f,%.1f", x_of(x), y_of(dnorm(x, mu, sd))), collapse = " L")
  if (!close) return(paste0("M", pts))
  sprintf("M%.1f,%.1f L%s L%.1f,%.1f Z", x_of(from), y_of(0), pts, x_of(to), y_of(0))
}

# Linijka σ pod osią: kreski co 1σ od mu - k·sd do mu + k·sd z podpisami −2σ … +2σ.
z6p_sigma_ruler <- function(x_of, y, mu = z6p_mu, sd = z6p_sd, k = 3) {
  z <- seq(-k, k)
  lab <- ifelse(z == 0, "μ", paste0(ifelse(z > 0, "+", "−"), abs(z), "σ"))
  paste0(
    sprintf('<line class="z6p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x_of(mu - k * sd), x_of(mu + k * sd), y, y),
    paste(sprintf('<line class="z6p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="z6p-tick" x="%g" y="%g">%s</text>',
      x_of(mu + z * sd), x_of(mu + z * sd), y - 4, y + 4, x_of(mu + z * sd), y + 18, lab), collapse = "")
  )
}

# Odczyt ryciny lcSF wypełniany przez JS: S.setValue(root, key, tekst).
z6p_read <- function(key, label, color = NULL, text = "–") {
  lc_readout(label, lc_sf_value(key, text), color = color, swatch = !is.null(color))
}

# ============================================================================
# SCENY W PRZEGLĄDARCE (modules/sceny.js, window.lcZ6)
# ============================================================================

# Przycisk sceny: wygląd lc_action(), bez wejścia Shiny — klik obsługuje scena
# po atrybucie data-z6p (scene.on[act]).
z6p_button <- function(act, label = NULL, icon = NULL, variant = "outline", aria_label = NULL) {
  b <- lc_action(paste0("z6p-", act), label, icon = icon, variant = variant, aria_label = aria_label)
  b$attribs$id <- NULL
  b$attribs$class <- sub("action-button ", "", b$attribs$class, fixed = TRUE)
  b$attribs[["data-z6p"]] <- act
  b
}

z6p_prev_next <- function() tagList(z6p_button("prev", "Wstecz", icon = "prev"),
  z6p_button("next", "Dalej", icon = "next", variant = "solid"))

# Przełącznik sceny: przyciski data-z6p = act, data-value = wartość; scena dostaje
# kliknięty przycisk w scene.on[act](btn) i sama ustawia aria-pressed
# (lcZ6.util.press(btn)). choices: c("Etykieta" = "wartość").
z6p_seg <- function(act, label, choices, selected = choices[[1]]) {
  lc_group(label, tags$div(class = "lc-seg", role = "group", `aria-label` = label,
    lapply(seq_along(choices), function(i) tags$button(type = "button", `data-z6p` = act,
      `data-value` = unname(choices[[i]]),
      `aria-pressed` = if (identical(unname(choices[[i]]), unname(selected))) "true" else "false",
      names(choices)[[i]]))))
}

# Kontener sceny: ... to treść w kolejności (sloty z6p_out() i zwykłe tagi).
z6p_scene_ui <- function(num, kind, title, scene, config, ..., width_mode = "wide") {
  z6p_panel(num, kind, title, width_mode = width_mode, z6p_css,
    tags$div(class = "z6p-scene", `data-scene` = scene,
      `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA), ...))
}
z6p_out <- function(name, ...) tags$div(`data-z6p-out` = name, ...)

# Scena z kadrami policzonymi w R. frames: lista list(name, text, read); svg(i)
# zwraca rysunek kadru i (z6p_svg).
z6p_frames_ui <- function(num, kind, title, frames, svg, body = z6p_frame_body, width_mode = "wide") {
  cfg <- list(frames = lapply(seq_along(frames), function(i) list(
    name = frames[[i]]$name,
    svg = as.character(svg(i)),
    body = as.character(body(frames[[i]]))
  )))
  z6p_scene_ui(num, kind, title, "kadry", cfg,
    z6p_out("head"), z6p_out("stage"), z6p_out("body"),
    tags$div(class = "z6p-nav", z6p_prev_next()), width_mode = width_mode)
}

# Treść kadru: opis i (gdy jest) odczyt.
z6p_frame_body <- function(f) {
  tagList(tags$p(class = "z6p-caption", f$text),
    if (!is.null(f$read) && !(is.character(f$read) && !nzchar(f$read))) tags$p(class = "z6p-read", f$read))
}
