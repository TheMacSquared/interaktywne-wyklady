# PROTOTYPY PUNKTÓW STOP (2026-11-14) — wspólne narzędzia wykładu 09 -------------
#
# Numery jak w PUNKTY-STOP-PROPOZYCJA.md. Prototypy działają w przeglądarce, bez
# serwera R (także offline):
#   - sceny drzewa: R podaje opis drzewa i kadrów (f9p_tree_ui), rysunek SVG robi
#     modules/sceny.js (lcF9.util.tree) — jedno drzewo, jedna konwencja symboli;
#   - sceny z kadrami SVG policzonymi w R (f9p_frames_ui), JS tylko przełącza;
#   - sceny interaktywne: rysunek i stan w modules/proto_*.js (lcF9.register),
#     R podaje dane i teksty w data-config (f9p_scene_ui);
#   - wykresy: ryciny lcSF (R/lc_stat_figures.js, ECharts) rejestrowane w
#     modules/proto_*.js, kontener lc_sf_figure() w R.
# Każdy plik stop_proto_*.R definiuje obiekty UI f9p_<nr>_ui; wpięcie w block.R
# przez f9p_get("f9p_<nr>_ui") — brakujący obiekt daje panel „w budowie”.
#
# Wspólny język wizualny: drzewo błędów Bananpolu TOP = I ∩ (D ∪ S). Zdarzenie
# szczytowe i pośrednie to prostokąt, pod nim symbol bramki (AND z płaską
# podstawą, OR ze szpicem), zdarzenie bazowe to koło z literą. Zdarzenie, które
# zachodzi, ma kolor akcentu razem z gałęzią do rodzica; liść, który na pewno nie
# zachodzi — szałwię. Liczby w mono: prawdopodobieństwo pod liściem, wynik bramki
# obok symbolu.

# Parametry drzewa Bananpolu (R/bananpol.R) i wyniki z R/risk_math.R.
f9p_pi <- bananpol$fta$initiation           # 0.005
f9p_d <- bananpol$fta$detection_failure     # 0.05
f9p_s <- bananpol$fta$suppression_failure   # 0.08
f9p_or <- risk_gate_or(c(f9p_d, f9p_s))     # 0.126
f9p_top <- risk_fta_top(f9p_pi, f9p_d, f9p_s)  # 0.00063

# Panel prototypu: złota plakietka „Prototyp P4 · piaskownica”. Po akceptacji
# proto = FALSE daje zwykłą figurę z etykietą rodzaju.
f9p_panel <- function(num, kind, title, ..., width_mode = "wide", proto = TRUE) {
  body <- tags$div(`data-proto` = num, ...)
  if (!proto) return(figure_panel(label = kind, title = title, width_mode = width_mode, body))
  figure_panel(label = paste0("Prototyp ", num, " · ", kind), title = title,
    color = upwr_single_alt, width_mode = width_mode, body)
}

# Obiekt prototypu po nazwie; gdy pliku jeszcze nie ma, panel zastępczy.
f9p_get <- function(name) {
  obj <- get0(name, inherits = TRUE)
  if (!is.null(obj)) return(obj)
  figure_panel(label = "Prototyp · w budowie", title = name, color = upwr_single_alt,
    width_mode = "text", lc_caption("Ten punkt stop nie jest jeszcze zbudowany."))
}

# Podpis wariantu nad panelem (np. to samo miejsce w dwóch wersjach do porównania).
f9p_variant <- function(text) lc_caption(tags$strong("Wariant:"), " ", text, tone = "info")

f9p_css <- tags$style(HTML("
  .f9p-svg { display: block; width: 100%; height: auto; margin: .2em auto .4em; }
  .f9p-svg text { font-family: var(--upwr-sans); }
  .f9p-svg text:not([fill]):not([style*='fill']) { fill: var(--upwr-ink); }
  .f9p-mono { font-family: var(--upwr-mono) !important; }
  /* !important: reguła .f9p-svg text:not([fill]) ma wyższą specyficzność niż sama klasa. */
  .f9p-subtle { fill: var(--upwr-ink-subtle) !important; }
  .f9p-ok { fill: var(--upwr-sage) !important; }
  .f9p-bad { fill: var(--upwr-accent) !important; }
  .f9p-ink { fill: var(--upwr-ink) !important; }
  .f9p-paper { fill: var(--upwr-surface); stroke: var(--upwr-rule); stroke-width: 1.2; }
  .f9p-line { stroke: var(--upwr-ink-subtle); stroke-width: 1.6; fill: none; }
  .f9p-line-on { stroke: var(--upwr-accent); stroke-width: 3; fill: none; }
  .f9p-line-ok { stroke: var(--upwr-sage); stroke-width: 3; fill: none; }
  .f9p-line-dash { stroke: var(--upwr-ink-subtle); stroke-width: 1.4; stroke-dasharray: 5 4; fill: none; }
  /* Drzewo: węzły (lcF9.util.tree). */
  .f9p-edge { stroke: var(--upwr-ink-subtle); stroke-width: 1.6; fill: none; }
  .f9p-edge.is-on { stroke: var(--upwr-accent); stroke-width: 3.2; }
  .f9p-edge.is-ghost { stroke-dasharray: 5 4; }
  .f9p-node .f9p-shape { fill: var(--upwr-surface); stroke: var(--upwr-ink-soft); stroke-width: 1.6; }
  .f9p-node .f9p-gate { fill: var(--upwr-surface-sunken, var(--upwr-panel)); stroke: var(--upwr-ink-soft); stroke-width: 1.6; }
  .f9p-node.is-on .f9p-shape, .f9p-node.is-on .f9p-gate { fill: var(--upwr-accent-tint); stroke: var(--upwr-accent); stroke-width: 2.6; }
  .f9p-node.is-on .f9p-sym, .f9p-node.is-on .f9p-gate-t { fill: var(--upwr-accent) !important; }
  .f9p-node.is-no .f9p-shape { fill: var(--upwr-sage-tint); stroke: var(--upwr-sage); stroke-width: 2.4; }
  .f9p-node.is-no .f9p-sym { fill: var(--upwr-sage) !important; }
  .f9p-node.is-mark .f9p-shape, .f9p-node.is-mark .f9p-gate { stroke: var(--upwr-cat-kurkuma); stroke-width: 3.4; }
  .f9p-node.is-ghost .f9p-shape, .f9p-node.is-ghost .f9p-gate { stroke-dasharray: 5 4; stroke: var(--upwr-ink-subtle); }
  .f9p-node.is-ghost text { fill: var(--upwr-ink-subtle) !important; }
  .f9p-node.is-dim, .f9p-edge.is-dim { opacity: .28; }
  .f9p-node[data-f9p] { cursor: pointer; }
  .f9p-node[data-f9p]:hover .f9p-shape { stroke-width: 3; }
  .f9p-sym { font-weight: 700; text-anchor: middle; }
  .f9p-gate-t { font-size: 11px; font-weight: 700; text-anchor: middle; letter-spacing: .04em; }
  .f9p-lab { font-size: 13px; text-anchor: middle; }
  .f9p-val { font-family: var(--upwr-mono) !important; font-size: 14px; font-weight: 600; }
  .f9p-pv { font-family: var(--upwr-mono) !important; font-size: 13px; text-anchor: middle; fill: var(--upwr-ink-soft) !important; }
  /* Układ scen. */
  .f9p-head { display: flex; justify-content: space-between; align-items: baseline; gap: 1em; margin-bottom: .3em; }
  .f9p-name { font-weight: 700; font-size: 1.05em; }
  .f9p-step { font-family: var(--upwr-mono); font-size: .9em; color: var(--upwr-ink-subtle); }
  .f9p-caption { min-height: 3em; margin: .4em 0; }
  .f9p-read { font-family: var(--upwr-mono); font-weight: 600; margin: .2em 0 .6em; }
  .f9p-nav { display: flex; gap: .6em; flex-wrap: wrap; align-items: center; margin-top: .4em; }
  .f9p-nav .lc-seg button[aria-pressed='true'], .f9p-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
  .f9p-row { display: flex; flex-wrap: wrap; gap: .6em 1.6em; align-items: flex-start; justify-content: center; }
  .f9p-cell { flex: 1 1 15em; min-width: 0; }
  .f9p-cell > h4 { margin: .2em 0 .1em; font-size: .98em; text-align: center; }
  .f9p-cell > .f9p-note { text-align: center; font-family: var(--upwr-mono); font-size: .92em; margin: 0 0 .4em; }
  .f9p-row.is-mini .f9p-cell { flex: 0 1 10.5em; }
  .f9p-row.is-mini .f9p-cell > h4 { font-family: var(--upwr-mono); font-size: .9em; }
  .f9p-cell.is-hit > h4 { color: var(--upwr-accent); }
"))

# Owija fragmenty SVG w <svg> o szerokości w (viewBox w × h).
f9p_svg <- function(w, h, aria, ...) {
  HTML(paste0(sprintf('<svg class="f9p-svg" style="max-width:%dpx" viewBox="0 0 %d %d" role="img" aria-label="%s">', w, w, h, aria),
    paste(unlist(list(...)), collapse = ""), "</svg>"))
}

# Liczba z kropką dziesiętną, bez końcowych zer (do tekstów i SVG).
f9p_num <- function(x, digits = 6) format(round(x, digits), nsmall = 0, scientific = FALSE, trim = TRUE)

# Odczyt ryciny lcSF wypełniany przez JS: S.setValue(root, key, tekst).
f9p_read <- function(key, label, color = NULL, text = "–") {
  lc_readout(label, lc_sf_value(key, text), color = color, swatch = !is.null(color))
}

# ============================================================================
# SCENY W PRZEGLĄDARCE (modules/sceny.js, window.lcF9)
# ============================================================================

# Przycisk sceny: wygląd lc_action(), bez wejścia Shiny — klik obsługuje scena
# po atrybucie data-f9p (scene.on[act]).
f9p_button <- function(act, label = NULL, icon = NULL, variant = "outline", aria_label = NULL) {
  b <- lc_action(paste0("f9p-", act), label, icon = icon, variant = variant, aria_label = aria_label)
  b$attribs$id <- NULL
  b$attribs$class <- sub("action-button ", "", b$attribs$class, fixed = TRUE)
  b$attribs[["data-f9p"]] <- act
  b
}

f9p_prev_next <- function() tagList(f9p_button("prev", "Wstecz", icon = "prev"),
  f9p_button("next", "Dalej", icon = "next", variant = "solid"))

# Przełącznik sceny: przyciski data-f9p = act, data-value = wartość; scena dostaje
# kliknięty przycisk w scene.on[act](btn) i sama ustawia aria-pressed
# (lcF9.util.press(btn)). choices: c("Etykieta" = "wartość").
f9p_seg <- function(act, label, choices, selected = choices[[1]]) {
  lc_group(label, tags$div(class = "lc-seg f9p-seg", role = "group", `aria-label` = label,
    lapply(seq_along(choices), function(i) tags$button(type = "button", `data-f9p` = act,
      `data-value` = unname(choices[[i]]),
      `aria-pressed` = if (identical(unname(choices[[i]]), unname(selected))) "true" else "false",
      names(choices)[[i]]))))
}

# Suwak sceny (natywny, bez Shiny); odczyt wartości odświeża sceny.js.
f9p_slider <- function(id, label, min, max, value, step) {
  tags$div(class = "lc-grp lc-sf-slider",
    tags$label(`for` = id, class = "lc-grp-l", label, " ", tags$output(`data-value` = id, value)),
    tags$input(id = id, type = "range", min = min, max = max, value = value, step = step))
}

# Kontener sceny: ... to treść w kolejności (sloty f9p_out() i zwykłe tagi).
f9p_scene_ui <- function(num, kind, title, scene, config, ..., width_mode = "wide") {
  f9p_panel(num, kind, title, width_mode = width_mode, f9p_css,
    tags$div(class = "f9p-scene", `data-scene` = scene,
      `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA, null = "null"), ...))
}
f9p_out <- function(name, ...) tags$div(`data-f9p-out` = name, ...)

# Scena z kadrami policzonymi w R. frames: lista list(name, text, read); svg(i)
# zwraca rysunek kadru i (f9p_svg albo dowolny HTML).
f9p_frames_ui <- function(num, kind, title, frames, svg, body = f9p_frame_body, width_mode = "wide") {
  cfg <- list(frames = lapply(seq_along(frames), function(i) list(
    name = frames[[i]]$name,
    svg = as.character(svg(i)),
    body = as.character(body(frames[[i]]))
  )))
  f9p_scene_ui(num, kind, title, "kadry", cfg,
    f9p_out("head"), f9p_out("stage"), f9p_out("body"),
    tags$div(class = "f9p-nav", f9p_prev_next()), width_mode = width_mode)
}

# Treść kadru: opis i (gdy jest) odczyt.
f9p_frame_body <- function(f) {
  tagList(tags$p(class = "f9p-caption", f$text),
    if (!is.null(f$read) && !(is.character(f$read) && !nzchar(f$read))) tags$p(class = "f9p-read", f$read))
}

# ============================================================================
# DRZEWO BŁĘDÓW (rysuje lcF9.util.tree w sceny.js)
# ============================================================================

# Węzeł drzewa. Liść: f9p_leaf("D", "Brak detekcji"); ev — zdarzenie, które liść
# oznacza (dwa liście o tym samym ev to jedno zdarzenie narysowane dwa razy).
# Zdarzenie z bramką: f9p_gate("M", "Zabezpieczenia zawiodły", "or", dzieci…).
# label: wektor wierszy napisu; short: napis w wersji mini.
f9p_leaf <- function(id, label, sym = id, ev = id) {
  list(id = id, label = as.list(label), sym = sym, ev = ev)
}
f9p_gate <- function(id, label, gate, ..., short = id) {
  list(id = id, label = as.list(label), gate = gate, short = short, children = list(...))
}

# Drzewo Bananpolu: TOP = I ∩ (D ∪ S), inicjacja po lewej, bramka OR po prawej.
f9p_tree_bananpol <- f9p_gate("TOP", c("Nieopanowany pożar", "magazynu w roku"), "and",
  f9p_leaf("I", "Inicjacja"),
  f9p_gate("M", c("Zabezpieczenia", "zawiodły"), "or",
    f9p_leaf("D", c("Brak", "detekcji")),
    f9p_leaf("S", c("Brak", "tłumienia")), short = "D ∪ S"))

# Opis jednego rysunku drzewa (panelu). tree: nazwa drzewa z cfg$trees.
# lit: zdarzenia bazowe, które zachodzą (stany bramek liczy JS); no: liście, które
# na pewno nie zachodzą; mark / dim / ghost / hide: id węzłów; values: wynik przy
# bramce (id = "tekst"); p: prawdopodobieństwo pod liściem (id = "tekst");
# svg: gotowy rysunek zamiast drzewa; note: wiersz pod rysunkiem; hit: wyróżnij tytuł.
f9p_view <- function(tree = "main", title = NULL, lit = NULL, no = NULL, mark = NULL, dim = NULL,
                     ghost = NULL, hide = NULL, values = NULL, p = NULL, svg = NULL, note = NULL,
                     mini = FALSE, hit = FALSE, max_width = NULL, grow = NULL) {
  v <- list(tree = tree, title = title, lit = as.list(lit), no = as.list(no), mark = as.list(mark),
    dim = as.list(dim), ghost = as.list(ghost), hide = as.list(hide),
    values = as.list(values), p = as.list(p), note = note, mini = mini, hit = hit, maxWidth = max_width, grow = grow)
  if (!is.null(svg)) v$svg <- as.character(svg)
  v
}

# Prawdopodobieństwa liści drzewa Bananpolu do f9p_view(p = …).
f9p_p_bananpol <- c(I = "0.005", D = "0.05", S = "0.08")

# Kadr sceny drzewa: nazwa, tekst, odczyt i jeden albo kilka rysunków (f9p_view).
f9p_frame <- function(name, text, ..., read = NULL) {
  list(name = name, body = as.character(f9p_frame_body(list(text = text, read = read))), panels = list(...))
}

# Scena drzewa. frames: lista f9p_frame(); trees: nazwane drzewa (domyślnie
# main = drzewo Bananpolu). nav: "kadry" (Wstecz / Dalej), "seg" (przełącznik
# z nazwami kadrów) albo "none" (jeden nieruchomy obraz).
f9p_tree_ui <- function(num, kind, title, frames, trees = list(main = f9p_tree_bananpol),
                        nav = c("kadry", "seg", "none"), seg_label = "Widok", width_mode = "wide", after = NULL) {
  nav <- match.arg(nav)
  cfg <- list(trees = trees, frames = frames, nav = nav)
  f9p_scene_ui(num, kind, title, "drzewo", cfg,
    if (nav == "seg") tags$div(class = "f9p-nav", f9p_seg("view", seg_label,
      stats::setNames(as.character(seq_along(frames) - 1L), vapply(frames, function(f) f$name, "")))),
    if (nav == "kadry") f9p_out("head"),
    f9p_out("stage"), f9p_out("body"),
    if (nav == "kadry") tags$div(class = "f9p-nav", f9p_prev_next()),
    after, width_mode = width_mode)
}
