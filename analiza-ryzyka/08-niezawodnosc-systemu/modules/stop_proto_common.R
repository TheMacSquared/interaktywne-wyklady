# PROTOTYPY PUNKTÓW STOP (2026-11-14) — wspólne narzędzia wykładu 08 -------------
#
# Numery jak w PUNKTY-STOP-PROPOZYCJA.md. Prototypy działają w przeglądarce, bez
# serwera R (także offline):
#   - sceny z kadrami: R rysuje SVG każdego kadru z góry (s8p_frames_ui),
#     modules/sceny.js tylko przełącza kadry;
#   - widoki z przełącznikami: R rysuje SVG każdego wariantu (s8p_views_ui),
#     JS pokazuje ten, który odpowiada wciśniętym przyciskom;
#   - klikalny schemat blokowy: R rysuje wszystkie 2ⁿ stanów (s8p_rbd_scene),
#     JS przełącza stan po kliknięciu bloku;
#   - sceny losowe: rysunek i stan w modules/proto_*.js (window.lcS8.register);
#   - wykresy: ryciny lcSF (R/lc_stat_figures.js, ECharts) rejestrowane w
#     modules/proto_*.js, kontener lc_sf_figure() w R.
# Każdy plik stop_proto_r*.R definiuje obiekty UI s8p_<nr>_ui; wpięcie w block.R
# przez s8p_get("s8p_<nr>_ui") — brakujący obiekt daje panel „w budowie”.
#
# Wspólny język wizualny: schemat blokowy niezawodności (s8p_rbd) z lampką na
# wyjściu. Linie świecą szałwią tam, dokąd da się dojść od wejścia przez
# działające bloki; blok zepsuty jest w kolorze akcentu, z krzyżykiem w rogu.
# Drugi motyw: kratki misji (s8p_cells), zawiedzione w kolorze akcentu.

# Dane Bananpolu (R/bananpol.R) i liczby przykładów 8.4–8.8.
s8p_sys <- bananpol$system
s8p_r_fan <- s8p_sys$fan_reliability        # wentylator (A) 0.92
s8p_r_det <- s8p_sys$detector_reliability   # czujnik 0.95, w przykładach także wentylator B
s8p_r_pow <- s8p_sys$power_reliability      # zasilanie 0.98, w przykładzie 8.5 sterownik C
s8p_q <- s8p_sys$common_power_failure       # utrata wspólnego zasilania 0.01
s8p_r_a <- 0.92; s8p_r_b <- 0.95; s8p_r_c <- 0.98
s8p_r_ab <- risk_parallel_reliability(c(s8p_r_a, s8p_r_b))   # 0.996
s8p_r_mixed <- s8p_r_c * s8p_r_ab                             # 0.97608

# Panel prototypu: złota plakietka „Prototyp P4 · piaskownica”. Po akceptacji
# proto = FALSE daje zwykłą figurę z etykietą rodzaju.
s8p_panel <- function(num, kind, title, ..., width_mode = "wide", proto = TRUE) {
  body <- tags$div(`data-proto` = num, ...)
  if (!proto) return(figure_panel(label = kind, title = title, width_mode = width_mode, body))
  figure_panel(label = paste0("Prototyp ", num, " · ", kind), title = title,
    color = upwr_single_alt, width_mode = width_mode, body)
}

# Obiekt prototypu po nazwie; gdy pliku jeszcze nie ma, panel zastępczy.
s8p_get <- function(name) {
  obj <- get0(name, inherits = TRUE)
  if (!is.null(obj)) return(obj)
  figure_panel(label = "Prototyp · w budowie", title = name, color = upwr_single_alt,
    width_mode = "text", lc_caption("Ten punkt stop nie jest jeszcze zbudowany."))
}

# Podpis wariantu nad panelem (np. to samo miejsce w dwóch wersjach do porównania).
s8p_variant <- function(text) lc_caption(tags$strong("Wariant:"), " ", text, tone = "info")

s8p_css <- tags$style(HTML("
  .s8p-svg { display: block; width: 100%; height: auto; margin: .2em auto .4em; }
  .s8p-svg text { font-family: var(--upwr-sans); }
  .s8p-svg text:not([fill]):not([style*='fill']) { fill: var(--upwr-ink); }
  .s8p-mono { font-family: var(--upwr-mono) !important; }
  /* !important: reguła .s8p-svg text:not([fill]) ma wyższą specyficzność niż sama klasa. */
  .s8p-subtle { fill: var(--upwr-ink-subtle) !important; }
  .s8p-ok { fill: var(--upwr-sage) !important; }
  .s8p-bad { fill: var(--upwr-accent) !important; }
  .s8p-gold { fill: var(--upwr-single-alt) !important; }
  .s8p-paper { fill: var(--upwr-surface); stroke: var(--upwr-rule); stroke-width: 1.2; }
  .s8p-sunken { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1; }
  .s8p-rule { stroke: var(--upwr-rule); stroke-width: 1; fill: none; }
  .s8p-axis { stroke: var(--upwr-ink-subtle); stroke-width: 1.2; fill: none; }
  .s8p-tick { font-family: var(--upwr-mono) !important; font-size: 12px; fill: var(--upwr-ink-subtle) !important; text-anchor: middle; }
  .s8p-dash { stroke: var(--upwr-ink); stroke-width: 1.5; stroke-dasharray: 6 4; fill: none; }
  /* Schemat blokowy */
  .s8p-wire { stroke: var(--upwr-rule); stroke-width: 3; fill: none; stroke-linecap: round; }
  .s8p-wire.is-live { stroke: var(--upwr-sage); stroke-width: 4; }
  .s8p-node { fill: var(--upwr-ink-subtle); }
  .s8p-node.is-live { fill: var(--upwr-sage); }
  .s8p-blk rect { fill: var(--upwr-surface); stroke: var(--upwr-ink-soft); stroke-width: 1.6; }
  .s8p-blk.is-live rect { stroke: var(--upwr-sage); stroke-width: 2.6; }
  .s8p-blk.is-down rect { fill: color-mix(in srgb, var(--upwr-accent) 16%, var(--upwr-surface)); stroke: var(--upwr-accent); stroke-width: 2.2; }
  .s8p-badge { fill: var(--upwr-accent); stroke: var(--upwr-surface); stroke-width: 1.5; }
  .s8p-badge-x { stroke: var(--upwr-surface); stroke-width: 2; stroke-linecap: round; fill: none; }
  .s8p-blk.is-down text { fill: var(--upwr-accent) !important; }
  .s8p-blk.is-off rect { fill: none; stroke: var(--upwr-rule); stroke-dasharray: 5 4; }
  .s8p-blk.is-off text { fill: var(--upwr-ink-subtle) !important; }
  .s8p-blk.is-mark rect { stroke: var(--upwr-single-alt); stroke-width: 3.2; stroke-dasharray: 7 4; }
  .s8p-blk-name { font-weight: 600; text-anchor: middle; }
  .s8p-blk-sub { font-family: var(--upwr-mono) !important; text-anchor: middle; fill: var(--upwr-ink-soft) !important; }
  .s8p-click { cursor: pointer; }
  .s8p-click:hover rect { stroke-width: 3; }
  .s8p-click:focus-visible { outline: 2px solid var(--upwr-accent); outline-offset: 2px; }
  .s8p-lamp { fill: var(--upwr-surface); stroke: var(--upwr-accent); stroke-width: 2.4; }
  .s8p-lamp.is-on { fill: var(--upwr-sage); stroke: var(--upwr-sage); }
  .s8p-halo { fill: var(--upwr-sage); opacity: .22; }
  .s8p-voter { fill: var(--upwr-surface-sunken); stroke: var(--upwr-ink-soft); stroke-width: 1.4; }
  .s8p-voter.is-live { stroke: var(--upwr-sage); stroke-width: 2.4; }
  /* Kafle stanów */
  .s8p-tile { fill: var(--upwr-surface); stroke: var(--upwr-rule); stroke-width: 1.2; }
  .s8p-tile.is-lit { fill: color-mix(in srgb, var(--upwr-sage) 13%, var(--upwr-surface)); stroke: var(--upwr-sage); stroke-width: 2; }
  .s8p-tile.is-out { fill: color-mix(in srgb, var(--upwr-accent) 7%, var(--upwr-surface)); }
  /* Kratki misji */
  .s8p-cell-ok { fill: color-mix(in srgb, var(--upwr-sage) 55%, var(--upwr-surface)); }
  .s8p-cell-one { fill: color-mix(in srgb, var(--upwr-cat-kurkuma) 70%, var(--upwr-surface)); }
  .s8p-cell-bad { fill: var(--upwr-accent); }
  .s8p-cell-alt { fill: var(--upwr-single-alt); }
  .s8p-cell-idle { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1; }
  .s8p-head { display: flex; justify-content: space-between; align-items: baseline; gap: 1em; margin-bottom: .3em; }
  .s8p-name { font-weight: 700; font-size: 1.05em; }
  .s8p-step { font-family: var(--upwr-mono); font-size: .9em; color: var(--upwr-ink-subtle); }
  .s8p-caption { min-height: 3em; margin: .4em 0; }
  .s8p-read { font-family: var(--upwr-mono); font-weight: 600; margin: .2em 0 .6em; }
  .s8p-nav { display: flex; gap: .6em 1.2em; flex-wrap: wrap; align-items: flex-end; margin-bottom: .5em; }
  .s8p-nav .lc-seg button[aria-pressed='true'] { background: var(--upwr-accent); color: var(--upwr-surface); }
  .s8p-row { display: grid; grid-template-columns: repeat(auto-fit, minmax(15em, 1fr)); gap: .6em 1.4em; align-items: start; }
  .s8p-row h4 { margin: .2em 0; font-size: 1em; }
  .s8p-stack > div { border-top: 1px solid var(--upwr-rule); padding: .5em 0 .2em; }
  .s8p-stack > div:first-child { border-top: 0; }
  .s8p-stack h4 { margin: .1em 0; font-size: 1em; }
  .s8p-stack .s8p-read { margin: 0 0 .3em; }
"))

# Owija fragmenty SVG w <svg> o szerokości w (viewBox w × h).
s8p_svg <- function(w, h, aria, ..., max_width = w) {
  HTML(paste0(sprintf('<svg class="s8p-svg" style="max-width:%dpx" viewBox="0 0 %d %d" role="img" aria-label="%s">',
      as.integer(max_width), as.integer(w), as.integer(h), aria),
    paste(unlist(list(...)), collapse = ""), "</svg>"))
}

# Liczba z kropką dziesiętną, bez końcowych zer (do tekstów i SVG).
s8p_num <- function(x, digits = 3) format(round(x, digits), nsmall = 0, scientific = FALSE, trim = TRUE)
# Stała liczba miejsc (np. 0.990).
s8p_fix <- function(x, digits = 3) sprintf(paste0("%.", digits, "f"), x)

# Napis SVG. cls: klasy (np. "s8p-mono s8p-subtle"); anchor: start / middle / end.
s8p_text <- function(x, y, text, size = 14, cls = NULL, anchor = "start", weight = NULL) {
  sprintf('<text x="%g" y="%g" font-size="%g"%s%s%s>%s</text>', x, y, size,
    if (!is.null(cls)) sprintf(' class="%s"', cls) else "",
    if (anchor != "start") sprintf(' text-anchor="%s"', anchor) else "",
    if (!is.null(weight)) sprintf(' font-weight="%s"', weight) else "", text)
}

# ============================================================================
# SCHEMAT BLOKOWY NIEZAWODNOŚCI
# ============================================================================

# Czy lampka świeci: każdy etap (grupa równoległa) musi mieć co najmniej k
# działających bloków (domyślnie 1). stages: lista wektorów numerów bloków.
s8p_rbd_works <- function(stages, up, k = NULL) {
  if (is.null(k)) k <- rep(1L, length(stages))
  all(vapply(seq_along(stages), function(s) sum(up[stages[[s]]]) >= k[[s]], logical(1)))
}

# Rysunek schematu: szereg etapów, każdy etap to jeden blok albo grupa równoległa.
#   stages — lista wektorów numerów bloków, np. list(3, c(1, 2)) = C, potem A ‖ B
#   labels — nazwy bloków; sub — drugi wiersz (np. "0.92"), NULL = brak
#   up     — stan bloków (TRUE działa); k — minimalna liczba działających w etapie
#   x, y   — położenie kropki wejścia (y = oś schematu); scale — skala całości
#   click  — bloki klikalne (data-s8p="blk", data-value = numer)
#   lamp   — podpis lampki (NULL = bez podpisu); mark — numer bloku w złotej ramce
#   off    — numery bloków „poza schematem” (obrys przerywany, bez przewodów)
#   bw, bh — rozmiar bloku
# Zwraca napis SVG z atrybutami "w" (szerokość), "up" i "down" (zasięg nad i pod osią).
s8p_rbd <- function(stages, labels, sub = NULL, up = NULL, k = NULL, x = 0, y = 0, scale = 1,
                    click = FALSE, lamp = "chłodzenie", mark = NULL, off = NULL,
                    bw = 116, bh = 48, vgap = 16, lead = 30, gap = 38, pad = 24) {
  n <- length(labels)
  if (is.null(up)) up <- rep(TRUE, n)
  if (is.null(k)) k <- rep(1L, length(stages))
  ok <- vapply(seq_along(stages), function(s) sum(up[stages[[s]]]) >= k[[s]], logical(1))
  reach <- c(TRUE, cumprod(ok) == 1)          # reach[s]: prąd dochodzi do etapu s
  dim <- character(0); live <- character(0); blocks <- character(0)
  wire <- function(x1, y1, x2, y2, on) {
    d <- sprintf('<line class="s8p-wire" x1="%g" y1="%g" x2="%g" y2="%g"/>', x1, y1, x2, y2)
    dim <<- c(dim, d)
    if (on) live <<- c(live, sub("s8p-wire", "s8p-wire is-live", d, fixed = TRUE))
  }
  # znaczek awarii w rogu bloku: krzyżyk w kółku
  badge <- function(bx, by) sprintf(
    '<circle class="s8p-badge" cx="%g" cy="%g" r="9"/><path class="s8p-badge-x" d="M%g,%g l8,8 m0,-8 l-8,8"/>', bx, by, bx - 4, by - 4)
  cx <- 0; prev <- 0
  for (s in seq_along(stages)) {
    m <- stages[[s]]; len <- length(m)
    start <- cx + if (s == 1) lead else gap
    wire(prev, 0, start, 0, reach[s])
    ys <- (seq_len(len) - 1 - (len - 1) / 2) * (bh + vgap)
    multi <- len > 1
    bx <- start + if (multi) pad else 0
    end <- bx + bw + if (multi) pad else 0
    if (multi) {
      wire(start, min(ys), start, max(ys), reach[s])
      dim <- c(dim, sprintf('<line class="s8p-wire" x1="%g" y1="%g" x2="%g" y2="%g"/>', end, min(ys), end, max(ys)))
    }
    for (j in seq_len(len)) {
      b <- m[j]; yj <- ys[j]; on_out <- reach[s] && up[b] && k[[s]] <= 1
      if (multi) {
        wire(start, yj, bx, yj, reach[s])
        wire(bx + bw, yj, end, yj, reach[s] && up[b])
        # pion szyny zbiorczej od gałęzi do osi świeci, gdy świeci gałąź
        if (yj != 0 && on_out) live <- c(live, sprintf('<line class="s8p-wire is-live" x1="%g" y1="%g" x2="%g" y2="0"/>', end, yj, end))
      }
      cls <- paste(c("s8p-blk", if (!up[b]) "is-down" else if (reach[s]) "is-live",
        if (b %in% mark) "is-mark", if (click) "s8p-click"), collapse = " ")
      has_sub <- !is.null(sub) && nzchar(sub[b])
      blocks <- c(blocks, paste0(
        sprintf('<g class="%s"%s>', cls, if (click) sprintf(
          ' data-s8p="blk" data-value="%d" role="button" tabindex="0" aria-label="%s: %s"', b, labels[b],
          if (up[b]) "działa" else "nie działa") else ""),
        sprintf('<rect x="%g" y="%g" width="%g" height="%g" rx="6"/>', bx, yj - bh / 2, bw, bh),
        if (!up[b]) badge(bx + bw - 3, yj - bh / 2 + 3),
        sprintf('<text class="s8p-blk-name" x="%g" y="%g" font-size="15">%s</text>', bx + bw / 2, yj + if (has_sub) -2 else 5, labels[b]),
        if (has_sub) sprintf('<text class="s8p-blk-sub" x="%g" y="%g" font-size="12.5">%s</text>', bx + bw / 2, yj + 15, sub[b]),
        "</g>"))
    }
    if (multi && k[[s]] > 1) {
      # głosowanie k z n: skrzynka na szynie zbiorczej
      blocks <- c(blocks, sprintf(
        '<rect class="s8p-voter%s" x="%g" y="-15" width="58" height="30" rx="15"/><text class="s8p-mono" x="%g" y="5" font-size="13" text-anchor="middle">%d z %d</text>',
        if (reach[s + 1]) " is-live" else "", end - 8, end + 21, k[[s]], len))
      end <- end + 50
    }
    cx <- end; prev <- end
  }
  lamp_x <- cx + lead + 12
  wire(prev, 0, lamp_x - 12, 0, reach[length(stages) + 1])
  on <- reach[length(stages) + 1]
  extra <- character(0)
  if (!is.null(off)) {
    # bloki poza schematem: pod osią, bez przewodów
    for (j in seq_along(off)) {
      b <- off[j]; bx <- lead + (j - 1) * (bw + 20); yj <- bh + vgap + 8
      extra <- c(extra, sprintf('<g class="s8p-blk is-off%s"><rect x="%g" y="%g" width="%g" height="%g" rx="6"/>%s<text class="s8p-blk-name" x="%g" y="%g" font-size="15">%s</text></g>',
        if (!up[b]) " is-down" else "", bx, yj - bh / 2, bw, bh,
        if (!up[b]) badge(bx + bw - 3, yj - bh / 2 + 3) else "",
        bx + bw / 2, yj + 5, labels[b]))
    }
  }
  nmax <- max(lengths(stages))
  half <- (nmax * bh + (nmax - 1) * vgap) / 2
  out <- paste0(
    sprintf('<g transform="translate(%g,%g) scale(%g)">', x, y, scale),
    paste(dim, collapse = ""), paste(live, collapse = ""),
    sprintf('<circle class="s8p-node is-live" cx="0" cy="0" r="5"/>'),
    paste(blocks, collapse = ""), paste(extra, collapse = ""),
    if (on) sprintf('<circle class="s8p-halo" cx="%g" cy="0" r="19"/>', lamp_x) else "",
    sprintf('<circle class="s8p-lamp%s" cx="%g" cy="0" r="11"/>', if (on) " is-on" else "", lamp_x),
    if (!on) sprintf('<path class="s8p-cross" stroke="var(--upwr-accent)" stroke-width="2" d="M%g,-5 l10,10 m0,-10 l-10,10"/>', lamp_x - 5) else "",
    if (!is.null(lamp)) sprintf('<text class="s8p-subtle" x="%g" y="30" font-size="12" text-anchor="middle">%s</text>', lamp_x, lamp) else "",
    "</g>")
  structure(out, w = (lamp_x + 24) * scale, up = (half + 4) * scale,
    down = (max(half + 4, if (!is.null(lamp)) 36 else 0, if (!is.null(off)) bh * 1.5 + vgap + 12 else 0)) * scale, on = on)
}

# Schemat wyśrodkowany w cx (oś na wysokości y); pozostałe argumenty jak s8p_rbd().
s8p_rbd_center <- function(cx, y, ..., scale = 1) {
  w <- attr(s8p_rbd(..., scale = scale), "w")
  s8p_rbd(..., x = cx - w / 2, y = y, scale = scale)
}

# Wentylator jako przedmiot (sceny w hali): obudowa, trzy łopaty, piasta.
# up = FALSE: kolor akcentu i przekreślenie. label: podpis pod spodem.
s8p_fan <- function(x, y, r = 30, up = TRUE, label = NULL, sub = NULL) {
  col <- if (up) "var(--upwr-ink-soft)" else "var(--upwr-accent)"
  blade <- sprintf('<ellipse cx="0" cy="%g" rx="%g" ry="%g" fill="%s" opacity="%s"/>', -r * .48, r * .2, r * .4, col, if (up) ".75" else ".5")
  paste0(
    sprintf('<g transform="translate(%g,%g)">', x, y),
    sprintf('<circle cx="0" cy="0" r="%g" fill="var(--upwr-surface)" stroke="%s" stroke-width="2.4"/>', r, col),
    blade, sprintf('<g transform="rotate(120)">%s</g><g transform="rotate(240)">%s</g>', blade, blade),
    sprintf('<circle cx="0" cy="0" r="%g" fill="%s"/>', r * .14, col),
    if (!up) sprintf('<path d="M%g,%g L%g,%g M%g,%g L%g,%g" stroke="var(--upwr-accent)" stroke-width="2.6" stroke-linecap="round" fill="none"/>',
      -r * .7, -r * .7, r * .7, r * .7, -r * .7, r * .7, r * .7, -r * .7) else "",
    if (!is.null(label)) sprintf('<text x="0" y="%g" font-size="15" font-weight="600" text-anchor="middle"%s>%s</text>', r + 20,
      if (!up) ' class="s8p-bad"' else "", label) else "",
    if (!is.null(sub)) sprintf('<text class="s8p-mono s8p-subtle" x="0" y="%g" font-size="12.5" text-anchor="middle">%s</text>', r + 37, sub) else "",
    "</g>")
}

# Werdykt jako pastylka: tone = "ok" (szałwia), "bad" (akcent), "idle" (szary).
s8p_chip <- function(x, y, text, tone = "ok", w = 150) {
  col <- c(ok = "var(--upwr-sage)", bad = "var(--upwr-accent)", idle = "var(--upwr-ink-subtle)")[[tone]]
  sprintf('<rect x="%g" y="%g" width="%g" height="28" rx="14" fill="%s" opacity=".16"/><text x="%g" y="%g" font-size="14" font-weight="700" text-anchor="middle" style="fill:%s">%s</text>',
    x, y - 19, w, col, x + w / 2, y, col, text)
}

# Wszystkie stany n bloków jako macierz logiczna 2ⁿ × n; wiersz 1 = wszystkie
# działają, kolejność jak w tabeli stanów (pierwszy blok zmienia się najwolniej).
s8p_states <- function(n) {
  g <- as.matrix(expand.grid(rep(list(c(TRUE, FALSE)), n)))[, n:1, drop = FALSE]
  unname(g)
}
# Numer stanu jako maska bitowa (bit i − 1 ustawiony = blok i działa); tak
# indeksuje stany scena „uklad” w JS.
s8p_mask <- function(up) sum(2^(which(up) - 1))

# ============================================================================
# KRATKI MISJI
# ============================================================================

# Siatka kratek: counts = c(klasa = liczba, …) w kolejności rysowania (wierszami).
s8p_cells <- function(counts, ncol, x = 0, y = 0, size = 14, gap = 3) {
  cls <- rep(names(counts), counts)
  i <- seq_along(cls) - 1
  paste(sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g" rx="2"/>', cls,
    x + (i %% ncol) * (size + gap), y + (i %/% ncol) * (size + gap), size, size), collapse = "")
}

# Odczyt ryciny lcSF wypełniany przez JS: S.setValue(root, key, tekst).
s8p_read <- function(key, label, color = NULL, text = "–") {
  lc_readout(label, lc_sf_value(key, text), color = color, swatch = !is.null(color))
}

# ============================================================================
# SCENY W PRZEGLĄDARCE (modules/sceny.js, window.lcS8)
# ============================================================================

# Przycisk sceny: wygląd lc_action(), bez wejścia Shiny — klik obsługuje scena
# po atrybucie data-s8p (scene.on[act]).
s8p_button <- function(act, label = NULL, icon = NULL, variant = "outline", aria_label = NULL) {
  b <- lc_action(paste0("s8p-", act), label, icon = icon, variant = variant, aria_label = aria_label)
  b$attribs$id <- NULL
  b$attribs$class <- sub("action-button ", "", b$attribs$class, fixed = TRUE)
  b$attribs[["data-s8p"]] <- act
  b
}

s8p_prev_next <- function() tagList(s8p_button("prev", "Wstecz", icon = "prev"),
  s8p_button("next", "Dalej", icon = "next", variant = "solid"))

# Przełącznik sceny: przyciski data-s8p = act, data-value = wartość; scena dostaje
# kliknięty przycisk w scene.on[act](btn) i sama ustawia aria-pressed
# (lcS8.util.press(btn)). choices: c("Etykieta" = "wartość").
s8p_seg <- function(act, label, choices, selected = choices[[1]]) {
  lc_group(label, tags$div(class = "lc-seg", role = "group", `aria-label` = label,
    lapply(seq_along(choices), function(i) tags$button(type = "button", `data-s8p` = act,
      `data-value` = unname(choices[[i]]),
      `aria-pressed` = if (identical(unname(choices[[i]]), unname(selected))) "true" else "false",
      names(choices)[[i]]))))
}

# Sam kontener sceny (bez panelu): do kilku scen w jednym panelu.
s8p_scene_div <- function(scene, config, ...) {
  tags$div(class = "s8p-scene", `data-scene` = scene,
    `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA), ...)
}
# Kontener sceny w panelu: ... to treść w kolejności (sloty s8p_out() i zwykłe tagi).
s8p_scene_ui <- function(num, kind, title, scene, config, ..., width_mode = "wide") {
  s8p_panel(num, kind, title, width_mode = width_mode, s8p_css, s8p_scene_div(scene, config, ...))
}
s8p_out <- function(name, ...) tags$div(`data-s8p-out` = name, ...)

# Scena z kadrami policzonymi w R. frames: lista list(name, text, read); svg(i)
# zwraca rysunek kadru i (s8p_svg).
s8p_frames_ui <- function(num, kind, title, frames, svg, body = s8p_frame_body, width_mode = "wide") {
  cfg <- list(frames = lapply(seq_along(frames), function(i) list(
    name = frames[[i]]$name,
    svg = as.character(svg(i)),
    body = as.character(body(frames[[i]]))
  )))
  s8p_scene_ui(num, kind, title, "kadry", cfg,
    s8p_out("head"), s8p_out("stage"), s8p_out("body"),
    tags$div(class = "s8p-nav", s8p_prev_next()), width_mode = width_mode)
}

# Treść kadru albo widoku: opis i (gdy jest) odczyt.
s8p_frame_body <- function(f) {
  tagList(tags$p(class = "s8p-caption", f$text),
    if (!is.null(f$read) && !(is.character(f$read) && !nzchar(f$read))) tags$p(class = "s8p-read", f$read))
}

# Widoki z przełącznikami: segs = lista s8p_seg(); views = nazwana lista
# list(svg, text, read). Klucz widoku to wartości wciśniętych przycisków kolejnych
# przełączników połączone znakiem "|" (jeden przełącznik: sama wartość).
s8p_views_div <- function(segs, views) {
  cfg <- list(views = lapply(views, function(v) list(svg = as.character(v$svg),
    body = as.character(s8p_frame_body(v)))))
  s8p_scene_div("widoki", cfg, tags$div(class = "s8p-nav", segs), s8p_out("stage"), s8p_out("body"))
}
s8p_views_ui <- function(num, kind, title, segs, views, ..., width_mode = "wide") {
  s8p_panel(num, kind, title, width_mode = width_mode, s8p_css, s8p_views_div(segs, views), ...)
}

# Klikalny schemat: wszystkie 2ⁿ stanów policzone w R. draw(up) zwraca s8p_svg();
# status(up) zwraca tekst (HTML) pod schematem. Bloki w draw() muszą być klikalne
# (s8p_rbd(click = TRUE)). Przycisk „Napraw wszystko” przywraca stan początkowy.
s8p_rbd_views <- function(n, draw, status) {
  st <- s8p_states(n)
  masks <- apply(st, 1, s8p_mask)
  views <- vector("list", 2^n)
  for (r in seq_len(nrow(st))) {
    views[[masks[r] + 1]] <- list(svg = as.character(draw(st[r, ])), body = as.character(status(st[r, ])))
  }
  list(n = n, views = views)
}
s8p_rbd_reset <- function() tags$div(class = "s8p-nav", s8p_button("reset", "Napraw wszystko", icon = "reset"))
s8p_rbd_scene <- function(n, draw, status) {
  s8p_scene_div("uklad", s8p_rbd_views(n, draw, status), s8p_out("stage"), s8p_out("body"), s8p_rbd_reset())
}
# Kilka klikalnych schematów pod przełącznikiem architektury. archs: nazwana lista
# list(n, draw, status); seg: s8p_seg("arch", …) z tymi samymi kluczami.
s8p_rbd_scene_multi <- function(archs, seg) {
  cfg <- list(archs = lapply(archs, function(a) s8p_rbd_views(a$n, a$draw, a$status)))
  s8p_scene_div("uklad", cfg, tags$div(class = "s8p-nav", seg), s8p_out("stage"), s8p_out("body"), s8p_rbd_reset())
}
