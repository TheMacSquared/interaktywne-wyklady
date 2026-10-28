# PROTOTYPY (2026-10-28) — sceny do przeglądu -------------------------------
#
# Prototypy rysowane po stronie serwera (SVG z R + renderUI), bez animacji.
# Jeśli scena przejdzie przegląd, docelowo trafi do JS jak sceny z wykładu 03.
#   p4_proto_bananas_ui   — Scena „Pająk w kiści” (kadry historii) (1a): wynik 0/1 wprost
#   p4_proto_threshold_ui — Scena „Kontroler i próg” (kadry historii) (1b): 0/1 z liczby i normy
#   (1a, 1b, 2 i 9 zaakceptowane 2026-10-28: plakietka „Scena” zamiast „Prototyp”)
#   p4_proto_pallet_ui    — Scena „Partia na palecie” (historia z powtórzeniem) (2)
#   p4_proto_drivers_ui   — Scena „Hala, 100 kierowców” (historia z powtórzeniem) (9)
#   p4_proto_receipt_ui   — Scena „Odbiór dostawy” (historia z powtórzeniem) (11)
# Serwer: p4_proto_scenes_server(input, output, session).
# Wymaga: stop_proto_wykresy.R (p4_proto_panel, p4_proto_css).

p4_scene_css <- tags$style(HTML("
  .p4s-head { display: flex; justify-content: space-between; align-items: baseline; gap: 1em; margin-bottom: .3em; }
  .p4s-name { font-weight: 700; font-size: 1.05em; }
  .p4s-step { font-family: var(--upwr-mono); font-size: .9em; color: var(--upwr-ink-subtle); }
  .p4s-caption { min-height: 3em; margin: .4em 0; }
  .p4s-read { font-family: var(--upwr-mono); font-weight: 600; margin: .2em 0 .6em; }
  .p4s-nav { display: flex; gap: .6em; flex-wrap: wrap; }
  .p4s-svg { display: block; width: 100%; height: auto; margin: 0 auto; }
  /* kolor domyślny tylko dla napisów bez własnego fill (atrybutu albo stylu) */
  .p4s-svg text { font-family: var(--upwr-sans); }
  .p4s-svg text:not([fill]):not([style*='fill']) { fill: var(--upwr-ink); }
  .p4s-mono { font-family: var(--upwr-mono) !important; }
  .p4s-subtle { fill: var(--upwr-ink-subtle) !important; }
  .p4s-ok { fill: color-mix(in srgb, var(--upwr-sage) 30%, var(--upwr-surface)); stroke: var(--upwr-sage); stroke-width: 1.2; }
  .p4s-bad { fill: var(--upwr-accent); stroke: var(--upwr-accent); }
  .p4s-idle { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1.2; }
  .p4s-band { fill: var(--upwr-sage); fill-opacity: .15; }
  .p4s-band-edge { stroke: var(--upwr-sage); stroke-width: 1.4; }
  .p4s-band-late { fill: var(--upwr-single-alt); fill-opacity: .18; }
  .p4s-band-late-edge { stroke: var(--upwr-single-alt); stroke-width: 1.4; stroke-dasharray: 5 4; }
  .p4s-axis { stroke: var(--upwr-ink-subtle); stroke-width: 1.2; }
  .p4s-stamp { font-weight: 800 !important; font-size: 26px; letter-spacing: .08em; }
  .p4s-stamp-ok { fill: var(--upwr-sage) !important; }
  .p4s-stamp-bad { fill: var(--upwr-accent) !important; }
  .p4s-stamp-box-ok { fill: none; stroke: var(--upwr-sage); stroke-width: 3; }
  .p4s-stamp-box-bad { fill: none; stroke: var(--upwr-accent); stroke-width: 3; }
  .p4s-phobia-slot { margin-left: auto; }
  .p4s-layer { opacity: 0; animation: p4s-show 1ms linear both; }
  @keyframes p4s-show { to { opacity: 1; } }
  .p4s-fly { animation-name: p4s-fly; animation-timing-function: cubic-bezier(.3, .1, .3, 1); animation-fill-mode: both; }
  .p4s-fly.is-out { animation-name: p4s-fly-out; }
  @keyframes p4s-fly { 0% { transform: translate(var(--dx), var(--dy)); opacity: 0; }
    15% { transform: translate(var(--dx), var(--dy)); opacity: 1; } 100% { transform: none; opacity: .85; } }
  @keyframes p4s-fly-out { 0% { transform: translate(var(--dx), var(--dy)); opacity: 0; }
    15% { transform: translate(var(--dx), var(--dy)); opacity: 1; } 90% { transform: none; opacity: 1; } 100% { transform: none; opacity: 0; } }
  @media (prefers-reduced-motion: reduce) {
    .p4s-layer, .p4s-fly { animation: none !important; opacity: 1; }
    .p4s-fly.is-out { opacity: 0; }
  }
  .p4s-calc { margin-top: .9em; padding: .7em 1em; border-top: 1px solid var(--upwr-rule); font-size: .95em; }
  .p4s-calc-row { display: flex; gap: .8em; flex-wrap: wrap; padding: .12em 0; }
  .p4s-calc-l { min-width: 15em; color: var(--upwr-ink-soft); }
  .p4s-calc-v { font-family: var(--upwr-mono); }
  .p4s-tally { border-collapse: collapse; margin: .4em 0; font-family: var(--upwr-mono); }
  .p4s-tally th, .p4s-tally td { border: 1px solid var(--upwr-rule); padding: 4px 12px; text-align: center; }
  .p4s-tally th { font-family: var(--upwr-sans); font-weight: 600; }
  .p4s-tally .is-err { color: var(--upwr-accent); font-weight: 700; }
"))

p4_scene_shell <- function(id, kind, title, buttons, width_mode = "wide", proto = TRUE, after = NULL) {
  panel <- if (proto) p4_proto_panel else function(kind, title, ...) figure_panel(label = kind, title = title, ...)
  panel(kind, title, width_mode = width_mode,
    p4_scene_css,
    uiOutput(paste0(id, "_head")),
    uiOutput(paste0(id, "_stage")),
    uiOutput(paste0(id, "_caption")),
    tags$div(class = "p4s-nav", buttons),
    after
  )
}

p4_svg <- function(w, h, aria, ...) {
  HTML(paste0(sprintf('<svg class="p4s-svg" style="max-width:%dpx" viewBox="0 0 %d %d" role="img" aria-label="%s">', w, w, h, aria),
    paste(unlist(list(...)), collapse = ""), "</svg>"))
}

# ============================================================================
# (1a) PAJĄK W KIŚCI — kadry historii; doświadczenie samo daje wynik 0/1
# ============================================================================

# Dwanaście kiści, pająki w kiściach 4 i 9 (stałe, żeby narracja się zgadzała).
p4_banana_spider <- seq_len(12) %in% c(4, 9)

p4_banana_frames <- list(
  list(name = "Dostawa",
    text = "Do dojrzewalni Bananpolu przyjechała skrzynia bananów z Ekwadoru. Z zewnątrz każda kiść wygląda tak samo."),
  list(name = "Otwieramy",
    text = "Kontroler rozchyla kiść po kiści. Albo jest czysta, albo siedzi w niej pająk. Trzeciej możliwości nie ma."),
  list(name = "Zero albo jeden",
    text = "Wynik każdej kontroli to tak albo nie. Nikt nie musi niczego mierzyć ani ustalać granicy: doświadczenie samo odpowiada zerem albo jedynką.")
)

# Żart z fobiami: phobia = 1 pająk, 2 klaun (po „arachnofobii”), 3 znak ostrzegawczy
# (po „koulrofobii”). Zmienia się tylko rysunek lokatora kiści, wynik 0/1 zostaje.
p4_banana_phobia <- list(
  list(button = "arachnofobia", who = "z pająkiem"),
  list(button = "koulrofobia", who = "z klaunem"),
  list(button = NULL, who = "z zagrożeniem")
)

p4_banana_guest <- function(bx, by, phobia) {
  if (phobia == 1L) {
    legs <- paste(vapply(c(-1, 1), function(sg) paste(vapply(c(-6, -2, 2, 6), function(dy) sprintf(
      '<path d="M %g %g l %g %g l %g %g" fill="none" stroke="#1c1a17" stroke-width="2"/>',
      bx, by + dy, sg * 12, -5 + dy * 0.8, sg * 5, 8), character(1)), collapse = ""), character(1)), collapse = "")
    return(paste0(legs, sprintf('<ellipse cx="%g" cy="%g" rx="8" ry="10" fill="#1c1a17"/><circle cx="%g" cy="%g" r="5.5" fill="#1c1a17"/>',
      bx, by + 2, bx, by - 11)))
  }
  if (phobia == 2L) {
    # klaun: kolorowe włosy, biała twarz, czerwony nos, uśmiech
    hair <- sprintf('<circle cx="%g" cy="%g" r="7" fill="#e0457b"/><circle cx="%g" cy="%g" r="7" fill="#3a8fd6"/><circle cx="%g" cy="%g" r="6" fill="#f2a516"/>',
      bx - 12, by - 6, bx + 12, by - 6, bx, by - 16)
    return(paste0(hair, sprintf(paste0(
      '<circle cx="%g" cy="%g" r="13" fill="#fffaf0" stroke="#1c1a17" stroke-width="1.4"/>',
      '<circle cx="%g" cy="%g" r="1.8" fill="#1c1a17"/><circle cx="%g" cy="%g" r="1.8" fill="#1c1a17"/>',
      '<circle cx="%g" cy="%g" r="4" fill="#d62828"/>',
      '<path d="M %g %g q 7 7 14 0" fill="none" stroke="#d62828" stroke-width="2" stroke-linecap="round"/>'),
      bx, by, bx - 5, by - 4, bx + 5, by - 4, bx, by + 1, bx - 7, by + 5)))
  }
  # znak ostrzegawczy
  sprintf(paste0('<path d="M %g %g L %g %g L %g %g Z" fill="#f2c200" stroke="#1c1a17" stroke-width="2" stroke-linejoin="round"/>',
    '<rect x="%g" y="%g" width="4" height="11" rx="1.5" fill="#1c1a17"/><circle cx="%g" cy="%g" r="2.4" fill="#1c1a17"/>'),
    bx, by - 18, bx + 17, by + 12, bx - 17, by + 12, bx - 2, by - 8, bx, by + 7)
}

p4_banana_svg_one <- function(cx, cy, open, spider, phobia = 1L) {
  # trzy leżące banany (półksiężyce) spięte szypułką po prawej
  bunch <- paste(vapply(c(-12, 0, 12), function(dy) {
    x <- cx; y <- cy + dy
    sprintf('<path d="M %g %g Q %g %g %g %g Q %g %g %g %g Z" fill="#e7c33c" stroke="#a9821a" stroke-width="1.4"/><circle cx="%g" cy="%g" r="2" fill="#5a4a20"/>',
      x - 30, y - 8, x - 2, y + 22, x + 26, y - 6, x - 2, y + 8, x - 30, y - 8, x - 30, y - 8)
  }, character(1)), collapse = "")
  stem <- sprintf('<path d="M %g %g q 8 -4 12 -14" fill="none" stroke="#6b5a2a" stroke-width="4" stroke-linecap="round"/>', cx + 24, cy - 14)
  guest <- if (open && spider) p4_banana_guest(cx - 2, cy - 8, phobia) else ""
  ring <- if (open) sprintf('<circle cx="%g" cy="%g" r="40" fill="none" stroke="%s" stroke-width="2"/>',
    cx, cy - 2, if (spider) "var(--upwr-accent)" else "var(--upwr-sage)") else ""
  paste0(ring, bunch, stem, guest)
}

p4_bananas_svg <- function(step, phobia = 1L) {
  open <- step >= 2
  items <- vapply(seq_along(p4_banana_spider), function(i) {
    cx <- 60 + ((i - 1) %% 6) * 112; cy <- 70 + ((i - 1) %/% 6) * 120
    paste0(p4_banana_svg_one(cx, cy, open, p4_banana_spider[i], phobia),
      if (step >= 3) sprintf('<text class="p4s-mono" x="%g" y="%g" text-anchor="middle" font-size="20" font-weight="700" fill="%s">%d</text>',
        cx, cy + 50, if (p4_banana_spider[i]) "var(--upwr-accent)" else "var(--upwr-ink-subtle)", as.integer(p4_banana_spider[i])) else "")
  }, character(1))
  p4_svg(680, 280, "Dwanaście kiści bananów; po otwarciu w dwóch coś siedzi.",
    items, if (step >= 3) sprintf('<text x="670" y="272" text-anchor="end" font-size="15" font-weight="600">X = liczba kiści %s = %d</text>',
      p4_banana_phobia[[phobia]]$who, sum(p4_banana_spider)) else "")
}

p4_proto_bananas_ui <- p4_scene_shell("p4s_ban", "Scena", "Pająk w kiści", proto = FALSE,
  tagList(lc_action("p4s_ban_prev", "Wstecz", icon = "prev"), lc_action("p4s_ban_next", "Dalej", icon = "next", variant = "solid"),
    uiOutput("p4s_ban_phobia_btn", class = "p4s-phobia-slot")))

# ============================================================================
# (1b) KONTROLER I PRÓG — kadry historii
# ============================================================================

p4_valve_pressure <- c(3.05, 2.93, 3.12, 2.75, 3.01, 2.88, 3.24, 2.97, 3.08, 2.95)
p4_tol <- c(2.8, 3.2)
p4_tol_late <- c(2.7, 3.3)

p4_threshold_frames <- list(
  list(name = "Pomiary",
    text = "Z zaworami do chłodni jest trudniej niż z bananami. Kontroler mierzy ciśnienie otwarcia dziesięciu zaworów i dostaje liczby w barach, a nie odpowiedź tak albo nie."),
  list(name = "Kryterium na piśmie",
    text = "Zanim zmierzył pierwszy zawór, w protokole stała tolerancja producenta: od 2.8 do 3.2 bara."),
  list(name = "Zero albo jeden",
    text = "Każdy pomiar staje się jednym z dwóch wyników: w tolerancji albo poza nią. Z dziesięciu liczb zostaje dziesięć zer i jedynek."),
  list(name = "Granica po fakcie",
    text = "Kierownik dostawy proponuje „nie przesadzać” i poszerzyć tolerancję do 2.7–3.3 bara. Wady znikają z protokołu, ale zawory są te same.")
)

p4_threshold_svg <- function(step) {
  W <- 760; H <- 300; x0 <- 110; dx <- 62; yt <- 30; yb <- 210
  y <- function(v) yb - (v - 2.6) / 0.8 * (yb - yt)
  tol <- if (step >= 4) p4_tol_late else p4_tol
  bad <- p4_valve_pressure < tol[1] | p4_valve_pressure > tol[2]
  bad_before <- p4_valve_pressure < p4_tol[1] | p4_valve_pressure > p4_tol[2]
  band <- function(t, cls, edge) sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g"/><line class="%s" x1="%g" x2="%g" y1="%g" y2="%g"/><line class="%s" x1="%g" x2="%g" y1="%g" y2="%g"/>',
    cls, x0 - 30, y(t[2]), dx * 10, y(t[1]) - y(t[2]),
    edge, x0 - 30, x0 - 30 + dx * 10, y(t[1]), y(t[1]), edge, x0 - 30, x0 - 30 + dx * 10, y(t[2]), y(t[2]))
  ticks <- paste(vapply(seq(2.6, 3.4, 0.2), function(v) sprintf(
    '<line class="p4s-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="p4s-mono p4s-subtle" x="%g" y="%g" text-anchor="end" font-size="12">%.1f</text>',
    x0 - 38, x0 - 32, y(v), y(v), x0 - 42, y(v) + 4, v), character(1)), collapse = "")
  bands <- c(
    if (step >= 2) band(p4_tol, "p4s-band", "p4s-band-edge"),
    if (step >= 4) band(p4_tol_late, "p4s-band-late", "p4s-band-late-edge")
  )
  dots <- vapply(seq_along(p4_valve_pressure), function(i) {
    cx <- x0 + (i - 1) * dx; v <- p4_valve_pressure[i]
    cls <- if (step < 3) "p4s-idle" else if (bad[i]) "p4s-bad" else "p4s-ok"
    paste0(
      sprintf('<circle class="%s" cx="%g" cy="%g" r="9"/>', cls, cx, y(v)),
      if (step >= 4 && bad_before[i]) sprintf('<circle cx="%g" cy="%g" r="15" fill="none" stroke="var(--upwr-accent)" stroke-width="1.6" stroke-dasharray="4 3"/>', cx, y(v)) else "",
      sprintf('<text class="p4s-mono p4s-subtle" x="%g" y="%g" text-anchor="middle" font-size="11">%.2f</text>', cx, y(v) - 15, v),
      if (step >= 3) sprintf('<text class="p4s-mono" x="%g" y="252" text-anchor="middle" font-size="20" font-weight="700" fill="%s">%d</text>',
        cx, if (bad[i]) "var(--upwr-accent)" else "var(--upwr-ink-subtle)", as.integer(bad[i])) else ""
    )
  }, character(1))
  p4_svg(W, H, "Dziesięć pomiarów ciśnienia otwarcia zaworów na tle pasa tolerancji.",
    sprintf('<line class="p4s-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x0 - 35, x0 - 35, yt, yb),
    '<text class="p4s-subtle" x="12" y="24" font-size="12">bar</text>',
    ticks, bands, dots,
    if (step >= 3) sprintf('<text x="%g" y="288" font-size="15" font-weight="600">X = liczba jedynek = %d</text>', x0 - 30, sum(bad)) else "")
}

p4_proto_threshold_ui <- p4_scene_shell("p4s_thr", "Scena", "Kontroler i próg", proto = FALSE,
  tagList(lc_action("p4s_thr_prev", "Wstecz", icon = "prev"), lc_action("p4s_thr_next", "Dalej", icon = "next", variant = "solid")))

# ============================================================================
# (2) PARTIA NA PALECIE — historia z powtórzeniem
# ============================================================================

# Animacja (CSS, bez JS): przy „Skontroluj partię” i „+10” kolejne partie z tej
# porcji (`recent`) pojawiają się na palecie jedna po drugiej, a z każdej wylatuje
# kropka na swój słupek. `slot` to czas jednej partii w ms; przy „+100” recent = NULL.
p4_pallet_grid <- function(cur, cls_extra = "", style = "") {
  paste0(sprintf('<g class="%s" style="%s">', cls_extra, style), paste(vapply(seq_along(cur), function(i) {
    sprintf('<circle class="%s" cx="%g" cy="%g" r="10"/>', if (cur[i]) "p4s-bad" else "p4s-ok",
      40 + ((i - 1) %% 10) * 30, 50 + ((i - 1) %/% 10) * 26)
  }, character(1)), collapse = ""), "</g>")
}

p4_pallet_svg <- function(cur, xs, recent = NULL, slot = 0) {
  W <- 780; H <- 320
  m <- length(xs); kmax <- 8; n_new <- length(recent)
  bx <- function(k) 400 + k * 44
  dot_y <- function(r) 290 - (r - 0.5) * 11
  grid <- if (n_new) {
    paste(vapply(seq_len(n_new), function(j) p4_pallet_grid(recent[[j]], "p4s-layer",
      sprintf("animation-delay:%dms", round((j - 1) * slot))), character(1)), collapse = "")
  } else if (length(cur)) p4_pallet_grid(cur) else paste(vapply(1:100, function(i) sprintf('<circle class="p4s-idle" cx="%g" cy="%g" r="10"/>',
    40 + ((i - 1) %% 10) * 30, 50 + ((i - 1) %/% 10) * 26), character(1)), collapse = "")
  # stos partii: jedna kropka = jedna partia; gdy słupek przekracza 22 kropki, stos zamienia się
  # w wykres słupkowy (wysokość proporcjonalna, liczba partii nad słupkiem), żeby rzadkie
  # wartości 7 i 8+ nie znikały przez zaokrąglenie do zera kropek
  bins <- pmin(xs, kmax)
  counts <- tabulate(bins + 1L, kmax + 1L)
  scaled <- m > 0 && max(counts) > 22
  rank <- ave(seq_along(bins), bins, FUN = seq_along)
  bar_h <- function(cnt) if (cnt == 0) 0 else max(2, cnt / max(counts) * 220)
  is_new <- seq_len(m) > m - n_new
  fly <- function(i, j, out) {
    k <- bins[i]; y <- if (scaled) 294 - bar_h(counts[k + 1]) + 5 else dot_y(rank[i])
    sprintf('<circle class="p4s-bad %s" cx="%g" cy="%g" r="5" style="--dx:%gpx;--dy:%gpx;animation-delay:%dms;animation-duration:%dms"/>',
      if (out) "p4s-fly is-out" else "p4s-fly", bx(k), y, 180 - bx(k), 165 - y,
      round((j - 1) * slot + 0.35 * slot), round(0.6 * slot))
  }
  stack <- if (!scaled) {
    paste(vapply(seq_len(m), function(i) if (is_new[i]) fly(i, i - (m - n_new), FALSE) else
      sprintf('<circle class="p4s-bad" style="opacity:.85" cx="%g" cy="%g" r="5"/>', bx(bins[i]), dot_y(rank[i])), character(1)), collapse = "")
  } else {
    paste0(paste(vapply(0:kmax, function(k) {
      cnt <- counts[k + 1]; h <- bar_h(cnt)
      paste0(if (cnt) sprintf('<rect class="p4s-bad" style="opacity:.85" x="%g" y="%g" width="26" height="%g" rx="2"/>', bx(k) - 13, 294 - h, h) else "",
        sprintf('<text class="p4s-mono p4s-subtle" x="%g" y="%g" text-anchor="middle" font-size="11">%d</text>', bx(k), 294 - h - 5, cnt))
    }, character(1)), collapse = ""),
    paste(vapply(which(is_new), function(i) fly(i, i - (m - n_new), TRUE), character(1)), collapse = ""))
  }
  labels <- paste(vapply(0:kmax, function(k) sprintf('<text class="p4s-mono p4s-subtle" x="%g" y="310" text-anchor="middle" font-size="12">%s</text>',
    bx(k), if (k == kmax) "8+" else k), character(1)), collapse = "")
  p4_svg(W, H, sprintf("Paleta stu zaworów z zaznaczonymi niesprawnymi i stos liczby wad w %d partiach.", m),
    '<text x="25" y="22" font-size="14" font-weight="600">Ta partia</text>',
    sprintf('<text x="378" y="22" font-size="14" font-weight="600">Wszystkie partie (%d)</text>', m),
    '<rect x="20" y="32" width="320" height="270" rx="6" fill="none" stroke="var(--upwr-rule)"/>',
    grid,
    sprintf('<line class="p4s-axis" x1="380" x2="%g" y1="294" y2="294"/>', bx(kmax) + 20),
    labels, stack,
    if (scaled) '<text class="p4s-subtle" x="378" y="40" font-size="11">liczba partii nad słupkiem</text>' else "")
}

p4_proto_pallet_ui <- p4_scene_shell("p4s_pal", "Scena", "Partia na palecie", proto = FALSE,
  tagList(lc_action("p4s_pal_one", "Skontroluj partię", variant = "solid"),
    lc_action("p4s_pal_ten", "+10 partii"), lc_action("p4s_pal_hund", "+100 partii"),
    lc_action("p4s_pal_reset", icon = "reset", variant = "ghost", aria_label = "Nowa paleta od zera")))

# ============================================================================
# (9) HALA, 100 KIEROWCÓW — historia z powtórzeniem
# ============================================================================

# Ziarno 31: po 10 latach 22 kierowców z kolizją, zgodnie z modelem (22.1).
p4_drivers_first <- local({ set.seed(31); rgeom(100, 1e-4) + 1 })

p4_drivers_svg <- function(year) {
  hit <- p4_drivers_first <= 250 * year
  figs <- paste(vapply(1:100, function(i) {
    cx <- 40 + ((i - 1) %% 20) * 36; cy <- 40 + ((i - 1) %/% 20) * 56
    cls <- if (hit[i]) "p4s-bad" else "p4s-idle"
    paste0(sprintf('<circle class="%s" cx="%g" cy="%g" r="7"/><rect class="%s" x="%g" y="%g" width="16" height="20" rx="5"/>',
      cls, cx, cy, cls, cx - 8, cy + 9),
      if (hit[i]) sprintf('<text class="p4s-mono" x="%g" y="%g" text-anchor="middle" font-size="10" fill="var(--upwr-accent)">r%d</text>',
        cx, cy + 42, ceiling(p4_drivers_first[i] / 250)) else "")
  }, character(1)), collapse = "")
  p4_svg(760, 300, sprintf("Sto kierowców wózków po %d latach; %d miało co najmniej jedną kolizję.", year, sum(hit)), figs)
}

p4_proto_drivers_ui <- p4_scene_shell("p4s_drv", "Scena", "Hala, 100 kierowców", proto = FALSE,
  tagList(lc_action("p4s_drv_next", "Kolejny rok", variant = "solid"),
    lc_action("p4s_drv_reset", icon = "reset", variant = "ghost", aria_label = "Od początku")),
  after = uiOutput("p4s_drv_calc"))

# Rachunek pod sceną: ryzyko skumulowane jednego kierowcy po y latach, wzór (4.6),
# obok odsetka kierowców z co najmniej jedną kolizją w hali.
p4_drivers_calc <- function(year) {
  sup <- function(x) chartr("0123456789", "⁰¹²³⁴⁵⁶⁷⁸⁹", format(x, big.mark = "", scientific = FALSE))
  n <- 250L * year
  none <- 0.9999^n
  hall <- sum(p4_drivers_first <= n)
  line <- function(label, value) tags$div(class = "p4s-calc-row", tags$span(class = "p4s-calc-l", label), tags$span(class = "p4s-calc-v", value))
  tags$div(class = "p4s-calc",
    line("Ekspozycja skumulowana:", if (year == 0) "n = 0 przejazdów" else sprintf("n = 250 · %d = %s przejazdów", year, format(n, big.mark = " "))),
    line("Bez kolizji w jednym przejeździe:", "1 - p = 1 - 0.0001 = 0.9999"),
    line("Bez kolizji przez całe n:", if (year == 0) "—" else sprintf("0.9999%s ≈ %.3f", sup(n), none)),
    line("Ryzyko skumulowane (4.6):", if (year == 0) "—" else sprintf("1 - %.3f ≈ %.3f", none, 1 - none)),
    line("W hali:", if (year == 0) "—" else sprintf("%d na 100 kierowców z co najmniej jedną kolizją", hall))
  )
}

# ============================================================================
# (11) ODBIÓR DOSTAWY — historia z powtórzeniem
# ============================================================================

p4_receipt_n <- 50L; p4_receipt_c <- 1L
p4_receipt_quality <- c(good = 0.02, weak = 0.08)
p4_receipt_share_good <- 0.7

p4_receipt_draw <- function() {
  q <- if (runif(1) < p4_receipt_share_good) "good" else "weak"
  s <- runif(p4_receipt_n) < p4_receipt_quality[[q]]
  list(quality = q, sample = s, accepted = sum(s) <= p4_receipt_c)
}

p4_receipt_svg <- function(d) {
  cells <- if (is.null(d)) paste(vapply(1:p4_receipt_n, function(i) sprintf('<circle class="p4s-idle" cx="%g" cy="%g" r="10"/>',
    40 + ((i - 1) %% 10) * 30, 50 + ((i - 1) %/% 10) * 28), character(1)), collapse = "") else
    paste(vapply(seq_along(d$sample), function(i) sprintf('<circle class="%s" cx="%g" cy="%g" r="10"/>',
      if (d$sample[i]) "p4s-bad" else "p4s-ok", 40 + ((i - 1) %% 10) * 30, 50 + ((i - 1) %/% 10) * 28), character(1)), collapse = "")
  stamp <- if (is.null(d)) "" else {
    ok <- d$accepted; word <- if (ok) "PRZYJĘTA" else "ODRZUCONA"
    paste0(sprintf('<g transform="rotate(-8 520 90)"><rect class="%s" x="410" y="62" width="220" height="50" rx="6"/><text class="p4s-stamp %s" x="520" y="97" text-anchor="middle">%s</text></g>',
      if (ok) "p4s-stamp-box-ok" else "p4s-stamp-box-bad", if (ok) "p4s-stamp-ok" else "p4s-stamp-bad", word),
      sprintf('<text x="410" y="160" font-size="15">Niesprawnych w próbce: <tspan class="p4s-mono" font-weight="700">%d</tspan> (limit c = %d)</text>', sum(d$sample), p4_receipt_c),
      # po fakcie: prawdziwa jakość dostawy i werdykt decyzji, wyraźnie pod stemplem
      local({
        good <- d$quality == "good"
        right <- d$accepted == good
        col <- if (good) "var(--upwr-sage)" else "var(--upwr-accent)"
        verdict <- if (right) "✓ Decyzja trafna" else if (good) "✗ Błąd: odrzucono dobrą dostawę" else "✗ Błąd: przepuszczono słabą dostawę"
        paste0(
          sprintf('<rect x="404" y="170" width="290" height="38" rx="6" fill="%s" fill-opacity=".18" stroke="%s" stroke-width="1.8"/>', col, col),
          sprintf('<text x="416" y="195" font-size="15">Po fakcie: <tspan font-weight="800" fill="%s">%s</tspan></text>',
            col, if (good) "DOBRA dostawa, p = 0.02" else "SŁABA dostawa, p = 0.08"),
          sprintf('<text x="410" y="232" font-size="16" font-weight="700" style="fill:%s">%s</text>',
            if (right) "var(--upwr-sage)" else "var(--upwr-accent)", verdict))
      }))
  }
  p4_svg(700, 245, "Próbka 50 zaworów z dostawy, decyzja o przyjęciu i prawdziwa jakość dostawy.",
    '<text x="25" y="22" font-size="14" font-weight="600">Próbka 50 zaworów z palety</text>', cells, stamp)
}

p4_proto_receipt_ui <- p4_scene_shell("p4s_rcp", "Scena", "Odbiór dostawy: plan (50, 1)",
  tagList(lc_action("p4s_rcp_one", "Kolejna dostawa", variant = "solid"),
    lc_action("p4s_rcp_twenty", "+20 dostaw"), lc_action("p4s_rcp_hund", "+100 dostaw"),
    lc_action("p4s_rcp_reset", icon = "reset", variant = "ghost", aria_label = "Wyzeruj dostawy")))

# ============================================================================
# SERWER
# ============================================================================

p4_scene_head <- function(name, step) tags$div(class = "p4s-head",
  tags$span(class = "p4s-name", name), tags$span(class = "p4s-step", step))

p4_proto_scenes_server <- function(input, output, session) {
  # (1a) banany
  ban <- reactiveVal(1L)
  n_ban <- length(p4_banana_frames)
  observeEvent(input$p4s_ban_next, ban(min(n_ban, ban() + 1L)))
  observeEvent(input$p4s_ban_prev, ban(max(1L, ban() - 1L)))
  output$p4s_ban_head <- renderUI(p4_scene_head(p4_banana_frames[[ban()]]$name, sprintf("Kadr %d z %d", ban(), n_ban)))
  phobia <- reactiveVal(1L)
  observeEvent(input$p4s_ban_phobia, phobia(min(3L, phobia() + 1L)))
  output$p4s_ban_stage <- renderUI(p4_bananas_svg(ban(), phobia()))
  output$p4s_ban_phobia_btn <- renderUI({
    label <- p4_banana_phobia[[phobia()]]$button
    # zwykły przycisk HTML, żeby ponowne renderowanie nie zerowało licznika actionButton
    if (ban() >= 2 && !is.null(label)) tags$button(type = "button", class = "lc-action is-ghost",
      onclick = "Shiny.setInputValue('p4s_ban_phobia', Date.now(), {priority: 'event'})", tags$span(label))
  })
  output$p4s_ban_caption <- renderUI(tags$p(class = "p4s-caption", p4_banana_frames[[ban()]]$text))

  # (1b) kadry z progiem
  thr <- reactiveVal(1L)
  n_thr <- length(p4_threshold_frames)
  observeEvent(input$p4s_thr_next, thr(min(n_thr, thr() + 1L)))
  observeEvent(input$p4s_thr_prev, thr(max(1L, thr() - 1L)))
  output$p4s_thr_head <- renderUI(p4_scene_head(p4_threshold_frames[[thr()]]$name, sprintf("Kadr %d z %d", thr(), n_thr)))
  output$p4s_thr_stage <- renderUI(p4_threshold_svg(thr()))
  output$p4s_thr_caption <- renderUI(tags$p(class = "p4s-caption", p4_threshold_frames[[thr()]]$text))

  # (2) paleta
  pal <- reactiveVal(list(cur = logical(0), xs = integer(0), recent = NULL, slot = 0))
  pal_add <- function(m) {
    s <- pal()
    batch <- lapply(seq_len(m), function(i) runif(100) < 0.02)
    s$xs <- c(s$xs, vapply(batch, sum, integer(1)))
    s$cur <- batch[[m]]
    s$recent <- if (m <= 10) batch else NULL
    s$slot <- if (m == 1) 1400 else 500
    pal(s)
  }
  observeEvent(input$p4s_pal_one, pal_add(1))
  observeEvent(input$p4s_pal_ten, pal_add(10))
  observeEvent(input$p4s_pal_hund, pal_add(100))
  observeEvent(input$p4s_pal_reset, pal(list(cur = logical(0), xs = integer(0), recent = NULL, slot = 0)))
  output$p4s_pal_head <- renderUI({
    s <- pal(); m <- length(s$xs)
    p4_scene_head("Kontrola przyjęcia", if (m) sprintf("Partia nr %d · X = %d", m, sum(s$cur)) else "Paleta czeka")
  })
  output$p4s_pal_stage <- renderUI({ s <- pal(); p4_pallet_svg(s$cur, s$xs, s$recent, s$slot) })
  output$p4s_pal_caption <- renderUI({
    m <- length(pal()$xs)
    tags$p(class = "p4s-caption", if (m == 0) "Na rampie stoi paleta stu zaworów. Kontroler sprawdza każdy i odkłada niesprawne na bok."
      else if (m < 10) "Krzyżyki za każdym razem leżą gdzie indziej: czasem parami, czasem wcale."
      else "Pojedyncza paleta nie daje się przewidzieć, ale stos partii za każdym razem rośnie w ten sam kształt.")
  })

  # (9) kierowcy
  drv <- reactiveVal(0L)
  observeEvent(input$p4s_drv_next, drv(min(10L, drv() + 1L)))
  observeEvent(input$p4s_drv_reset, drv(0L))
  output$p4s_drv_head <- renderUI({
    y <- drv(); k <- sum(p4_drivers_first <= 250 * y)
    p4_scene_head(sprintf("Rok %d · %s przejazdów na kierowcę", y, format(250L * y, big.mark = " ")),
      sprintf("Z kolizją: %d na 100", k))
  })
  output$p4s_drv_stage <- renderUI(p4_drivers_svg(drv()))
  output$p4s_drv_calc <- renderUI(p4_drivers_calc(drv()))
  output$p4s_drv_caption <- renderUI({
    y <- drv()
    tags$p(class = "p4s-caption", if (y == 0) "Sto kierowców wózków w hali Bananpolu. Każdy raz dziennie przejeżdża przez skrzyżowanie, na którym kolizja zdarza się raz na 10 000 przejazdów."
      else if (y < 10) "Każdy przejazd jest prawie bezpieczny. Kolejny rok — kolejne 250 przejazdów na kierowcę."
      else sprintf("Po dziesięciu latach kolizję ma za sobą %d kierowców na 100. Rzadko na przejazd nie znaczy rzadko w karierze.",
        sum(p4_drivers_first <= 2500)))
  })

  # (11) odbiór dostaw
  rcp <- reactiveVal(list(last = NULL, log = data.frame(quality = character(0), accepted = logical(0))))
  rcp_add <- function(m) {
    s <- rcp()
    for (i in seq_len(m)) { d <- p4_receipt_draw(); s$last <- d
      s$log <- rbind(s$log, data.frame(quality = d$quality, accepted = d$accepted)) }
    rcp(s)
  }
  observeEvent(input$p4s_rcp_one, rcp_add(1))
  observeEvent(input$p4s_rcp_twenty, rcp_add(20))
  observeEvent(input$p4s_rcp_hund, rcp_add(100))
  observeEvent(input$p4s_rcp_reset, rcp(list(last = NULL, log = data.frame(quality = character(0), accepted = logical(0)))))
  output$p4s_rcp_head <- renderUI(p4_scene_head("Magazyn Bananpolu", sprintf("Dostaw: %d", nrow(rcp()$log))))
  output$p4s_rcp_stage <- renderUI(p4_receipt_svg(rcp()$last))
  output$p4s_rcp_caption <- renderUI({
    lg <- rcp()$log
    if (nrow(lg) == 0) return(tags$p(class = "p4s-caption",
      "Magazynier losuje 50 zaworów z każdej dostawy i przyjmuje ją, gdy znajdzie najwyżej jeden niesprawny. Nie wie, który dostawca miał dobry dzień."))
    cnt <- function(q, a) sum(lg$quality == q & lg$accepted == a)
    tagList(
      tags$table(class = "p4s-tally",
        tags$tr(tags$th(""), tags$th("przyjęta"), tags$th("odrzucona")),
        tags$tr(tags$th("dobra (p = 0.02)"), tags$td(cnt("good", TRUE)), tags$td(class = "is-err", cnt("good", FALSE))),
        tags$tr(tags$th("słaba (p = 0.08)"), tags$td(class = "is-err", cnt("weak", TRUE)), tags$td(cnt("weak", FALSE)))
      ),
      tags$p(class = "p4s-caption", "Burgund: decyzje błędne. Magazynier widzi tylko próbkę, nie wie, w którym wierszu stoi dostawa.")
    )
  })
}
