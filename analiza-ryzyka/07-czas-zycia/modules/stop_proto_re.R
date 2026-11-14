# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa re (park wentylatorów) --------------
#
#   c7p_7_ui   — Piaskownica „Te same awarie, dwa mianowniki” (P7): scena okno
#                (modules/proto_re.js), park 200 wentylatorów, okno 300 h. Przy
#                t = 1000 h oferta A: 19 z 200 i 19 ze 103; przy 3000 h: 5 z 200 i 5 z 27.
#   c7p_9_ui   — Pojęcie „MTTF to pole pod R(t)” (P9), 4 kadry policzone w R: sto
#                linii → obwiednia R(t) → prostokąt o tym samym polu → oferta B.
#   c7p_11_ui  — Pojęcie „Ocalali wyglądają jak nowi” (P11), 4 kadry: nowy park →
#                po 1000 h → zegar od zera → to samo dla parku, który się zużywa.
#                Liczby: 143 z 200 i 74 ze 103 (0.72); oferta B: 92 ze 141 (0.65)
#                wobec 183 z 200 (0.92), jak w przykładach 7.5 i 7.11.
#   c7p_17b_ui — Piaskownica „Trzy parki, jedna średnia” (P17B): scena parki
#                (modules/proto_re.js), modele z przykładu 7.8.
# Wymaga: stop_proto_common.R.

c7pe_css <- tags$style(HTML("
  .c7p7-hit { stroke: var(--upwr-accent) !important; stroke-width: 2.2 !important; }
  .c7pe-curve { stroke: var(--upwr-ink); stroke-width: 2.2; fill: none; }
  .c7pe-ghost { stroke: var(--upwr-ink); stroke-width: 1.6; stroke-dasharray: 5 4; fill: none; }
  .c7pe-box { fill: color-mix(in srgb, var(--upwr-ink) 7%, transparent); stroke: var(--upwr-ink); stroke-width: 1.6; stroke-dasharray: 6 4; }
  .c7pe-slider { min-width: min(100%, 16em); flex: 1 1 16em; }
  .c7pe-reads { display: flex; flex-wrap: wrap; gap: .4em 1.6em; margin: .2em 0 .6em; }
"))

# Suwak sceny (bez Shiny i bez lcSF): odczyt wartości w <output data-for=id>.
c7pe_slider <- function(id, label, min, max, value, step, suffix = NULL) {
  tags$div(class = "lc-grp lc-sf-slider c7pe-slider",
    tags$label(`for` = id, class = "lc-grp-l", label, " ", tags$output(`data-for` = id, value), suffix),
    tags$input(id = id, type = "range", min = min, max = max, value = value, step = step))
}

# ============================================================================
# P7 — PIASKOWNICA „TE SAME AWARIE, DWA MIANOWNIKI”
# ============================================================================

c7p_7_ui <- tagList(c7pe_css,
  c7p_scene_ui("P7", "piaskownica", "Te same awarie, dwa mianowniki", "okno",
    list(n = 200, dt = 300, tmax = 4500,
      models = list(a = list(beta = 1, eta = c7p_mttf), b = list(beta = c7p_beta, eta = c7p_eta))),
    tags$div(class = "c7p-nav",
      c7pe_slider("c7p7_t", "Początek okna t", 0, 3600, 1000, 100, suffix = " h"),
      c7p_seg("model", "Park", c("oferta A" = "a", "oferta B" = "b"))),
    tags$div(class = "c7pe-reads",
      lc_readout("W oknie 300 h zawodzi", c7p_out("k"), color = upwr_accent),
      lc_readout("Na wszystkie nowe", c7p_out("all")),
      lc_readout("Na działające w chwili t", c7p_out("alive"), color = upwr_accent),
      lc_readout("Z modelu (dla działających)", c7p_out("exact"))),
    c7p_out("stage"),
    tags$p(class = "c7p-caption",
      "Linia to jeden wentylator, od najkrócej do najdłużej żyjącego. Linie w kolorze akcentu kończą się w oknie. Te same awarie można odnieść do wszystkich dwustu nowych wentylatorów albo tylko do tych, które dożyły początku okna.")))

# ============================================================================
# Park w kadrach R: linie od najkrótszej (góra) do najdłuższej (dół), więc obwiednia
# liczona od dołu to R(t).
# ============================================================================

c7pe_x0 <- 90; c7pe_w <- 700; c7pe_tmax <- 4500; c7pe_top <- 30; c7pe_h <- 270
c7pe_base <- c7pe_top + c7pe_h
c7pe_x <- function(t) c7pe_x0 + pmin(t, c7pe_tmax) / c7pe_tmax * c7pe_w
c7pe_yr <- function(r) c7pe_base - r * c7pe_h   # udział działających → piksel

# Linie parku (t rosnąco) w wierszach wypełniających wysokość; dim: wektor logiczny.
c7pe_park <- function(t, cls, dim = rep(FALSE, length(t)), x_of = c7pe_x) {
  n <- length(t); y <- c7pe_top + (seq_len(n) - 0.5) * c7pe_h / n
  one <- function(idx, extra) if (!any(idx)) "" else sprintf('<path class="c7p-life %s%s" style="stroke-width:%.2f" d="%s"/>',
    cls, extra, min(2, 0.72 * c7pe_h / n), paste(sprintf("M%.1f,%.1fH%.1f", x_of(0), y[idx], x_of(t[idx])), collapse = ""))
  paste0(one(dim, " c7p-life-dim"), one(!dim, ""))
}
c7pe_yaxis <- function() paste0(
  sprintf('<line class="c7p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', c7pe_x0 - 8, c7pe_x0 - 8, c7pe_yr(0), c7pe_yr(1)),
  paste(sprintf('<line class="c7p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="c7p-tick" style="text-anchor:end" x="%g" y="%g">%s</text>',
    c7pe_x0 - 13, c7pe_x0 - 8, c7pe_yr(c(0, .5, 1)), c7pe_yr(c(0, .5, 1)), c7pe_x0 - 17, c7pe_yr(c(0, .5, 1)) + 4, c("0", "0.5", "1")), collapse = ""),
  sprintf('<text class="c7p-subtle" x="%g" y="%g" font-size="12" transform="rotate(-90 %g %g)" text-anchor="middle">udział działających</text>',
    20, c7pe_yr(.5), 20, c7pe_yr(.5)))
c7pe_rcurve <- function(beta, eta, cls = "c7pe-curve", x_of = c7pe_x, to = c7pe_tmax, scale = 1)
  sprintf('<path class="%s" d="%s"/>', cls, c7p_path(function(t) scale * c7p_R(t, beta, eta), x_of, c7pe_yr, 0, to, n = 160))

# ============================================================================
# P9 — POJĘCIE „MTTF TO POLE POD R(t)”
# ============================================================================

c7p9_a <- c7p_park(100, 1, c7p_mttf); c7p9_b <- c7p_park(100, c7p_beta, c7p_eta)
c7p9_box <- function(m, label) paste0(
  sprintf('<rect class="c7pe-box" x="%.1f" y="%g" width="%.1f" height="%g"/>', c7pe_x(0), c7pe_yr(1), c7pe_x(m) - c7pe_x(0), c7pe_h),
  sprintf('<text class="c7p-mono" x="%.1f" y="%g" font-size="13">%s</text>', c7pe_x(m) + 8, c7pe_yr(1) + 16, label))

c7p9_svg <- function(step) {
  b <- step == 4
  t <- if (b) c7p9_b else c7p9_a
  aria <- c("Sto linii życia oferty A ułożonych od najkrótszej do najdłuższej.",
    "Obwiednia linii życia to krzywa niezawodności; po 1000 godzinach działa 51 ze 100.",
    "Prostokąt o podstawie 1500 godzin i wysokości 1 ma to samo pole co obszar pod krzywą niezawodności.",
    "Oferta B: inny kształt krzywej niezawodności, to samo pole 1507 godzin.")[step]
  c7p_svg(860, c7pe_base + 44, aria,
    c7pe_park(t, if (b) "c7p-life-b" else "c7p-life-a"),
    c7p_axis(c7pe_x, c7pe_base + 4, seq(0, 4000, 1000), "czas pracy (h)"),
    if (step >= 2) paste0(c7pe_yaxis(), c7pe_rcurve(if (b) c7p_beta else 1, if (b) c7p_eta else c7p_mttf)),
    if (step == 2) paste0(c7p_vmark(c7pe_x(1000), c7pe_top - 12, c7pe_base, "1000 h"),
      sprintf('<circle class="c7p-bad" cx="%.1f" cy="%.1f" r="5.5"/>', c7pe_x(1000), c7pe_yr(.51)),
      sprintf('<text class="c7p-mono" x="%.1f" y="%.1f" font-size="13">51 linii sięga dalej: R = 0.51</text>', c7pe_x(1000) + 12, c7pe_yr(.51) - 8)),
    if (step == 3) c7p9_box(1500, "1500 h × 1"),
    if (step == 4) c7p9_box(1507, "1507 h × 1"))
}

c7p9_frames <- list(
  list(name = "Sto linii życia",
    text = "Park stu wentylatorów oferty A z rozdziału 1: linia to czas pracy jednego egzemplarza, od najkrótszej do najdłuższej.",
    read = NULL),
  list(name = "Obwiednia",
    text = "W każdej chwili t liczymy linie, które sięgają dalej. Ich udział to R(t), a brzeg zamalowanego obszaru to krzywa niezawodności.",
    read = "R(1000) = 51 ze 100"),
  list(name = "Pole",
    text = "Zamalowany obszar to suma długości linii podzielona przez ich liczbę, czyli średni czas życia. Prostokąt 1500 h × 1 ma to samo pole.",
    read = "MTTF = pole pod R(t) = 1500 h"),
  list(name = "Oferta B",
    text = "Oferta B ma inną krzywą: mało krótkich linii, żadnej bardzo długiej. Pole wychodzi prawie to samo.",
    read = "pole pod R(t): A 1500 h · B 1507 h")
)

c7p_9_ui <- tagList(c7pe_css,
  c7p_frames_ui("P9", "pojęcie", "MTTF to pole pod R(t)", c7p9_frames, c7p9_svg))

# ============================================================================
# P11 — POJĘCIE „OCALALI WYGLĄDAJĄ JAK NOWI”
# ============================================================================

c7p11_n <- 200L
c7p11_s <- 1000   # wiek ocalałych (h)
c7p11_u <- 500    # dalszy czas pracy (h)
c7p11_park <- list(a = c7p_park(c7p11_n, 1, c7p_mttf), b = c7p_park(c7p11_n, c7p_beta, c7p_eta))
c7p11_cnt <- lapply(c7p11_park, function(t) c(
  new = sum(t > c7p11_u), alive = sum(t > c7p11_s), later = sum(t > c7p11_s + c7p11_u)))
c7p11_frac <- function(k) c7p_num(c7p11_cnt[[k]][["later"]] / c7p11_cnt[[k]][["alive"]], 2)

c7p11_svg <- function(step) {
  k <- if (step == 4) "b" else "a"
  cls <- paste0("c7p-life-", k)
  t <- c7p11_park[[k]]; cnt <- c7p11_cnt[[k]]
  pars <- if (k == "a") c(1, c7p_mttf) else c(c7p_beta, c7p_eta)
  axis <- function(label) c7p_axis(c7pe_x, c7pe_base + 4, seq(0, 4000, 1000), label)
  body <- if (step == 1) paste0(
    c7pe_park(t, cls, dim = t <= c7p11_u), axis("czas pracy od uruchomienia (h)"),
    c7p_vmark(c7pe_x(c7p11_u), c7pe_top - 12, c7pe_base, "500 h"))
  else if (step == 2) paste0(
    c7pe_park(t, cls, dim = t <= c7p11_s), axis("czas pracy od uruchomienia (h)"),
    c7p_vmark(c7pe_x(c7p11_s), c7pe_top - 12, c7pe_base, "1000 h: przegląd"))
  else {
    # Ocalali z zegarem od zera: te same linie skrócone o 1000 h, rozciągnięte na
    # całą wysokość, żeby kształt dało się porównać z nowym parkiem (kontur).
    rest <- t[t > c7p11_s] - c7p11_s
    paste0(c7pe_park(rest, cls, dim = rest <= c7p11_u), axis("dalszy czas pracy ocalałych (h)"),
      c7pe_rcurve(pars[1], pars[2], "c7pe-ghost"),
      c7p_vmark(c7pe_x(c7p11_u), c7pe_top - 12, c7pe_base, "+500 h"),
      sprintf('<text class="c7p-subtle" x="%g" y="%g" font-size="13" text-anchor="end">przerywana linia: brzeg nowego parku</text>', c7pe_x0 + c7pe_w, c7pe_top + 4))
  }
  aria <- c(sprintf("Nowy park 200 wentylatorów: po 500 godzinach działa %d.", cnt[["new"]]),
    sprintf("Po 1000 godzinach działa %d z 200 wentylatorów.", cnt[["alive"]]),
    sprintf("Ocalali z zegarem od zera: kolejne 500 godzin przetrwa %d ze %d; kształt jak w nowym parku.", cnt[["later"]], cnt[["alive"]]),
    sprintf("Park, który się zużywa: kolejne 500 godzin przetrwa %d ze %d ocalałych, mniej niż w nowym parku.", cnt[["later"]], cnt[["alive"]]))[step]
  c7p_svg(860, c7pe_base + 44, aria, body)
}

c7p11_frames <- local({
  a <- c7p11_cnt$a; b <- c7p11_cnt$b
  list(
    list(name = "Nowy park",
      text = "Dwieście nowych wentylatorów o stałym hazardzie (oferta A). Zapamiętaj kształt brzegu i udział tych, które przetrwają pierwsze 500 h.",
      read = sprintf("pierwsze 500 h przetrwa %d z 200 = %s", a[["new"]], c7p_num(a[["new"]] / 200, 2))),
    list(name = "Po 1000 h",
      text = "Mija 1000 h. Część parku zawiodła; zostają ci, którzy wciąż działają.",
      read = sprintf("po 1000 h działa %d z 200", a[["alive"]])),
    list(name = "Zegar od zera",
      text = "Bierzemy tylko ocalałych i liczymy im czas od nowa. Brzeg ma ten sam kształt co w nowym parku: wiek niczego nie zmienił.",
      read = sprintf("kolejne 500 h przetrwa %d ze %d = %s", a[["later"]], a[["alive"]], c7p11_frac("a"))),
    list(name = "Park, który się zużywa",
      text = "To samo dla oferty B, w której hazard rośnie. Ocaleli po 1000 h wypadają poniżej brzegu nowego parku: ten park pamięta swój wiek.",
      read = sprintf("kolejne 500 h przetrwa %d ze %d = %s; nowych: %d z 200 = %s", b[["later"]], b[["alive"]], c7p11_frac("b"),
        b[["new"]], c7p_num(b[["new"]] / 200, 2)))
  )
})

c7p_11_ui <- tagList(c7pe_css,
  c7p_frames_ui("P11", "pojęcie", "Ocalali wyglądają jak nowi", c7p11_frames, c7p11_svg))

# ============================================================================
# P17B — PIASKOWNICA „TRZY PARKI, JEDNA ŚREDNIA”
# ============================================================================

c7p17b_shapes <- c(0.7, 1, 2.5)
c7p17b_scales <- c7p_mttf / gamma(1 + 1 / c7p17b_shapes)   # 1185, 1500, 1691 h (przykład 7.8)

c7p_17b_ui <- tagList(c7pe_css,
  c7p_scene_ui("P17B", "piaskownica", "Trzy parki, jedna średnia", "parki",
    list(n = 100, tmax = 3600, models = list(
      list(beta = 0.7, eta = c7p17b_scales[1], cls = "c", name = "β = 0.7 · hazard maleje", short = "β = 0.7"),
      list(beta = 1, eta = c7p17b_scales[2], cls = "a", name = "β = 1 · hazard stały", short = "β = 1"),
      list(beta = 2.5, eta = c7p17b_scales[3], cls = "b", name = "β = 2.5 · hazard rośnie", short = "β = 2.5"))),
    tags$div(class = "c7p-nav", c7pe_slider("c7p17b_t", "Czas misji", 100, 3500, 1000, 50, suffix = " h")),
    c7p_out("stage"),
    tags$p(class = "c7p-read", c7p_out("read")),
    tags$p(class = "c7p-caption",
      "Każdy park ma sto wentylatorów i średni czas życia 1500 h, więc zamalowane pola są równe. Kreska misji odcina z każdego inną liczbę linii.")))
