# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa ri (część B i domknięcie) -----------
#
#   c7p_15_ui  — Scena „Trzy dzienniki awarii” (P15), 4 kadry policzone w R: trzy
#                typy urządzeń o MTTF 1500 h (modele z przykładu 7.8: β = 0.7, 1, 2.5),
#                po 200 egzemplarzy; kreski awarii → okna po 500 h → awarie na
#                działających → trzy kierunki.
#   c7p_18_ui  — Scena „Trzy zegary jednego wentylatora” (P18), 3 kadry: trzy
#                mechanizmy z przykładu 7.9 jako trzy zegary; psuje ten, który
#                zadzwoni pierwszy. Skala czasu: hazardy względne z przykładu
#                podzielone przez 1000 na godzinę (założenie sceny, bez liczb na obrazie).
#   c7p_23a_ui — Domknięcie „Karty katalogowe, wersja poprawiona” (P23A): statyczne.
#   c7p_23b_ui — Domknięcie „Pięć pytań o wentylator” (P23B): statyczne.
# Wymaga: stop_proto_common.R.

c7pi_css <- tags$style(HTML("
  .c7pi-rug { stroke-width: 1.3; fill: none; opacity: .8; }
  .c7pi-bar { stroke: none; }
  .c7pi-win { stroke: var(--upwr-rule); stroke-width: 1; stroke-dasharray: 3 3; }
  .c7pi-clock-1 { fill: var(--upwr-cat-terakota); }
  .c7pi-clock-2 { fill: var(--upwr-cat-niebo); }
  .c7pi-clock-3 { fill: var(--upwr-cat-bursztyn); }
  .c7pi-late { opacity: .3; }
  .c7pi-line-1 { stroke: var(--upwr-cat-terakota); }
  .c7pi-line-2 { stroke: var(--upwr-cat-niebo); }
  .c7pi-line-3 { stroke: var(--upwr-cat-bursztyn); }
  .c7pi-cards { display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 1em 1.4em; margin: .4em 0; }
  .c7pi-card { border: 1px solid var(--upwr-rule); border-top: 6px solid var(--c7pi-color); background: var(--upwr-surface); padding: .8em 1em 1em; }
  .c7pi-card h4 { margin: 0 0 .1em; font-size: 1.1em; }
  .c7pi-kicker { font-size: .72em; font-weight: 700; letter-spacing: .08em; color: var(--upwr-ink-subtle); margin: 0; }
  .c7pi-old { color: var(--upwr-ink-subtle); margin: 0 0 .6em; }
  .c7pi-old s { text-decoration-thickness: 1.5px; }
  .c7pi-rows { display: grid; grid-template-columns: 1fr auto; gap: .25em 1em; margin: 0; }
  .c7pi-rows dt { color: var(--upwr-ink-soft); }
  .c7pi-rows dd { margin: 0; font-family: var(--upwr-mono); font-weight: 600; text-align: right; }
  .c7pi-src { font-family: var(--upwr-sans); font-weight: 400; font-size: .8em; color: var(--upwr-ink-subtle); margin-left: .5em; }
  .c7pi-verdict { margin: .7em 0 0; padding-top: .6em; border-top: 1px solid var(--upwr-rule); }
  .c7pi-q { display: grid; grid-template-columns: 2.2em minmax(0, 1fr) minmax(0, 1.5fr); gap: .5em 1em; align-items: baseline;
    padding: .55em 0; border-bottom: 1px solid var(--upwr-rule); }
  .c7pi-q-num { font-family: var(--upwr-mono); font-weight: 700; color: var(--upwr-accent); }
  .c7pi-q-ask { font-weight: 600; }
  @media (max-width: 640px) { .c7pi-q { grid-template-columns: 2.2em 1fr; } .c7pi-q-ans { grid-column: 2; } }
"))

# ============================================================================
# P15 — SCENA „TRZY DZIENNIKI AWARII”
# ============================================================================

c7p15_n <- 200L
c7p15_shapes <- c(0.7, 1, 2.5)
c7p15_scales <- c7p_mttf / gamma(1 + 1 / c7p15_shapes)
c7p15_times <- lapply(1:3, function(j) c7p_park(c7p15_n, c7p15_shapes[j], c7p15_scales[j]))
c7p15_win <- seq(0, 3000, 500)
c7p15_alive <- lapply(c7p15_times, function(t) vapply(head(c7p15_win, -1), function(a) sum(t > a), integer(1)))
c7p15_fail <- lapply(c7p15_times, function(t) vapply(1:6, function(i) sum(t > c7p15_win[i] & t <= c7p15_win[i + 1]), integer(1)))
c7p15_rate <- Map(function(f, a) f / a, c7p15_fail, c7p15_alive)
c7p15_cls <- c("c", "a", "b")

c7p15_x <- function(t) 120 + t / 3000 * 650
c7p15_top <- c(20, 150, 280)

c7p15_block <- function(j, step) {
  t <- c7p15_times[[j]]; y0 <- c7p15_top[j]; ok <- t <= 3000
  mid <- (head(c7p15_win, -1) + c7p15_win[-1]) / 2
  bar_base <- y0 + 104; bar_h <- 44
  name <- if (step == 4) c("ryzyko maleje", "ryzyko stoi", "ryzyko rośnie")[j] else ""
  paste0(
    sprintf('<text x="0" y="%g" font-size="15" font-weight="700">Dziennik %d</text>', y0 + 22, j),
    if (step == 4) sprintf('<text class="c7p-fill-%s" x="0" y="%g" font-size="13" font-weight="700">%s</text>', c7p15_cls[j], y0 + 42, name) else "",
    if (step == 4) sprintf('<text class="c7p-subtle" x="0" y="%g" font-size="12">%s</text>', y0 + 59,
      c("wczesne defekty", "awarie losowe", "zużycie")[j]) else "",
    sprintf('<rect class="c7p-paper" x="%g" y="%g" width="650" height="36" rx="3"/>', c7p15_x(0), y0),
    sprintf('<path class="c7pi-rug c7p-life-%s" d="%s"/>', c7p15_cls[j],
      paste(sprintf("M%.1f,%gv28", c7p15_x(t[ok]), y0 + 4), collapse = "")),
    sprintf('<text class="c7p-mono c7p-subtle" x="%g" y="%g" font-size="11">+%d później</text>', c7p15_x(3000) + 6, y0 + 22, sum(!ok)),
    if (step >= 2) paste0(
      paste(sprintf('<line class="c7pi-win" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>', c7p15_x(c7p15_win), c7p15_x(c7p15_win), y0 - 4, y0 + if (step >= 3) 104 else 58), collapse = ""),
      if (step == 2) paste(sprintf('<text class="c7p-mono" x="%.1f" y="%g" font-size="13" text-anchor="middle">%d</text>',
        c7p15_x(mid), y0 + 56, c7p15_fail[[j]]), collapse = "") else "") else "",
    if (step >= 3) paste0(
      paste(sprintf('<rect class="c7pi-bar c7p-fill-%s" x="%.1f" y="%.1f" width="%.1f" height="%.1f"/>', c7p15_cls[j],
        c7p15_x(mid) - 32, bar_base - c7p15_rate[[j]] * bar_h, 64, c7p15_rate[[j]] * bar_h), collapse = ""),
      paste(sprintf('<text class="c7p-mono" x="%.1f" y="%.1f" font-size="12" text-anchor="middle">%d z %d</text>',
        c7p15_x(mid), bar_base - c7p15_rate[[j]] * bar_h - 5, c7p15_fail[[j]], c7p15_alive[[j]]), collapse = ""),
      sprintf('<line class="c7p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', c7p15_x(0), c7p15_x(3000), bar_base, bar_base)) else ""
  )
}

c7p15_svg <- function(step) {
  aria <- c("Trzy dzienniki awarii: kreski awarii na osi czasu dla trzech typów urządzeń.",
    "Te same dzienniki podzielone na okna po 500 godzin z liczbą awarii w każdym oknie.",
    "Pod każdym dziennikiem słupki: awarie w oknie podzielone przez liczbę egzemplarzy działających na początku okna.",
    "Trzy kierunki: w pierwszym dzienniku ryzyko maleje, w drugim stoi, w trzecim rośnie.")[step]
  c7p_svg(860, 432, aria,
    paste(vapply(1:3, c7p15_block, character(1), step = step), collapse = ""),
    c7p_axis(c7p15_x, 392, c7p15_win, "czas pracy (h)"))
}

c7p15_read <- function(j) paste(c7p_num(c7p15_rate[[j]][c(1, 6)], 2), collapse = " → ")
c7p15_frames <- list(
  list(name = "Trzy dzienniki",
    text = "Utrzymanie ruchu prowadzi dzienniki awarii trzech typów urządzeń, po 200 egzemplarzy. Kreska to awaria jednego egzemplarza. Średni czas życia jest wszędzie ten sam: 1500 h.",
    read = NULL),
  list(name = "Okna po 500 h",
    text = "Liczymy awarie w kolejnych oknach po 500 h. W każdym dzienniku liczby maleją, bo z czasem ubywa egzemplarzy, które mogłyby zawieść.",
    read = NULL),
  list(name = "Na ilu działających",
    text = "Dlatego awarie w oknie dzielimy przez liczbę egzemplarzy, które dożyły początku okna. To ryzyko okna dla urządzenia, które wciąż działa.",
    read = NULL),
  list(name = "Trzy kierunki",
    text = "Ta sama średnia, a ryzyko dla działającego egzemplarza zachowuje się na trzy sposoby. O tym, który dziennik prowadzimy, decyduje mechanizm awarii.",
    read = sprintf("pierwsze → szóste okno: %s · %s · %s", c7p15_read(1), c7p15_read(2), c7p15_read(3)))
)

c7p_15_ui <- tagList(c7pi_css,
  c7p_frames_ui("P15", "scena", "Trzy dzienniki awarii", c7p15_frames, c7p15_svg))

# ============================================================================
# P18 — SCENA „TRZY ZEGARY JEDNEGO WENTYLATORA”
# ============================================================================

# Trzy zegary z hazardów przykładu 7.9 (w / 1000 na godzinę): lut — H₁(t) =
# 0.42 · (1 − e^(−t/350)), więc zegar dzwoni tylko u egzemplarzy z wadą;
# przepięcie — stały hazard 0.00012/h; łożysko — H₃(t) = t⁴ / (4 · 4000³ · 1000).
# Ziarno 258 wybrane tak, żeby wśród 12 wentylatorów było 4, 3 i 5 awarii
# kolejnych mechanizmów, uporządkowanych w czasie.
c7p18_n <- 12L
c7p18_m <- local({
  set.seed(258)
  e1 <- rexp(c7p18_n)
  t1 <- rep(Inf, c7p18_n); has <- e1 < 0.42
  t1[has] <- -350 * log(1 - e1[has] / 0.42)
  t2 <- rexp(c7p18_n, 0.00012)
  t3 <- (4 * 4000^3 * 1000 * rexp(c7p18_n))^0.25
  cbind(t1, t2, t3)
})
c7p18_life <- apply(c7p18_m, 1, min)
c7p18_who <- apply(c7p18_m, 1, which.min)
c7p18_tmax <- 5000
c7p18_x <- function(t) 70 + pmin(t, c7p18_tmax) / c7p18_tmax * 740
c7p18_names <- c("wadliwy lut", "przepięcie", "łożysko")

# Znacznik zegara: kwadrat (lut), koło (przepięcie), trójkąt (łożysko).
c7p18_mark <- function(k, x, y, late = FALSE, s = 7) {
  cls <- sprintf("c7pi-clock-%d%s", k, ifelse(late, " c7pi-late", ""))
  switch(k,
    sprintf('<rect class="%s" x="%.1f" y="%.1f" width="%g" height="%g"/>', cls, x - s, y - s, 2 * s, 2 * s),
    sprintf('<circle class="%s" cx="%.1f" cy="%.1f" r="%g"/>', cls, x, y, s + 0.5),
    sprintf('<path class="%s" d="M%.1f,%.1fl%g,%gh%gz"/>', cls, x, y - s - 1, s + 1, 2 * s + 1, -2 * (s + 1)))
}
c7p18_legend <- function(y) paste(vapply(1:3, function(k) paste0(
  c7p18_mark(k, 90 + (k - 1) * 200, y, s = 6),
  sprintf('<text x="%g" y="%g" font-size="13">%s</text>', 104 + (k - 1) * 200, y + 5, c7p18_names[k])), character(1)), collapse = "")

c7p18_row <- function(i, y, colored = FALSE, label = NULL) {
  m <- c7p18_m[i, ]; life <- c7p18_life[i]; who <- c7p18_who[i]
  paste0(
    if (!is.null(label)) sprintf('<text class="c7p-tick" style="text-anchor:end" x="56" y="%g">%s</text>', y + 4, label) else "",
    sprintf('<line class="c7p-life %s" style="stroke-width:4" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
      if (colored) paste0("c7pi-line-", who) else "", c7p18_x(0), c7p18_x(life), y, y),
    paste(vapply(1:3, function(k) if (m[k] > c7p18_tmax) "" else
      paste(c7p18_mark(k, c7p18_x(m[k]), y, late = k != who), collapse = ""), character(1)), collapse = ""))
}

c7p18_one <- which(c7p18_who == 1 & c7p18_m[, 2] < c7p18_tmax & c7p18_m[, 3] < c7p18_tmax)[1]
if (is.na(c7p18_one)) c7p18_one <- which(c7p18_who == 2 & c7p18_m[, 3] < c7p18_tmax)[1]

c7p18_svg <- function(step) {
  if (step == 1) {
    i <- c7p18_one
    return(c7p_svg(860, 170, "Jeden wentylator z trzema zegarami na osi czasu; linia życia kończy się na pierwszym z nich.",
      c7p18_legend(22), c7p18_row(i, 78),
      sprintf('<text class="c7p-mono" x="%.1f" y="60" font-size="13" text-anchor="middle">awaria: %s</text>',
        max(c7p18_x(c7p18_life[i]), 150), c7p18_names[c7p18_who[i]]),
      c7p_axis(c7p18_x, 118, seq(0, 5000, 1000), "czas pracy (h)")))
  }
  idx <- if (step == 3) order(c7p18_life) else seq_len(c7p18_n)
  y <- 54 + (seq_len(c7p18_n) - 1) * 22
  aria <- c("", "Dwanaście wentylatorów, każdy z trzema zegarami; linia życia kończy się na pierwszym.",
    sprintf("Wentylatory uporządkowane według czasu życia: %d zepsuł lut, %d przepięcie, %d łożysko.",
      sum(c7p18_who == 1), sum(c7p18_who == 2), sum(c7p18_who == 3)))[step]
  c7p_svg(860, 368, aria, c7p18_legend(22),
    paste(vapply(seq_along(idx), function(r) c7p18_row(idx[r], y[r], colored = step == 3, label = r), character(1)), collapse = ""),
    c7p_axis(c7p18_x, 322, seq(0, 5000, 1000), "czas pracy (h)"))
}

c7p18_frames <- list(
  list(name = "Trzy zegary",
    text = "Wentylator może zawieść z trzech powodów. Każdy powód ma własny zegar; wentylator staje, gdy zadzwoni pierwszy z nich. Pozostałe już się nie liczą.",
    read = "czas życia = najkrótszy z trzech czasów"),
  list(name = "Dwanaście wentylatorów",
    text = "Zegar lutu dzwoni tylko w egzemplarzach z wadą i zawsze wcześnie. Przepięcie może przyjść w dowolnej chwili. Zegar łożyska dzwoni późno, ale u każdego.",
    read = NULL),
  list(name = "Kto zadzwonił pierwszy",
    text = "Te same wentylatory od najkrócej żyjącego, w kolorze mechanizmu, który je zatrzymał. Początek należy do lutu, środek do przepięć, koniec do łożysk.",
    read = sprintf("lut: %d · przepięcie: %d · łożysko: %d", sum(c7p18_who == 1), sum(c7p18_who == 2), sum(c7p18_who == 3)))
)

c7p_18_ui <- tagList(c7pi_css,
  c7p_frames_ui("P18", "scena", "Trzy zegary jednego wentylatora", c7p18_frames, c7p18_svg))

# ============================================================================
# P23A — DOMKNIĘCIE „KARTY KATALOGOWE, WERSJA POPRAWIONA”
# ============================================================================

c7p23_p <- function(x) formatC(x, format = "f", digits = 3)
c7p23_row <- function(label, value, src = NULL) tagList(tags$dt(label),
  tags$dd(value, if (!is.null(src)) tags$span(class = "c7pi-src", src)))

c7p23_card <- function(name, color, mttf, beta, eta, hazard, hazard_src, verdict) {
  used <- c7p_R(1500, beta, eta) / c7p_R(1000, beta, eta)
  tags$section(class = "c7pi-card", style = paste0("--c7pi-color:", color),
    tags$p(class = "c7pi-kicker", "KARTA KATALOGOWA · WERSJA POPRAWIONA"),
    tags$h4(name),
    tags$p(class = "c7pi-old", tags$s(paste0("MTTF ", mttf, " h")), " — sama średnia nie wystarcza"),
    tags$dl(class = "c7pi-rows",
      c7p23_row("Hazard", hazard, hazard_src),
      c7p23_row("R(1000 h)", c7p23_p(c7p_R(1000, beta, eta)), "(7.12)"),
      c7p23_row("R(3000 h)", c7p23_p(c7p_R(3000, beta, eta)), "(7.12)"),
      c7p23_row("B10", paste(round(eta * (-log(0.9))^(1 / beta)), "h"), "(7.16)"),
      c7p23_row("Używany (1000 h): +500 h", c7p23_p(used), "(7.17)"),
      c7p23_row("Nowy: +500 h", c7p23_p(c7p_R(500, beta, eta)), "(7.12)")),
    tags$p(class = "c7pi-verdict", tags$strong("Wymiana profilaktyczna:"), " ", verdict))
}

c7p_23a_ui <- c7p_panel("P23A", "domknięcie", "Karty katalogowe, wersja poprawiona", c7pi_css,
  tags$div(class = "c7pi-cards",
    c7p23_card("Wentylator A", "var(--upwr-cat-niebo)", "1500", 1, c7p_mttf, "stały", "(7.8)",
      "nic nie zmienia; lepiej wydać na redundancję."),
    c7p23_card("Wentylator B", "var(--upwr-cat-bursztyn)", "1507", c7p_beta, c7p_eta, "rośnie", "(7.13)",
      "pomaga; termin przed B10.")),
  lc_caption("Dwie karty z rozdziału 1. Średnie są prawie równe, a każda z pozostałych liczb jest inna; o wyborze decyduje czas misji i mechanizm awarii."))

# ============================================================================
# P23B — DOMKNIĘCIE „PIĘĆ PYTAŃ O WENTYLATOR”
# ============================================================================

c7p23_q <- function(num, ask, ...) tags$div(class = "c7pi-q",
  tags$span(class = "c7pi-q-num", num), tags$span(class = "c7pi-q-ask", ask), tags$span(class = "c7pi-q-ans", ...))

c7p_23b_ui <- c7p_panel("P23B", "domknięcie", "Pięć pytań o wentylator Bananpolu", c7pi_css,
  tags$div(
    c7p23_q("1", "Co rozpoczyna i kończy czas życia?",
      "Montaż wentylatora i pierwsza awaria; zegar liczy godziny pracy (definicja 7.1)."),
    c7p23_q("2", "Jaki jest wspólny czas misji?",
      "1000 h do planowego przeglądu: R(1000 h) = 0.513 dla oferty A i 0.707 dla oferty B."),
    c7p23_q("3", "Czy obserwacje działające są cenzorowane?",
      "Tak: po 1200 h działały 4 z 8. Szacunek z ich godzinami pracy to 1812.5 h, średnia z samych awarii 612.5 h (7.2)."),
    c7p23_q("4", "Czy hazard jest stały, rośnie czy maleje?",
      "Oferta A: stały, bez pamięci (7.9). Oferta B: rośnie, β = 2 (7.13). Pełna wanna to suma trzech mechanizmów (7.15)."),
    c7p23_q("5", "Jak wynik zmienia decyzję utrzymaniową?",
      "Oferta B: przegląd z wymianą przed 552 h utrzymuje R ≥ 0.90 (7.16). Oferta A: wymiana sprawnego egzemplarza niczego nie zmienia (7.17).")),
  width_mode = "text")
