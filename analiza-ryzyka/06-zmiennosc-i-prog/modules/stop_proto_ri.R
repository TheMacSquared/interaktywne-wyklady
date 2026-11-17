# PROTOTYP PUNKTU STOP (2026-11-17) — grupa ri: P18 ---------------------------------
#
#   z6p_18_ui — Przykłady „Obciążenie i wytrzymałość w praktyce” (P18): po scenie
#               liny dźwigu (P11) pięć innych sytuacji, w których losowe są obie strony:
#               co obciąża (L), co wytrzymuje (S), skąd zmienność każdej strony i co
#               jest tu awarią. Ostatni slajd zestawia je z liną na osi „na 1000”.
#               Kadry SVG policzone w R, bez JS. Model: L i S normalne, niezależne;
#               P(L > S) = Φ(−(μ_S − μ_L)/√(σ_L² + σ_S²)). Liczby fikcyjne.
# Wymaga: stop_proto_common.R (z6p_*).

# ============================================================================
# P18 — PRZYKŁADY „OBCIĄŻENIE I WYTRZYMAŁOŚĆ W PRAKTYCE”
# ============================================================================

z6p18_examples <- list(
  list(name = "Łożysko i smar", short = "łożysko i smar", unit = "°C", axis = "temperatura (°C)",
    l_name = "temperatura łożyska", s_name = "temperatura, którą znosi smar",
    ml = 82, sl = 3, ms = 95, ss = 4,
    text = "Dotąd próg 85°C był stały. W rzeczywistości granicą jest temperatura, od której smar traci lepkość, a ta różni się między partiami smaru i spada z jego wiekiem. Awaria: łożysko pracuje goręcej, niż znosi akurat ten smar."),
  list(name = "Regał w chłodni", short = "regał w chłodni", unit = "kg", axis = "masa (kg)",
    l_name = "masa palety", s_name = "nośność miejsca na regale",
    ml = 900, sl = 80, ms = 1200, ss = 100,
    text = "Masa palety bananów zależy od liczby kartonów i od tego, ile wilgoci nasiąknęły. Nośność miejsca na regale zależy od montażu, korozji i uderzeń wózka widłowego. Awaria: paleta cięższa, niż wytrzyma akurat ta belka."),
  list(name = "Wyłącznik silnika", short = "wyłącznik silnika", unit = "A", axis = "prąd (A)",
    l_name = "prąd przy rozruchu", s_name = "prąd zadziałania wyłącznika",
    ml = 40, sl = 4, ms = 50, ss = 3,
    text = "Przy rozruchu silnik wentylatora pobiera chwilowo duży prąd, zależny od napięcia w sieci i temperatury. Wyłącznik zadziała przy prądzie, który ma tolerancję producenta. Awaria to tu fałszywe wyłączenie: wentylator staje, choć nic się nie zepsuło."),
  list(name = "Dach magazynu zimą", short = "dach magazynu zimą", unit = "kN/m²", axis = "obciążenie dachu (kN/m²)",
    l_name = "śnieg w najgorszym dniu zimy", s_name = "nośność dachu",
    ml = 1.2, sl = 0.4, ms = 2.5, ss = 0.25,
    text = "Obciążenie śniegiem zależy od zimy, a nośność dachu od wykonania i zużycia konstrukcji. Awaria: najcięższy śnieg w sezonie przekracza nośność. W rzeczywistości rozkład śniegu ma cięższy prawy ogon niż normalny; do kształtu ogona wrócimy w rozdziale 6."),
  list(name = "Wał przeciwpowodziowy", short = "wał przeciwpowodziowy", unit = "m", axis = "wysokość wody i wału (m)",
    l_name = "najwyższy stan wody w roku", s_name = "wysokość korony wału",
    ml = 5.0, sl = 0.6, ms = 6.5, ss = 0.2,
    text = "Najwyższy stan rzeki zmienia się z roku na rok. Wysokość korony wału też nie jest stała: wał osiada i jest nierówny na długości. Awaria: woda przelewa się przez wał w danym roku.")
)

# Lina ze sceny P11 dochodzi do zestawienia.
z6p18_crane <- list(short = "lina dźwigu (scena)", ml = 85, sl = 8, ms = 95, ss = 7)

z6p18_p <- function(e) pnorm(0, e$ms - e$ml, sqrt(e$sl^2 + e$ss^2))
z6p18_per1000 <- function(p) {
  v <- 1000 * p
  if (v >= 10) format(round(v), big.mark = " ") else formatC(v, format = "f", digits = 1)
}
z6p18_fmt_p <- function(p) if (p >= 0.01) formatC(p, format = "f", digits = 3) else formatC(p, format = "f", digits = 4)

z6p18_css <- tags$style(HTML("
  .z6p18-l { stroke: var(--upwr-cat-bursztyn, #b06a12); stroke-width: 2.6; fill: none; }
  .z6p18-s { stroke: var(--upwr-cat-niebo, #3d7ea6); stroke-width: 2.6; fill: none; }
  .z6p18-lt { fill: var(--upwr-cat-bursztyn, #b06a12) !important; font-weight: 700; }
  .z6p18-st { fill: var(--upwr-cat-niebo, #3d7ea6) !important; font-weight: 700; }
  .z6p18-dot { fill: var(--upwr-accent); }
  .z6p18-big { font-family: var(--upwr-mono) !important; font-size: 26px; font-weight: 700; fill: var(--upwr-accent) !important; }
"))

# Ładne podziałki osi dla zakresu [lo, hi]: około 5 kresek.
z6p18_ticks <- function(lo, hi) {
  t <- pretty(c(lo, hi), n = 5)
  t[t >= lo & t <= hi]
}

# Slajd przykładu: dwie gęstości na wspólnej osi i prawa kolumna z opisem.
z6p18_slide <- function(e) {
  lo <- min(e$ml - 3.5 * e$sl, e$ms - 3.5 * e$ss)
  hi <- max(e$ml + 3.5 * e$sl, e$ms + 3.5 * e$ss)
  peak <- max(dnorm(0, 0, e$sl), dnorm(0, 0, e$ss))
  x_of <- function(v) 30 + (v - lo) / (hi - lo) * 440
  y_of <- function(d) 270 - d / peak * 200
  p <- z6p18_p(e)
  num <- function(v) format(v, decimal.mark = ".", trim = TRUE)
  paste0(
    sprintf('<path class="z6p18-l" d="%s"/>', z6p_norm_path(e$ml, e$sl, x_of, y_of, lo, hi)),
    sprintf('<path class="z6p18-s" d="%s"/>', z6p_norm_path(e$ms, e$ss, x_of, y_of, lo, hi)),
    sprintf('<text class="z6p18-lt" x="%g" y="%g" font-size="18" text-anchor="middle">L</text>',
      x_of(e$ml), y_of(dnorm(0, 0, e$sl)) - 10),
    sprintf('<text class="z6p18-st" x="%g" y="%g" font-size="18" text-anchor="middle">S</text>',
      x_of(e$ms), y_of(dnorm(0, 0, e$ss)) - 10),
    z6p_axis(x_of, 270, z6p18_ticks(lo, hi), label = e$axis,
      labels = vapply(z6p18_ticks(lo, hi), num, "")),
    # prawa kolumna
    sprintf('<text class="z6p18-lt" x="520" y="40" font-size="15">L · %s</text>', e$l_name),
    sprintf('<text class="z6p-mono" x="520" y="62" font-size="15">N(%s, %s) %s</text>', num(e$ml), num(e$sl), e$unit),
    sprintf('<text class="z6p18-st" x="520" y="100" font-size="15">S · %s</text>', e$s_name),
    sprintf('<text class="z6p-mono" x="520" y="122" font-size="15">N(%s, %s) %s</text>', num(e$ms), num(e$ss), e$unit),
    '<text x="520" y="168" font-size="15">awaria, gdy L &gt; S</text>',
    sprintf('<text class="z6p18-big" x="520" y="212">%s</text>', z6p18_fmt_p(p)),
    sprintf('<text class="z6p-mono z6p-subtle" x="520" y="236" font-size="13">około %s na 1000</text>', z6p18_per1000(p))
  )
}

# Ostatni slajd: wszystkie przykłady i lina na osi logarytmicznej „na 1000”.
z6p18_summary <- function() {
  rows <- c(list(z6p18_crane), lapply(z6p18_examples, function(e) e[c("short", "ml", "sl", "ms", "ss")]))
  ps <- vapply(rows, z6p18_p, 0)
  ord <- order(ps, decreasing = TRUE)
  x_of <- function(v) 300 + (log10(v) - log10(1)) / (log10(300) - log10(1)) * 440   # v: na 1000
  body <- vapply(seq_along(ord), function(k) {
    i <- ord[k]; y <- 40 + (k - 1) * 40; v <- 1000 * ps[i]
    paste0(
      sprintf('<line class="z6p-grid" x1="300" x2="740" y1="%g" y2="%g"/>', y, y),
      sprintf('<text x="0" y="%g" font-size="14">%s</text>', y + 5, rows[[i]]$short),
      sprintf('<circle class="z6p18-dot" cx="%g" cy="%g" r="7"/>', x_of(v), y),
      sprintf('<text class="z6p-mono" x="%g" y="%g" font-size="13">%s</text>', x_of(v) + 13, y + 5, z6p18_per1000(ps[i]))
    )
  }, "")
  y_end <- 40 + (length(rows) - 1) * 40 + 26
  paste0(paste(body, collapse = ""),
    z6p_axis(x_of, y_end, c(1, 10, 100), label = "awarie na 1000 (skala logarytmiczna)", labels = c("1", "10", "100")))
}

z6p18_n <- length(z6p18_examples)

z6p18_frames <- c(
  lapply(z6p18_examples, function(e) {
    p <- z6p18_p(e)
    list(name = e$name, text = e$text,
      read = sprintf("P(L > S) ≈ %s · około %s na 1000", z6p18_fmt_p(p), z6p18_per1000(p)))
  }),
  list(list(name = "Zestawienie",
    text = "Ten sam schemat w sześciu sytuacjach: awaria to chwila, w której obciążenie przegoni wytrzymałość. Lina ma średni zapas 10 kN, ale wahania obu stron są podobnej wielkości, więc co szóste podniesienie kończy się awarią. W pozostałych przykładach średni zapas jest duży w porównaniu z wahaniami i awarie są rzadkie.",
    read = NULL))
)

z6p18_svg <- function(step) {
  if (step > z6p18_n) {
    return(z6p_svg(800, 310, "Zestawienie: liczba awarii na 1000 dla liny i pięciu przykładów, na osi logarytmicznej.",
      z6p18_summary()))
  }
  e <- z6p18_examples[[step]]
  z6p_svg(800, 312,
    sprintf("%s: rozkład obciążenia N(%s, %s) i wytrzymałości N(%s, %s) %s na wspólnej osi; prawdopodobieństwo awarii %s.",
      e$name, e$ml, e$sl, e$ms, e$ss, e$unit, z6p18_fmt_p(z6p18_p(e))),
    z6p18_slide(e))
}

z6p_18_ui <- tagList(z6p18_css,
  z6p_frames_ui("P18", "przykłady", "Obciążenie i wytrzymałość w praktyce", z6p18_frames, z6p18_svg, proto = FALSE))
