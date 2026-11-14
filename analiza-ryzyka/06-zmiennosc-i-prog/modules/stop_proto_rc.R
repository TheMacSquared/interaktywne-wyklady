# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rc: P6, P7, P8, P8b ------------------
#
#   z6p_6_ui  — Pojęcie „Temperatura i ciśnienie na jednej linijce” (P6): cztery kadry
#               SVG policzone w R (linijka σ, wspólna oś z, pola ogonów).
#   z6p_7_ui  — Piaskownica „Próg temperatury łożyska” (P7): rycina lcSF z6_prog
#               (modules/proto_rc.js), stały model N(82, 3), suwak progu c.
#   z6p_8_ui  — Scena „Dzienniki zmian” (P8): losowe zmiany po 8 pomiarów, scena
#               zmiany w modules/proto_rc.js (window.lcZ6); R podaje config.
#   z6p_8b_ui — Przypadek brzegowy „Te same pomiary, inne zmiany” (P8b): statyczny
#               rysunek z R, dwie siatki (niezależne i skorelowane), bez JS.
# Wymaga: stop_proto_common.R (z6p_*), R/lc_stat_figures.R (lc_sf_figure, lc_sf_slider).
# Zmienne w tym pliku mają prefiks z6p6_, z6p7_, z6p8_, z6p8b_ (pomocnicze lokalnie).

# ============================================================================
# P6 — POJĘCIE „TEMPERATURA I CIŚNIENIE NA JEDNEJ LINIJCE”
# ============================================================================

z6p6_css <- tags$style(HTML("
  .z6p6-area-strong { fill: color-mix(in srgb, var(--upwr-accent) 85%, transparent); stroke: none; }
  .z6p6-name { font-size: 17px; font-weight: 700; fill: var(--upwr-ink); }
  .z6p6-label { font-size: 14px; fill: var(--upwr-ink); }
  .z6p6-mark { fill: var(--upwr-accent); stroke: var(--upwr-surface); stroke-width: 2; }
"))

z6p6_H <- 372          # wysokość viewBox wszystkich kadrów
z6p6_x0 <- 260         # lewa krawędź osi (po lewej nazwy rzędów)
z6p6_x1 <- 740         # prawa krawędź osi; μ ± 4σ zajmuje całą szerokość w obu rzędach
z6p6_k <- 90 * sqrt(2 * pi)  # wysokość krzywej: szczyt 90 px dla każdego σ (kształt ten sam)

# Dwa rzędy: temperatura (base 120) i ciśnienie (base 300). Osie: μ ± 4σ = 70–94°C
# i 48–64 bar, czyli ta sama szerokość w pikselach.
z6p6_rows <- list(
  temp = list(name = "Temperatura łożyska", par = "N(82, 3) °C", mu = 82, sd = 3, thr = 85,
    thr_label = "próg 85°C", base = 120, ticks = seq(70, 94, by = 3)),
  press = list(name = "Ciśnienie w instalacji", par = "N(56, 2) bar", mu = 56, sd = 2, thr = 62,
    thr_label = "próg 62 bar", base = 300, ticks = seq(48, 64, by = 2))
)

z6p6_xof <- function(r) function(v) z6p6_x0 + (v - (r$mu - 4 * r$sd)) / (8 * r$sd) * (z6p6_x1 - z6p6_x0)
z6p6_yof <- function(r) function(d) r$base - d * r$sd * z6p6_k

# Jeden rząd: nazwa, krzywa N(μ, σ) na μ ± 4σ, oś, próg (przerywana kreska), opcjonalnie linijka σ.
z6p6_row <- function(r, ruler = FALSE) {
  x_of <- z6p6_xof(r); y_of <- z6p6_yof(r)
  yb <- r$base; xt <- x_of(r$thr)
  paste0(
    sprintf('<text class="z6p6-name" x="20" y="%g">%s</text>', yb - 44, r$name),
    sprintf('<text class="z6p-mono z6p-subtle" x="20" y="%g" font-size="14">%s</text>', yb - 22, r$par),
    sprintf('<path class="z6p-curve" d="%s"/>', z6p_norm_path(r$mu, r$sd, x_of, y_of, r$mu - 4 * r$sd, r$mu + 4 * r$sd)),
    z6p_axis(x_of, yb, r$ticks),
    sprintf('<line class="z6p-limit" x1="%g" x2="%g" y1="%g" y2="%g"/>', xt, xt, yb - 100, yb),
    sprintf('<text class="z6p6-label" x="%g" y="%g">%s</text>', xt + 6, yb - 96, r$thr_label),
    if (ruler) paste0(
      z6p_sigma_ruler(x_of, yb + 40, r$mu, r$sd),
      sprintf('<circle class="z6p6-mark" cx="%g" cy="%g" r="5"/>', xt, yb + 40))
  )
}

# Standaryzowana oś z: z = −4 … 4, x = 120 + (z + 4) · 70, rysunek na tej samej osi.
z6p6_x3 <- function(z) 120 + (z + 4) * 70
z6p6_y3 <- function(d) 300 - d * 220 / 0.3989423

z6p6_axis3 <- function() z6p_axis(z6p6_x3, 300, -4:4, label = "wynik z (odchylenia σ)",
  labels = c("−4", "−3", "−2", "−1", "0", "1", "2", "3", "4"))

z6p6_frame3 <- function() paste0(
  sprintf('<path class="z6p-area" d="%s"/>', z6p_norm_path(0, 1, z6p6_x3, z6p6_y3, 1, 4, close = TRUE)),
  sprintf('<path class="z6p6-area-strong" d="%s"/>', z6p_norm_path(0, 1, z6p6_x3, z6p6_y3, 3, 4, close = TRUE)),
  sprintf('<path class="z6p-curve" d="%s"/>', z6p_norm_path(0, 1, z6p6_x3, z6p6_y3, -4, 4)),
  z6p6_axis3(),
  sprintf('<line class="z6p-limit" x1="%g" x2="%g" y1="300" y2="60"/>', z6p6_x3(1), z6p6_x3(1)),
  sprintf('<line class="z6p-limit" x1="%g" x2="%g" y1="300" y2="60"/>', z6p6_x3(3), z6p6_x3(3)),
  sprintf('<text class="z6p6-label" x="%g" y="50" text-anchor="end">temperatura · z = 1</text>', z6p6_x3(1) - 6),
  sprintf('<text class="z6p6-label" x="%g" y="50">ciśnienie · z = 3</text>', z6p6_x3(3) + 6)
)

z6p6_frame4 <- function() paste0(
  z6p6_frame3(),
  sprintf('<text class="z6p-mono" x="500" y="186" font-size="18" font-weight="700">0.159</text>'),
  sprintf('<text class="z6p-mono" x="500" y="206" font-size="14">159 na 1000</text>'),
  sprintf('<line class="z6p-axis" x1="684" x2="%g" y1="112" y2="%g"/>', z6p6_x3(3.4), z6p6_y3(0.0012)),
  sprintf('<text class="z6p-mono" x="690" y="96" font-size="18" font-weight="700">0.00135</text>'),
  sprintf('<text class="z6p-mono" x="690" y="118" font-size="14">1.3 na 1000</text>')
)

z6p6_svg <- function(step) {
  aria <- c(
    "Dwie krzywe normalne na osiach temperatury i ciśnienia, każda o tej samej szerokości; próg 85 stopni i próg 62 bar.",
    "Te same dwie krzywe z linijką odchyleń standardowych: próg temperatury w plus jednym sigma, próg ciśnienia w plus trzech sigma.",
    "Jedna krzywa standardowa z progami w z równym 1 i z równym 3, pole na prawo od progów.",
    "Pola ogonów: około 0.159 przy z równym 1 i 0.00135 przy z równym 3.")[step]
  switch(step,
    z6p_svg(800, z6p6_H, aria,
      z6p6_row(z6p6_rows$temp), z6p6_row(z6p6_rows$press)),
    z6p_svg(800, z6p6_H, aria,
      z6p6_row(z6p6_rows$temp, ruler = TRUE), z6p6_row(z6p6_rows$press, ruler = TRUE)),
    z6p_svg(800, z6p6_H, aria, z6p6_frame3()),
    z6p_svg(800, z6p6_H, aria, z6p6_frame4())
  )
}

z6p6_frames <- list(
  list(name = "Dwa czujniki",
    text = "Każdy czujnik ma własny rozkład i własny próg. Zapas 3°C i zapas 6 bar nie są porównywalne, dopóki nie zmierzymy ich w odchyleniach.",
    read = "zapas: 3°C · 6 bar"),
  list(name = "Linijka σ",
    text = "Linijka pod osią mierzy odległość od średniej w odchyleniach standardowych. Próg temperatury wypada w +1σ, próg ciśnienia w +3σ.",
    read = "z = (85 − 82)/3 = 1 · z = (62 − 56)/2 = 3"),
  list(name = "Wspólna oś z",
    text = "Po standaryzacji obie krzywe są jedną krzywą N(0, 1). Progi leżą w z = 1 i z = 3.",
    read = NULL),
  list(name = "Pola",
    text = "Pole na prawo od progu to prawdopodobieństwo przekroczenia. Oba pola leżą na tej samej osi, więc można je porównać.",
    read = "przekroczenie temperatury jest około 118 razy częstsze")
)

z6p_6_ui <- tagList(z6p6_css,
  z6p_frames_ui("P6", "pojęcie", "Temperatura i ciśnienie na jednej linijce", z6p6_frames, z6p6_svg))

# ============================================================================
# P7 — PIASKOWNICA „PRÓG TEMPERATURY ŁOŻYSKA”
# ============================================================================

z6p_7_ui <- z6p_panel("P7", "piaskownica", "Próg temperatury łożyska",
  lc_sf_figure("z6_prog",
    control = list(lc_sf_slider("z6p7_c", "Próg c", 78, 95, 85, 0.5, suffix = "°C")),
    toolbar = lc_readouts(z6p_read("z", "z progu"), z6p_read("p", "P(T > c)", upwr_accent),
      z6p_read("f", "Częstość"), z6p_read("d", "Następne +0.5°C zabiera")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    config = list(mu = z6p_mu, sd = z6p_sd),
    after = lc_caption("Porównaj dwa kroki: z 82 do 82.5°C oraz z 91 do 91.5°C. Ten sam krok o pół stopnia zmienia prawdopodobieństwo wyraźnie w środku rozkładu, a w ogonie prawie wcale.")))

# ============================================================================
# P8 — SCENA „DZIENNIKI ZMIAN” (historia z powtórzeniem)
# ============================================================================

# Config sceny: liczby policzone w R. p_shift_cor symulowany dla 200 000 zmian AR(1)
# ze stałym ziarnem; seed = 7 daje pierwszą zmianę z dokładnie jedną czerwoną kratką.
z6p8_config <- local({
  n <- 8L; rho <- 0.8
  p <- pnorm(z6p_c, z6p_mu, z6p_sd, lower.tail = FALSE)
  set.seed(2026)
  m <- 200000L
  zz <- matrix(0, m, n)
  zz[, 1] <- rnorm(m)
  for (j in 2:n) zz[, j] <- rho * zz[, j - 1] + sqrt(1 - rho^2) * rnorm(m)
  list(n = n, mu = z6p_mu, sd = z6p_sd, c = z6p_c, rho = rho, p = p,
    p_shift_ind = 1 - (1 - p)^n,
    p_shift_cor = mean(rowSums(zz > (z6p_c - z6p_mu) / z6p_sd) > 0),
    seed = 7L)
})

z6p8_css <- tags$style(HTML("
  .z6p8-track { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1; }
  .z6p8-fill { fill: var(--upwr-accent); }
  .z6p8-empty { fill: none; stroke: var(--upwr-ink-subtle); stroke-width: 1.4; }
  .z6p8-pair { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 1em; }
"))

z6p_8_ui <- tagList(z6p8_css,
  z6p_scene_ui("P8", "scena", "Dzienniki zmian", "zmiany", z6p8_config,
    z6p_out("read"),
    z6p_out("grid"),
    z6p_out("bars"),
    tags$div(class = "z6p-nav",
      z6p_button("one", "Następna zmiana", variant = "solid"),
      z6p_button("m40", "+40"),
      z6p_button("m1000", "+1000"),
      z6p_button("reset", "Od nowa")),
    tags$div(class = "z6p-nav",
      z6p_seg("mode", "Pomiary w zmianie", c("niezależne" = "ind", "skorelowane" = "cor")),
      z6p_seg("model", "Model", c("ukryty" = "off", "pokazany" = "on"))),
    tags$p(class = "z6p-caption",
      "Kratka to pomiar, wiersz to zmiana: czerwona kratka to pomiar ponad 85°C. Policz osobno czerwone kratki i wiersze z co najmniej jedną czerwoną.")))

# ============================================================================
# P8b — PRZYPADEK BRZEGOWY „TE SAME POMIARY, INNE ZMIANY” (statyczny)
# ============================================================================

z6p8b_rho <- 0.8
# Ziarna znalezione jednorazowo pętlą po ziarnach (flags() poniżej, 40 zmian × 8 pomiarów):
# lewa siatka (niezależne): ziarno 104 — 51 czerwonych kratek z 320, 30 z 40 zmian z alarmem;
# prawa siatka (skorelowane, ρ = 0.8): ziarno 42 — 51 z 320, 18 z 40.
z6p8b_seed_ind <- 104L
z6p8b_seed_cor <- 42L

# Macierz 40 × 8 logiczna: TRUE = pomiar ponad 85°C (z > 1 w jednostkach σ).
z6p8b_flags <- function(seed, cor) {
  set.seed(seed)
  z <- matrix(0, 40, 8)
  if (!cor) z[] <- rnorm(320) else {
    z[, 1] <- rnorm(40)
    for (j in 2:8) z[, j] <- z6p8b_rho * z[, j - 1] + sqrt(1 - z6p8b_rho^2) * rnorm(40)
  }
  z > (z6p_c - z6p_mu) / z6p_sd
}

# Siatka 2 kolumny × 20 wierszy, jak w P8 (kratka = pomiar, znacznik = alarm).
z6p8b_svg <- function(flags, title) {
  rows <- vapply(seq_len(nrow(flags)), function(i) {
    x0 <- ((i - 1) %/% 20) * 200; y <- 44 + ((i - 1) %% 20) * 21
    paste0(
      sprintf('<text class="z6p-mono z6p-subtle" x="%g" y="%g" font-size="12">%d</text>', x0, y + 4, i),
      paste(vapply(seq_len(8), function(j) sprintf('<rect class="%s" x="%g" y="%g" width="13" height="13" rx="2"/>',
        if (flags[i, j]) "z6p-cell-bad" else "z6p-cell-ok", x0 + 34 + (j - 1) * 16, y - 6.5), character(1)), collapse = ""),
      if (any(flags[i, ])) sprintf('<circle class="z6p-bad" cx="%g" cy="%g" r="6"/>', x0 + 178, y)
      else sprintf('<circle class="z6p8-empty" cx="%g" cy="%g" r="6"/>', x0 + 178, y))
  }, character(1))
  z6p_svg(400, 470, paste0(title, ": 40 zmian po 8 pomiarów, czerwone kratki to pomiary ponad progiem."),
    sprintf('<text x="0" y="22" font-size="15" font-weight="700">%s</text>', title), rows)
}

z6p8b_fi <- z6p8b_flags(z6p8b_seed_ind, FALSE)
z6p8b_fc <- z6p8b_flags(z6p8b_seed_cor, TRUE)
z6p8b_read <- function(f) sprintf("czerwonych kratek: %d z %d · zmian z alarmem: %d z %d",
  sum(f), length(f), sum(rowSums(f) > 0), nrow(f))

z6p_8b_ui <- z6p_panel("P8b", "przypadek brzegowy", "Te same pomiary, inne zmiany",
  z6p_css, z6p8_css,
  tags$div(class = "z6p8-pair",
    tags$div(z6p8b_svg(z6p8b_fi, "pomiary niezależne"), tags$p(class = "z6p-read", z6p8b_read(z6p8b_fi))),
    tags$div(z6p8b_svg(z6p8b_fc, "pomiary skorelowane (ρ = 0.8)"), tags$p(class = "z6p-read", z6p8b_read(z6p8b_fc)))),
  tags$p(class = "z6p-caption",
    "Obie siatki mają tyle samo czerwonych kratek. Przy pomiarach skorelowanych zbijają się one w mniejszej liczbie zmian, więc alarmów jest mniej, niż mówi wzór 1 − (1 − p)ⁿ."))
