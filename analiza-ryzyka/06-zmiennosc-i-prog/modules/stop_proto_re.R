# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa re: rozdział 5 „Obciążenie–wytrzymałość” ----
#
#   z6p_11_ui — Scena „Zawiesie dźwigu” (P11): historia z powtórzeniem na otwarcie rozdziału,
#               przed pojęciem. Cztery kadry, przyciski „Podnieś ładunek”, +10, +100, +1000,
#               „Od nowa”. Losowanie i rysunek w modules/proto_re.js (window.lcZ6, scena „dzwig”).
#   z6p_12_ui — Pojęcie „Od chmury par do marginesu” (P12): cztery kadry policzone w R
#               (SVG), chmura par, zapas każdej pary, histogram D, cztery rozkłady odchyleń.
# Wymaga: stop_proto_common.R (z6p_panel, z6p_css, z6p_svg, z6p_axis, z6p_norm_path,
#         z6p_sigma_ruler, z6p_scene_ui, z6p_frames_ui, z6p_button, z6p_prev_next) oraz
#         modules/proto_re.js (scena „dzwig”).
# Model: L ~ N(85, 8), S ~ N(95, 7), niezależne; awaria, gdy L > S; D = S − L ~ N(10, √113).

# ============================================================================
# P11 — SCENA „ZAWIESIE DŹWIGU”
# ============================================================================

z6p11_p_model <- pnorm(0, 10, sqrt(113))  # P(D < 0) = P(L > S) ≈ 0.173

z6p11_frames <- list(
  list(name = "Jedno podniesienie",
    text = "Dźwigowy podnosi ładunek. Zawiesie średnio wytrzymuje 95 kN, a ładunek średnio waży 85 kN. Każdy ładunek i każde zawiesie jest trochę inne."),
  list(name = "Para na płaszczyźnie",
    text = "Jedno podniesienie to jeden punkt. Pod przekątną ładunek był cięższy, niż zawiesie mogło unieść."),
  list(name = "Powtarzamy",
    text = "Podnoś dalej i licz punkty pod przekątną."),
  list(name = "Model",
    text = sprintf("Średni zapas to 10 kN, a pod przekątną ląduje około %d podniesień na 1000.",
      round(1000 * z6p11_p_model)))
)

# Konfiguracja dla JS: parametry rozkładów, p z modelu i pierwsze podniesienie (awaria).
z6p11_config <- list(
  mu_l = 85, sd_l = 8, mu_s = 95, sd_s = 7,
  p_model = z6p11_p_model,
  first = list(l = 91.3, s = 88.0),
  frames = lapply(z6p11_frames, function(f) list(name = f$name, text = f$text))
)

z6p_11_ui <- z6p_scene_ui("P11", "scena", "Zawiesie dźwigu", "dzwig", z6p11_config,
  z6p_out("head"), z6p_out("stage"), z6p_out("read"), z6p_out("body"),
  tags$div(class = "z6p-nav",
    z6p_prev_next(),
    z6p_button("one", "Podnieś ładunek", variant = "solid"),
    z6p_button("m10", "+10"),
    z6p_button("m100", "+100"),
    z6p_button("m1000", "+1000"),
    z6p_button("reset", "Od nowa", variant = "ghost")))

# ============================================================================
# P12 — POJĘCIE „OD CHMURY PAR DO MARGINESU”
# ============================================================================

# Ziarno 52: w 400 parach (rozkłady jak w P11) dokładnie 69 awarii, 17.3%.
# Szukanie jednorazowe: pierwsze ziarno 1…N z sum(S − L < 0) == 69.
z6p12_seed <- 52L
z6p12_n <- 400L
z6p12 <- local({
  set.seed(z6p12_seed)
  L <- rnorm(z6p12_n, 85, 8)
  S <- rnorm(z6p12_n, 95, 7)
  list(L = L, S = S, D = S - L, fail = S - L < 0)
})
z6p12_fail_n <- sum(z6p12$fail)

# Zaokrąglenie połówek w górę (17.25 → 17.3) i zapis z kropką dziesiętną.
z6p12_round <- function(x, d = 1) floor(x * 10^d + 0.5) / 10^d
z6p12_fmt <- function(x, d = 1) formatC(z6p12_round(x, d), format = "f", digits = d)

# Wybór 14 par do kadru 2: 7 nad przekątną (D > 0) i 7 pod nią (D < 0), rozrzuconych
# po osi L (co k-ta w kolejności rosnącego L).
z6p12_sel <- local({
  L <- z6p12$L; D <- z6p12$D
  pick <- function(idx, k) idx[order(L[idx])][round(seq(1, length(idx), length.out = k))]
  c(pick(which(D > 0), 7), pick(which(D < 0), 7))
})

# Geometria chmury par: kwadrat 300 × 300, dolny lewy róg (250, 330), domena 55–125 kN.
z6p12_px <- 250; z6p12_pw <- 300; z6p12_py <- 330; z6p12_ph <- 300
z6p12_x_of <- function(v) z6p12_px + (v - 55) / 70 * z6p12_pw
z6p12_y_of <- function(v) z6p12_py - (v - 55) / 70 * z6p12_ph

# Pionowa oś z podziałką po lewej stronie (jak z6p_axis, ale w pionie).
z6p12_yaxis <- function(y_of, x, ticks) {
  paste0(
    sprintf('<line class="z6p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x, x, y_of(max(ticks)), y_of(min(ticks))),
    paste(sprintf('<line class="z6p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="z6p-tick" style="text-anchor:end" x="%g" y="%g">%s</text>',
      x - 5, x, y_of(ticks), y_of(ticks), x - 8, y_of(ticks) + 4, ticks), collapse = "")
  )
}

# Chmura par: punkty awarii w akcencie, reszta szałwią-niebem (z6p-dot); sel = wybrane
# pary, reszta przygaszona. seg = TRUE rysuje odcinki od pary do przekątnej.
z6p12_plane <- function(sel = NULL, seg = FALSE) {
  L <- z6p12$L; S <- z6p12$S; D <- z6p12$D; fail <- z6p12$fail
  cls <- ifelse(fail, "z6p-dot-bad", "z6p-dot")
  r <- rep(3.2, length(L)); sty <- rep("", length(L))
  ord <- seq_along(L)
  if (!is.null(sel)) {
    sty[-sel] <- ' style="opacity:.15"'
    sty[sel] <- ' style="opacity:1"'
    r[sel] <- 4.6
    ord <- c(setdiff(ord, sel), sel)
  }
  segs <- if (seg) sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke-width="3" style="stroke: %s"/>',
      z6p12_x_of(L[sel]), z6p12_y_of(S[sel]), z6p12_x_of(L[sel]), z6p12_y_of(L[sel]),
      ifelse(D[sel] > 0, "var(--upwr-sage)", "var(--upwr-accent)")) else character(0)
  dots <- sprintf('<circle class="%s" cx="%.1f" cy="%.1f" r="%.1f"%s/>',
    cls, z6p12_x_of(L), z6p12_y_of(S), r, sty)
  paste0(
    z6p_axis(z6p12_x_of, z6p12_py, c(60, 70, 80, 90, 100, 110, 120), label = "obciążenie L (kN)",
      labels = sub("-", "−", c(60, 70, 80, 90, 100, 110, 120))),
    z6p12_yaxis(z6p12_y_of, z6p12_px, c(60, 70, 80, 90, 100, 110, 120)),
    sprintf('<text class="z6p-subtle" x="%g" y="22" font-size="12">wytrzymałość S (kN)</text>', z6p12_px),
    sprintf('<line class="z6p-limit" x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f"/>',
      z6p12_x_of(55), z6p12_y_of(55), z6p12_x_of(125), z6p12_y_of(125)),
    sprintf('<text class="z6p-mono" x="%.1f" y="%.1f" font-size="13">L = S</text>', z6p12_x_of(112) + 14, z6p12_y_of(112) + 14),
    paste(segs, collapse = ""),
    paste(dots[ord], collapse = "")
  )
}

# Legenda chmury (awaria / bezpieczna para) w prawej części kadru.
z6p12_legend <- function() {
  paste0(
    '<circle class="z6p-dot-bad" cx="590" cy="64" r="5"/><text x="604" y="68" font-size="14">awaria: L &gt; S</text>',
    '<circle class="z6p-dot" cx="590" cy="92" r="5"/><text x="604" y="96" font-size="14">para bezpieczna</text>'
  )
}

# Histogram D: przedziały co 5 kN od −30 do 50.
z6p12_breaks <- seq(-30, 50, by = 5)
z6p12_counts <- hist(z6p12$D, breaks = z6p12_breaks, plot = FALSE, right = FALSE)$counts
z6p12_hx0 <- 100; z6p12_hw <- 600; z6p12_hbase <- 300; z6p12_htop <- 60
z6p12_hx_of <- function(d) z6p12_hx0 + (d + 30) / 80 * z6p12_hw
# Oczekiwana liczba par w przedziale (5 kN) przy N(10, √113).
z6p12_expected <- function(d) z6p12_n * 5 * dnorm(d, 10, sqrt(113))
z6p12_ymax <- ceiling(max(z6p12_counts, z6p12_expected(seq(-30, 50, 0.5))) * 1.1 / 10) * 10
z6p12_hy_of <- function(c) z6p12_hbase - c / z6p12_ymax * (z6p12_hbase - z6p12_htop)

z6p12_hist_svg <- function() {
  lo <- head(z6p12_breaks, -1); hi <- tail(z6p12_breaks, -1)
  bars <- vapply(seq_along(z6p12_counts), function(j) {
    if (!z6p12_counts[j]) return("")
    x1 <- z6p12_hx_of(lo[j]) + 1; x2 <- z6p12_hx_of(hi[j]) - 1
    y <- z6p12_hy_of(z6p12_counts[j]); h <- z6p12_hbase - y
    st <- if (hi[j] <= 0) "fill: var(--upwr-accent)" else "fill: var(--upwr-secondary); opacity: .55"
    sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="1.5" style="%s"/>', x1, y, x2 - x1, h, st)
  }, character(1))
  d_grid <- seq(-30, 50, by = 0.5)
  curve <- sprintf("%.1f,%.1f", z6p12_hx_of(d_grid), z6p12_hy_of(z6p12_expected(d_grid)))
  paste0(
    paste(bars, collapse = ""),
    sprintf('<polyline points="%s" fill="none" style="stroke: var(--upwr-ink)" stroke-width="2.2" stroke-linejoin="round"/>',
      paste(curve, collapse = " ")),
    sprintf('<line class="z6p-limit" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
      z6p12_hx_of(0), z6p12_hx_of(0), z6p12_htop - 8, z6p12_hbase),
    sprintf('<text class="z6p-mono" x="%.1f" y="%g" font-size="13">D = 0</text>', z6p12_hx_of(0) + 6, z6p12_htop - 12),
    z6p_axis(z6p12_hx_of, z6p12_hbase, seq(-30, 50, by = 10), label = "D = S − L (kN)",
      labels = sub("-", "−", seq(-30, 50, by = 10))),
    z6p_sigma_ruler(z6p12_hx_of, 362, mu = 10, sd = sqrt(113), k = 3),
    '<rect x="520" y="34" width="12" height="12" rx="2" style="fill: var(--upwr-accent)"/><text x="538" y="45" font-size="13">awarie, D &lt; 0</text>',
    sprintf('<rect x="520" y="54" width="12" height="12" rx="2" style="fill: var(--upwr-secondary); opacity: .55"/><text x="538" y="65" font-size="13">pary bezpieczne</text>')
  )
}

# Krzywe odchyleń od średniej: L (σ = 8), S (σ = 7), D (σ = √113 ≈ 10.63), wspólna oś.
z6p12_dev_svg <- function() {
  base <- 300; top <- 60
  x_of <- function(d) 100 + (d + 35) / 70 * 600
  ymax <- dnorm(0, 0, 7) * 1.1
  y_of <- function(v) base - v / ymax * (base - top)
  curve_path <- function(sd, style, width) {
    sprintf('<path d="%s" fill="none" style="stroke: %s" stroke-width="%g"/>',
      z6p_norm_path(0, sd, x_of, y_of, -35, 35, n = 141), style, width)
  }
  paste0(
    sprintf('<line class="z6p-mean" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>', x_of(0), x_of(0), top, base),
    curve_path(8, "var(--upwr-cat-bursztyn)", 2.4),
    curve_path(7, "var(--upwr-cat-niebo)", 2.4),
    curve_path(sqrt(113), "var(--upwr-ink)", 3.6),
    z6p_axis(x_of, base, seq(-30, 30, by = 10), label = "odchylenie od średniej (kN)",
      labels = sub("-", "−", seq(-30, 30, by = 10))),
    # Legenda: krzywe rozpoznaje kolor i grubość, podpisy stoją przy szczycie każdej krzywej.
    sprintf('<line x1="560" x2="590" y1="30" y2="30" stroke-width="2.4" style="stroke: var(--upwr-cat-bursztyn)"/><text x="598" y="34" font-size="14">L: σ = 8</text>'),
    sprintf('<line x1="560" x2="590" y1="50" y2="50" stroke-width="2.4" style="stroke: var(--upwr-cat-niebo)"/><text x="598" y="54" font-size="14">S: σ = 7</text>'),
    sprintf('<line x1="560" x2="590" y1="70" y2="70" stroke-width="3.6" style="stroke: var(--upwr-ink)"/><text x="598" y="74" font-size="14">D: σ = √113 ≈ 10.63</text>')
  )
}

z6p12_beta <- 10 / sqrt(113)
z6p12_pfail <- pnorm(-z6p12_beta)

z6p12_frames <- list(
  list(name = "Chmura par",
    text = sprintf("Każdy punkt to jedna para: obciążenie L i wytrzymałość S z jednego podniesienia. Pary pod przekątną to awarie. Z 400 par awarii jest %d.", z6p12_fail_n),
    read = sprintf("awarii: %d z 400 (%s%%)", z6p12_fail_n, z6p12_fmt(100 * z6p12_fail_n / 400, 1))),
  list(name = "Zapas każdej pary",
    text = "Dla wybranych par odcinek łączy punkt z przekątną. Jego długość to zapas D = S − L. Kolor mówi, czy zapas jest dodatni, czy ujemny.",
    read = "D = S − L: zapas pary"),
  list(name = "Margines D",
    text = "Zapasy wszystkich 400 par składamy w jeden rozkład. Słupki na lewo od zera to awarie. Indeks β mówi, ile odchyleń σ_D mieści się między średnim zapasem a zerem.",
    read = sprintf("β = 10 / 10.63 ≈ %s · P(D < 0) = Φ(−β) ≈ %s",
      z6p12_fmt(z6p12_beta, 2), z6p12_fmt(z6p12_pfail, 3))),
  list(name = "Szerszy niż oba",
    text = "Zapas waha się bardziej niż obciążenie i wytrzymałość z osobna, bo wariancje się dodają, także przy odejmowaniu.",
    read = "σ_D = √(8² + 7²) ≈ 10.63 kN")
)

z6p12_svg <- function(i) {
  if (i == 1) return(z6p_svg(800, 400,
    "Chmura 400 par obciążenie–wytrzymałość; punkty pod przekątną to awarie, których jest 69.",
    z6p12_plane(), z6p12_legend()))
  if (i == 2) return(z6p_svg(800, 400,
    "Ta sama chmura; czternaście wybranych par połączono odcinkiem z przekątną, długość odcinka to zapas.",
    z6p12_plane(sel = z6p12_sel, seg = TRUE), z6p12_legend()))
  if (i == 3) return(z6p_svg(800, 400,
    "Histogram zapasów D z przedziałami co 5 kN; słupki poniżej zera to awarie; krzywa normalna N(10, 10.63).",
    z6p12_hist_svg()))
  z6p_svg(800, 400,
    "Trzy krzywe odchyleń od średniej: L z σ = 8, S z σ = 7, D z σ = 10.63; D jest najszerszą krzywą.",
    z6p12_dev_svg())
}

z6p_12_ui <- z6p_frames_ui("P12", "pojęcie", "Od chmury par do marginesu", z6p12_frames, z6p12_svg)
