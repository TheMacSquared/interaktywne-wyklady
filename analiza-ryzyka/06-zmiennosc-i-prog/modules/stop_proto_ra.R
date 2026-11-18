# PROTOTYPY PUNKTÓW STOP (2026-11-14) — P1 i P4 (wzorzec dla pozostałych) ---------
#
#   z6p_1_ui — Scena „Trzy wentylatory, jedna średnia” (P1), 4 kadry policzone w R:
#              raport → dzienniki pomiarów → próg → liczba przekroczeń. Ziarno 122:
#              w 1000 pomiarów dokładnie 1, 159 i 274 przekroczenia (σ = 1, 3, 5°C),
#              tyle co w przykładzie 6.1.
#   z6p_4_ui — Piaskownica „Przesuń i rozszerz krzywą” (P4): rycina lcSF z6_musigma
#              (modules/proto_ra.js), stałe osie, kontur N(82, 3), próg 85°C.
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R.

# ============================================================================
# P1 — SCENA „TRZY WENTYLATORY, JEDNA ŚREDNIA”
# ============================================================================

z6p1_sd <- c(1, 3, 5)
z6p1_n <- 1000L
z6p1_data <- local({
  set.seed(122)
  z <- matrix(rnorm(3 * z6p1_n), z6p1_n)
  lapply(1:3, function(j) z6p_mu + z6p1_sd[j] * z[, j])
})
z6p1_over <- vapply(z6p1_data, function(t) sum(t > z6p_c), integer(1))
z6p1_mean <- vapply(z6p1_data, mean, numeric(1))

# Geometria: trzy pasy dzienników na wspólnej skali temperatury 67–97°C.
z6p1_x0 <- 170; z6p1_x1 <- 640
z6p1_top <- c(24, 154, 284); z6p1_h <- 104
z6p1_tmin <- 67; z6p1_tmax <- 97
z6p1_x <- function(i) z6p1_x0 + (i - 0.5) / z6p1_n * (z6p1_x1 - z6p1_x0)
z6p1_y <- function(t, j) z6p1_top[j] + z6p1_h * (1 - (pmin(pmax(t, z6p1_tmin), z6p1_tmax) - z6p1_tmin) / (z6p1_tmax - z6p1_tmin))

# Punkty jako jedna ścieżka z odcinkami zerowej długości (okrągłe końce): tysiąc
# pomiarów mieści się w kilkunastu kilobajtach.
z6p1_dots <- function(i, t, j, cls) {
  if (!length(i)) return("")
  sprintf('<path class="%s" d="%s"/>', cls,
    paste(sprintf("M%.1f,%.1fh0", z6p1_x(i), z6p1_y(t, j)), collapse = ""))
}

z6p1_strip <- function(j, step) {
  t <- z6p1_data[[j]]; i <- seq_len(z6p1_n); over <- t > z6p_c
  y_mean <- z6p1_y(z6p_mu, j); y_c <- z6p1_y(z6p_c, j)
  paste0(
    sprintf('<rect class="z6p-paper" x="%g" y="%g" width="%g" height="%g" rx="4"/>', z6p1_x0, z6p1_top[j], z6p1_x1 - z6p1_x0, z6p1_h),
    sprintf('<text x="0" y="%g" font-size="16" font-weight="700">Wentylator %d</text>', z6p1_top[j] + 40, j),
    sprintf('<text class="z6p-mono z6p-subtle" x="0" y="%g" font-size="13">średnia %.0f°C</text>', z6p1_top[j] + 60, z6p1_mean[j]),
    # podziałka temperatury po lewej stronie pasa
    paste(sprintf('<text class="z6p-tick" style="text-anchor:end" x="%g" y="%g">%d</text>', z6p1_x0 - 6,
      z6p1_y(c(70, 80, 90), j) + 4, c(70L, 80L, 90L)), collapse = ""),
    if (step >= 3) z6p1_dots(i[!over], t[!over], j, "z6p1-dot") else z6p1_dots(i, t, j, "z6p1-dot"),
    if (step >= 3) z6p1_dots(i[over], t[over], j, "z6p1-dot is-over"),
    sprintf('<line class="z6p-mean" x1="%g" x2="%g" y1="%g" y2="%g"/>', z6p1_x0, z6p1_x1, y_mean, y_mean),
    if (step >= 3) sprintf('<line class="z6p-limit" x1="%g" x2="%g" y1="%g" y2="%g"/>', z6p1_x0, z6p1_x1, y_c, y_c),
    if (step >= 4) paste0(
      sprintf('<text class="z6p-mono z6p-bad" x="%g" y="%g" font-size="26" font-weight="700">%d</text>', z6p1_x1 + 18, z6p1_top[j] + 48, z6p1_over[j]),
      sprintf('<text class="z6p-mono z6p-subtle" x="%g" y="%g" font-size="13">na 1000</text>', z6p1_x1 + 18, z6p1_top[j] + 68),
      sprintf('<text class="z6p-mono z6p-subtle" x="0" y="%g" font-size="13">σ = %d°C</text>', z6p1_top[j] + 78, z6p1_sd[j]))
  )
}

# Kadr 1 (17.11.2026): wiersz 2 raportu P21 z początku rozdziału — ten sam szablon,
# wycinek jednego wiersza; ocena „w normie” wyróżniona.
z6p1_report <- function() {
  paste0(
    '<rect class="z6p-paper" x="60" y="40" width="680" height="230" rx="6"/>',
    '<rect x="60" y="40" width="680" height="4" rx="2" style="fill:var(--upwr-accent)"/>',
    '<text class="z6p-mono z6p-subtle" x="90" y="80" font-size="13" letter-spacing="1.2">RAPORT Z OCENY RYZYKA · WENTYLATOR 2 · MAJ · WIERSZ 2</text>',
    '<line class="z6p-grid" x1="90" x2="710" y1="96" y2="96"/>',
    '<text class="z6p-mono z6p-subtle" x="90" y="150" font-size="20" font-weight="700">2</text>',
    '<text x="130" y="150" font-size="20" font-weight="700">Wynik</text>',
    '<text x="260" y="150" font-size="19">Średnia 82°C przy progu 85°C:</text>',
    '<rect class="z6p-ok" x="256" y="168" width="104" height="32" rx="16" opacity=".2"/>',
    '<text class="z6p-ok" x="308" y="191" font-size="17" font-weight="700" text-anchor="middle">w normie</text>',
    '<text x="362" y="191" font-size="19">, ryzyko przekroczeń pomijalne.</text>',
    '<text class="z6p-subtle" x="90" y="244" font-size="14">Ocena oparta na dwóch liczbach: średniej i progu.</text>'
  )
}

z6p1_frames <- list(
  list(name = "Raport",
    text = "Wiersz 2 raportu z początku rozdziału ocenia wentylator 2 po samej średniej: 82°C przy progu 85°C, więc „w normie”.",
    read = "średnia 82°C · próg 85°C · zapas 3°C"),
  list(name = "Dzienniki",
    text = "Tę samą średnią mogą mieć bardzo różne dzienniki. Obok wentylatora 2 stawiamy dwa inne, też ze średnią 82°C. Punkt to jeden pomiar, przerywana linia to średnia.",
    read = NULL),
  list(name = "Próg",
    text = "Dorysowujemy próg 85°C. Pomiary ponad progiem są w kolorze akcentu.",
    read = NULL),
  list(name = "Liczymy",
    text = "Ta sama średnia, a przekroczeń jest 1, 159 i 274 na tysiąc pomiarów. Wentylatory różni tylko rozrzut; nasz wentylator 2 ma σ = 3°C i 159 przekroczeń na 1000.",
    read = sprintf("ponad 85°C: %s na 1000", paste(z6p1_over, collapse = " · ")))
)

z6p1_svg <- function(step) {
  if (step == 1) return(z6p_svg(800, 330,
    "Wiersz 2 raportu: średnia 82 stopnie przy progu 85 stopni, ocena w normie.",
    z6p1_report()))
  aria <- c("",
    "Dzienniki trzech wentylatorów: tysiąc pomiarów każdy, ta sama średnia, coraz większy rozrzut.",
    "Te same dzienniki z linią progu 85 stopni; pomiary ponad progiem wyróżnione.",
    sprintf("Liczba pomiarów ponad progiem na tysiąc: %s.", paste(z6p1_over, collapse = ", ")))[step]
  z6p_svg(800, 420, aria,
    paste(vapply(1:3, z6p1_strip, character(1), step = step), collapse = ""),
    sprintf('<text class="z6p-subtle" x="%g" y="412" font-size="12" text-anchor="end">kolejne pomiary (1–1000) · oś pionowa: temperatura (°C)</text>', z6p1_x1))
}

z6p1_css <- tags$style(HTML("
  .z6p1-dot { stroke: var(--upwr-secondary); stroke-width: 2.6; stroke-linecap: round; opacity: .45; fill: none; }
  .z6p1-dot.is-over { stroke: var(--upwr-accent); stroke-width: 3.4; opacity: .95; }
"))

z6p_1_ui <- tagList(z6p1_css,
  z6p_frames_ui("P1", "scena", "Trzy wentylatory, jedna średnia", z6p1_frames, z6p1_svg, proto = FALSE))

# ============================================================================
# P4 — PIASKOWNICA „PRZESUŃ I ROZSZERZ KRZYWĄ”
# ============================================================================

z6p_4_ui <- z6p_panel("P4", "piaskownica", "Przesuń i rozszerz krzywą", proto = FALSE,
  lc_sf_figure("z6_musigma",
    control = list(lc_sf_slider("z6p4_mu", "Średnia μ", 76, 88, z6p_mu, 0.5, suffix = "°C"),
      lc_sf_slider("z6p4_sd", "Odchylenie σ", 1.5, 6, z6p_sd, 0.25, suffix = "°C")),
    toolbar = lc_readouts(z6p_read("z", "z progu"), z6p_read("p", "P(T > 85°C)", upwr_accent),
      z6p_read("f", "Częstość")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    config = list(mu = z6p_mu, sd = z6p_sd, c = z6p_c),
    after = lc_caption("Próg 85°C stoi w miejscu. Przerywany kontur to stan bazowy N(82, 3). Oś pionowa jest stała: mniejsze σ daje krzywą węższą i wyższą."))
)
