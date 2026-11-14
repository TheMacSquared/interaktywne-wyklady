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

# Kadr 1: sam raport — trzy wiersze z tą samą średnią i adnotacją „w normie”.
z6p1_report <- function() {
  rows <- vapply(1:3, function(j) {
    y <- 118 + (j - 1) * 62
    paste0(
      sprintf('<line class="z6p-grid" x1="130" x2="670" y1="%g" y2="%g"/>', y - 34, y - 34),
      sprintf('<text x="150" y="%g" font-size="18" font-weight="600">Wentylator %d</text>', y, j),
      sprintf('<text class="z6p-mono" x="360" y="%g" font-size="18">82°C</text>', y),
      sprintf('<text class="z6p-mono" x="470" y="%g" font-size="18">85°C</text>', y),
      sprintf('<rect class="z6p-ok" x="560" y="%g" width="96" height="28" rx="14" opacity=".2"/>', y - 20),
      sprintf('<text class="z6p-ok" x="608" y="%g" font-size="15" font-weight="700" text-anchor="middle">w normie</text>', y)
    )
  }, character(1))
  paste0(
    '<rect class="z6p-paper" x="110" y="20" width="580" height="290" rx="6"/>',
    '<text x="150" y="54" font-size="15" font-weight="700">RAPORT MIESIĘCZNY · temperatura łożysk</text>',
    '<text class="z6p-subtle" x="360" y="78" font-size="13">średnia</text>',
    '<text class="z6p-subtle" x="470" y="78" font-size="13">próg</text>',
    '<text class="z6p-subtle" x="560" y="78" font-size="13">ocena</text>',
    paste(rows, collapse = "")
  )
}

z6p1_frames <- list(
  list(name = "Raport",
    text = "Raport miesięczny podaje średnią temperaturę łożysk trzech wentylatorów. Każdy ma 82°C przy progu 85°C i adnotację „w normie”.",
    read = "średnia 82°C · próg 85°C · zapas 3°C"),
  list(name = "Dzienniki",
    text = "Za każdą średnią stoi tysiąc pomiarów z dziennika. Punkt to jeden pomiar, przerywana linia to średnia.",
    read = NULL),
  list(name = "Próg",
    text = "Dorysowujemy próg 85°C. Pomiary ponad progiem są w kolorze akcentu.",
    read = NULL),
  list(name = "Liczymy",
    text = "Ta sama średnia, a przekroczeń jest 1, 159 i 274 na tysiąc pomiarów. Wentylatory różni tylko rozrzut.",
    read = sprintf("ponad 85°C: %s na 1000", paste(z6p1_over, collapse = " · ")))
)

z6p1_svg <- function(step) {
  if (step == 1) return(z6p_svg(800, 330,
    "Raport: trzy wentylatory o średniej temperaturze 82 stopnie, próg 85 stopni, wszystkie ocenione jako w normie.",
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
  z6p_frames_ui("P1", "scena", "Trzy wentylatory, jedna średnia", z6p1_frames, z6p1_svg))

# ============================================================================
# P4 — PIASKOWNICA „PRZESUŃ I ROZSZERZ KRZYWĄ”
# ============================================================================

z6p_4_ui <- z6p_panel("P4", "piaskownica", "Przesuń i rozszerz krzywą",
  lc_sf_figure("z6_musigma",
    control = list(lc_sf_slider("z6p4_mu", "Średnia μ", 76, 88, z6p_mu, 0.5, suffix = "°C"),
      lc_sf_slider("z6p4_sd", "Odchylenie σ", 1.5, 6, z6p_sd, 0.25, suffix = "°C")),
    toolbar = lc_readouts(z6p_read("z", "z progu"), z6p_read("p", "P(T > 85°C)", upwr_accent),
      z6p_read("f", "Częstość")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    config = list(mu = z6p_mu, sd = z6p_sd, c = z6p_c),
    after = lc_caption("Próg 85°C stoi w miejscu. Przerywany kontur to stan bazowy N(82, 3). Oś pionowa jest stała: mniejsze σ daje krzywą węższą i wyższą."))
)
