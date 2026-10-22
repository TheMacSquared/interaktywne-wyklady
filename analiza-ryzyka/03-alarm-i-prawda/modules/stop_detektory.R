# Punkt stop sekcji „Wszystkie alarmy” (po pytaniu d3_chk_ppv):
#   a3_stop_detectors_ui — Galeria „Trzy detektory w tej samej hali”.
# Trzy kolumny stosów jak w „Dzienniku dyżurnego”: prawdziwe alarmy, fałszywe
# alarmy i przeoczone awarie na 10 000 zmian, jedno pole = 5 zmian. Pod każdą
# kolumną P(awaria | 🚨). Obraz statyczny (SVG z R), bez serwera.
# Wymaga: palette.R, lecture_layout.R, risk_math.R.

a3_detectors_data <- function(prevalence = .01, population = 10000L) {
  specs <- data.frame(
    id = c("A", "B", "C"),
    sensitivity = c(.95, .80, 1.00),
    fpr = c(.05, .01, .05)
  )
  rows <- lapply(seq_len(nrow(specs)), function(i) {
    d <- risk_detector_counts(population, prevalence, specs$sensitivity[i], specs$fpr[i])
    data.frame(id = specs$id[i], sensitivity = specs$sensitivity[i], fpr = specs$fpr[i],
      tp = d$alarm[1], fp = d$alarm[2], missed = d$no_alarm[1])
  })
  out <- do.call(rbind, rows)
  out$ppv <- out$tp / (out$tp + out$fp)
  out
}

a3_detectors_svg <- function(d = a3_detectors_data(), per_cell = 5) {
  cols <- 10; pitch <- 14; size <- 11
  x_lab <- 150; col_w <- 205; y_true <- 52; y_false <- 104; y_miss <- 270
  cells <- function(n, x0, y0, cls) {
    k <- round(n / per_cell)
    if (k == 0) return("")
    paste(sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g" rx="2"/>', cls,
      x0 + ((seq_len(k) - 1) %% cols) * pitch, y0 + ((seq_len(k) - 1) %/% cols) * pitch, size, size),
      collapse = "")
  }
  count <- function(x, y, n, cls) sprintf('<text class="a3-dt2-n %s" x="%g" y="%g">%s</text>', cls, x, y, n)
  columns <- vapply(seq_len(nrow(d)), function(i) {
    x <- x_lab + (i - 1) * col_w
    paste0(
      sprintf('<text class="a3-dt2-head" x="%g" y="22">%s</text>', x, d$id[i]),
      sprintf('<text class="a3-dt2-par" x="%g" y="40">czułość %.2f · FPR %.2f</text>', x + 20, d$sensitivity[i], d$fpr[i]),
      cells(d$tp[i], x, y_true, "a3-dt2-tp"), count(x + cols * pitch + 4, y_true + 10, d$tp[i], "is-tp"),
      cells(d$fp[i], x, y_false, "a3-dt2-fp"), count(x + cols * pitch + 4, y_false + 10, d$fp[i], "is-fp"),
      cells(d$missed[i], x, y_miss, "a3-dt2-miss"), count(x + cols * pitch + 4, y_miss + 10, d$missed[i], "is-miss"),
      sprintf('<text class="a3-dt2-ppv" x="%g" y="318">%.2f</text>', x + cols * pitch / 2, d$ppv[i])
    )
  }, character(1))
  label <- function(y, text) sprintf('<text class="a3-dt2-row" x="%g" y="%g">%s</text>', x_lab - 14, y, text)
  HTML(paste0(
    sprintf('<svg class="a3-dt2-svg" viewBox="0 0 %g 330" role="img" aria-label="%s">', x_lab + 3 * col_w,
      paste(sprintf("Detektor %s: %d prawdziwych i %d fałszywych alarmów, %d przeoczonych awarii, P(awaria | alarm) %.2f.",
        d$id, d$tp, d$fp, d$missed, d$ppv), collapse = " ")),
    label(y_true + 10, "🚨 prawdziwe"), label(y_false + 10, "🚨 fałszywe"),
    label(y_miss + 10, "🔕 przeoczone awarie"), label(318, "P(awaria | 🚨)"),
    paste(columns, collapse = ""),
    '</svg>'
  ))
}

a3_stop_detectors_css <- tags$style(HTML("
  .a3-dt2-svg { display: block; width: 100%; max-width: 820px; height: auto; margin: .2em auto .4em; }
  .a3-dt2-svg text { font-family: var(--upwr-mono); }
  .a3-dt2-head { font: 700 20px var(--upwr-sans) !important; fill: var(--upwr-ink); }
  .a3-dt2-par { font-size: 12.5px; fill: var(--upwr-ink-subtle); }
  .a3-dt2-row { font: 600 13px var(--upwr-sans) !important; fill: var(--upwr-ink-soft); text-anchor: end; }
  .a3-dt2-tp { fill: var(--upwr-accent); }
  .a3-dt2-fp { fill: var(--upwr-single-alt); }
  .a3-dt2-miss { fill: none; stroke: var(--upwr-accent); stroke-width: 1.6; }
  .a3-dt2-n { font-size: 13px; font-weight: 600; }
  .a3-dt2-n.is-tp, .a3-dt2-n.is-miss { fill: var(--upwr-accent); }
  .a3-dt2-n.is-fp { fill: var(--upwr-single-alt); }
  .a3-dt2-ppv { font-size: 26px; font-weight: 700; fill: var(--upwr-accent); text-anchor: middle; }
"))

a3_stop_detectors_ui <- figure_panel(
  label = "Galeria",
  title = "Trzy detektory w tej samej hali",
  full_width = TRUE,
  a3_stop_detectors_css,
  a3_detectors_svg(),
  lc_caption("Hala z awarią na 1 zmianie na 100, 10 000 zmian. Jedno pole = 5 zmian. Burgund: prawdziwe alarmy, złoto: fałszywe, pusty obrys: awarie bez alarmu.")
)
