# Ryciny wykładu 08 liczone w przeglądarce (modules/wykresy.js, silnik lcSF)
# oraz schematy blokowe rysowane przy budowaniu strony.
# Tutaj: to, co R liczy raz — rysunki trzech architektur (dawny wykres ggplot
# zależny tylko od listy „Układ”), odczyty z zaokrągleniem procentów i stałe.
# Wymaga: risk_math.R, risk_block.R, s8p_svg() ze stop_proto_common.R.

# Schemat blokowy niezawodności: bloki elementów i linie dróg sukcesu.
# Współrzędne jak w dawnym wykresie (jednostka = 70 px, oś Y w górę).
s8_diagram_svg <- function(kind) {
  spec <- switch(kind,
    series = list(
      boxes = data.frame(x = c(2, 4.5, 7), y = 0, w = 1, label = c("Czujnik", "Sterownik", "Wentylator")),
      lines = data.frame(xs = c(.3, 3, 5.5, 8), xe = c(1, 3.5, 6, 8.7), ys = 0, ye = 0),
      x = c(0, 9), y = c(-1.5, 1.5),
      aria = "Schemat blokowy: układ szeregowy, jedna droga przez czujnik, sterownik i wentylator."
    ),
    parallel = list(
      boxes = data.frame(x = 4.5, y = c(.9, -.9), w = 1.4, label = c("Wentylator A", "Wentylator B")),
      lines = data.frame(
        xs = c(.5, 2, 2, 2, 2, 5.9, 5.9, 7, 7, 7),
        xe = c(2, 2, 2, 3.1, 3.1, 7, 7, 7, 7, 8.5),
        ys = c(0, 0, 0, .9, -.9, .9, -.9, .9, -.9, 0),
        ye = c(0, .9, -.9, .9, -.9, .9, -.9, 0, 0, 0)
      ),
      x = c(0, 9), y = c(-1.8, 1.8),
      aria = "Schemat blokowy: układ równoległy, dwie drogi przez wentylator A albo wentylator B."
    ),
    mixed = list(
      boxes = data.frame(
        x = c(1.9, 6.4, 6.4), y = c(0, .9, -.9), w = c(1.2, 1.4, 1.4),
        label = c("Sterownik C", "Wentylator A", "Wentylator B")
      ),
      lines = data.frame(
        xs = c(0, 3.1, 4.2, 4.2, 4.2, 4.2, 7.8, 7.8, 8.6, 8.6, 8.6),
        xe = c(.7, 4.2, 4.2, 4.2, 5, 5, 8.6, 8.6, 8.6, 8.6, 9.5),
        ys = c(0, 0, 0, 0, .9, -.9, .9, -.9, .9, -.9, 0),
        ye = c(0, 0, .9, -.9, .9, -.9, .9, -.9, 0, 0, 0)
      ),
      x = c(-.2, 9.7), y = c(-1.8, 1.8),
      aria = "Schemat blokowy: układ mieszany, sterownik C w szeregu z równoległymi wentylatorami A i B."
    )
  )
  u <- 70
  px <- function(x) round((x - spec$x[1]) * u, 1)
  py <- function(y) round((spec$y[2] - y) * u, 1)
  b <- spec$boxes; l <- spec$lines
  ends <- c(min(l$xs), max(l$xe))
  s8p_svg(diff(spec$x) * u, diff(spec$y) * u, spec$aria, max_width = 760,
    sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="var(--upwr-reference)" stroke-width="2.6" stroke-linecap="round"/>',
            px(l$xs), py(l$ys), px(l$xe), py(l$ye)),
    sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="var(--upwr-secondary)" stroke="var(--upwr-surface)" stroke-width="1.5"/>',
            px(b$x - b$w), py(b$y + .45), round(2 * b$w * u, 1), round(.9 * u, 1)),
    sprintf('<text x="%s" y="%s" text-anchor="middle" dominant-baseline="central" font-weight="700" font-size="17" fill="var(--upwr-surface)">%s</text>',
            px(b$x), py(b$y), b$label),
    # Wejście i wyjście schematu: droga sukcesu biegnie od lewej kropki do prawej.
    sprintf('<circle cx="%s" cy="%s" r="6.5" fill="var(--upwr-accent)"/>', px(ends), py(0)),
    sprintf('<text x="%s" y="%s" text-anchor="%s" font-size="15" fill="var(--upwr-reference)">%s</text>',
            px(ends), py(-.42) + 5, c("start", "end"), c("wejście", "wyjście"))
  )
}

# s8m_common: niezawodność układu równoległego (0.92, 0.95) bez wspólnej przyczyny
# i udział wspólnej przyczyny w ryzyku dla każdego położenia suwaka (0–0.15 co 0.005).
s8_common_config <- local({
  base <- risk_parallel_reliability(c(.92, .95))
  q <- seq(0, .15, .005)
  r <- risk_common_cause_reliability(base, q)
  list(base = base, base_text = paste0("bez wspólnej przyczyny: ", formatC(base, format = "f", digits = 3)),
       step = .005, share = sprintf("%.0f%%", 100 * q / (1 - r)))
})

# s8m_improvement: wzrost R szeregu (0.92, 0.95, 0.98) po poprawie jednego elementu o 0.02.
s8_improvement_config <- local({
  base <- c(.92, .95, .98)
  sys0 <- risk_series_reliability(base)
  gains <- vapply(seq_along(base), function(i) {
    x <- base
    x[i] <- min(1, x[i] + .02)
    risk_series_reliability(x) - sys0
  }, numeric(1))
  list(labels = c("Wentylator (0.92)", "Czujnik (0.95)", "Zasilanie (0.98)"),
       gains = gains, text = sprintf("%.4f", gains), reads = vapply(gains, risk_fmt_p, ""))
})
