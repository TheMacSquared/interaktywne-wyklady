# Punkty stop: „Dwa mianowniki” (pojęcie) i „Kampania a hala” (galeria) ------
#
# Oba obrazy są statyczne: SVG generowane w R, style w tags$style dołączonym do
# obiektu UI. Kolory wyłącznie przez tokeny --upwr-*, więc działają w trybie
# ciemnym. Prawdziwe alarmy = akcent, fałszywe = złoto (jak siatka alarmów).

.a3_num <- function(n) format(n, big.mark = " ", trim = TRUE, scientific = FALSE)
.a3_p2 <- function(p) formatC(p, format = "f", digits = 2)

# ---- Element 2: Dwa mianowniki --------------------------------------------

a3_stop_denominators_css <- tags$style(HTML("
  .a3-mn-pair { display: flex; flex-wrap: wrap; gap: 1.2em 2.4em; justify-content: center; }
  .a3-mn-side { flex: 1 1 300px; max-width: 460px; min-width: 0; }
  .a3-mn-head { font: 600 1em var(--upwr-sans); color: var(--upwr-ink); margin: 0 0 .3em; text-align: center; }
  .a3-mn-svg { display: block; width: 100%; height: auto; }
  .a3-mn-svg text { font-family: var(--upwr-sans); fill: var(--upwr-ink-soft); }
  .a3-mn-svg .a3-mn-lab { font-size: 14px; }
  .a3-mn-svg .a3-mn-tp { fill: var(--upwr-accent); }
  .a3-mn-svg .a3-mn-fp { fill: var(--upwr-single-alt); }
  .a3-mn-svg .a3-mn-fn { fill: var(--upwr-surface); stroke: var(--upwr-accent); stroke-width: 2; }
  .a3-mn-svg .a3-mn-dim { opacity: .3; }
  .a3-mn-svg .a3-mn-rest { fill: var(--upwr-surface-sunken); stroke: var(--upwr-rule); stroke-width: 1; }
  .a3-mn-svg .a3-mn-rest-n { font: 600 16px var(--upwr-mono); fill: var(--upwr-ink-subtle); }
  .a3-mn-svg .a3-mn-frame { fill: none; stroke: var(--upwr-ink); stroke-width: 2.5; }
  .a3-mn-svg .a3-mn-count { font: 700 20px var(--upwr-mono); fill: var(--upwr-ink); }
  .a3-mn-side .lc-reads { justify-content: center; margin-top: .5em; }
"))

# Ten sam świat 1000 zmian w układzie tablicy 2×2: wiersze awaria / bez awarii,
# kolumny alarm / bez alarmu. Każde pole to jedna zmiana; 931 zmian bez awarii
# i bez alarmu to jedna szara płaszczyzna (nie jest w żadnym mianowniku).
# group = "failure" obrysowuje wiersz awarii, "alarm" — kolumnę alarmów.
a3_denominator_svg <- function(group = c("failure", "alarm"),
                               tp = 19L, fn = 1L, fp = 49L, tn = 931L) {
  group <- match.arg(group)
  s <- 16; pitch <- 20; ncol <- 7L
  x_alarm <- 96; x_none <- x_alarm + ncol * pitch + 24
  y_fail <- 52
  rows_tp <- ceiling(tp / ncol)
  y_ok <- y_fail + rows_tp * pitch + 20
  rows_fp <- ceiling(fp / ncol)
  y_end <- y_ok + rows_fp * pitch - (pitch - s)
  col_w <- ncol * pitch - (pitch - s)

  squares <- function(n, x0, y0, cls) paste0(vapply(seq_len(n) - 1L, function(i) sprintf(
    '<rect class="%s" x="%g" y="%g" width="%g" height="%g" rx="2"/>',
    cls, x0 + (i %% ncol) * pitch, y0 + (i %/% ncol) * pitch, s, s), ""), collapse = "")
  dim_fp <- if (group == "failure") " a3-mn-dim" else ""
  dim_fn <- if (group == "alarm") " a3-mn-dim" else ""

  rest_w <- 150
  rest <- sprintf(
    '<rect class="a3-mn-rest" x="%g" y="%g" width="%g" height="%g" rx="3"/><text class="a3-mn-rest-n" x="%g" y="%g" text-anchor="middle">%s</text>',
    x_none, y_ok, rest_w, y_end - y_ok, x_none + rest_w / 2, (y_ok + y_end) / 2 + 6, .a3_num(tn))

  pad <- 7
  if (group == "failure") {
    fx <- x_alarm - pad; fy <- y_fail - pad
    fw <- (x_none + s * fn + pad) - fx; fh <- rows_tp * pitch - (pitch - s) + 2 * pad
    count <- sprintf('<text class="a3-mn-count" x="%g" y="%g">%s</text>', fx + fw + 10, fy + fh / 2 + 7, .a3_num(tp + fn))
  } else {
    fx <- x_alarm - pad; fy <- y_fail - pad
    fw <- col_w + 2 * pad; fh <- (y_end + pad) - fy
    count <- sprintf('<text class="a3-mn-count" x="%g" y="%g" text-anchor="middle">%s</text>', fx + fw / 2, fy + fh + 24, .a3_num(tp + fp))
  }
  frame <- sprintf('<rect class="a3-mn-frame" x="%g" y="%g" width="%g" height="%g" rx="8"/>', fx, fy, fw, fh)
  height <- y_end + 40
  width <- x_none + rest_w + 8

  aria <- if (group == "failure") {
    sprintf("Wśród %s zmian z awarią czujnik alarmował w %s; pozostałe zmiany są przygaszone.", tp + fn, tp)
  } else {
    sprintf("Wśród %s zmian z alarmem %s to zmiany z awarią, a %s to fałszywe alarmy.", tp + fp, tp, fp)
  }

  HTML(paste0(
    sprintf('<svg class="a3-mn-svg" viewBox="0 0 %g %g" role="img" aria-label="%s">', width, height, aria),
    sprintf('<text class="a3-mn-lab" x="%g" y="34" text-anchor="middle">alarm</text>', x_alarm + col_w / 2),
    sprintf('<text class="a3-mn-lab" x="%g" y="34" text-anchor="middle">bez alarmu</text>', x_none + rest_w / 2),
    sprintf('<text class="a3-mn-lab" x="%g" y="%g" text-anchor="end">awaria</text>', x_alarm - 16, y_fail + (rows_tp * pitch) / 2 + 2),
    sprintf('<text class="a3-mn-lab" x="%g" y="%g" text-anchor="end">bez awarii</text>', x_alarm - 16, (y_ok + y_end) / 2 + 5),
    squares(tp, x_alarm, y_fail, "a3-mn-tp"),
    squares(fn, x_none, y_fail, paste0("a3-mn-fn", dim_fn)),
    squares(fp, x_alarm, y_ok, paste0("a3-mn-fp", dim_fp)),
    rest, frame, count,
    '</svg>'
  ))
}

a3_stop_denominators_ui <- local({
  tp <- 19L; fn <- 1L; fp <- 49L; tn <- 931L
  side <- function(head, group, label, num, den, approx) tags$div(class = "a3-mn-side",
    tags$p(class = "a3-mn-head", head),
    a3_denominator_svg(group, tp, fn, fp, tn),
    lc_readouts(lc_readout(label,
      sprintf("%s/%s %s %s", num, den, if (approx) "≈" else "=", .a3_p2(num / den)),
      color = upwr_accent))
  )
  figure_panel(
    label = "Pojęcie",
    title = "Dwa mianowniki: te same 1000 zmian",
    full_width = TRUE,
    a3_stop_denominators_css,
    tags$div(class = "a3-mn-pair",
      side("Wśród zmian z awarią", "failure", "P(alarm | awaria)", tp, tp + fn, FALSE),
      side("Wśród zmian z alarmem", "alarm", "P(awaria | alarm)", tp, tp + fp, TRUE)
    )
  )
})

# ---- Element 4: Kampania a hala -------------------------------------------

a3_stop_campaign_css <- tags$style(HTML("
  .a3-kp-pair { display: flex; flex-wrap: wrap; gap: 1.2em 2.4em; justify-content: center; }
  .a3-kp-side { flex: 1 1 300px; max-width: 460px; min-width: 0; }
  .a3-kp-head { font: 600 1em var(--upwr-sans); color: var(--upwr-ink); margin: 0 0 .3em; text-align: center; }
  .a3-kp-svg { display: block; width: 100%; height: auto; }
  .a3-kp-svg text { font-family: var(--upwr-sans); fill: var(--upwr-ink-soft); }
  .a3-kp-svg .a3-kp-lab { font-size: 14px; }
  .a3-kp-svg .a3-kp-band { fill: var(--upwr-surface-sunken); }
  .a3-kp-svg .a3-kp-cell { fill: var(--upwr-surface); stroke: var(--upwr-rule); stroke-width: 1; }
  .a3-kp-svg .a3-kp-cell.is-tp { fill: var(--upwr-accent); stroke: none; }
  .a3-kp-svg .a3-kp-cell.is-fp { fill: var(--upwr-single-alt); stroke: none; }
  .a3-kp-svg .a3-kp-n { font: 700 18px var(--upwr-mono); fill: var(--upwr-ink); }
  .a3-kp-svg .a3-kp-n.is-tp { fill: var(--upwr-surface); }
  .a3-kp-svg .a3-kp-n.is-fp { fill: #1c1a17; }
  .a3-kp-svg .a3-kp-row { font: 600 17px var(--upwr-mono); fill: var(--upwr-ink-soft); }
  .a3-kp-svg .a3-kp-arrow { stroke: var(--upwr-ink-subtle); stroke-width: 1.5; fill: none; }
  .a3-kp-svg .a3-kp-frame { fill: none; stroke: var(--upwr-ink); stroke-width: 2.5; }
  .a3-kp-svg .a3-kp-bar-bg { fill: var(--upwr-single-alt); }
  .a3-kp-svg .a3-kp-bar { fill: var(--upwr-accent); }
  .a3-kp-svg .a3-kp-col { font: 700 24px var(--upwr-mono); fill: var(--upwr-accent); }
  .a3-kp-side .lc-reads { justify-content: center; margin-top: .4em; }
"))

# Tablica 2×2: pasma wierszy z ilorazem wierszowym po prawej (czytamy wzdłuż
# wiersza), obrys kolumny alarmów z paskiem składu i ilorazem kolumnowym pod nią.
a3_campaign_svg <- function(tp, fn, fp, tn, aria) {
  x_lab <- 92; x1 <- 104; cw <- 118; gap <- 6; x2 <- x1 + cw + gap
  y1 <- 46; ch <- 58; y2 <- y1 + ch + gap
  x_row <- x2 + cw + 40
  cell <- function(x, y, n, cls = "") sprintf(
    '<rect class="a3-kp-cell%s" x="%g" y="%g" width="%g" height="%g" rx="4"/><text class="a3-kp-n%s" x="%g" y="%g" text-anchor="middle">%s</text>',
    if (nzchar(cls)) paste0(" ", cls) else "", x, y, cw, ch,
    if (nzchar(cls)) paste0(" ", cls) else "", x + cw / 2, y + ch / 2 + 6, .a3_num(n))
  band <- function(y) sprintf('<rect class="a3-kp-band" x="%g" y="%g" width="%g" height="%g" rx="6"/>',
    8, y - 3, x_row + 52 - 8, ch + 6)
  row_ratio <- function(y, p) sprintf(
    '<path class="a3-kp-arrow" d="M%g %g h18 m-6 -5 l6 5 l-6 5"/><text class="a3-kp-row" x="%g" y="%g">%s</text>',
    x2 + cw + 8, y + ch / 2, x_row, y + ch / 2 + 6, .a3_p2(p))
  ppv <- tp / (tp + fp)
  bar_y <- y2 + ch + 18; bar_h <- 14
  frame <- sprintf('<rect class="a3-kp-frame" x="%g" y="%g" width="%g" height="%g" rx="8"/>',
    x1 - 6, y1 - 6, cw + 12, (bar_y + bar_h + 6) - (y1 - 6))
  HTML(paste0(
    sprintf('<svg class="a3-kp-svg" viewBox="0 0 %g %g" role="img" aria-label="%s">', x_row + 56, bar_y + bar_h + 48, aria),
    band(y1), band(y2),
    sprintf('<text class="a3-kp-lab" x="%g" y="32" text-anchor="middle">alarm</text>', x1 + cw / 2),
    sprintf('<text class="a3-kp-lab" x="%g" y="32" text-anchor="middle">bez alarmu</text>', x2 + cw / 2),
    sprintf('<text class="a3-kp-lab" x="%g" y="%g" text-anchor="end">awaria</text>', x_lab, y1 + ch / 2 + 5),
    sprintf('<text class="a3-kp-lab" x="%g" y="%g" text-anchor="end">bez awarii</text>', x_lab, y2 + ch / 2 + 5),
    cell(x1, y1, tp, "is-tp"), cell(x2, y1, fn),
    cell(x1, y2, fp, "is-fp"), cell(x2, y2, tn),
    row_ratio(y1, tp / (tp + fn)), row_ratio(y2, fp / (fp + tn)),
    sprintf('<rect class="a3-kp-bar-bg" x="%g" y="%g" width="%g" height="%g"/>', x1, bar_y, cw, bar_h),
    sprintf('<rect class="a3-kp-bar" x="%g" y="%g" width="%g" height="%g"/>', x1, bar_y, round(cw * ppv, 1), bar_h),
    frame,
    sprintf('<text class="a3-kp-col" x="%g" y="%g" text-anchor="middle">%s</text>', x1 + cw / 2, bar_y + bar_h + 36, .a3_p2(ppv)),
    '</svg>'
  ))
}

a3_stop_campaign_ui <- local({
  side <- function(head, tp, fn, fp, tn) {
    ppv <- tp / (tp + fp)
    aria <- sprintf(paste0(
      "%s. Awarie: %s z alarmem, %s bez. Bez awarii: %s z alarmem, %s bez. ",
      "Czułość %s i FPR %s; wśród %s alarmów prawdziwych jest %s, czyli %s."),
      head, tp, fn, fp, tn, .a3_p2(tp / (tp + fn)), .a3_p2(fp / (fp + tn)),
      tp + fp, tp, .a3_p2(ppv))
    tags$div(class = "a3-kp-side",
      tags$p(class = "a3-kp-head", head),
      a3_campaign_svg(tp, fn, fp, tn, aria),
      lc_readouts(lc_readout("P(awaria | alarm)",
        sprintf("%s/%s", tp, .a3_num(tp + fp)), color = upwr_accent))
    )
  }
  figure_panel(
    label = "Galeria",
    title = "Ten sam czujnik w kampanii testowej i w hali",
    full_width = TRUE,
    a3_stop_campaign_css,
    tags$div(class = "a3-kp-pair",
      side("Kampania testowa · 460 zmian", 57L, 3L, 20L, 380L),
      side("Hala Bananpolu · 10 000 zmian", 95L, 5L, 495L, 9405L)
    )
  )
})
