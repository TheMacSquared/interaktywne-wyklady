# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rc (rozdz. 2, cenzorowanie) ---------
#
#   c7p_3_ui — Scena „Badanie kończy się po 1200 h” (P3), 4 kadry policzone w R:
#              rejestr awarii (cztery wpisy, średnia 612.5 h) → czego nie ma
#              w rejestrze (osiem linii) → co wiemy o działających → podgląd
#              przyszłości (prawdziwa średnia 1368.75 h).
#   c7p_5_ui — Pojęcie „Łączny czas pracy” (P5), 5 kadrów: osiem linii → jedna
#              taśma 7250 h → taśma na osiem wentylatorów (dolna granica 906 h)
#              → taśma na cztery awarie (1812.5 h) → taśma bez działających
#              (2450 h, 612.5 h). Liczby z przykładu 7.2.
# Wymaga: stop_proto_common.R.

c7pc_times <- c(220, 480, 760, 990, 1350, 1750, 2300, 3100)  # rzeczywiste czasy życia (h)
c7pc_end <- 1200                                             # koniec badania (h)
c7pc_failed <- c7pc_times <= c7pc_end
c7pc_obs <- pmin(c7pc_times, c7pc_end)
c7pc_naive <- mean(c7pc_times[c7pc_failed])                  # 612.5
c7pc_true <- mean(c7pc_times)                                # 1368.75
c7pc_total <- sum(c7pc_obs)                                  # 7250
c7pc_est <- c7pc_total / sum(c7pc_failed)                    # 1812.5
c7pc_lower <- c7pc_total / length(c7pc_times)                # 906.25 (dolna granica)

c7pc_css <- tags$style(HTML("
  .c7pc-life { stroke: var(--upwr-secondary); stroke-width: 6; fill: none; }
  .c7pc-life.is-alive { stroke: var(--upwr-sage); }
  .c7pc-life.is-dim { opacity: .22; }
  .c7pc-tick { stroke: var(--upwr-accent); stroke-width: 3.2; fill: none; }
  .c7pc-tick.is-ghost { opacity: .55; }
  .c7pc-tri { fill: var(--upwr-sage); }
  .c7pc-tri.is-dim { opacity: .22; }
  .c7pc-mark { fill: var(--upwr-accent); stroke: var(--upwr-surface); stroke-width: 1.5; }
  .c7pc-mark.is-true { fill: var(--upwr-ink); }
  .c7pc-mark.is-est { fill: var(--upwr-cat-niebo); }
  .c7pc-cross { stroke: var(--upwr-ink-subtle); stroke-width: 1.6; }
  .c7pc-seg { fill: color-mix(in srgb, var(--upwr-secondary) 70%, var(--upwr-surface)); stroke: var(--upwr-surface); stroke-width: 1.5; }
  .c7pc-seg.is-alive { fill: color-mix(in srgb, var(--upwr-sage) 75%, var(--upwr-surface)); }
  .c7pc-cut { stroke: var(--upwr-ink); stroke-width: 1.6; stroke-dasharray: 4 3; fill: none; }
"))

# ============================================================================
# P3 — SCENA „BADANIE KOŃCZY SIĘ PO 1200 H”
# ============================================================================

c7p3_x <- function(t) 70 + t / 3300 * 740
c7p3_y <- function(i) 44 + (i - 1) * 26
c7p3_base <- 262

c7p3_diamond <- function(t, cls, label, dy = 0) paste0(
  sprintf('<path class="c7pc-mark %s" d="M%.1f,%gl8,9l-8,9l-8,-9z"/>', cls, c7p3_x(t), c7p3_base + 42 + dy),
  sprintf('<text class="c7p-mono" x="%.1f" y="%g" font-size="13">%s</text>', c7p3_x(t) + 13, c7p3_base + 56 + dy, label))

# Kadr 1: rejestr awarii jako kartka z czterema wpisami i średnią.
c7p3_register <- function() {
  x0 <- 260; w <- 340; y0 <- 26; row <- 34
  fo <- c7pc_obs[c7pc_failed]; idx <- which(c7pc_failed)
  yr <- y0 + 92 + (seq_along(fo) - 1) * row
  c7p_svg(860, 350, "Rejestr awarii z czterema wpisami: 220, 480, 760 i 990 godzin; średnia 612.5 godziny.",
    sprintf('<rect class="c7p-paper" x="%g" y="%g" width="%g" height="%g" rx="4"/>', x0, y0, w, 298),
    sprintf('<text x="%g" y="%g" font-size="12" font-weight="700" class="c7p-subtle" style="letter-spacing:.06em">REJESTR AWARII</text>', x0 + 22, y0 + 28),
    sprintf('<text x="%g" y="%g" font-size="13" class="c7p-subtle">stan po 1200 h badania</text>', x0 + 22, y0 + 48),
    sprintf('<text x="%g" y="%g" font-size="13" font-weight="700">wentylator</text><text x="%g" y="%g" font-size="13" font-weight="700" text-anchor="end">czas do awarii</text>',
      x0 + 22, y0 + 76, x0 + w - 22, y0 + 76),
    sprintf('<line class="c7p-grid" x1="%g" x2="%g" y1="%g" y2="%g"/>', x0 + 22, x0 + w - 22, y0 + 84, y0 + 84),
    paste(sprintf('<text class="c7p-mono" x="%g" y="%g" font-size="16">nr %d</text><text class="c7p-mono" x="%g" y="%g" font-size="16" text-anchor="end">%d h</text>',
      x0 + 22, yr + 16, idx, x0 + w - 22, yr + 16, as.integer(fo)), collapse = ""),
    sprintf('<line class="c7p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x0 + 22, x0 + w - 22, y0 + 236, y0 + 236),
    sprintf('<text x="%g" y="%g" font-size="15" font-weight="700">średnia</text><text class="c7p-mono c7p-bad" x="%g" y="%g" font-size="18" font-weight="700" text-anchor="end">612.5 h</text>',
      x0 + 22, y0 + 268, x0 + w - 22, y0 + 268))
}

c7p3_svg <- function(step) {
  if (step == 1) return(c7p3_register())
  i <- seq_along(c7pc_times); y <- c7p3_y(i)
  rows <- paste0(
    paste(sprintf('<text class="c7p-tick" style="text-anchor:end" x="56" y="%g">%d</text>', y + 4, i), collapse = ""),
    # przyszłość działających (kadr 4): przerywana linia i wyblakła kreska awarii
    if (step == 4) paste(sprintf('<line class="c7p-life-ghost" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/><line class="c7pc-tick is-ghost" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
      c7p3_x(c7pc_end), c7p3_x(c7pc_times[!c7pc_failed]), y[!c7pc_failed], y[!c7pc_failed],
      c7p3_x(c7pc_times[!c7pc_failed]), c7p3_x(c7pc_times[!c7pc_failed]), y[!c7pc_failed] - 8, y[!c7pc_failed] + 8), collapse = "") else "",
    paste(sprintf('<line class="c7pc-life%s" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
      ifelse(c7pc_failed, "", " is-alive"), c7p3_x(0), c7p3_x(c7pc_obs), y, y), collapse = ""),
    paste(sprintf('<line class="c7pc-tick" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
      c7p3_x(c7pc_obs[c7pc_failed]), c7p3_x(c7pc_obs[c7pc_failed]), y[c7pc_failed] - 9, y[c7pc_failed] + 9), collapse = ""),
    paste(sprintf('<path class="c7pc-tri" d="M%.1f,%gl-11,-7v14z"/>', c7p3_x(c7pc_end) + 15, y[!c7pc_failed]), collapse = ""),
    # kadr 2: działające nie mają wpisu w rejestrze
    if (step == 2) paste(sprintf('<text class="c7p-mono c7p-subtle" x="%.1f" y="%g" font-size="13">brak wpisu w rejestrze</text>',
      c7p3_x(c7pc_end) + 24, y[!c7pc_failed] + 4), collapse = "") else "",
    # kadr 3: co wiemy o działających
    if (step == 3) paste(sprintf('<text class="c7p-mono c7p-ok" x="%.1f" y="%g" font-size="13">T &gt; 1200 h</text>',
      c7p3_x(c7pc_end) + 24, y[!c7pc_failed] + 4), collapse = "") else ""
  )
  aria <- c("",
    "Osiem linii życia; po 1200 godzinach cztery zakończone awarią, cztery nadal działają i nie mają wpisu w rejestrze.",
    "Cztery działające wentylatory z dopiskiem: czas życia dłuższy niż 1200 godzin.",
    "Pełne linie życia wszystkich ośmiu wentylatorów; prawdziwa średnia 1368.75 godziny.")[step]
  c7p_svg(860, 350, aria, rows,
    c7p_axis(c7p3_x, c7p3_base, seq(0, 3000, 500), "czas pracy (h)"),
    c7p_vmark(c7p3_x(c7pc_end), 16, c7p3_base, "koniec badania 1200 h"),
    c7p3_diamond(c7pc_naive, "", "średnia z rejestru 612.5 h"),
    if (step == 4) c7p3_diamond(c7pc_true, "is-true", "prawdziwa średnia 1368.75 h", dy = 22))
}

c7p3_frames <- list(
  list(name = "Rejestr awarii",
    text = "Po 1200 h badania analityk dostaje rejestr awarii. Są w nim cztery wpisy, więc liczy z nich średni czas życia wentylatora.",
    read = "(220 + 480 + 760 + 990) / 4 = 612.5 h"),
  list(name = "Czego nie ma w rejestrze",
    text = "Do badania weszło osiem wentylatorów. Cztery wciąż pracują (trójkąt) i dlatego nie mają wpisu: rejestr awarii zapisuje tylko to, co się zepsuło.",
    read = "awarie: 4 z 8; działa: 4 z 8"),
  list(name = "Co wiemy",
    text = "Każdy z pominiętych przepracował 1200 h i działa dalej. Średnia z rejestru jest krótsza niż czas pracy każdego z nich.",
    read = "cztery wentylatory: T > 1200 h"),
  list(name = "Podgląd przyszłości",
    text = "W symulacji możemy zajrzeć za koniec badania. Pominięte wentylatory zawiodłyby po 1350, 1750, 2300 i 3100 h.",
    read = "średnia z rejestru 612.5 h; prawdziwa średnia 1368.75 h")
)

c7p_3_ui <- tagList(c7pc_css,
  c7p_frames_ui("P3", "scena", "Badanie kończy się po 1200 h", c7p3_frames, c7p3_svg))

# ============================================================================
# PROTOKÓŁ BADANIA — ten sam dokument co rejestr awarii z kadru 1 sceny P3,
# ale wypełniony poprawnie: osiem wierszy, czas obserwacji i status (definicja 7.4).
# ============================================================================

c7p_protocol_svg <- function() {
  x0 <- 150; w <- 560; y0 <- 20; row <- 30
  n <- length(c7pc_times); yr <- y0 + 92 + (seq_len(n) - 1) * row
  col <- c(x0 + 22, x0 + 250, x0 + 280, x0 + w - 22)   # wentylator | czas (do prawej) | status | co wiemy (do prawej)
  status <- ifelse(c7pc_failed, "awaria", "cenzorowana")
  known <- ifelse(c7pc_failed, sprintf("T = %d h", as.integer(c7pc_obs)), sprintf("T &gt; %d h", as.integer(c7pc_end)))
  yb <- y0 + 92 + n * row
  c7p_svg(860, yb + 78,
    "Protokół badania ośmiu wentylatorów po 1200 godzinach: cztery wiersze z awarią po 220, 480, 760 i 990 godzinach, cztery cenzorowane w 1200 godzinach; łącznie 7250 godzin pracy i 4 awarie.",
    sprintf('<rect class="c7p-paper" x="%g" y="%g" width="%g" height="%g" rx="4"/>', x0, y0, w, yb + 46),
    sprintf('<text x="%g" y="%g" font-size="12" font-weight="700" class="c7p-subtle" style="letter-spacing:.06em">PROTOKÓŁ BADANIA</text>', col[1], y0 + 28),
    sprintf('<text x="%g" y="%g" font-size="13" class="c7p-subtle">stan po 1200 h badania, wszystkie egzemplarze</text>', col[1], y0 + 48),
    sprintf('<text x="%g" y="%g" font-size="13" font-weight="700">wentylator</text><text x="%g" y="%g" font-size="13" font-weight="700" text-anchor="end">czas obserwacji</text><text x="%g" y="%g" font-size="13" font-weight="700">status</text><text x="%g" y="%g" font-size="13" font-weight="700" text-anchor="end">co wiemy</text>',
      col[1], y0 + 76, col[2], y0 + 76, col[3], y0 + 76, col[4], y0 + 76),
    sprintf('<line class="c7p-grid" x1="%g" x2="%g" y1="%g" y2="%g"/>', col[1], col[4], y0 + 84, y0 + 84),
    paste(sprintf('<text class="c7p-mono" x="%g" y="%g" font-size="16">nr %d</text><text class="c7p-mono" x="%g" y="%g" font-size="16" text-anchor="end">%d h</text><text class="c7p-mono %s" x="%g" y="%g" font-size="15">%s</text><text class="c7p-mono" x="%g" y="%g" font-size="15" text-anchor="end">%s</text>',
      col[1], yr + 16, seq_len(n), col[2], yr + 16, as.integer(c7pc_obs),
      ifelse(c7pc_failed, "c7p-bad", "c7p-ok"), col[3], yr + 16, status, col[4], yr + 16, known), collapse = ""),
    sprintf('<line class="c7p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', col[1], col[4], yb + 6, yb + 6),
    sprintf('<text x="%g" y="%g" font-size="15" font-weight="700">łącznie</text><text class="c7p-mono" x="%g" y="%g" font-size="18" font-weight="700" text-anchor="end">%d h</text><text class="c7p-mono c7p-bad" x="%g" y="%g" font-size="15" font-weight="700">4 awarie</text>',
      col[1], yb + 36, col[2], yb + 36, as.integer(c7pc_total), col[3], yb + 36))
}

c7p_protocol_ui <- figure_panel("Zapis danych", title = "Ten sam dokument, wypełniony poprawnie", width_mode = "wide",
  c7pc_css, c7p_css, c7p_protocol_svg(),
  tags$p("Rejestr awarii miał cztery wiersze. Protokół ma osiem: każdy wentylator dostaje parę „czas obserwacji i status”. Cztery pierwsze czasy to czasy życia, cztery ostatnie to tylko dolne granice. W podsumowaniu nie ma średniej, są dwie liczby, których użyjemy dalej: łączny czas pracy i liczba awarii."))

# ============================================================================
# GALERIA „KIEDY WZÓR (7.2) TRAFIA” — trzy parki o tej samej średniej 1500 h,
# to samo badanie przerwane po 1200 h. Ryzyko stałe: szacunek trafia; ryzyko
# rosnące (zużycie): zawyża; ryzyko malejące (wczesne wady): zaniża.
# Parki to kwantyle modeli (c7p_park), więc liczby są powtarzalne.
# ============================================================================

c7pb_n <- 40
c7pb_models <- list(
  list(name = "Ryzyko stałe", sub = "wiek nie ma znaczenia", beta = 1, eta = c7p_mttf),
  list(name = "Ryzyko rośnie z wiekiem", sub = "zużycie", beta = c7p_beta, eta = c7p_eta),
  list(name = "Ryzyko maleje z wiekiem", sub = "wczesne wady", beta = 0.7, eta = c7p_mttf / gamma(1 + 1 / 0.7))
)
c7pb_calc <- lapply(c7pb_models, function(m) {
  t <- c7p_park(c7pb_n, m$beta, m$eta)
  d <- sum(t <= c7pc_end)
  c(m, list(t = t, d = d, est = sum(pmin(t, c7pc_end)) / d))
})
c7pb_est <- vapply(c7pb_calc, function(m) round(m$est / 10) * 10, numeric(1))   # do tekstu: ok. 1500, 2600, 1080 h

c7pb_css <- tags$style(HTML("
  .c7pb-life { stroke: var(--upwr-secondary); stroke-width: 1.8; fill: none; }
  .c7pb-life.is-alive { stroke: var(--upwr-sage); }
  .c7pb-future { stroke: var(--upwr-ink-subtle); stroke-width: 1; stroke-dasharray: 2 3; fill: none; opacity: .6; }
  .c7pb-true { stroke: var(--upwr-ink); stroke-width: 1.4; stroke-dasharray: 5 4; fill: none; }
"))

c7pb_svg <- function() {
  xmax <- 4000; x <- function(t) 250 + pmin(t, xmax) / xmax * 580
  rowh <- 150; top <- 34; step <- 2.5
  rows <- vapply(seq_along(c7pb_calc), function(k) {
    m <- c7pb_calc[[k]]; y0 <- top + (k - 1) * rowh
    y <- y0 + 6 + (seq_len(c7pb_n) - 1) * step; base <- y0 + 6 + c7pb_n * step + 4
    failed <- m$t <= c7pc_end; obs <- pmin(m$t, c7pc_end)
    verdict <- c("trafia", "zawyża", "zaniża")[k]
    paste0(
      sprintf('<text x="20" y="%g" font-size="15" font-weight="700">%s</text>', y0 + 22, m$name),
      sprintf('<text class="c7p-subtle" x="20" y="%g" font-size="13">%s</text>', y0 + 42, m$sub),
      sprintf('<text class="c7p-mono" x="20" y="%g" font-size="13">awarie: %d z %d</text>', y0 + 70, m$d, c7pb_n),
      sprintf('<text class="c7p-mono" x="20" y="%g" font-size="13" font-weight="700">1/λ̂ = %s h</text>', y0 + 90, c7p_num(c7pb_est[[k]], 0)),
      sprintf('<text class="c7p-mono %s" x="20" y="%g" font-size="13" font-weight="700">%s</text>', if (k == 1) "c7p-ok" else "c7p-bad", y0 + 110, verdict),
      paste(sprintf('<line class="c7pb-future" x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f"/>', x(c7pc_end), x(m$t[!failed]), y[!failed], y[!failed]), collapse = ""),
      paste(sprintf('<line class="c7pb-life%s" x1="%.1f" x2="%.1f" y1="%.1f" y2="%.1f"/>', ifelse(failed, "", " is-alive"), x(0), x(obs), y, y), collapse = ""),
      sprintf('<line class="c7p-axis" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>', x(0), x(xmax), base, base),
      sprintf('<path class="c7pc-mark is-est" d="M%.1f,%gl7,8l-7,8l-7,-8z"/>', x(c7pb_est[[k]]), base + 4)
    )
  }, "")
  bottom <- top + length(c7pb_calc) * rowh
  ticks <- seq(0, xmax, 1000)
  c7p_svg(860, bottom + 30,
    sprintf("Trzy parki po %d wentylatorów o średnim czasie życia 1500 godzin, badanie przerwane po 1200 godzinach. Szacunek jeden przez lambda z daszkiem: około %s, %s i %s godzin.",
      c7pb_n, c7p_num(c7pb_est[[1]], 0), c7p_num(c7pb_est[[2]], 0), c7p_num(c7pb_est[[3]], 0)),
    sprintf('<line class="c7p-limit" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/><text class="c7p-mono" x="%.1f" y="%g" font-size="13" text-anchor="end">koniec badania 1200 h</text>',
      x(c7pc_end), x(c7pc_end), top - 8, bottom - 12, x(c7pc_end) - 6, top - 14),
    sprintf('<line class="c7pb-true" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/><text class="c7p-mono" x="%.1f" y="%g" font-size="13">prawdziwa średnia 1500 h</text>',
      x(c7p_mttf), x(c7p_mttf), top - 8, bottom - 12, x(c7p_mttf) + 6, top - 14),
    rows,
    paste(sprintf('<text class="c7p-tick" x="%.1f" y="%g">%d</text>', x(ticks), bottom + 6, as.integer(ticks)), collapse = ""),
    sprintf('<text class="c7p-subtle" x="%.1f" y="%g" font-size="12" text-anchor="end">czas pracy (h)</text>', x(xmax), bottom + 24))
}

c7p_bias_ui <- figure_panel("Galeria", title = "Kiedy szacunek \\(1/\\hat\\lambda\\) trafia, a kiedy nie", width_mode = "wide",
  c7pc_css, c7p_css, c7pb_css, c7pb_svg(),
  tags$p("Trzy parki po 40 wentylatorów mają ten sam średni czas życia, 1500 h, i to samo badanie przerwane po 1200 h. Pełna linia to czas pracy widziany w badaniu, przerywana to dalsze życie, którego badanie nie widzi. Niebieski znacznik pokazuje szacunek \\(1/\\hat\\lambda\\) ze wzoru (7.2)."))

# ============================================================================
# P5 — POJĘCIE „ŁĄCZNY CZAS PRACY”
# ============================================================================

c7p5_x <- function(t) 40 + t / 7250 * 780   # skala taśmy: 0–7250 h

# Taśma: odcinki czasu pracy sklejone jeden za drugim; kreska awarii na końcu
# odcinka zakończonego awarią.
c7p5_tape <- function(obs, failed, y, h = 30) {
  start <- cumsum(c(0, head(obs, -1))); end <- start + obs
  paste0(
    paste(sprintf('<rect class="c7pc-seg%s" x="%.1f" y="%g" width="%.1f" height="%g"/>',
      ifelse(failed, "", " is-alive"), c7p5_x(start), y, c7p5_x(end) - c7p5_x(start), h), collapse = ""),
    paste(sprintf('<line class="c7pc-tick" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
      c7p5_x(end[failed]), c7p5_x(end[failed]), y - 6, y + h + 6), collapse = ""),
    paste(sprintf('<text class="c7p-mono c7p-subtle" x="%.1f" y="%g" font-size="11" text-anchor="middle">%d</text>',
      c7p5_x((start + end) / 2), y + h + 20, as.integer(obs)), collapse = ""))
}

# Podział taśmy długości total na d równych części.
c7p5_parts <- function(total, d, y, h = 30, digits = 1) {
  cut <- total / d * seq_len(d - 1); mid <- total / d * (seq_len(d) - 0.5)
  paste0(
    paste(sprintf('<line class="c7pc-cut" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>', c7p5_x(cut), c7p5_x(cut), y - 26, y + h + 4), collapse = ""),
    paste(sprintf('<text class="c7p-mono" x="%.1f" y="%g" font-size="13" font-weight="700" text-anchor="middle">%s h</text>',
      c7p5_x(mid), y - 12, c7p_num(total / d, digits)), collapse = ""))
}

c7p5_svg <- function(step) {
  if (step == 1) {
    i <- seq_along(c7pc_times); y <- c7p3_y(i)
    return(c7p_svg(860, 300, "Osiem linii życia do końca badania po 1200 godzinach, z czasem pracy każdego wentylatora.",
      paste(sprintf('<text class="c7p-tick" style="text-anchor:end" x="56" y="%g">%d</text>', y + 4, i), collapse = ""),
      paste(sprintf('<line class="c7pc-life%s" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
        ifelse(c7pc_failed, "", " is-alive"), c7p3_x(0), c7p3_x(c7pc_obs), y, y), collapse = ""),
      paste(sprintf('<line class="c7pc-tick" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
        c7p3_x(c7pc_obs[c7pc_failed]), c7p3_x(c7pc_obs[c7pc_failed]), y[c7pc_failed] - 9, y[c7pc_failed] + 9), collapse = ""),
      paste(sprintf('<path class="c7pc-tri" d="M%.1f,%gl-11,-7v14z"/>', c7p3_x(c7pc_end) + 15, y[!c7pc_failed]), collapse = ""),
      paste(sprintf('<text class="c7p-mono" x="%.1f" y="%g" font-size="13">%d h</text>',
        c7p3_x(c7pc_obs) + ifelse(c7pc_failed, 10, 24), y + 4, as.integer(c7pc_obs)), collapse = ""),
      c7p_axis(c7p3_x, c7p3_base, seq(0, 3000, 500), "czas pracy (h)")))
  }
  y1 <- 70; y2 <- 190
  fo <- c7pc_obs[c7pc_failed]
  aria <- c("", "Czasy pracy ośmiu wentylatorów sklejone w jedną taśmę długości 7250 godzin z czterema awariami.",
    "Taśma 7250 godzin podzielona na osiem równych części po około 906 godzin, po jednej na wentylator.",
    "Taśma 7250 godzin podzielona na cztery równe części po 1812.5 godziny, po jednej na awarię.",
    "Dwie taśmy: wszystkie godziny pracy (7250) i same godziny egzemplarzy z awarią (2450), każda podzielona przez cztery awarie.")[step]
  c7p_svg(860, if (step == 5) 262 else 150, aria,
    sprintf('<text x="40" y="26" font-size="14" font-weight="700">Wszystkie godziny pracy: 7250 h, 8 wentylatorów, 4 awarie</text>'),
    c7p5_tape(c7pc_obs, c7pc_failed, y1),
    if (step == 3) c7p5_parts(c7pc_total, 8, y1, digits = 0),
    if (step >= 4) c7p5_parts(c7pc_total, 4, y1),
    if (step == 5) paste0(
      '<text x="40" y="160" font-size="14" font-weight="700">Tylko egzemplarze z awarią: 2450 h, 4 awarie</text>',
      c7p5_tape(fo, rep(TRUE, 4), y2),
      sprintf('<text class="c7p-mono c7p-bad" x="%.1f" y="%g" font-size="13" font-weight="700">2450 / 4 = 612.5 h</text>', c7p5_x(2450) + 16, y2 + 20)))
}

c7p5_frames <- list(
  list(name = "Osiem linii",
    text = "Każdy wentylator przepracował w badaniu pewną liczbę godzin: do awarii albo do końca badania.",
    read = "220 + 480 + 760 + 990 + 4 · 1200 = 7250 h"),
  list(name = "Jedna taśma",
    text = "Sklejamy wszystkie godziny pracy w jedną taśmę. Kreska to awaria; zielone odcinki należą do wentylatorów, które nie zawiodły.",
    read = "7250 h pracy; 4 awarie"),
  list(name = "Godziny na wentylator",
    text = "Pierwsza poprawka dzieli taśmę między wszystkie osiem wentylatorów, jakby działające zawiodły w chwili zakończenia badania. Każdy z nich żył dłużej, więc to pewna dolna granica.",
    read = "7250 / 8 ≈ 906 h (dolna granica)"),
  list(name = "Godziny na awarię",
    text = "Druga poprawka dzieli tę samą taśmę przez liczbę awarii. Jeśli ryzyko awarii nie zależy od wieku, każda godzina pracy jest tak samo narażona, a na jedną awarię przypada ćwierć taśmy.",
    read = "1/λ̂ = 7250 / 4 = 1812.5 h"),
  list(name = "Bez działających",
    text = "Średnia z rejestru awarii używa tych samych czterech awarii, ale krótszej taśmy: wycina godziny wentylatorów, które przetrwały.",
    read = "rejestr 612.5 h; dolna granica 906 h; 1/λ̂ = 1812.5 h")
)

c7p_5_ui <- tagList(c7pc_css,
  c7p_frames_ui("P5", "pojęcie", "Łączny czas pracy", c7p5_frames, c7p5_svg))
