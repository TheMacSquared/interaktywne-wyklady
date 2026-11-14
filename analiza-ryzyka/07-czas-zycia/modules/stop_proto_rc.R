# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rc (rozdz. 2, cenzorowanie) ---------
#
#   c7p_3_ui — Scena „Badanie kończy się po 1200 h” (P3), 4 kadry policzone w R:
#              koniec badania → analityk pomija działające → co o nich wiemy →
#              podgląd przyszłości. Liczby z tekstu: 612.5 h i 1368.75 h.
#   c7p_5_ui — Pojęcie „Łączny czas pracy” (P5), 4 kadry: osiem linii → jedna
#              taśma 7250 h → jedna awaria na 1812.5 h → taśma bez działających
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

c7p3_svg <- function(step) {
  i <- seq_along(c7pc_times); y <- c7p3_y(i)
  dim_alive <- step == 2
  rows <- paste0(
    paste(sprintf('<text class="c7p-tick" style="text-anchor:end" x="56" y="%g">%d</text>', y + 4, i), collapse = ""),
    # przyszłość działających (kadr 4): przerywana linia i wyblakła kreska awarii
    if (step == 4) paste(sprintf('<line class="c7p-life-ghost" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/><line class="c7pc-tick is-ghost" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
      c7p3_x(c7pc_end), c7p3_x(c7pc_times[!c7pc_failed]), y[!c7pc_failed], y[!c7pc_failed],
      c7p3_x(c7pc_times[!c7pc_failed]), c7p3_x(c7pc_times[!c7pc_failed]), y[!c7pc_failed] - 8, y[!c7pc_failed] + 8), collapse = "") else "",
    paste(sprintf('<line class="c7pc-life%s" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
      ifelse(c7pc_failed, "", if (dim_alive) " is-alive is-dim" else " is-alive"), c7p3_x(0), c7p3_x(c7pc_obs), y, y), collapse = ""),
    paste(sprintf('<line class="c7pc-tick" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
      c7p3_x(c7pc_obs[c7pc_failed]), c7p3_x(c7pc_obs[c7pc_failed]), y[c7pc_failed] - 9, y[c7pc_failed] + 9), collapse = ""),
    paste(sprintf('<path class="c7pc-tri%s" d="M%.1f,%gl-11,-7v14z"/>', if (dim_alive) " is-dim" else "",
      c7p3_x(c7pc_end) + 15, y[!c7pc_failed]), collapse = ""),
    # kadr 2: działające przekreślone („bez wyniku”)
    if (step == 2) paste(sprintf('<line class="c7pc-cross" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>',
      c7p3_x(0), c7p3_x(c7pc_end) + 16, y[!c7pc_failed], y[!c7pc_failed]), collapse = "") else "",
    # kadr 3: co wiemy o działających
    if (step == 3) paste(sprintf('<text class="c7p-mono c7p-ok" x="%.1f" y="%g" font-size="13">T &gt; 1200 h</text>',
      c7p3_x(c7pc_end) + 24, y[!c7pc_failed] + 4), collapse = "") else ""
  )
  aria <- c("Osiem linii życia; po 1200 godzinach cztery zakończone awarią, cztery nadal działają.",
    "Cztery działające wentylatory przekreślone; średnia z czterech awarii 612.5 godziny.",
    "Cztery działające wentylatory z dopiskiem: czas życia dłuższy niż 1200 godzin.",
    "Pełne linie życia wszystkich ośmiu wentylatorów; prawdziwa średnia 1368.75 godziny.")[step]
  c7p_svg(860, 350, aria, rows,
    c7p_axis(c7p3_x, c7p3_base, seq(0, 3000, 500), "czas pracy (h)"),
    c7p_vmark(c7p3_x(c7pc_end), 16, c7p3_base, "koniec badania 1200 h"),
    if (step >= 2) c7p3_diamond(c7pc_naive, "", "średnia z awarii 612.5 h"),
    if (step == 4) c7p3_diamond(c7pc_true, "is-true", "prawdziwa średnia 1368.75 h", dy = 22))
}

c7p3_frames <- list(
  list(name = "Koniec badania",
    text = "Osiem wentylatorów uruchomiono jednocześnie. Po 1200 h badanie się kończy: cztery zawiodły (kreska), cztery wciąż pracują (trójkąt).",
    read = "awarie: 220, 480, 760 i 990 h · działa: 4 z 8"),
  list(name = "Tylko awarie",
    text = "Analityk liczy średni czas życia z tego, co się zepsuło. Działające egzemplarze pomija, bo nie mają wyniku.",
    read = "(220 + 480 + 760 + 990) / 4 = 612.5 h"),
  list(name = "Co wiemy",
    text = "Każdy z pominiętych przepracował 1200 h i działa dalej. Wyliczona średnia jest krótsza niż czas pracy każdego z nich.",
    read = "cztery wentylatory: T > 1200 h"),
  list(name = "Podgląd przyszłości",
    text = "W symulacji możemy zajrzeć za koniec badania. Pominięte wentylatory zawiodłyby po 1350, 1750, 2300 i 3100 h.",
    read = "średnia z awarii 612.5 h · prawdziwa średnia 1368.75 h")
)

c7p_3_ui <- tagList(c7pc_css,
  c7p_frames_ui("P3", "scena", "Badanie kończy się po 1200 h", c7p3_frames, c7p3_svg))

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
c7p5_parts <- function(total, d, y, h = 30) {
  cut <- total / d * seq_len(d - 1); mid <- total / d * (seq_len(d) - 0.5)
  paste0(
    paste(sprintf('<line class="c7pc-cut" x1="%.1f" x2="%.1f" y1="%g" y2="%g"/>', c7p5_x(cut), c7p5_x(cut), y - 26, y + h + 4), collapse = ""),
    paste(sprintf('<text class="c7p-mono" x="%.1f" y="%g" font-size="13" font-weight="700" text-anchor="middle">%s h</text>',
      c7p5_x(mid), y - 12, c7p_num(total / d, 1)), collapse = ""))
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
    "Taśma 7250 godzin podzielona na cztery równe części po 1812.5 godziny.",
    "Dwie taśmy: wszystkie godziny pracy (7250) i same godziny egzemplarzy z awarią (2450), każda podzielona przez cztery awarie.")[step]
  c7p_svg(860, if (step == 4) 262 else 150, aria,
    sprintf('<text x="40" y="26" font-size="14" font-weight="700">Wszystkie godziny pracy: 7250 h, 4 awarie</text>'),
    c7p5_tape(c7pc_obs, c7pc_failed, y1),
    if (step >= 3) c7p5_parts(c7pc_total, 4, y1),
    if (step == 4) paste0(
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
    read = "7250 h pracy · 4 awarie"),
  list(name = "Godziny na awarię",
    text = "Przy stałym hazardzie każda godzina pracy jest tak samo narażona. Na jedną awarię przypada ćwierć taśmy.",
    read = "MTTF ≈ 7250 / 4 = 1812.5 h"),
  list(name = "Bez działających",
    text = "Średnia z samych awarii używa tych samych czterech awarii, ale krótszej taśmy: wycina godziny wentylatorów, które przetrwały.",
    read = "2450 / 4 = 612.5 h wobec 7250 / 4 = 1812.5 h")
)

c7p_5_ui <- tagList(c7pc_css,
  c7p_frames_ui("P5", "pojęcie", "Łączny czas pracy", c7p5_frames, c7p5_svg))
