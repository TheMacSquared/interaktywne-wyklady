# PUNKTY STOP (2026-11-03) — taśma kontroli: dwaj audytorzy i trzy reguły ----------
#
# Kadry sceny P1 rysuje R z góry (d5p_frames_ui), przełącza je modules/sceny.js.
# Wspólna taśma d5p_shared_tape: 50 kontroli palet, wady w kontrolach 7, 15, 24, 38, 44.
#   d5p_rule_ui   — Scena „Dwóch audytorów” (kadry historii) (P1): Wstecz / Dalej
#   d5p_close_ui  — Domknięcie „Trzy reguły, jedna taśma” (P12): statyczne, bez przycisków
# Bez serwera: P1 przełącza kadry w przeglądarce, P12 jest statyczne.
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css, d5p_svg, d5p_tape, d5p_axis).

# Stała taśma: TRUE = wada w tej kontroli (wpisana w kod, żeby narracja zgadzała się z obrazem).
d5p_shared_tape <- seq_len(50) %in% c(7L, 15L, 24L, 38L, 44L)
d5p_tape_n <- 50L
# Kadr 5 sceny P1: ta sama taśma przedłużona do 60 kontroli, szósta wada w 57.
d5p_long_tape <- seq_len(60) %in% c(7L, 15L, 24L, 38L, 44L, 57L)
d5p_sixth_wad <- which(cumsum(d5p_long_tape) == 6L)[1]

# Parametry świata: Bananpol, p = 0.10 wady na kontrolę, cel r = 3 wady.
d5p_p <- 0.10
d5p_r <- 3L

# Liczby używane w tekście — liczone z taśmy i z rozkładów, nie wpisywane ręcznie.
d5p_wads_50 <- sum(d5p_shared_tape[seq_len(d5p_tape_n)])          # Y po 50 kontrolach
d5p_first_wad <- which(d5p_shared_tape)[1]                           # X: pierwsza wada
d5p_third_wad <- which(cumsum(d5p_shared_tape) == d5p_r)[1]          # X: trzecia wada
d5p_plan_95 <- qnbinom(0.95, size = d5p_r, prob = d5p_p) + d5p_r     # plan 95%: liczba kontroli

# Jeden wiersz taśmy ze zatrzymaniem po kontroli `stop`: kratki po zatrzymaniu
# przygaszone, linia d5p-limit za zatrzymaniem z etykietą. stop = NULL — bez zatrzymania.
d5p_stop_row <- function(y, stop = NULL, x0, pitch, size, n_cells = d5p_tape_n, limit_label = NULL,
                         tape_outcomes = d5p_shared_tape) {
  dim <- if (is.null(stop)) NULL else seq_len(n_cells) > stop
  tape <- d5p_tape(tape_outcomes, x0, y, pitch = pitch, size = size, n_cells = n_cells, dim = dim)
  if (is.null(stop)) return(tape)
  xl <- x0 + stop * pitch - (pitch - size) / 2
  paste0(tape,
    sprintf('<line class="d5p-limit" x1="%g" x2="%g" y1="%g" y2="%g"/>', xl, xl, y - 8, y + size + 8),
    sprintf('<text class="d5p-mono d5p-subtle" x="%g" y="%g" text-anchor="middle" font-size="13">%s</text>',
      xl, y - 14, limit_label))
}

# Scena: wiersz z etykietą po lewej (A, B) i podziałką numerów co 10 kontroli.
d5p_rule_x0 <- 60
d5p_rule_pitch <- 12  # 60 kratek kadru 5 mieści się w 800 px
d5p_rule_size <- 10

d5p_rule_row <- function(y, stop = NULL, label = NULL, limit_label = NULL,
                         n_cells = d5p_tape_n, tape_outcomes = d5p_shared_tape) {
  row <- d5p_stop_row(y, stop = stop, x0 = d5p_rule_x0, pitch = d5p_rule_pitch,
    size = d5p_rule_size, n_cells = n_cells, limit_label = limit_label, tape_outcomes = tape_outcomes)
  lab <- if (is.null(label)) "" else sprintf(
    '<text x="0" y="%g" font-size="16" font-weight="700">%s</text>', y + d5p_rule_size - 1, label)
  paste0(lab, row)
}

d5p_rule_axis <- function(y, max_n = d5p_tape_n) {
  d5p_axis(function(i) d5p_rule_x0 + (i - 1) * d5p_rule_pitch + d5p_rule_size / 2,
    y, seq(10, max_n, 10))
}

d5p_rule_frames <- list(
  list(name = "Ten sam protokół",
    text = "Taśma to 50 kontroli palet z Bananpolu. Dwóch audytorów czyta ten sam protokół. Wada to kratka w kolorze akcentu.",
    read = NULL),
  list(name = "Audytor A",
    text = "Audytor A ma zlecenie: skontrolować 50 palet. Kończy po pięćdziesiątej kontroli, bez względu na liczbę wad.",
    read = sprintf("n = 50 ustalone · wad: Y = %d", d5p_wads_50)),
  list(name = "Audytor B",
    text = "Audytor B ma inne zlecenie: znaleźć trzy wady. Kończy w chwili trzeciej wady, a kolejne kontrole się nie odbywają.",
    read = sprintf("r = %d ustalone · kontroli: X = %d", d5p_r, d5p_third_wad)),
  list(name = "Co jest losowe",
    text = "Te same kontrole, inna chwila zatrzymania. A z góry zna liczbę kontroli, a liczbę wad pokaże dopiero protokół. B z góry zna liczbę wad, a nie wie, ile kontroli to zajmie.",
    read = "A: losowa liczba wad · B: losowa liczba kontroli"),
  list(name = "Gdy B potrzebuje więcej",
    text = "Zmieńmy zlecenie B: sześć wad zamiast trzech. W 50 kontrolach A jest ich tylko pięć, więc B kontroluje dalej, niż sięga cały protokół A. Reguła „do r-tej wady” nie ma z góry ustalonej granicy liczby kontroli.",
    read = sprintf("A: n = 50 · Y = %d · B: r = 6 · X = %d", d5p_wads_50, d5p_sixth_wad))
)

d5p_rule_svg <- function(step) {
  body <- switch(step,
    paste0(d5p_rule_row(80), d5p_rule_axis(111)),
    paste0(d5p_rule_row(80, stop = d5p_tape_n, limit_label = "stop: 50"), d5p_rule_axis(111)),
    paste0(d5p_rule_row(80, stop = d5p_third_wad, label = "B", limit_label = sprintf("stop: %d", d5p_third_wad)),
      d5p_rule_axis(111)),
    paste0(d5p_rule_row(60, stop = d5p_tape_n, label = "A", limit_label = "stop: 50"),
      d5p_rule_row(130, stop = d5p_third_wad, label = "B", limit_label = sprintf("stop: %d", d5p_third_wad)),
      d5p_rule_axis(161)),
    paste0(d5p_rule_row(60, stop = d5p_tape_n, label = "A", limit_label = "stop: 50"),
      d5p_rule_row(130, stop = d5p_sixth_wad, label = "B", n_cells = length(d5p_long_tape),
        tape_outcomes = d5p_long_tape, limit_label = sprintf("stop: %d", d5p_sixth_wad)),
      d5p_rule_axis(161, max_n = length(d5p_long_tape)))
  )
  aria <- switch(step,
    "Taśma 50 kontroli palet z pięcioma wadami, wspólna dla obu audytorów.",
    "Taśma 50 kontroli z pionową linią po pięćdziesiątej kontroli.",
    "Taśma kontroli zatrzymana po trzeciej wadzie, przy 24. kontroli.",
    "Dwie taśmy: linia A po 50. kontroli, linia B po 24. kontroli.",
    sprintf("Dwie taśmy: A kończy po 50. kontroli z pięcioma wadami, B czeka na szóstą wadę do %d. kontroli.", d5p_sixth_wad))
  d5p_svg(800, 185, aria, body)
}

# Domknięcie: trzy wiersze tej samej taśmy (61 kratek: 50 kontroli + plan 95%).
d5p_close_svg <- function() {
  x0 <- 175; pitch <- 10; size <- 8; n_cells <- d5p_plan_95
  rules <- list(
    list(y = 30, stop = d5p_tape_n, title = "Stop po n = 50", sub = "dwumianowy",
      read = sprintf("Y = %d · E(Y) = np = %g", d5p_wads_50, d5p_tape_n * d5p_p)),
    list(y = 110, stop = d5p_first_wad, title = sprintf("Stop: %d. wada", 1L), sub = "geometryczny",
      read = sprintf("X = %d · E(X) = 1/p = %g", d5p_first_wad, 1 / d5p_p)),
    list(y = 190, stop = d5p_third_wad, title = sprintf("Stop: %d. wada", d5p_r), sub = "ujemny dwumianowy",
      read = sprintf("X = %d · E(X) = r/p = %g · plan 95%%: %d", d5p_third_wad, d5p_r / d5p_p, d5p_plan_95))
  )
  rows <- vapply(rules, function(rl) {
    paste0(
      sprintf('<text x="0" y="%g" font-size="14" font-weight="600">%s</text>', rl$y + size - 1, rl$title),
      sprintf('<text class="d5p-subtle" x="0" y="%g" font-size="12">%s</text>', rl$y + size + 12, rl$sub),
      d5p_stop_row(rl$y, stop = rl$stop, x0 = x0, pitch = pitch, size = size,
        n_cells = n_cells, limit_label = as.character(rl$stop)),
      sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="13">%s</text>', x0, rl$y + size + 26, rl$read)
    )
  }, character(1))
  # znacznik planu 95% nad kratką 61. w trzecim wierszu
  plan_x <- x0 + (d5p_plan_95 - 1) * pitch + size / 2
  plan_mark <- sprintf('<text class="d5p-mono d5p-subtle" x="%g" y="%g" text-anchor="middle" font-size="11">plan 95%%</text>',
    plan_x, rules[[3]]$y - 14)
  d5p_svg(800, 245, "Trzy reguły zatrzymania na tej samej taśmie 50 kontroli.",
    rows, plan_mark)
}

d5p_rule_ui <- d5p_frames_ui("Scena", "Dwóch audytorów", d5p_rule_frames, d5p_rule_svg, d5p_frame_body)

d5p_close_ui <- d5p_panel("Domknięcie", "Trzy reguły, jedna taśma",
  d5p_css,
  tags$p(class = "d5p-caption",
    "Ta sama taśma, trzy momenty zatrzymania. Kratki po zatrzymaniu są przygaszone; linia pokazuje moment zatrzymania."),
  d5p_close_svg()
)

