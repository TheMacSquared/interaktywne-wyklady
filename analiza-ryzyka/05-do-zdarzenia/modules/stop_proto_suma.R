# PUNKTY STOP (2026-11-03) — pojęcia: suma czasów oczekiwania i układy wad ------
#
# Kadry rysuje R z góry (d5p_frames_ui), przełącza je modules/sceny.js.
#   d5p_one_ui — Pojęcie „Jeden audyt” (P5a): skąd wzór (5.5), 2 kadry krokowe
#   d5p_sum_ui — Pojęcie „Trzy kolejki czekania” (P5), 3 kadry krokowe
# Rysunki: d5p_sum_svg(step) i d5p_one_svg(step) zwracają HTML (SVG), bez serwera.
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css, d5p_svg, d5p_tape, d5p_frames_ui, d5p_axis).

# Wzór w odczycie; kolory części jak we wzorze (5.5): ułożenia, wady, kontrole bez wady.
# Wzór w zwykłym HTML (bez MathJax): sceny wstawiają go w przeglądarce, a wersja
# offline nie ma MathJax, więc TeX zostałby surowym tekstem. Styl: d5p-math w d5p_css.
# Składany jako jeden ciąg bez białych znaków (htmltools wcięłoby tagi, a w tekście
# wcięcia stają się spacjami, np. „0.1 ³”).
d5p_col <- function(col, html) sprintf('<span style="color:var(--upwr-cat-%s)">%s</span>', col, html)
d5p_binom <- function(n, k) sprintf('<span class="d5p-binom">(<span class="d5p-binom-in"><span>%s</span><span>%s</span></span>)</span>', n, k)

# ============================================================================
# P5 — TRZY KOLEJKI CZEKANIA
# ============================================================================

# Stała seria: wady w kontrolach 8, 21, 27. Kolejki G = (8, 13, 6), X = 27.
d5p_sum_outcomes <- seq_len(27) %in% c(8L, 21L, 27L)
d5p_sum_G <- c(8L, 13L, 6L)
d5p_sum_names <- c("G₁", "G₂", "G₃")
d5p_sum_x0 <- 40
d5p_sum_pitch <- 24
d5p_sum_size <- 18
d5p_sum_xl <- function(i) d5p_sum_x0 + (i - 1) * d5p_sum_pitch
d5p_sum_xr <- function(i) d5p_sum_xl(i) + d5p_sum_size
d5p_sum_xc <- function(i) d5p_sum_xl(i) + d5p_sum_size / 2

# Kadr „Cały audyt” pokazuje teraz P5a przy wzorze (5.5); P5 zaczyna od kolejek,
# a d5p_sum_svg() dostaje numer kadru przesunięty o 1.
d5p_sum_frames <- list(
  list(name = "Trzy kolejki",
    text = "Ten sam audyt co przy wzorze (5.5), tym razem podzielony na trzy odcinki: od początku do pierwszej wady, potem do drugiej i do trzeciej. Każdy kończy się wadą.",
    read = "X = G₁ + G₂ + G₃ = 8 + 13 + 6 = 27"),
  list(name = "Od nowa",
    text = "Po każdej wadzie audyt zaczyna się od nowa, jak stoper z międzyczasami, który zeruje się i liczy od początku. Każda kolejka to oczekiwanie na pierwszą wadę.",
    read = ""),
  list(name = "Średnie się dodają",
    text = "Każda kolejka czeka na wadę średnio 1/p = 10 kontroli. Średnia całości to suma trzech średnich.",
    read = "E(X) = 10 + 10 + 10 = r/p = 30")
)

d5p_sum_svg <- function(step) {
  W <- 760
  y0 <- 50
  ends <- cumsum(d5p_sum_G)
  starts <- c(1L, ends[-length(ends)] + 1L)
  if (step == 1) {
    return(d5p_svg(W, 140, "Taśma 27 kontroli z wadami w kontrolach 8, 21 i 27.",
      d5p_tape(d5p_sum_outcomes, d5p_sum_x0, y0, d5p_sum_pitch, d5p_sum_size),
      d5p_axis(d5p_sum_xc, 96, c(1L, 8L, 21L, 27L), label = "nr kontroli")))
  }
  if (step == 2) {
    brackets <- vapply(seq_along(d5p_sum_G), function(k) {
      l <- d5p_sum_xl(starts[k]) + 2
      r <- d5p_sum_xr(ends[k]) - 2
      paste0(
        sprintf('<path class="d5p-axis" fill="none" d="M %g %g V %g H %g V %g"/>', l, 76, 84, r, 76),
        sprintf('<text class="d5p-mono" x="%g" y="%g" text-anchor="middle" font-size="15" font-weight="700">%s = %d</text>',
          (l + r) / 2, 116, d5p_sum_names[k], d5p_sum_G[k])
      )
    }, character(1))
    return(d5p_svg(W, 130, "Taśma 27 kontroli podzielona na trzy kolejki: 8, 13 i 6 kontroli.",
      d5p_tape(d5p_sum_outcomes, d5p_sum_x0, y0, d5p_sum_pitch, d5p_sum_size),
      brackets))
  }
  # kadry 3 i 4: trzy fragmenty w trzech wierszach wyrównanych do lewej
  rows_y <- c(34, 84, 134)
  rows <- vapply(seq_along(d5p_sum_G), function(k) {
    g <- d5p_sum_G[k]
    y <- rows_y[k]
    x_end <- d5p_sum_x0 + (g - 1) * d5p_sum_pitch + d5p_sum_size
    paste0(
      d5p_tape(c(rep(FALSE, g - 1L), TRUE), d5p_sum_x0, y, d5p_sum_pitch, d5p_sum_size),
      sprintf('<text class="d5p-mono" x="%g" y="%g" font-size="15" font-weight="700">%s = %d</text>',
        x_end + 14, y + 14, d5p_sum_names[k], g),
      if (step == 4) sprintf('<text class="d5p-mono" x="440" y="%g" font-size="15">średnio 1/p = 10</text>', y + 14) else ""
    )
  }, character(1))
  d5p_svg(W, 170, "Trzy kolejki w trzech wierszach; każdy kończy się wadą.", rows)
}

# Kadry P5 zaczynają się od rysunku nr 2 (d5p_sum_svg(step + 1)).
d5p_sum_ui <- d5p_frames_ui("Pojęcie", "Trzy kolejki czekania", d5p_sum_frames,
  function(i) d5p_sum_svg(i + 1L), d5p_frame_body)

# ============================================================================
# P5a — JEDEN AUDYT: SKĄD WZÓR (5.5)
# ============================================================================
# Ta sama taśma co w P5 (wady w 8, 21, 27). Kadry: cały audyt → inne układy
# dwóch pierwszych wad wśród 26 kontroli, ten sam wynik X = 27.

d5p_one_pitch <- 20
d5p_one_size <- 16
d5p_one_xl <- function(i) d5p_sum_x0 + (i - 1) * d5p_one_pitch
d5p_one_xc <- function(i) d5p_one_xl(i) + d5p_one_size / 2
d5p_one_alt <- list(c(8L, 21L), c(1L, 2L), c(13L, 14L), c(5L, 26L))

d5p_one_frames <- list(
  list(name = "Cały audyt",
    text = "Każda kratka to jedna kontrola palety. Wyróżnione kratki to wady, a audyt kończy się na trzeciej z nich.",
    read = "X = 27 kontroli do trzeciej wady"),
  list(name = "Inne układy, ten sam wynik",
    text = "Trzecia wada stoi w 27. kontroli, a dwie pierwsze mogą stać w dowolnych miejscach wśród 26 wcześniejszych. Takich układów jest tyle, na ile sposobów można wybrać 2 miejsca z 26 — to symbol Newtona z wykładu 04. Każdy taki układ kończy audyt na 27. kontroli i ma to samo prawdopodobieństwo: trzy wady i 24 kontrole bez wady.",
    read = HTML(paste0('<span class="d5p-math"><i>P</i>(<i>X</i> = 27) = ', d5p_col("szalwia", d5p_binom(26, 2)), " ",
      d5p_col("terakota", "<i>p</i><sup>3</sup>"), " ", d5p_col("indygo", "(1 − <i>p</i>)<sup>24</sup>"),
      " = 325 · 0.1<sup>3</sup> · 0.9<sup>24</sup>", sprintf(" ≈ %.3f", dnbinom(24, 3, 0.10)), "</span>")))
)

d5p_one_row <- function(defects, y) {
  d5p_tape(seq_len(27) %in% c(defects, 27L), d5p_sum_x0, y, d5p_one_pitch, d5p_one_size)
}

d5p_one_svg <- function(step) {
  W <- 760
  if (step == 1) {
    return(d5p_svg(W, 130, "Taśma 27 kontroli z wadami w kontrolach 8, 21 i 27.",
      d5p_one_row(c(8L, 21L), 40),
      d5p_axis(d5p_one_xc, 82, c(1L, 8L, 21L, 27L), label = "nr kontroli")))
  }
  rows_y <- c(24, 56, 88, 120)
  rows <- vapply(seq_along(d5p_one_alt), function(i) d5p_one_row(d5p_one_alt[[i]], rows_y[i]), character(1))
  more <- sprintf('<text class="d5p-mono d5p-subtle" x="%g" y="%g" font-size="14">… razem 325 układów</text>',
    d5p_sum_x0, 166)
  d5p_svg(W, 180, "Cztery taśmy po 27 kontroli, każda kończy się wadą w 27. kontroli, dwie wcześniejsze wady w różnych miejscach.",
    rows, more)
}

d5p_one_ui <- d5p_frames_ui("Pojęcie", "Jeden audyt, trzecia wada w 27. kontroli",
  d5p_one_frames, d5p_one_svg, d5p_frame_body)
