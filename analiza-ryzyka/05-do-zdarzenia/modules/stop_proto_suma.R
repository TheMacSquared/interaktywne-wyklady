# PUNKTY STOP (2026-11-03) — pojęcia: suma czasów oczekiwania i układy wad ------
#
# Punkty stop rysowane po stronie serwera (SVG z R + renderUI), jak w wykładzie 04.
#   d5p_one_ui — Pojęcie „Jeden audyt” (P5a): skąd wzór (5.5), 2 kadry krokowe
#   d5p_sum_ui — Pojęcie „Trzy kolejki czekania” (P5), 3 kadry krokowe
# Rysunki: d5p_sum_svg(step) i d5p_one_svg(step) zwracają HTML (SVG), bez serwera.
# Serwer: d5p_suma_server(input, output, session).
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css, d5p_svg, d5p_head, d5p_tape, d5p_axis).
# Wejścia: d5p_sum_prev, d5p_sum_next, d5p_one_prev, d5p_one_next.

# Odczyt kadru: zwykły tekst albo wzór MathJax (d5p_math) — wtedy składany po wstawieniu.
d5p_read_ui <- function(txt) {
  if (is.character(txt) && !nzchar(txt)) return(NULL)
  withMathJax(tags$p(class = "d5p-read", txt))
}

# Wzór w odczycie; kolory części jak we wzorze (5.5): ułożenia, wady, kontrole bez wady.
d5p_tex_col <- function(tex, col) sprintf("\\style{color:var(--upwr-cat-%s)}{%s}", col, tex)
d5p_math <- function(tex) HTML(paste0("\\(\\displaystyle ", tex, "\\)"))

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

d5p_sum_ui <- d5p_panel("Pojęcie", "Trzy kolejki czekania",
  d5p_css,
  uiOutput("d5p_sum_head"),
  uiOutput("d5p_sum_stage"),
  uiOutput("d5p_sum_caption"),
  uiOutput("d5p_sum_read"),
  tags$div(class = "d5p-nav",
    lc_action("d5p_sum_prev", "Wstecz", icon = "prev"),
    lc_action("d5p_sum_next", "Dalej", icon = "next", variant = "solid")
  )
)

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
    read = d5p_math(paste0("P(X=27)=", d5p_tex_col("\\binom{26}{2}", "szalwia"), "\\,",
      d5p_tex_col("p^{3}", "terakota"), "\\,", d5p_tex_col("(1-p)^{24}", "indygo"),
      sprintf("=325\\cdot 0.1^{3}\\cdot 0.9^{24}\\approx %.3f", dnbinom(24, 3, 0.10)))))
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

d5p_one_ui <- d5p_panel("Pojęcie", "Jeden audyt, trzecia wada w 27. kontroli",
  d5p_css,
  uiOutput("d5p_one_head"),
  uiOutput("d5p_one_stage"),
  uiOutput("d5p_one_caption"),
  uiOutput("d5p_one_read"),
  tags$div(class = "d5p-nav",
    lc_action("d5p_one_prev", "Wstecz", icon = "prev"),
    lc_action("d5p_one_next", "Dalej", icon = "next", variant = "solid")
  )
)

d5p_suma_server <- function(input, output, session) {
  # P5a
  one_step <- reactiveVal(1L)
  n_one <- length(d5p_one_frames)
  observeEvent(input$d5p_one_next, one_step(min(n_one, one_step() + 1L)))
  observeEvent(input$d5p_one_prev, one_step(max(1L, one_step() - 1L)))
  output$d5p_one_head <- renderUI(d5p_head(d5p_one_frames[[one_step()]]$name,
    sprintf("Kadr %d z %d", one_step(), n_one)))
  output$d5p_one_stage <- renderUI(d5p_one_svg(one_step()))
  output$d5p_one_caption <- renderUI(tags$p(class = "d5p-caption", d5p_one_frames[[one_step()]]$text))
  output$d5p_one_read <- renderUI(d5p_read_ui(d5p_one_frames[[one_step()]]$read))

  # P5
  sum_step <- reactiveVal(1L)
  n_sum <- length(d5p_sum_frames)
  observeEvent(input$d5p_sum_next, sum_step(min(n_sum, sum_step() + 1L)))
  observeEvent(input$d5p_sum_prev, sum_step(max(1L, sum_step() - 1L)))
  output$d5p_sum_head <- renderUI(d5p_head(d5p_sum_frames[[sum_step()]]$name,
    sprintf("Kadr %d z %d", sum_step(), n_sum)))
  output$d5p_sum_stage <- renderUI(d5p_sum_svg(sum_step() + 1L))
  output$d5p_sum_caption <- renderUI(tags$p(class = "d5p-caption", d5p_sum_frames[[sum_step()]]$text))
  output$d5p_sum_read <- renderUI(d5p_read_ui(d5p_sum_frames[[sum_step()]]$read))
}
