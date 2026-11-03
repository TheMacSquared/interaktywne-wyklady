# PROTOTYPY (2026-11-03) — pojęcia: suma czasów oczekiwania i układy wad ------
#
# Prototypy rysowane po stronie serwera (SVG z R + renderUI), jak w wykładzie 04.
#   d5p_sum_ui — Pojęcie „Trzy kolejki czekania” (P5), 4 kadry krokowe
#   d5p_arr_ui — Pojęcie „Sześć układów” (P6, przykład 5.4(a)), 4 kadry krokowe
# Rysunki: d5p_sum_svg(step) i d5p_arr_svg(step) zwracają HTML (SVG), bez serwera.
# Serwer: d5p_suma_server(input, output, session).
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css, d5p_svg, d5p_head, d5p_tape, d5p_axis).
# Wejścia: d5p_sum_prev, d5p_sum_next, d5p_arr_prev, d5p_arr_next.

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

d5p_sum_frames <- list(
  list(name = "Cały audyt",
    text = "Każda kratka to jedna kontrola palety. Wyróżnione kratki to wady, a audyt kończy się na trzeciej z nich.",
    read = "X = 27 kontroli do trzeciej wady"),
  list(name = "Trzy kolejki",
    text = "Trzy odcinki: od początku do pierwszej wady, potem do drugiej i do trzeciej. Każdy kończy się wadą.",
    read = "X = G₁ + G₂ + G₃ = 8 + 13 + 6 = 27"),
  list(name = "Od nowa",
    text = "Po każdej wadzie audyt zaczyna się od nowa. Każda kolejka to oczekiwanie na pierwszą wadę.",
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
# P6 — SZEŚĆ UKŁADÓW (przykład 5.4(a): trzecia wada w 5. kontroli)
# ============================================================================

d5p_arr_x0 <- 120
d5p_arr_pitch <- 30
d5p_arr_size <- 22
d5p_arr_xl <- function(i) d5p_arr_x0 + (i - 1) * d5p_arr_pitch
d5p_arr_xc <- function(i) d5p_arr_xl(i) + d5p_arr_size / 2
d5p_arr_right <- d5p_arr_x0 + 4 * d5p_arr_pitch + d5p_arr_size + 18

# Pary wcześniejszych wad wśród kontroli 1–4; piąta kontrola to zawsze wada.
d5p_arr_pairs <- list(c(1L, 2L), c(1L, 3L), c(1L, 4L), c(2L, 3L), c(2L, 4L), c(3L, 4L))
# Układy C(5, 3) bez piątej kontroli: trzy wady wśród 1–4, audyt kończy się wcześniej.
d5p_arr_early <- list(c(1L, 2L, 3L), c(1L, 2L, 4L), c(1L, 3L, 4L), c(2L, 3L, 4L))

d5p_arr_frames <- list(
  list(name = "Ostatnia wymuszona",
    text = "Audyt kończy się na trzeciej wadzie, więc piąta kontrola musi ją wykryć.",
    read = ""),
  list(name = "Wszystkie układy",
    text = "Dwie wcześniejsze wady mogą leżeć w dowolnych dwóch z kontroli 1–4.",
    read = "C(4, 2) = 6 układów"),
  list(name = "Każdy tak samo",
    text = "Każdy układ ma trzy wady i dwie kontrole bez wady, więc każdy ma to samo prawdopodobieństwo.",
    read = "P(X = 5) = 6 · 0.00081 = 0.00486"),
  list(name = "Dlaczego nie C(5, 3)",
    text = "Cztery dodatkowe układy kończą audyt już w 3. albo 4. kontroli, więc piątej nie ma.",
    read = "C(5, 3) = 10 liczyłoby też te 4 — audyt skończyłby się wcześniej.")
)

# Jeden wiersz: taśma 5 kratek (out — wady; n_cells — ile kratek; dim — przygaszenie),
# opcjonalnie przekreślenie, etykieta po lewej (zbiór pozycji) i po prawej.
d5p_arr_row <- function(y, out, left, right = NULL, dim = NULL, n_cells = 5L,
                        cross = FALSE, quiet = FALSE) {
  cls_extra <- if (cross) " d5p-subtle" else ""
  tape <- d5p_tape(out, d5p_arr_x0, y, d5p_arr_pitch, d5p_arr_size, n_cells = n_cells, dim = dim)
  forced <- if (length(out) == 5L) sprintf(
    '<rect x="%g" y="%g" width="%g" height="%g" rx="4" fill="none" stroke="var(--upwr-ink)" stroke-width="2.4"/>',
    d5p_arr_xl(5) - 4, y - 4, d5p_arr_size + 8, d5p_arr_size + 8) else ""
  strike <- if (cross) sprintf(
    '<line x1="%g" x2="%g" y1="%g" y2="%g" stroke="var(--upwr-ink-subtle)" stroke-width="1.6"/>',
    d5p_arr_x0 - 8, d5p_arr_xl(5) + d5p_arr_size + 4, y + d5p_arr_size / 2, y + d5p_arr_size / 2) else ""
  paste0(tape, forced, strike,
    sprintf('<text class="d5p-mono%s" x="16" y="%g" font-size="14">%s</text>', cls_extra, y + 15, left),
    if (!is.null(right)) sprintf('<text class="d5p-mono%s" x="%g" y="%g" font-size="14">%s</text>',
      cls_extra, d5p_arr_right, y + 15, right) else ""
  )
}

d5p_arr_label_set <- function(s) paste0("{", paste(s, collapse = ","), "}")

d5p_arr_svg <- function(step) {
  W <- 760
  numbers <- paste(vapply(1:5, function(i) sprintf('<text class="d5p-tick" x="%g" y="24">%d</text>',
    d5p_arr_xc(i), i), character(1)), collapse = "")
  row_y <- 36 + (seq_along(d5p_arr_pairs) - 1) * 36
  prob <- "0.1³ · 0.9² = 0.00081"
  if (step == 1) {
    out <- c(TRUE, TRUE, FALSE, FALSE, TRUE)
    return(d5p_svg(W, 100, "Jeden układ z wadami w kontrolach 1, 2 i 5; piąta kontrola wymuszona.",
      numbers,
      d5p_arr_row(36, out, left = "{1,2}"),
      sprintf('<text x="%g" y="80" text-anchor="middle" font-size="13" font-weight="600">wymuszona</text>',
        d5p_arr_xc(5))))
  }
  valid <- vapply(seq_along(d5p_arr_pairs), function(k) {
    pair <- d5p_arr_pairs[[k]]
    out <- c(seq_len(4) %in% pair, TRUE)
    d5p_arr_row(row_y[k], out, left = d5p_arr_label_set(pair),
      right = if (step >= 3) prob else NULL)
  }, character(1))
  if (step <= 3) {
    return(d5p_svg(W, 250, "Sześć układów z dwiema wadami wśród kontroli 1–4 i wadą w piątej.",
      numbers, valid))
  }
  early <- vapply(seq_along(d5p_arr_early), function(k) {
    tri <- d5p_arr_early[[k]]
    out <- seq_len(5) %in% tri
    out <- out[1:4]
    d5p_arr_row(266 + (k - 1) * 36, out, left = d5p_arr_label_set(tri),
      right = sprintf("koniec w %d.", max(tri)),
      dim = rep(TRUE, 5), n_cells = 5L, cross = TRUE)
  }, character(1))
  d5p_svg(W, 410, "Sześć układów zaliczonych i cztery odrzucone, w których audyt kończy się w 3. lub 4. kontroli.",
    numbers, valid,
    sprintf('<line x1="16" x2="420" y1="252" y2="252" stroke="var(--upwr-ink-subtle)" stroke-width="1" stroke-dasharray="4 4"/>'),
    early)
}

d5p_arr_ui <- d5p_panel("Pojęcie", "Sześć układów",
  d5p_css,
  uiOutput("d5p_arr_head"),
  uiOutput("d5p_arr_stage"),
  uiOutput("d5p_arr_caption"),
  uiOutput("d5p_arr_read"),
  tags$div(class = "d5p-nav",
    lc_action("d5p_arr_prev", "Wstecz", icon = "prev"),
    lc_action("d5p_arr_next", "Dalej", icon = "next", variant = "solid")
  )
)

# ============================================================================
# SERWER — obie sceny: kadr w reactiveVal, Wstecz/Dalej z obcięciem do zakresu
# ============================================================================

d5p_read_ui <- function(txt) {
  if (!nzchar(txt)) return(NULL)
  tags$p(class = "d5p-read", txt)
}

d5p_suma_server <- function(input, output, session) {
  # P5
  sum_step <- reactiveVal(1L)
  n_sum <- length(d5p_sum_frames)
  observeEvent(input$d5p_sum_next, sum_step(min(n_sum, sum_step() + 1L)))
  observeEvent(input$d5p_sum_prev, sum_step(max(1L, sum_step() - 1L)))
  output$d5p_sum_head <- renderUI(d5p_head(d5p_sum_frames[[sum_step()]]$name,
    sprintf("Kadr %d z %d", sum_step(), n_sum)))
  output$d5p_sum_stage <- renderUI(d5p_sum_svg(sum_step()))
  output$d5p_sum_caption <- renderUI(tags$p(class = "d5p-caption", d5p_sum_frames[[sum_step()]]$text))
  output$d5p_sum_read <- renderUI(d5p_read_ui(d5p_sum_frames[[sum_step()]]$read))

  # P6
  arr_step <- reactiveVal(1L)
  n_arr <- length(d5p_arr_frames)
  observeEvent(input$d5p_arr_next, arr_step(min(n_arr, arr_step() + 1L)))
  observeEvent(input$d5p_arr_prev, arr_step(max(1L, arr_step() - 1L)))
  output$d5p_arr_head <- renderUI(d5p_head(d5p_arr_frames[[arr_step()]]$name,
    sprintf("Kadr %d z %d", arr_step(), n_arr)))
  output$d5p_arr_stage <- renderUI(d5p_arr_svg(arr_step()))
  output$d5p_arr_caption <- renderUI(tags$p(class = "d5p-caption", d5p_arr_frames[[arr_step()]]$text))
  output$d5p_arr_read <- renderUI(d5p_read_ui(d5p_arr_frames[[arr_step()]]$read))
}
