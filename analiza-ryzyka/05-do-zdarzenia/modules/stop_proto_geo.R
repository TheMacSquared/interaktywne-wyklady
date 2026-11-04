# PUNKTY STOP (2026-11-03) — symulacje punktów stop wykładu 05, rozdział geometryczny
#
# Punkty stop rysowane po stronie serwera (SVG z R + renderUI), bez JS.
# Numery jak w PUNKTY-STOP-PROPOZYCJA.md.
#   d5p_geo_ui     — Symulacja „Seria za serią” (2): kontrole palet do pierwszej wady
#   d5p_memory_ui  — Symulacja „Po dwudziestu kontrolach bez wady” (4): brak pamięci, wzór (5.4), 3 kadry
# Serwer: d5p_geo_server(input, output, session) obsługuje oba punkty stop.
# Prefiksy input id: d5p_geo_* (P2), d5p_mem_prev/next (P4).
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css, d5p_svg, d5p_head, d5p_tape, d5p_series, d5p_axis).

# ============================================================================
# (2) SERIA ZA SERIĄ — stos X na stałej osi 1–80 (dalej „…”), linia cięcia n
# ============================================================================

d5p_geo_kmax <- 80L

# Środek kolumny k (k = 1..80); serie dłuższe niż 80 nie mają kolumny, oś kończy „…”.
d5p_geo_x <- function(k) 40 + (k - 0.5) * 10.5

d5p_geo_empty <- function(p) list(p = p, xs = integer(0), last = logical(0))

# Liczniki X = 1..80 (bez kumulowania dłuższych serii).
d5p_geo_counts <- function(xs) {
  xs <- as.integer(xs)
  tabulate(xs[xs <= d5p_geo_kmax], nbins = d5p_geo_kmax)
}

# Wynik linii cięcia: ile serii trwało dłużej niż n kontroli (także te poza osią).
d5p_geo_cut_text <- function(xs, cut) {
  if (!length(xs)) return(sprintf("X > %d: —", cut))
  k <- sum(xs > cut)
  sprintf("X > %d: %d z %d serii (%.1f%%)", cut, k, length(xs), 100 * k / length(xs))
}

# Pojedyncza linia odczytu: serie, najczęstsza wartość, średnia, najdłuższa.
d5p_geo_readout <- function(xs) {
  n <- length(xs)
  if (n == 0L) return("Serii: 0 · najczęściej: — · średnia: — · najdłuższa: —")
  # najczęstsza dokładna wartość X (przy remisie najmniejsza)
  mode_x <- which.max(tabulate(as.integer(xs)))
  sprintf("Serii: %d · najczęściej: %d · średnia: %.1f · najdłuższa: %d",
    n, mode_x, mean(xs), max(xs))
}

d5p_geo_caption <- function(n) {
  if (n == 0L) "Audytor stoi przy pierwszej palecie. Kliknij „Jedna seria” i zobacz, po ilu kontrolach wypadnie pierwsza wada."
  else if (n < 10L) "Każda seria kończy się w innym miejscu: raz po jednej kontroli, raz po kilkudziesięciu."
  else if (n < 100L) "Kształt zaczyna się wyłaniać: seria zwykle kończy się wcześnie, ale od czasu do czasu trafia się bardzo długa."
  else "Słupki maleją od pierwszej kontroli, a średnia leży dużo dalej, bo ciągną ją długie serie."
}

# Rysunek: taśma ostatniej serii u góry, pod nią stos (kropki) albo słupki (gdy
# najwyższy stos przekracza 20 kropek). Oś X jest stała, wysokość skaluje się.
# Linia cięcia po kontroli `cut`: serie dłuższe w kolorze akcentu, krótsze szałwiowe;
# wynik cięcia stoi pod rysunkiem, obok przycisków (d5p_geo_cut_read).
d5p_geo_svg <- function(xs, last = logical(0), cut = 10L) {
  W <- 960; H <- 340; base <- 290
  counts <- d5p_geo_counts(xs)
  scaled <- max(counts) > 20

  tape <- if (length(last)) paste0(
    '<text class="d5p-mono d5p-subtle" x="40" y="12" font-size="12">ostatnia seria</text>',
    d5p_tape(last, x0 = 40, y = 18, pitch = 11, size = 9, max_cells = d5p_geo_kmax)) else ""

  empty <- if (!length(xs)) '<text class="d5p-subtle" x="480" y="170" font-size="16" text-anchor="middle">Paleta czeka na pierwszą serię</text>' else ""

  marks <- lapply(which(counts > 0), function(k) {
    cx <- d5p_geo_x(k); n_k <- counts[k]
    cls <- if (k > cut) "d5p-bad" else "d5p-ok"
    if (scaled) {
      h <- n_k / max(counts) * 220
      sprintf('<rect class="%s" x="%g" y="%g" width="8" height="%g" rx="1.5"/><text class="d5p-mono d5p-subtle" x="%g" y="%g" text-anchor="middle" font-size="11">%d</text>',
        cls, cx - 4, base - h, h, cx, base - h - 5, n_k)
    } else {
      paste(vapply(seq_len(n_k), function(i) sprintf('<circle class="%s" cx="%g" cy="%g" r="4.5"/>',
        cls, cx, base - 5 - (i - 1) * 10), character(1)), collapse = "")
    }
  })

  x_end <- d5p_geo_x(d5p_geo_kmax) + 6
  base_line <- sprintf('<line class="d5p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', d5p_geo_x(1) - 5, x_end, base, base)
  axis <- d5p_axis(d5p_geo_x, base, c(1, 10, 20, 30, 40, 50, 60, 70, 80))
  axis_label <- sprintf('<text class="d5p-subtle" x="%g" y="%g" font-size="12">X: kontrola, przy której wypadła wada</text>',
    d5p_geo_x(1) - 5, base + 40)
  # „…” za osią: rozkład ciągnie się dalej, dłuższe serie nie mają tu kolumny
  etc <- paste(sprintf('<circle cx="%g" cy="%g" r="3" style="fill:var(--upwr-ink-subtle)"/>',
    x_end + c(12, 24, 36), base), collapse = "")

  xc <- 40 + cut * 10.5
  cut_line <- sprintf('<line class="d5p-limit" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="d5p-mono" x="%g" y="%g" text-anchor="middle" font-size="13">n = %d</text>',
    xc, xc, base - 236, base + 4, xc, base - 242, cut)

  d5p_svg(W, H, sprintf("Stos kontroli do pierwszej wady: %d serii na osi od 1 do 80, linia cięcia po %d. kontroli.", length(xs), cut),
    tape, empty, base_line, axis, axis_label, etc, marks, cut_line)
}

# ============================================================================
# (4) PO DWUDZIESTU KONTROLACH BEZ WADY — trzy kadry, brak pamięci
# ============================================================================
# Kadr 1: 5000 audytów od startu. Kadr 2: linia przy 20, zakończone wcześniej
# przygaszone. Kadr 3: ocalałe przesunięte o 20 w lewo, skala pionowa powiększona.
# W kadrach 1 i 3 linia: rozkład geometryczny p = 0.10 (wzór 5.1) razy liczba audytów.

# 5000 serii geometrycznych przy p = 0.10 (X = rgeom + 1). Ziarno dobrane tak, żeby
# odczyty trzymały się teorii (średnie ≈ 10, wada w pierwszej kontroli ≈ 10%),
# bo inaczej szum 600 serii zasłania puentę.
d5p_mem_draws <- local({ set.seed(2326); rgeom(5000, 0.10) + 1L })
d5p_mem_s <- 20L
d5p_mem_kmax <- 60L
d5p_mem_left <- d5p_mem_draws[d5p_mem_draws > d5p_mem_s] - d5p_mem_s

# Liczniki X = 1..60, bez kumulowania dłuższych serii (oś kończą kropki).
d5p_mem_counts <- function(x) {
  x <- as.integer(x)
  tabulate(x[x <= d5p_mem_kmax], nbins = d5p_mem_kmax)
}

d5p_mem_frames <- list(
  list(name = "Pięć tysięcy audytów",
    text = "Ten sam audyt przy p = 0.10 powtórzony 5000 razy. Każdy słupek to liczba audytów, w których pierwsza wada wypadła w danej kontroli. Linia to rozkład geometryczny przy p = 0.10 ze wzoru (5.1).",
    read = sprintf("Średnio do pierwszej wady: %.1f kontroli · wada już w pierwszej kontroli: %.1f%%",
      mean(d5p_mem_draws), 100 * mean(d5p_mem_draws == 1L))),
  list(name = "Kierownik po 20 kontrolach",
    text = "Kierownik: „Dwadzieścia kontroli bez wady, więc wada musi się zaraz pojawić.” Mówi o audytach na prawo od linii: tylko one przetrwały 20 kontroli bez wady. Pozostałe skończyły się wcześniej.",
    read = sprintf("Przetrwało 20 kontroli: %d z %d (%.1f%%)",
      length(d5p_mem_left), length(d5p_mem_draws), 100 * mean(d5p_mem_draws > d5p_mem_s))),
  list(name = "Liczymy od nowa",
    text = sprintf("Zostawiamy tylko te %d audytów i liczymy kontrole od 21. jako pierwszej. Skala pionowa jest powiększona. Linia to znów rozkład geometryczny przy p = 0.10, ten sam co na starcie, i znów pasuje: dwadzieścia kontroli bez wady niczego nie przybliża.", length(d5p_mem_left)),
    read = sprintf("Dalsze czekanie średnio %.1f kontroli · wada już w następnej kontroli: %.1f%%",
      mean(d5p_mem_left), 100 * mean(d5p_mem_left == 1L)))
)

d5p_mem_svg <- function(step) {
  W <- 960; H <- 350; base <- 290; top <- 40; cw <- 13.5; x0 <- 80
  x_of <- function(k) x0 + (k - 0.5) * cw
  xl <- function(k) x0 + (k - 1) * cw
  xr <- function(k) x0 + k * cw

  theory <- function(n) n * dgeom(seq_len(d5p_mem_kmax) - 1L, 0.10)
  if (step < 3) {
    counts <- d5p_mem_counts(d5p_mem_draws)
    cls <- if (step == 1) rep("d5p-ok", d5p_mem_kmax) else
      ifelse(seq_len(d5p_mem_kmax) > d5p_mem_s, "d5p-bad", "d5p-ok d5p-cell-dim")
    ref <- if (step == 1) theory(length(d5p_mem_draws)) else NULL
  } else {
    counts <- d5p_mem_counts(d5p_mem_left)
    cls <- rep("d5p-bad", d5p_mem_kmax)
    ref <- theory(length(d5p_mem_left))
  }
  ymax <- max(counts, ref)
  step_y <- if (ymax > 200) 100 else if (ymax > 50) 20 else 10
  ymax <- ceiling(ymax * 1.1 / step_y) * step_y
  y_of <- function(v) base - v / ymax * (base - top)

  bars <- paste(vapply(which(counts > 0), function(k) sprintf(
    '<rect class="%s" x="%g" y="%g" width="10" height="%g" rx="1.5"/>',
    cls[k], x_of(k) - 5, y_of(counts[k]), base - y_of(counts[k])), character(1)), collapse = "")

  contour <- if (!is.null(ref)) {
    pts <- paste(sprintf("%g,%g", x_of(seq_len(d5p_mem_kmax)), y_of(ref)), collapse = " ")
    paste0(sprintf('<polyline points="%s" fill="none" style="stroke:var(--upwr-ink)" stroke-width="2.2" stroke-linejoin="round"/>', pts),
      '<line x1="560" x2="596" y1="22" y2="22" style="stroke:var(--upwr-ink)" stroke-width="2.2"/>',
      '<text class="d5p-subtle" x="604" y="26" font-size="13">rozkład geometryczny, p = 0.10</text>')
  } else ""

  cut <- if (step == 2) {
    xc <- xr(d5p_mem_s)
    sprintf('<line class="d5p-limit" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="d5p-mono" x="%g" y="%g" text-anchor="middle" font-size="13">20 kontroli bez wady</text>',
      xc, xc, top - 6, base + 4, xc, top - 12)
  } else ""

  yt <- seq(0, ymax, by = step_y)
  ytick <- paste(vapply(yt, function(v) sprintf(
    '<line class="d5p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="d5p-mono d5p-subtle" x="%g" y="%g" text-anchor="end" font-size="12">%g</text>',
    x0 - 6, x0, y_of(v), y_of(v), x0 - 10, y_of(v) + 4, v), character(1)), collapse = "")
  yaxis <- sprintf('<line class="d5p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="d5p-subtle" x="8" y="%g" font-size="12">audytów</text>',
    x0, x0, top, base, top - 14)

  x_end <- xr(d5p_mem_kmax) + 4
  base_line <- sprintf('<line class="d5p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x0, x_end, base, base)
  axis <- d5p_axis(x_of, base, c(1, 10, 20, 30, 40, 50, 60))
  etc <- paste(sprintf('<circle cx="%g" cy="%g" r="3" style="fill:var(--upwr-ink-subtle)"/>',
    x_end + c(12, 24, 36), base), collapse = "")
  axis_label <- sprintf('<text class="d5p-subtle" x="%g" y="%g" font-size="12">%s</text>', x0, base + 40,
    if (step == 3) "kontrola liczona od 21. jako pierwszej" else "X: kontrola, przy której wypadła pierwsza wada")

  d5p_svg(W, H, switch(step,
      "Histogram 5000 audytów: liczba kontroli do pierwszej wady, z linią rozkładu geometrycznego p = 0.10.",
      "Ten sam histogram z linią po 20. kontroli; audyty zakończone wcześniej są przygaszone.",
      "Histogram dalszego czekania audytów, które przetrwały 20 kontroli, z linią rozkładu geometrycznego p = 0.10."),
    ytick, yaxis, base_line, bars, contour, cut, axis, etc, axis_label)
}

# ============================================================================
# PANELE UI
# ============================================================================

d5p_geo_ui <- d5p_panel("Symulacja", "Seria za serią",
  width_mode = "wide",
  d5p_css,
  uiOutput("d5p_geo_head"),
  uiOutput("d5p_geo_stage"),
  uiOutput("d5p_geo_read"),
  uiOutput("d5p_geo_caption"),
  lc_slider("d5p_geo_cut", "Linia cięcia: n kontroli", 1, 80, 10, 1),
  tags$div(class = "d5p-nav",
    lc_segmented("d5p_geo_p", label = "Wady na kontrolę",
      choices = c("p = 0.10" = "0.10", "p = 0.33" = "0.33", "p = 0.03" = "0.03"), selected = "0.10"),
    lc_action("d5p_geo_one", "Jedna seria", variant = "solid"),
    lc_action("d5p_geo_ten", "+10"),
    lc_action("d5p_geo_hund", "+100"),
    lc_action("d5p_geo_reset", icon = "reset", variant = "ghost", aria_label = "Wyczyść serie"),
    uiOutput("d5p_geo_cut_read", style = "margin-left:auto; align-self:center;")))

d5p_memory_ui <- d5p_panel("Symulacja", "Po dwudziestu kontrolach bez wady",
  width_mode = "wide",
  d5p_css,
  uiOutput("d5p_mem_head"),
  uiOutput("d5p_mem_stage"),
  uiOutput("d5p_mem_caption"),
  tags$div(class = "d5p-nav",
    lc_action("d5p_mem_prev", "Wstecz", icon = "prev"),
    lc_action("d5p_mem_next", "Dalej", icon = "next", variant = "solid")))

# ============================================================================
# SERWER
# ============================================================================

d5p_geo_server <- function(input, output, session) {
  # (2) seria za serią
  geo <- reactiveVal(d5p_geo_empty(0.10))
  observeEvent(input$d5p_geo_p, geo(d5p_geo_empty(as.numeric(input$d5p_geo_p))), ignoreInit = TRUE)

  geo_add <- function(m) {
    s <- geo()
    batch <- replicate(m, d5p_series(1, s$p), simplify = FALSE)
    s$xs <- c(s$xs, vapply(batch, length, integer(1)))
    s$last <- batch[[m]]
    geo(s)
  }
  observeEvent(input$d5p_geo_one, geo_add(1L))
  observeEvent(input$d5p_geo_ten, geo_add(10L))
  observeEvent(input$d5p_geo_hund, geo_add(100L))
  observeEvent(input$d5p_geo_reset, geo(d5p_geo_empty(geo()$p)))

  output$d5p_geo_head <- renderUI(d5p_head("Audyt palet", sprintf("p = %.2f", geo()$p)))
  output$d5p_geo_stage <- renderUI(d5p_geo_svg(geo()$xs, geo()$last, cut = if (is.null(input$d5p_geo_cut)) 10L else as.integer(input$d5p_geo_cut)))
  output$d5p_geo_cut_read <- renderUI(tags$span(class = "d5p-mono",
    style = "font-size:1.5em; font-weight:700; color:var(--upwr-accent);",
    d5p_geo_cut_text(geo()$xs, if (is.null(input$d5p_geo_cut)) 10L else as.integer(input$d5p_geo_cut))))
  output$d5p_geo_read <- renderUI(tags$div(class = "d5p-read", d5p_geo_readout(geo()$xs)))
  output$d5p_geo_caption <- renderUI(tags$p(class = "d5p-caption", d5p_geo_caption(length(geo()$xs))))

  # (4) brak pamięci: trzy kadry
  mem <- reactiveVal(1L)
  n_mem <- length(d5p_mem_frames)
  observeEvent(input$d5p_mem_next, mem(min(n_mem, mem() + 1L)))
  observeEvent(input$d5p_mem_prev, mem(max(1L, mem() - 1L)))
  output$d5p_mem_head <- renderUI(d5p_head(d5p_mem_frames[[mem()]]$name, sprintf("Kadr %d z %d", mem(), n_mem)))
  output$d5p_mem_stage <- renderUI(d5p_mem_svg(mem()))
  output$d5p_mem_caption <- renderUI({
    f <- d5p_mem_frames[[mem()]]
    tagList(tags$p(class = "d5p-caption", f$text), tags$p(class = "d5p-read", f$read))
  })
}
