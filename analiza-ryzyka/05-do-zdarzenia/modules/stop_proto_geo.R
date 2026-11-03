# PROTOTYPY (2026-11-03) — symulacje punktów stop wykładu 05, rozdział geometryczny
#
# Prototypy rysowane po stronie serwera (SVG z R + renderUI), bez JS.
# Numery jak w PUNKTY-STOP-PROPOZYCJA.md.
#   d5p_geo_ui     — Symulacja „Seria za serią” (2): kontrole palet do pierwszej wady
#   d5p_memory_ui  — Symulacja „Po dwudziestu kontrolach bez wady” (4): brak pamięci, wzór (5.4)
# Serwer: d5p_geo_server(input, output, session) obsługuje oba prototypy.
# Prefiksy input id: d5p_geo_* (P2), d5p_mem_* (P4).
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css, d5p_svg, d5p_head, d5p_tape, d5p_series, d5p_axis).

# ============================================================================
# (2) SERIA ZA SERIĄ — stos X na stałej osi 1–80, koszyk 80+
# ============================================================================

d5p_geo_kmax <- 80L

# Środek kolumny k (k = 1..81; kolumna 81 to koszyk „80+”).
d5p_geo_x <- function(k) 40 + (k - 0.5) * 10.5

d5p_geo_empty <- function(p) list(p = p, xs = integer(0), last = logical(0))

# Liczniki X w koszykach 1..80 oraz 81 = 80+.
d5p_geo_counts <- function(xs) {
  tabulate(pmin(as.integer(xs), d5p_geo_kmax + 1L), nbins = d5p_geo_kmax + 1L)
}

# Pojedyncza linia odczytu: serie, najczęstsza wartość, średnia, najdłuższa.
d5p_geo_readout <- function(xs) {
  n <- length(xs)
  if (n == 0L) return("Serii: 0 · najczęściej: — · średnia: — · najdłuższa: —")
  # najczęstsza dokładna wartość X (bez koszyka 80+; przy remisie najmniejsza)
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
d5p_geo_svg <- function(xs, last = logical(0)) {
  W <- 960; H <- 332; base <- 290
  counts <- d5p_geo_counts(xs)
  scaled <- max(counts) > 20

  tape <- if (length(last)) paste0(
    '<text class="d5p-mono d5p-subtle" x="40" y="12" font-size="12">ostatnia seria</text>',
    d5p_tape(last, x0 = 40, y = 18, pitch = 11, size = 9, max_cells = d5p_geo_kmax)) else ""

  empty <- if (!length(xs)) '<text class="d5p-subtle" x="480" y="170" font-size="16" text-anchor="middle">Paleta czeka na pierwszą serię</text>' else ""

  marks <- lapply(which(counts > 0), function(k) {
    cx <- d5p_geo_x(k); n_k <- counts[k]
    if (scaled) {
      h <- n_k / max(counts) * 220
      sprintf('<rect class="d5p-bad" x="%g" y="%g" width="8" height="%g" rx="1.5"/><text class="d5p-mono d5p-subtle" x="%g" y="%g" text-anchor="middle" font-size="11">%d</text>',
        cx - 4, base - h, h, cx, base - h - 5, n_k)
    } else {
      paste(vapply(seq_len(n_k), function(i) sprintf('<circle class="d5p-bad" cx="%g" cy="%g" r="4.5"/>',
        cx, base - 5 - (i - 1) * 10), character(1)), collapse = "")
    }
  })

  base_line <- sprintf('<line class="d5p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', d5p_geo_x(1) - 5, 890, base, base)
  axis <- d5p_axis(d5p_geo_x, base, c(1, 10, 20, 30, 40, 50, 60, 70),
    label = "X: kontrola, przy której wypadła wada", x_label = 890)
  bucket <- sprintf('<line class="d5p-axis" style="stroke-dasharray:3 3" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="d5p-mono d5p-subtle" x="%g" y="%g" text-anchor="middle" font-size="12">80+</text>',
    880, 880, base - 230, base, d5p_geo_x(d5p_geo_kmax + 1L), base + 18)

  d5p_svg(W, H, sprintf("Stos kontroli do pierwszej wady: %d serii na osi od 1 do 80 i koszyku 80+.", length(xs)),
    tape, empty, base_line, axis, bucket, marks)
}

# ============================================================================
# (4) PO DWUDZIESTU KONTROLACH BEZ WADY — udziały, brak pamięci
# ============================================================================

# 5000 serii geometrycznych przy p = 0.10 (X = rgeom + 1), stałe ziarno.
d5p_mem_draws <- local({ set.seed(2026); rgeom(5000, 0.10) + 1L })

d5p_mem_kmax <- 60L

# Udziały w koszykach 1..60 oraz 60+ (61 kolumn).
d5p_mem_share <- function(x) {
  if (!length(x)) return(rep(0, d5p_mem_kmax + 1L))
  tabulate(pmin(as.integer(x), d5p_mem_kmax + 1L), nbins = d5p_mem_kmax + 1L) / length(x)
}

# Tryb „start”: wszystkie serie, X. Tryb „late”: serie z X > 20, wielkość X − 20.
d5p_mem_select <- function(mode) {
  if (identical(mode, "late")) d5p_mem_draws[d5p_mem_draws > 20L] - 20L else d5p_mem_draws
}

# Stała skala pionowa dla obu trybów, żeby przełączenie nie przeskalowywało rysunku.
d5p_mem_ymax <- local({
  m <- max(d5p_mem_share(d5p_mem_draws), d5p_mem_share(d5p_mem_select("late")))
  ceiling(m * 1.15 / 0.05) * 0.05
})

d5p_mem_readout <- function(mode) {
  y <- d5p_mem_select(mode)
  sprintf("Serii: %d · średnia czekania: %.1f · P(wada w 10 kolejnych) = %.2f",
    length(y), mean(y), mean(y <= 10))
}

d5p_mem_caption <- function(mode) {
  if (identical(mode, "late")) "Serie, które przetrwały 20 kontroli, czekają dalej tak samo jak nowe. Kontur i słupki się pokrywają."
  else "Kierownik: „Dwadzieścia kontroli bez wady, więc wada musi się zaraz pojawić.” Przełącz na „Po 20 kontrolach bez wady” i sprawdź, czy coś się zmieniło."
}

# Histogram udziałów: słupki wybranego trybu; w trybie „late” kontur histogramu „start”.
d5p_mem_svg <- function(mode = c("start", "late")) {
  mode <- match.arg(mode)
  W <- 960; H <- 360; base <- 300; top <- 40; cw <- 13.5; x0 <- 80
  x_of <- function(k) x0 + (k - 0.5) * cw
  xl <- function(k) x0 + (k - 1) * cw
  xr <- function(k) x0 + k * cw
  y_of <- function(s) base - s / d5p_mem_ymax * (base - top)

  shares <- d5p_mem_share(d5p_mem_select(mode))
  bars <- paste(vapply(which(shares > 0), function(k) sprintf(
    '<rect class="d5p-ok" x="%g" y="%g" width="10" height="%g" rx="1.5"/>',
    x_of(k) - 5, y_of(shares[k]), base - y_of(shares[k])), character(1)), collapse = "")

  contour <- if (identical(mode, "late")) {
    sh <- d5p_mem_share(d5p_mem_draws)
    d <- sprintf("M%g %g", xl(1), y_of(sh[1]))
    for (k in seq_len(d5p_mem_kmax + 1L)) {
      d <- paste0(d, sprintf(" H%g", xr(k)))
      if (k <= d5p_mem_kmax) d <- paste0(d, sprintf(" V%g", y_of(sh[k + 1L])))
    }
    sprintf('<path d="%s" fill="none" style="stroke:var(--upwr-ink)" stroke-width="1.8" stroke-dasharray="6 4"/>', d)
  } else ""

  yt <- seq(0, d5p_mem_ymax, by = 0.05)
  ytick <- paste(vapply(yt, function(v) sprintf(
    '<line class="d5p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="d5p-mono d5p-subtle" x="%g" y="%g" text-anchor="end" font-size="12">%.2f</text>',
    x0 - 6, x0, y_of(v), y_of(v), x0 - 10, y_of(v) + 4, v), character(1)), collapse = "")

  yaxis <- sprintf('<line class="d5p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="d5p-subtle" x="8" y="%g" font-size="12">udział</text>',
    x0, x0, top, base, top - 14)
  base_line <- sprintf('<line class="d5p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x0, x_of(d5p_mem_kmax + 1L) + 6, base, base)
  axis <- d5p_axis(x_of, base, c(1, 10, 20, 30, 40, 50),
    label = if (identical(mode, "late")) "kontrola po 20. bez wady" else "kontrola nr X",
    x_label = 890)
  bucket <- sprintf('<line class="d5p-axis" style="stroke-dasharray:3 3" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="d5p-mono d5p-subtle" x="%g" y="%g" text-anchor="middle" font-size="12">60+</text>',
    xr(d5p_mem_kmax), xr(d5p_mem_kmax), top, base, x_of(d5p_mem_kmax + 1L), base + 18)
  legend <- if (identical(mode, "late")) sprintf(
    '<line x1="560" x2="596" y1="22" y2="22" style="stroke:var(--upwr-ink)" stroke-width="1.8" stroke-dasharray="6 4"/><text class="d5p-subtle" x="604" y="26" font-size="13">kontur: wszystkie serie od startu</text>') else ""

  d5p_svg(W, H, sprintf("Histogram udziałów czekania na wadę (%s), skala od 1 do 60 i koszyk 60+.",
      if (identical(mode, "late")) "po 20 kontrolach bez wady" else "od startu audytu"),
    ytick, yaxis, base_line, bars, contour, axis, bucket, legend)
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
  tags$div(class = "d5p-nav",
    lc_segmented("d5p_geo_p", label = "Wady na kontrolę",
      choices = c("p = 0.10" = "0.10", "p = 0.30" = "0.30", "p = 0.03" = "0.03"), selected = "0.10"),
    lc_action("d5p_geo_one", "Jedna seria", variant = "solid"),
    lc_action("d5p_geo_ten", "+10"),
    lc_action("d5p_geo_hund", "+100"),
    lc_action("d5p_geo_reset", "Wyczyść", variant = "ghost")))

d5p_memory_ui <- d5p_panel("Symulacja", "Po dwudziestu kontrolach bez wady",
  width_mode = "wide",
  d5p_css,
  uiOutput("d5p_mem_head"),
  uiOutput("d5p_mem_stage"),
  uiOutput("d5p_mem_read"),
  uiOutput("d5p_mem_caption"),
  tags$div(class = "d5p-nav",
    lc_segmented("d5p_mem_mode", label = "Które serie liczymy",
      choices = c("Od startu audytu" = "start", "Po 20 kontrolach bez wady" = "late"), selected = "start")))

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
  output$d5p_geo_stage <- renderUI(d5p_geo_svg(geo()$xs, geo()$last))
  output$d5p_geo_read <- renderUI(tags$div(class = "d5p-read", d5p_geo_readout(geo()$xs)))
  output$d5p_geo_caption <- renderUI(tags$p(class = "d5p-caption", d5p_geo_caption(length(geo()$xs))))

  # (4) brak pamięci
  mem_mode <- reactive(if (identical(input$d5p_mem_mode, "late")) "late" else "start")
  output$d5p_mem_head <- renderUI(d5p_head("Czekanie na wadę",
    if (identical(mem_mode(), "late")) "Po 20 kontrolach" else "Od startu audytu"))
  output$d5p_mem_stage <- renderUI(d5p_mem_svg(mem_mode()))
  output$d5p_mem_read <- renderUI(tags$div(class = "d5p-read", d5p_mem_readout(mem_mode())))
  output$d5p_mem_caption <- renderUI(tags$p(class = "d5p-caption", d5p_mem_caption(mem_mode())))
}
