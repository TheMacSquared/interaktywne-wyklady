# PUNKTY STOP (2026-11-03) — symulacje punktów stop wykładu 05, rozdział geometryczny
#
# (2) rysuje i losuje modules/sceny.js (scena „geo”); (4) to kadry policzone w R.
# Numery jak w PUNKTY-STOP-PROPOZYCJA.md.
#   d5p_geo_ui     — Symulacja „Seria za serią” (2): kontrole palet do pierwszej wady
#   d5p_memory_ui  — Symulacja „Po dwudziestu kontrolach bez wady” (4): brak pamięci, wzór (5.4), 3 kadry
# Wymaga: stop_proto_common.R (d5p_panel, d5p_css, d5p_svg, d5p_tape, d5p_series, d5p_axis).

# ============================================================================
# (2) SERIA ZA SERIĄ — stos X na stałej osi 1–80 (dalej „…”), linia cięcia n
# ============================================================================
# Rysunek, losowanie serii i odczyty: modules/sceny.js (geoSvg, geoReadout,
# geoCutText). Linia cięcia po kontroli n: serie dłuższe w kolorze akcentu.

d5p_geo_ui <- d5p_scene_ui("Symulacja", "Seria za serią", "geo",
  list(captions = c(
    "Audytor stoi przy pierwszej palecie. Kliknij „Jedna seria” i zobacz, po ilu kontrolach wypadnie pierwsza wada.",
    "Każda seria kończy się w innym miejscu: raz po jednej kontroli, raz po kilkudziesięciu.",
    "Kształt zaczyna się wyłaniać: seria zwykle kończy się wcześnie, ale od czasu do czasu trafia się bardzo długa.",
    "Słupki maleją od pierwszej kontroli, a średnia leży dużo dalej, bo ciągną ją długie serie.")),
  d5p_out("head"), d5p_out("stage"), d5p_out("read"), d5p_out("caption"),
  lc_sf_slider("d5p_geo_cut", "Linia cięcia: n kontroli", 1, 80, 10, 1),
  tags$div(class = "d5p-nav",
    lc_segmented("d5p_geo_p", label = "Wady na kontrolę",
      choices = c("p = 0.10" = "0.10", "p = 0.33" = "0.33", "p = 0.03" = "0.03"), selected = "0.10"),
    d5p_button("one", "Jedna seria", variant = "solid"),
    d5p_button("ten", "+10"),
    d5p_button("hund", "+100"),
    d5p_button("reset", icon = "reset", variant = "ghost", aria_label = "Wyczyść serie"),
    d5p_out("cut", style = "margin-left:auto; align-self:center;")))

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


d5p_memory_ui <- d5p_frames_ui("Symulacja", "Po dwudziestu kontrolach bez wady",
  d5p_mem_frames, d5p_mem_svg, d5p_frame_body)
