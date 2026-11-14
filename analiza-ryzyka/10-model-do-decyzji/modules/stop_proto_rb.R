# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rb: P2, P3, P4 ----------------------
#
#   i10p_2_ui — Scena „Cztery protokoły misji” (P2), 5 kadrów policzonych w R:
#               cztery misje na osi czasu i zestawienie. Zdarzenia (I, D, S) i werdykt
#               TOP = I ∩ (D ∪ S) wynikają z flag misji; liczby z i10p_cfg.
#   i10p_3_ui — Scena „Awaria to nie niedostępność” (P3), scena JS „rok”
#               (modules/proto_rb.js): suwak dnia zapotrzebowania na osi roku,
#               okna niesprawności z i10p3_cfg (dane scenariusza, nie model).
#   i10p_4_ui — Scena „Osiem pytań, osiem modeli” (P4), 2 kadry policzone w R:
#               mapa kursu i drzewo Bananpolu z trzema wynikami; liczby z i10p_mission().
# Wymaga: stop_proto_common.R (i10p_cfg, i10p_mission, i10p_frames_ui, i10p_scene_ui),
# sceny.js (window.lcI10) i proto_rb.js (scena „rok”).

# ============================================================================
# P2 — CZTERY PROTOKOŁY MISJI
# ============================================================================

# Chwile zatrzymania jako ułamki czasu misji (scenariusz, nie parametr modelu).
i10p2_stop_a <- 0.4   # sterownik C w misji a
i10p2_stop_c <- 0.2   # wentylator A w misji c

i10p2_T <- function() i10p_cfg$time
i10p2_h <- function(frac) i10p_num(frac * i10p2_T(), 0)

# Zdarzenia: I zapotrzebowanie; D detektor milczy; S = sterownik zatrzymany
# albo oba wentylatory zatrzymane; TOP = I ∩ (D ∪ S).
i10p2_missions <- list(
  list(name = "Misja a", demand = TRUE, detector = "alarm", ctrl_stop = i10p2_stop_a,
    fan_a_stop = NA, fan_b_stop = NA, dim = FALSE, expect = TRUE,
    zapis = "I ∩ Dᶜ ∩ S", short = "padł sterownik",
    text = paste0("Zapotrzebowanie wykryto, ale po ", i10p2_h(i10p2_stop_a), " ", i10p_cfg$unit,
      " zatrzymał się sterownik. Sterownik jest szeregowy, więc chłodzenia zabrakło. ",
      "Wentylatory A i B pracują do końca, lecz bez sterownika nie dają chłodzenia.")),
  list(name = "Misja b", demand = FALSE, detector = "fałszywy", ctrl_stop = NA,
    fan_a_stop = NA, fan_b_stop = NA, dim = FALSE, expect = FALSE,
    zapis = "Iᶜ", short = "fałszywy alarm",
    text = "Alarm był fałszywy. Bez zapotrzebowania ochrony nie da się utracić; zbędne uruchomienie ma koszt, ale leży poza TOP."),
  list(name = "Misja c", demand = TRUE, detector = "alarm", ctrl_stop = NA,
    fan_a_stop = i10p2_stop_c, fan_b_stop = NA, dim = FALSE, expect = FALSE,
    zapis = "I ∩ Dᶜ ∩ Sᶜ", short = "padł tylko A",
    text = paste0("Wentylator A zatrzymał się po ", i10p2_h(i10p2_stop_c), " ", i10p_cfg$unit,
      ", B pracował do końca. Jeden wentylator wystarcza.")),
  list(name = "Misja d", demand = TRUE, detector = "milczy", ctrl_stop = NA,
    fan_a_stop = NA, fan_b_stop = NA, dim = TRUE, expect = TRUE,
    zapis = "I ∩ D", short = "detektor milczał",
    text = "Detektor milczał. Sprawny układ nie pomoże, jeśli nie dostanie sygnału.")
)

i10p2_top <- function(m) {
  S <- !is.na(m$ctrl_stop) || (!is.na(m$fan_a_stop) && !is.na(m$fan_b_stop))
  D <- m$detector == "milczy"
  m$demand && (D || S)
}
# Kontrola: werdykt z flag zgadza się z zapisem w wykładzie (TOP w misjach a i d).
stopifnot(identical(vapply(i10p2_missions, i10p2_top, logical(1)),
                    vapply(i10p2_missions, function(m) m$expect, logical(1))))

# Werdykt: ramka i napis TOP (akcent) albo brak TOP (szałwia).
i10p2_verdict <- function(top, x, y, w, h, size = 22) {
  paste0(
    sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g" rx="6"/>',
      if (top) "i10p-box-hit" else "i10p-box-ok", x, y, w, h),
    sprintf('<text class="%s" x="%g" y="%g" font-size="%g" font-weight="700" text-anchor="middle">%s</text>',
      if (top) "i10p-bad" else "i10p-ok", x + w / 2, y + h / 2 + size / 3, size, if (top) "TOP" else "brak TOP")
  )
}

i10p2_mission_svg <- function(m) {
  T <- i10p2_T(); W <- 420; X0 <- 180; BH <- 24
  xo <- function(t) X0 + t / T * W
  yo <- function(i) 40 + (i - 1) * 46          # górna krawędź wiersza i (1–5)
  mid <- function(i) yo(i) + BH / 2
  labels <- c("Zapotrzebowanie I", "Detektor", "Sterownik C", "Wentylator A", "Wentylator B")
  rows <- vapply(seq_along(labels), function(i)
    sprintf('<text x="0" y="%g" font-size="14">%s</text>', mid(i) + 5, labels[i]), character(1))

  # Wiersz 1: zapotrzebowanie.
  demand <- if (m$demand)
    paste0(sprintf('<rect class="i10p-fill-warn" x="%g" y="%g" width="%g" height="%g" rx="3"/>', X0, yo(1), W, BH),
      sprintf('<text x="%g" y="%g" font-size="13">jest od początku misji</text>', X0 + 10, mid(1) + 5))
  else
    paste0(sprintf('<rect class="i10p-fill-idle" x="%g" y="%g" width="%g" height="%g" rx="3"/>', X0, yo(1), W, BH),
      sprintf('<text class="i10p-subtle" x="%g" y="%g" font-size="13">brak</text>', X0 + 10, mid(1) + 5))

  # Wiersz 2: detektor — znacznik przy t = 0.
  det <- if (m$detector == "milczy")
    c(cls = "i10p-fill-bad", txt = "i10p-bad", label = "milczy")
  else
    c(cls = "i10p-fill-ok", txt = "i10p-ok", label = if (m$detector == "fałszywy") "alarm (fałszywy)" else "alarm")
  detector <- paste0(
    sprintf('<circle class="%s" cx="%g" cy="%g" r="7"/>', det[["cls"]], X0, mid(2)),
    sprintf('<text class="%s" x="%g" y="%g" font-size="13">%s</text>', det[["txt"]], X0 + 16, mid(2) + 5, det[["label"]]))

  # Wiersze 3–5: pasek pracy do chwili zatrzymania, dalej bezczynność.
  bar <- function(i, stop_frac) {
    end <- if (is.na(stop_frac)) T else stop_frac * T
    paste0(
      sprintf('<rect class="i10p-fill-ok" x="%g" y="%g" width="%g" height="%g" rx="3"/>', xo(0), yo(i), xo(end) - xo(0), BH),
      if (end < T) paste0(
        sprintf('<rect class="i10p-fill-idle" x="%g" y="%g" width="%g" height="%g" rx="3"/>', xo(end), yo(i), xo(T) - xo(end), BH),
        sprintf('<text class="i10p-mono" x="%g" y="%g" font-size="12">stop %s %s</text>', xo(end) + 6, mid(i) + 4,
          i10p_num(end, 0), i10p_cfg$unit)) else "")
  }
  dimmed <- function(s, on) if (on) paste0('<g opacity=".45">', s, "</g>") else s
  ctrl <- dimmed(bar(3, m$ctrl_stop), m$dim)
  fan_a <- dimmed(bar(4, m$fan_a_stop), m$dim)
  fan_b <- dimmed(bar(5, m$fan_b_stop), m$dim)

  verdict <- paste0(
    i10p2_verdict(i10p2_top(m), 630, 40, 156, 70),
    sprintf('<text class="i10p-mono" x="708" y="134" font-size="14" text-anchor="middle">%s</text>', m$zapis))

  i10p_svg(800, 310, paste0(m$name, ": ", m$short, "."),
    paste(rows, collapse = ""), demand, detector, ctrl, fan_a, fan_b, verdict,
    i10p_axis(xo, 270, c(0, T / 4, T / 2, 3 * T / 4, T), label = i10p_cfg$unit,
      labels = i10p_num(c(0, T / 4, T / 2, 3 * T / 4, T), 0)))
}

i10p2_table_svg <- function() {
  rows <- vapply(seq_along(i10p2_missions), function(i) {
    m <- i10p2_missions[[i]]; top <- i10p2_top(m)
    top_y <- 24 + (i - 1) * 64; mid <- top_y + 24
    paste0(
      if (i > 1) sprintf('<line class="i10p-grid" x1="20" x2="780" y1="%g" y2="%g"/>', top_y - 8, top_y - 8) else "",
      sprintf('<text class="i10p-mono" x="24" y="%g" font-size="20" font-weight="700">%s</text>', mid + 7, substr(m$name, 7, 7)),
      sprintf('<text class="i10p-mono" x="64" y="%g" font-size="14">%s</text>', mid + 5, m$zapis),
      sprintf('<text x="300" y="%g" font-size="14">%s</text>', mid + 5, m$short),
      sprintf('<rect class="%s" x="610" y="%g" width="150" height="30" rx="15"/>',
        if (top) "i10p-box-hit" else "i10p-box-ok", mid - 15),
      sprintf('<text class="%s" x="685" y="%g" font-size="14" font-weight="700" text-anchor="middle">%s</text>',
        if (top) "i10p-bad" else "i10p-ok", mid + 5, if (top) "TOP" else "brak TOP"))
  }, character(1))
  i10p_svg(800, 270, "Zestawienie: misje a i d kończą się TOP, b i c nie.", paste(rows, collapse = ""))
}

i10p2_frames <- c(
  lapply(i10p2_missions, function(m) list(name = m$name, text = m$text, read = NULL)),
  list(list(name = "Zestawienie",
    text = "Ochronę utracono w misjach a i d. W nawiasie jest suma D ∪ S: brak każdej z dwóch funkcji wystarcza.",
    read = "TOP = I ∩ (D ∪ S)"))
)

i10p_2_ui <- i10p_frames_ui("P2", "przykłady", "Cztery protokoły misji", i10p2_frames,
  function(i) if (i <= length(i10p2_missions)) i10p2_mission_svg(i10p2_missions[[i]]) else i10p2_table_svg())

# ============================================================================
# P3 — AWARIA TO NIE NIEDOSTĘPNOŚĆ
# ============================================================================

# Okna niesprawności (dni od–do) wentylatorów A i B; oś roku 0–days.
# Dane scenariusza do sceny „rok” (modules/proto_rb.js), nie model Bananpolu.
i10p3_cfg <- list(days = 365, day = 68, a = list(c(60, 75), c(200, 230)), b = list(c(215, 222)))

i10p_3_ui <- i10p_scene_ui("P3", "przypadek brzegowy", "Awaria to nie niedostępność", "rok", i10p3_cfg,
  tags$div(class = "i10p-nav",
    i10p_slider("i10p3_day", "Chwila zapotrzebowania: dzień", 1, i10p3_cfg$days, i10p3_cfg$day, 1)),
  i10p_out("stage"), i10p_out("status"),
  lc_caption("Oś roku jest ilustracją, nie danymi Bananpolu."))

# ============================================================================
# P4 — OSIEM PYTAŃ, OSIEM MODELI
# ============================================================================

i10p4_m <- i10p_mission()

i10p4_tiles <- list(
  list(wyklad = "wykład 02", q = "Warunek zmienia ocenę", model = "Warunkowe i całkowite"),
  list(wyklad = "wykład 03", q = "Co oznacza alarm", model = "Bayes"),
  list(wyklad = "wykład 04", q = "Ile zdarzeń w n próbach", model = "Dwumianowy"),
  list(wyklad = "wykład 05", q = "Ile prób do pierwszego zdarzenia", model = "Geometryczny"),
  list(wyklad = "wykład 05", q = "Ile prób do r zdarzeń", model = "Ujemny dwumianowy"),
  list(wyklad = "wykład 06", q = "Jak często przekraczamy próg", model = "Rozkład ciągły i ogon"),
  list(wyklad = "wykład 07", q = "Czy element dotrwa do czasu t", model = "Funkcje czasu życia"),
  list(wyklad = "wykłady 08–09", q = "Czy system spełni funkcję", model = "Funkcja struktury i FTA")
)
i10p4_selected <- c(2, 7, 8)

# Kafel 1–8: układ 4 × 2, kafel 180 × 120; wiersz 1 y = 10, wiersz 2 y = 190.
i10p4_tile_xy <- function(k) c(x = 10 + ((k - 1) %% 4) * 200, y = 10 + ((k - 1) %/% 4) * 180)

i10p4_tile <- function(k, state) {
  t <- i10p4_tiles[[k]]; xy <- i10p4_tile_xy(k); x <- xy[["x"]]; y <- xy[["y"]]
  cls <- if (state == "hit") "i10p-box-ok" else "i10p-paper"
  opening <- if (state == "dim") '<g opacity=".45">' else ""
  paste0(opening,
    sprintf('<rect class="%s" x="%g" y="%g" width="180" height="120" rx="6"/>', cls, x, y),
    sprintf('<text class="i10p-mono i10p-subtle" x="%g" y="%g" font-size="12">%s</text>', x + 10, y + 20, t$wyklad),
    paste(sprintf('<text x="%g" y="%g" font-size="13" text-anchor="middle">%s</text>', x + 90, y + 44 + 16 * (seq_along(strwrap(t$q, 24)) - 1), strwrap(t$q, 24)), collapse = ""),
    paste(sprintf('<text x="%g" y="%g" font-size="14" font-weight="700" text-anchor="middle">%s</text>', x + 90, y + 92 + 16 * (seq_along(strwrap(t$model, 18)) - 1), strwrap(t$model, 18)), collapse = ""),
    if (state == "dim") "</g>" else "")
}

# Kadr 1: wszystkie kafle zwykłe.
i10p4_svg_1 <- function() {
  tiles <- vapply(seq_along(i10p4_tiles), i10p4_tile, character(1), state = "plain")
  i10p_svg(800, 320, "Osiem kafli: osiem pytań kursu i osiem modeli.", paste(tiles, collapse = ""))
}

# Kadr 2: kafle 2, 7, 8 wyróżnione; linie do drzewa z trzema wynikami.
i10p4_svg_2 <- function() {
  sel <- i10p4_selected
  tiles <- vapply(seq_along(i10p4_tiles), function(k)
    i10p4_tile(k, if (k %in% sel) "hit" else "dim"), character(1))
  vals <- c(miss = i10p_num(i10p4_m$miss, 3), fan = i10p_p(i10p4_m$fan_r),
            sys = i10p_p(i10p4_m$system_r), top = i10p_p(i10p4_m$top))
  links <- paste0(
    # kafel 2 (wiersz 1): w dół przez szczelinę między wierszami, potem w szczelinę kolumny 2/3
    '<path class="i10p-link" d="M300,130 V160 H400 V350"/>',
    '<path class="i10p-link" d="M500,310 V350"/>',
    '<path class="i10p-link" d="M700,310 V350"/>',
    sprintf('<text class="i10p-mono i10p-subtle" x="392" y="176" font-size="12" text-anchor="end">1 − czułość = %s</text>', vals[["miss"]]),
    sprintf('<text class="i10p-mono i10p-subtle" x="492" y="334" font-size="12" text-anchor="end">R(t) = %s</text>', vals[["fan"]]),
    sprintf('<text class="i10p-mono i10p-subtle" x="692" y="334" font-size="12" text-anchor="end">R_sys = %s</text>', vals[["sys"]]))
  box <- paste0(
    sprintf('<rect class="i10p-box-hit" x="10" y="350" width="780" height="70" rx="8"/>'),
    '<text x="400" y="378" font-size="15" font-weight="700" text-anchor="middle">Drzewo ochrony termicznej Bananpolu</text>',
    sprintf('<text class="i10p-mono" x="400" y="406" font-size="14" text-anchor="middle">P(TOP) = %s</text>', vals[["top"]]))
  i10p_svg(800, 430, "Trzy wyniki trafiają do drzewa ochrony termicznej: przeoczenia detektora, czas życia wentylatora i struktura układu.",
    paste(tiles, collapse = ""), links, box)
}

i10p_4_ui <- i10p_frames_ui("P4", "domknięcie (mapa kursu)", "Osiem pytań, osiem modeli",
  list(
    list(name = "Osiem pytań",
      text = "Każde pytanie kursu ma swój model. Wybór zaczyna się od pytania, nie od danych.",
      read = NULL),
    list(name = "Trzy do drzewa",
      text = "Do drzewa trafiają wyniki trzech modeli: przeoczenia detektora, czas życia wentylatora i struktura układu.",
      read = NULL)
  ),
  function(i) if (i == 1) i10p4_svg_1() else i10p4_svg_2())
