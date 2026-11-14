# PROTOTYPY PUNKTÓW STOP (2026-11-14) — kadry: P3, P4A, P10, P15, P18, P24A, P24B ---
#
#   s8p_3_ui   — Scena „Co znaczy »działa«?” (P3): ta sama instalacja z dwiema
#                definicjami sukcesu; 0.902 i 0.857.
#   s8p_4a_ui  — Pojęcie „Dwa horyzonty na jednej osi czasu” (P4A, przykład 8.2).
#   s8p_10_ui  — Pojęcie „Zwijanie schematu” (P10, przykład 8.5).
#   s8p_15_ui  — Scena „Jedna rozdzielnica” (P15, przykład 8.8).
#   s8p_18_ui  — Scena „Czwarty wentylator” (P18, wzór 8.13 dla r = 0.9).
#   s8p_24a_ui — Domknięcie „Pięć kroków na jednym schemacie” (P24A).
#   s8p_24b_ui — Domknięcie „Pięć kroków i ich wyniki” (P24B, mapa).
# Wszystkie kadry policzone w R; przełącza je scena „kadry” (modules/sceny.js).
# Wymaga: stop_proto_common.R.

# Pozioma oś z podziałką (x_of: wartość → piksel).
s8pd_axis_x <- function(x_of, y, ticks, labels = format(ticks), label = NULL) paste0(
  sprintf('<line class="s8p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x_of(min(ticks)), x_of(max(ticks)), y, y),
  paste(sprintf('<line class="s8p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="s8p-tick" x="%g" y="%g">%s</text>',
    x_of(ticks), x_of(ticks), y, y + 5, x_of(ticks), y + 19, labels), collapse = ""),
  if (!is.null(label)) s8p_text(x_of(max(ticks)), y + 36, label, 12, "s8p-subtle", "end") else "")
s8pd_axis_y <- function(y_of, x, ticks, labels = format(ticks)) paste0(
  sprintf('<line class="s8p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x, x, y_of(min(ticks)), y_of(max(ticks))),
  paste(sprintf('<line class="s8p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="s8p-tick" style="text-anchor:end" x="%g" y="%g">%s</text>',
    x - 5, x, y_of(ticks), y_of(ticks), x - 8, y_of(ticks) + 4, labels), collapse = ""))

# ============================================================================
# P3 — SCENA „CO ZNACZY »DZIAŁA«?”
# ============================================================================

# Bloki: 1 = zasilanie, 2 = wentylator, 3 = czujnik z alarmem (karta Bananpolu).
s8p3_lab <- c("Zasilanie", "Wentylator", "Czujnik")
s8p3_r <- c(s8p_r_pow, s8p_r_fan, s8p_r_det)
s8p3_sub <- s8p_fix(s8p3_r, 2)
s8p3_def_a <- list(1, 2)          # „utrzymać 14.5 °C”
s8p3_def_b <- list(1, 2, 3)       # „utrzymać i zaalarmować”
s8p3_ra <- risk_series_reliability(s8p3_r[1:2])   # 0.9016
s8p3_rb <- risk_series_reliability(s8p3_r)        # 0.85652

# Karta definicji sukcesu nad schematem.
s8p3_card <- function(y, head, text) paste0(
  sprintf('<rect class="s8p-paper" x="40" y="%g" width="720" height="44" rx="6"/>', y),
  s8p_text(58, y + 28, head, 13, "s8p-subtle"), s8p_text(170, y + 28, text, 16, weight = 600))

s8p3_frames <- list(
  list(name = "Instalacja",
    text = "Chłodzenie komory: zasilanie, wentylator i czujnik temperatury z alarmem. Karta podaje niezawodność każdego elementu na 1000 h.",
    read = NULL),
  list(name = "Utrzymać temperaturę",
    text = "Pierwsza definicja wymaga tylko chłodzenia. Na drodze sukcesu stoją zasilanie i wentylator; czujnik zostaje poza schematem.",
    read = sprintf("R = 0.98 · 0.92 = %s", s8p_fix(s8p3_ra, 3))),
  list(name = "Utrzymać i zaalarmować",
    text = "Druga definicja wymaga jeszcze, żeby przekroczenie wyszło na jaw od razu. Do szeregu dochodzi czujnik.",
    read = sprintf("R = 0.98 · 0.92 · 0.95 = %s", s8p_fix(s8p3_rb, 3))),
  list(name = "Ta sama noc",
    text = "Noc, w której pada tylko czujnik. Według pierwszej definicji system działał, według drugiej zawiódł. Kto wymaga więcej, ma dłuższy szereg.",
    read = sprintf("%s a %s dla tych samych urządzeń", s8p_fix(s8p3_ra, 3), s8p_fix(s8p3_rb, 3)))
)
s8p3_svg <- function(step) {
  def_a <- "komora nie przekracza 14.5 °C"
  def_b <- "komora nie przekracza 14.5 °C i alarm zgłasza przekroczenie"
  switch(step,
    s8p_svg(800, 200, "Trzy urządzenia instalacji chłodzenia: zasilanie 0.98, wentylator 0.92, czujnik 0.95.",
      paste(vapply(1:3, function(i) { x <- 110 + (i - 1) * 220; paste0(
        sprintf('<rect class="s8p-sunken" x="%g" y="24" width="160" height="150" rx="8"/>', x),
        if (i == 2) s8p_fan(x + 80, 82, 34) else if (i == 1)
          sprintf('<path d="M%g,48 l-24,40 h20 l-12,34 l34,-46 h-20 l12,-28 z" fill="var(--upwr-ink-soft)" opacity=".8"/>', x + 86)
        else sprintf('<rect x="%g" y="46" width="14" height="56" rx="7" fill="none" stroke="var(--upwr-ink-soft)" stroke-width="2.4"/><circle cx="%g" cy="108" r="13" fill="var(--upwr-ink-soft)" opacity=".8"/><rect x="%g" y="72" width="6" height="34" fill="var(--upwr-ink-soft)" opacity=".8"/>', x + 73, x + 80, x + 77),
        s8p_text(x + 80, 146, s8p3_lab[i], 15, anchor = "middle", weight = 600),
        s8p_text(x + 80, 164, paste("R =", s8p3_sub[i]), 12.5, "s8p-mono s8p-subtle", "middle")) }, character(1)), collapse = "")),
    s8p_svg(800, 240, "Definicja pierwsza: szereg zasilanie i wentylator, czujnik poza schematem.",
      s8p3_card(14, "Sukces:", def_a),
      s8p_rbd_center(400, 116, stages = s8p3_def_a, labels = s8p3_lab, sub = s8p3_sub, off = 3)),
    s8p_svg(800, 180, "Definicja druga: szereg zasilanie, wentylator i czujnik.",
      s8p3_card(14, "Sukces:", def_b),
      s8p_rbd_center(400, 116, stages = s8p3_def_b, labels = s8p3_lab, sub = s8p3_sub)),
    s8p_svg(800, 330, "Noc z awarią czujnika: w pierwszej definicji lampka świeci, w drugiej gaśnie.",
      s8p3_card(10, "Sukces:", def_a),
      s8p_rbd_center(400, 96, stages = s8p3_def_a, labels = s8p3_lab, sub = s8p3_sub, up = c(TRUE, TRUE, FALSE), lamp = NULL),
      s8p3_card(170, "Sukces:", def_b),
      s8p_rbd_center(400, 258, stages = s8p3_def_b, labels = s8p3_lab, sub = s8p3_sub, up = c(TRUE, TRUE, FALSE), lamp = NULL))
  )
}
s8p_3_ui <- s8p_frames_ui("P3", "scena", "Co znaczy „działa”?", s8p3_frames, s8p3_svg)

# ============================================================================
# P4A — DWA HORYZONTY NA JEDNEJ OSI CZASU (przykład 8.2)
# ============================================================================

s8p4_t <- c(fan = 1000, det = 8760); s8p4_r <- c(fan = 0.92, det = 0.95)
s8p4_fan <- function(t) s8p4_r[["fan"]]^(t / s8p4_t[["fan"]])
s8p4_det <- function(t) s8p4_r[["det"]]^(t / s8p4_t[["det"]])
s8p4_det1000 <- s8p4_det(1000)                       # 0.9942
s8p4_sys <- s8p4_r[["fan"]] * s8p4_det1000           # 0.915
s8p4_x <- function(t) 80 + t / 9000 * 680
s8p4_y <- function(r) 250 - (r - 0.85) / 0.15 * 210
s8p4_path <- function(f, to, cls, style = "") {
  t <- seq(0, to, length.out = 80)
  sprintf('<path d="M%s" fill="none" %s/>', paste(sprintf("%.1f,%.1f", s8p4_x(t), s8p4_y(f(t))), collapse = " L"), style)
}
s8p4_pt <- function(t, r, col, label, dx = 10, dy = -10, anchor = "start") paste0(
  sprintf('<circle cx="%g" cy="%g" r="6.5" fill="%s" stroke="var(--upwr-surface)" stroke-width="1.5"/>', s8p4_x(t), s8p4_y(r), col),
  s8p_text(s8p4_x(t) + dx, s8p4_y(r) + dy, label, 14, "s8p-mono", anchor, 700))
s8p4_plot <- function(step) {
  fan_col <- "var(--upwr-secondary)"; det_col <- "var(--upwr-sage)"
  vline <- function(t, cls = "s8p-rule", extra = "") sprintf('<line class="%s" x1="%g" x2="%g" y1="%g" y2="%g" %s/>', cls, s8p4_x(t), s8p4_x(t), s8p4_y(1), s8p4_y(0.85), extra)
  paste0(
    s8pd_axis_x(s8p4_x, 250, seq(0, 9000, 1000), label = "czas misji (h)"),
    s8pd_axis_y(s8p4_y, 80, c(0.85, 0.90, 0.95, 1), s8p_fix(c(0.85, 0.90, 0.95, 1), 2)),
    s8p_text(86, 34, "R(t)", 12, "s8p-subtle"),
    vline(1000, extra = 'stroke-dasharray="4 4"'), vline(8760, extra = 'stroke-dasharray="4 4"'),
    if (step >= 3) paste0(
      # krzywa wentylatora opuszcza pokazany zakres przy R = 0.85 (ok. 1950 h)
      s8p4_path(s8p4_fan, 1000 * log(0.85) / log(0.92), style = sprintf('stroke="%s" stroke-width="2.4"', fan_col)),
      s8p4_path(s8p4_det, 9000, style = sprintf('stroke="%s" stroke-width="2.4"', det_col))),
    if (step >= 4) paste0(vline(1000, "s8p-dash"),
      s8p4_pt(1000, s8p4_det1000, det_col, s8p_fix(s8p4_det1000, 4), dx = 12, dy = -12)),
    s8p4_pt(1000, 0.92, fan_col, "wentylator 0.92", dx = 12, dy = 18),
    s8p4_pt(8760, 0.95, det_col, "czujnik 0.95", dx = -10, dy = 24, anchor = "end")
  )
}
s8p4_card <- paste0(
  '<rect class="s8p-paper" x="170" y="20" width="460" height="200" rx="6"/>',
  s8p_text(196, 52, "KARTA KATALOGOWA · niezawodność", 14, weight = 700),
  '<line class="s8p-rule" x1="190" x2="610" y1="66" y2="66"/>',
  s8p_text(196, 98, "Wentylator", 17, weight = 600), s8p_text(390, 98, "0.92", 19, "s8p-mono", weight = 700), s8p_text(460, 98, "na 1000 h", 14, "s8p-subtle"),
  s8p_text(196, 136, "Czujnik", 17, weight = 600), s8p_text(390, 136, "0.95", 19, "s8p-mono", weight = 700), s8p_text(460, 136, "na rok pracy ciągłej", 14, "s8p-subtle"),
  '<line class="s8p-rule" x1="190" x2="610" y1="158" y2="158"/>',
  s8p_text(196, 194, "iloczyn?", 15, "s8p-subtle"), s8p_text(390, 194, "0.874", 19, "s8p-mono s8p-bad", weight = 700))
s8p4_frames <- list(
  list(name = "Karta", text = "Karta podaje dwie niezawodności. Iloczyn 0.92 · 0.95 = 0.874 wygląda jak niezawodność szeregu.", read = NULL),
  list(name = "Oś czasu", text = "Każda liczba ma swój czas misji: 1000 h i rok, czyli 8760 h. Na osi czasu leżą w dwóch różnych miejscach.", read = NULL),
  list(name = "Krzywe", text = "Niezawodność maleje z czasem. W modelu wykładniczym przez każdy punkt przechodzi jedna krzywa R(t).", read = NULL),
  list(name = "Wspólna kreska", text = "Na misję 1000 h czujnik ma 0.9942, nie 0.95. Mnożyć wolno tylko wartości z jednej pionowej kreski.",
    read = sprintf("0.92 · %s = %s · ryzyko %s zamiast 0.126", s8p_fix(s8p4_det1000, 4), s8p_fix(s8p4_sys, 3), s8p_fix(1 - s8p4_sys, 3)))
)
s8p4_svg <- function(step) {
  if (step == 1) return(s8p_svg(800, 240, "Karta katalogowa: wentylator 0.92 na 1000 godzin, czujnik 0.95 na rok. Naiwny iloczyn 0.874.", s8p4_card))
  s8p_svg(800, 292, c("", "Oś czasu z dwoma punktami: 0.92 przy 1000 godzinach i 0.95 przy 8760 godzinach.",
    "Te same punkty z krzywymi niezawodności wentylatora i czujnika.",
    "Na kresce 1000 godzin czujnik ma 0.9942, wentylator 0.92.")[step], s8p4_plot(step))
}
s8p_4a_ui <- s8p_frames_ui("P4A", "pojęcie", "Dwa horyzonty na jednej osi czasu", s8p4_frames, s8p4_svg)

# ============================================================================
# P10 — ZWIJANIE SCHEMATU (przykład 8.5)
# ============================================================================

s8p10_risk <- 1 - s8p_r_mixed                         # 0.02392
s8p10_risk_c <- 1 - s8p_r_c                           # 0.020
s8p10_risk_ab <- s8p_r_c * (1 - s8p_r_ab)             # 0.00392
s8p10_frames <- list(
  list(name = "Układ", text = "Sukces: działa sterownik C oraz co najmniej jeden z wentylatorów A i B.", read = NULL),
  list(name = "Zwiń A i B", text = "Grupa równoległa zachowuje się jak jeden blok. Dwa wentylatory zastępuje blok o niezawodności 0.996.",
    read = sprintf("R_AB = 1 − 0.08 · 0.05 = %s", s8p_fix(s8p_r_ab, 3))),
  list(name = "Połącz z C", text = "Został szereg dwóch bloków. Mnożymy i cały układ staje się jednym blokiem.",
    read = sprintf("R_sys = 0.98 · 0.996 = %s", s8p_fix(s8p_r_mixed, 5))),
  list(name = "Co zostało", text = "Ryzyko systemu to 0.024. Z tego 0.020 to sterownik, którego redundancja nie dotyczy.",
    read = sprintf("ryzyko %s = sterownik %s + oba wentylatory %s", s8p_fix(s8p10_risk, 3), s8p_fix(s8p10_risk_c, 3), s8p_fix(s8p10_risk_ab, 3)))
)
s8p10_svg <- function(step) {
  full <- function(y) s8p_rbd_center(400, y, stages = list(1, c(2, 3)), labels = c("Sterownik C", "Wentylator A", "Wentylator B"),
    sub = s8p_fix(c(s8p_r_c, s8p_r_a, s8p_r_b), 2), bw = 132)
  two <- function(y) s8p_rbd_center(400, y, stages = list(1, 2), labels = c("Sterownik C", "A lub B"),
    sub = c(s8p_fix(s8p_r_c, 2), s8p_fix(s8p_r_ab, 3)), mark = 2, bw = 132)
  switch(step,
    s8p_svg(800, 170, "Układ mieszany: sterownik C w szeregu z równoległymi wentylatorami A i B.", full(80)),
    s8p_svg(800, 170, "Wentylatory A i B zwinięte w jeden blok o niezawodności 0.996.", two(80)),
    s8p_svg(800, 170, "Cały układ jako jeden blok o niezawodności 0.976.",
      s8p_rbd_center(400, 80, stages = list(1), labels = "System", sub = s8p_fix(s8p_r_mixed, 3), mark = 1, bw = 132)),
    s8p_svg(800, 250, "Pasek ryzyka systemu 0.024 podzielony na sterownik 0.020 i wentylatory 0.004.", two(70),
      {
        x0 <- 150; w <- 500; wc <- w * s8p10_risk_c / s8p10_risk
        paste0(
          s8p_text(x0, 158, "ryzyko systemu 0.024", 13, "s8p-subtle"),
          sprintf('<rect x="%g" y="168" width="%g" height="34" fill="var(--upwr-accent)"/>', x0, wc),
          sprintf('<rect x="%g" y="168" width="%g" height="34" fill="var(--upwr-single-alt)"/>', x0 + wc + 2, w - wc - 2),
          s8p_text(x0 + wc / 2, 190, "sterownik C · 0.020", 15, anchor = "middle", weight = 700) |> sub(pattern = "<text", replacement = '<text style="fill:var(--upwr-surface)"', fixed = TRUE),
          s8p_text(x0 + wc + (w - wc) / 2, 226, "A i B naraz · 0.004", 14, "s8p-mono", "middle"))
      })
  )
}
s8p_10_ui <- s8p_frames_ui("P10", "pojęcie", "Zwijanie schematu", s8p10_frames, s8p10_svg)

# ============================================================================
# P15 — SCENA „JEDNA ROZDZIELNICA” (przykład 8.8)
# ============================================================================

s8p15_r <- risk_common_cause_reliability(s8p_r_ab, s8p_q)     # 0.98604
s8p15_n <- 1000
s8p15_pow <- round(s8p15_n * s8p_q)                           # 10 misji: zasilanie
s8p15_fans0 <- round(s8p15_n * (1 - s8p_r_ab))                # 4 misje: oba wentylatory (obietnica)
s8p15_fans <- round(s8p15_n * (1 - s8p_q) * (1 - s8p_r_ab))   # 4 misje (3.96)
# Tysiąc misji w kratkach 100 × 10.
s8p15_grid <- function(y, counts, label) paste0(
  s8p_text(25, y - 8, label, 13, "s8p-subtle"),
  s8p_cells(counts, 100, 25, y, size = 6, gap = 1.5))
# Hala: rozdzielnica i dwa wentylatory na jej kablach.
s8p15_hall <- function(up) {
  col <- if (up) "var(--upwr-ink-soft)" else "var(--upwr-accent)"
  paste0(
    '<rect class="s8p-sunken" x="120" y="14" width="560" height="190" rx="8"/>',
    s8p_text(136, 38, "Hala dojrzewalni", 13, "s8p-subtle"),
    sprintf('<path d="M250,110 H360 M360,70 V150 M360,70 H470 M360,150 H470" fill="none" stroke="%s" stroke-width="3"/>', col),
    sprintf('<rect x="170" y="70" width="80" height="80" rx="6" fill="%s" stroke="%s" stroke-width="2.4"/>',
      if (up) "var(--upwr-surface)" else "color-mix(in srgb, var(--upwr-accent) 16%, var(--upwr-surface))", col),
    sprintf('<path d="M214,84 l-16,26 h13 l-8,24 l23,-32 h-13 l8,-18 z" fill="%s"/>', col),
    s8p_text(210, 172, if (up) "rozdzielnica" else "rozdzielnica pada", 14, if (!up) "s8p-bad", "middle", 600),
    s8p_fan(500, 70, 26, up), s8p_text(540, 75, if (up) "wentylator A" else "A stoi", 14, if (!up) "s8p-bad", weight = 600),
    s8p_fan(500, 150, 26, up), s8p_text(540, 155, if (up) "wentylator B" else "B stoi", 14, if (!up) "s8p-bad", weight = 600))
}
s8p15_frames <- list(
  list(name = "Obietnica", text = "Schemat dwóch wentylatorów obiecuje R = 0.996. Na tysiąc misji zawodzą cztery: te, w których stają oba naraz.",
    read = sprintf("zawiedzione: %d na 1000", s8p15_fans0)),
  list(name = "Hala", text = "W hali oba wentylatory są wpięte w jedną rozdzielnicę. Na schemacie jej nie było.", read = NULL),
  list(name = "Awaria", text = "Rozdzielnica pada i oba wentylatory stają w tej samej sekundzie. To nie zbieg dwóch awarii, tylko jedna awaria o dwóch skutkach.", read = NULL),
  list(name = "Brakujący blok", text = "Dopisujemy zasilanie jako blok w szeregu. Na tysiąc misji 10 kończy utrata zasilania, a 4 awaria obu wentylatorów.",
    read = sprintf("zawiedzione: %d na 1000 · R = 0.99 · 0.996 = %s", s8p15_pow + s8p15_fans, s8p_fix(s8p15_r, 3)))
)
s8p15_svg <- function(step) {
  lab <- c("Wentylator A", "Wentylator B", "Zasilanie"); sub <- c("0.92", "0.95", "0.99")
  switch(step,
    s8p_svg(800, 280, "Schemat równoległy dwóch wentylatorów i tysiąc misji, z których cztery są zawiedzione.",
      s8p_rbd_center(400, 84, stages = list(c(1, 2)), labels = lab, sub = sub, bw = 132),
      s8p15_grid(196, c("s8p-cell-bad" = s8p15_fans0, "s8p-cell-ok" = s8p15_n - s8p15_fans0), "tysiąc misji · burgund: stają oba wentylatory")),
    s8p_svg(800, 218, "Hala: rozdzielnica zasila kablami oba wentylatory.", s8p15_hall(TRUE)),
    s8p_svg(800, 218, "Rozdzielnica pada, oba wentylatory stoją.", s8p15_hall(FALSE)),
    s8p_svg(800, 280, "Schemat z blokiem zasilania w szeregu przed wentylatorami i tysiąc misji, z których czternaście jest zawiedzionych.",
      s8p_rbd_center(400, 84, stages = list(3, c(1, 2)), labels = lab, sub = sub, mark = 3, bw = 132),
      s8p15_grid(196, c("s8p-cell-alt" = s8p15_pow, "s8p-cell-bad" = s8p15_fans, "s8p-cell-ok" = s8p15_n - s8p15_pow - s8p15_fans),
        "tysiąc misji · złote: pada zasilanie · burgund: stają oba wentylatory"))
  )
}
s8p_15_ui <- s8p_frames_ui("P15", "scena", "Jedna rozdzielnica", s8p15_frames, s8p15_svg)

# ============================================================================
# P18 — SCENA „CZWARTY WENTYLATOR” (wzór 8.13, r = 0.9)
# ============================================================================

s8p18_r <- 0.9
s8p18_n <- 10000
s8p18_fail <- round(s8p18_n * (1 - s8p18_r)^(1:4))      # 1000, 100, 10, 1
s8p18_saved <- -diff(s8p18_fail)                         # 900, 90, 9
s8p18_cost <- 100
s8p18_row <- function(k) {
  y <- 54 + (k - 1) * 72; w <- 300 * s8p18_fail[k] / s8p18_fail[1]
  paste0(
    if (k > 1) sprintf('<line class="s8p-rule" x1="10" x2="790" y1="%g" y2="%g"/>', y - 36, y - 36) else "",
    paste(vapply(seq_len(k), function(j) s8p_fan(34 + (j - 1) * 50, y, 20), character(1)), collapse = ""),
    s8p_text(238, y + 5, sprintf("%d jedn.", k * s8p18_cost), 14, "s8p-mono s8p-subtle"),
    sprintf('<rect class="s8p-sunken" x="330" y="%g" width="300" height="18" rx="3"/>', y - 9),
    sprintf('<rect class="s8p-cell-bad" x="330" y="%g" width="%g" height="18" rx="2"/>', y - 9, max(w, 1.2)),
    s8p_text(330 + max(w, 1.2) + 8, y + 5, s8p18_fail[k], 15, "s8p-mono s8p-bad", weight = 700),
    if (k > 1) paste0(s8p_text(790, y - 2, sprintf("uratował %d", s8p18_saved[k - 1]), 15, "s8p-mono s8p-ok", "end", 700),
      s8p_text(790, y + 16, "misji", 12, "s8p-subtle", "end")) else "")
}
s8p18_frames <- list(
  list(name = "Jeden wentylator", text = "Gałąź chłodzenia ma niezawodność 0.9. Na 10 000 misji zawodzi tysiąc.", read = "koszt 100 jedn. · zawiedzione 1000 na 10 000"),
  list(name = "Drugi", text = "Kierownik dokłada drugi wentylator za kolejne 100 jednostek. Zawiedzionych misji jest dziesięć razy mniej.", read = "koszt 200 jedn. · zawiedzione 100 na 10 000"),
  list(name = "Trzeci", text = "Trzeci znowu zmniejsza ryzyko dziesięciokrotnie. Ratuje jednak już tylko 90 misji.", read = "koszt 300 jedn. · zawiedzione 10 na 10 000"),
  list(name = "Czwarty", text = "Czwarty ratuje 9 misji. Kosztuje tyle samo co drugi, który uratował 900.", read = "koszt 400 jedn. · zawiedzione 1 na 10 000"),
  list(name = "Rachunek", text = "Cena jednej uratowanej misji rośnie z każdym wentylatorem dziesięć razy. Względna korzyść jest stała, bezwzględna znika.",
    read = sprintf("cena uratowanej misji: %s jedn.", paste(s8p_num(s8p18_cost / s8p18_saved, 2), collapse = " → ")))
)
s8p18_svg <- function(step) {
  k <- min(step, 4)
  s8p_svg(800, 30 + k * 72, sprintf("Liczba wentylatorów od 1 do %d; zawiedzione misje na 10 000: %s.", k, paste(s8p18_fail[1:k], collapse = ", ")),
    s8p_text(238, 16, "koszt", 12, "s8p-subtle"), s8p_text(330, 16, "zawiedzione misje na 10 000", 12, "s8p-subtle"),
    paste(vapply(seq_len(k), s8p18_row, character(1)), collapse = ""))
}
s8p_18_ui <- s8p_frames_ui("P18", "scena", "Czwarty wentylator", s8p18_frames, s8p18_svg)

# ============================================================================
# P24A i P24B — DOMKNIĘCIE: PIĘĆ KROKÓW
# ============================================================================

# Bloki: 1 = C, 2 = A, 3 = B, 4 = zasilanie (wspólna przyczyna, 1 − q).
s8p24_lab <- c("Sterownik C", "Wentylator A", "Wentylator B", "Zasilanie")
s8p24_sub <- s8p_fix(c(s8p_r_c, s8p_r_a, s8p_r_b, 1 - s8p_q), 2)
s8p24_r <- risk_common_cause_reliability(s8p_r_mixed, s8p_q)          # 0.9663
# Zysk z poprawy elementu o 0.01 w pełnym układzie (istotność Birnbauma · 0.01).
s8p24_gain <- 0.01 * c("Sterownik C" = (1 - s8p_q) * s8p_r_ab, "Zasilanie" = s8p_r_mixed,
  "Wentylator B" = (1 - s8p_q) * s8p_r_c * (1 - s8p_r_a), "Wentylator A" = (1 - s8p_q) * s8p_r_c * (1 - s8p_r_b))
s8p24_steps <- c("Zdefiniuj sukces", "Wspólny czas misji", "Zredukuj etapami", "Wspólne przyczyny", "Wrażliwość")
s8p24_frames <- list(
  list(name = s8p24_steps[1], text = "Sukces: komora nie przekracza 14.5 °C. Potrzebny jest sterownik i co najmniej jeden wentylator.", read = "φ = x_C · [1 − (1 − x_A)(1 − x_B)]"),
  list(name = s8p24_steps[2], text = "Każdy blok dostaje niezawodność na tę samą misję: 1000 h.", read = "R_C = 0.98 · R_A = 0.92 · R_B = 0.95"),
  list(name = s8p24_steps[3], text = "Wentylatory zwijamy w jeden blok, potem mnożymy szereg.", read = sprintf("0.98 · 0.996 = %s", s8p_fix(s8p_r_mixed, 3))),
  list(name = s8p24_steps[4], text = "Wspólne zasilanie dopisujemy jako blok w szeregu przed całym układem.", read = sprintf("0.99 · %s = %s", s8p_fix(s8p_r_mixed, 3), s8p_fix(s8p24_r, 3))),
  list(name = s8p24_steps[5], text = "Ta sama poprawa o 0.01 jest warta prawie dwadzieścia razy więcej w bloku szeregowym niż w gałęzi z zapasem.",
    read = sprintf("najwięcej daje: %s", names(s8p24_gain)[which.max(s8p24_gain)]))
)
s8p24_svg <- function(step) {
  rbd <- function(...) s8p_rbd_center(400, 86, labels = s8p24_lab, bw = 128, ...)
  switch(step,
    s8p_svg(800, 176, "Schemat: sterownik C w szeregu z równoległymi wentylatorami A i B, bez liczb.", rbd(stages = list(1, c(2, 3)))),
    s8p_svg(800, 176, "Ten sam schemat z niezawodnościami na 1000 godzin.", rbd(stages = list(1, c(2, 3)), sub = s8p24_sub)),
    s8p_svg(800, 176, "Schemat po redukcji: sterownik C i blok zastępczy wentylatorów 0.996.",
      s8p_rbd_center(400, 86, stages = list(1, 2), labels = c("Sterownik C", "A lub B"), sub = c("0.98", "0.996"), mark = 2, bw = 128)),
    s8p_svg(800, 176, "Schemat z blokiem zasilania w szeregu przed sterownikiem i wentylatorami.",
      rbd(stages = list(4, 1, c(2, 3)), sub = s8p24_sub, mark = 4)),
    s8p_svg(800, 176, "Słupki zysku z poprawy każdego elementu o 0.01.",
      s8p_text(40, 20, "wzrost R systemu po poprawie elementu o 0.01", 12, "s8p-subtle"),
      paste(vapply(seq_along(s8p24_gain), function(j) { y <- 46 + (j - 1) * 34; w <- 460 * s8p24_gain[[j]] / 0.01; paste0(
        s8p_text(170, y + 5, names(s8p24_gain)[j], 14, anchor = "end", weight = 600),
        sprintf('<rect class="s8p-sunken" x="184" y="%g" width="460" height="18" rx="3"/>', y - 9),
        sprintf('<rect x="184" y="%g" width="%g" height="18" rx="3" fill="%s"/>', y - 9, w, if (j <= 2) "var(--upwr-accent)" else "var(--upwr-ink-subtle)"),
        s8p_text(656, y + 5, s8p_fix(s8p24_gain[[j]], 5), 14, "s8p-mono")) }, character(1)), collapse = ""))
  )
}
s8p_24a_ui <- s8p_frames_ui("P24A", "domknięcie", "Pięć kroków na jednym schemacie", s8p24_frames, s8p24_svg)

# Mapa: pięć kart w rzędzie, każda z wynikiem kroku.
s8p24_results <- list(
  c("C oraz", "(A lub B)"), c("wszystkie R", "na 1000 h"),
  c("0.98 · 0.996", sprintf("= %s", s8p_fix(s8p_r_mixed, 3))),
  c(sprintf("0.99 · %s", s8p_fix(s8p_r_mixed, 3)), sprintf("= %s", s8p_fix(s8p24_r, 3))),
  c(sprintf("C: +%s", s8p_fix(s8p24_gain[["Sterownik C"]], 4)), sprintf("A: +%s", s8p_fix(s8p24_gain[["Wentylator A"]], 4))))
s8p_24b_ui <- s8p_panel("P24B", "domknięcie", "Pięć kroków i ich wyniki", s8p_css,
  s8p_svg(800, 170, "Mapa pięciu kroków analizy: sukces, czas misji, redukcja 0.976, wspólna przyczyna 0.966, wrażliwość.",
    paste(vapply(1:5, function(i) { x <- 8 + (i - 1) * 160; paste0(
      sprintf('<rect class="s8p-paper" x="%g" y="10" width="144" height="148" rx="8"/>', x),
      sprintf('<circle cx="%g" cy="36" r="13" fill="var(--upwr-accent)"/>', x + 24),
      sprintf('<text x="%g" y="41" font-size="14" font-weight="700" text-anchor="middle" style="fill:var(--upwr-surface)">%d</text>', x + 24, i),
      s8p_text(x + 12, 72, s8p24_steps[i], 13, weight = 700),
      '<line class="s8p-rule" x1="', x + 12, '" x2="', x + 132, '" y1="84" y2="84"/>',
      s8p_text(x + 12, 110, s8p24_results[[i]][1], 14, "s8p-mono"),
      s8p_text(x + 12, 132, s8p24_results[[i]][2], 14, "s8p-mono", weight = if (i %in% 3:4) 700),
      if (i < 5) sprintf('<path d="M%g,84 l10,0 m-4,-4 l4,4 l-4,4" fill="none" stroke="var(--upwr-ink-subtle)" stroke-width="1.6"/>', x + 147) else "") },
      character(1)), collapse = "")),
  tags$p(class = "s8p-caption", "Układ z wykładu: sterownik C, wentylatory A i B, wspólne zasilanie o q = 0.01. Każdy krok zostawia jeden wynik, a ostatni mówi, gdzie poprawa o 0.01 daje najwięcej.")
)
