# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rf: P14A, P14B, P15A, P15B ----------
#
#   z6p_14a_ui — galeria „Ten sam środek, inny ogon” (P14A): trzy modele na jednej
#                osi, widok „Cały rozkład” / „Ogon powyżej 88°C”; ryciny lcSF z6_modele
#                (modules/proto_rf.js), tabela pod wykresem wypełniana przez JS.
#   z6p_14b_ui — galeria „Trzy modele obok siebie” (P14B): ryciny lcSF z6_modele_b,
#                po jednym wykresie na model.
#   z6p_15a_ui — pojęcie „Pięć pomiarów obok modelu” (P15A): cztery kadry SVG
#                policzone w R (pomiary → rosnąco → kwantyle modelu → wykres Q–Q).
#   z6p_15b_ui — pojęcie „Wykres Q–Q z pięciu pomiarów” (P15B): jeden statyczny SVG
#                z tabelą po lewej i wykresem Q–Q po prawej.
# Wymaga: stop_proto_common.R (z6p_panel, z6p_css, z6p_svg, z6p_frames_ui),
# R/lc_stat_figures.R (lc_sf_figure, lc_segmented) i modules/proto_rf.js.

# ============================================================================
# P14A i P14B — GALERIE TRZECH MODELI
# ============================================================================

z6p_14a_ui <- z6p_panel("P14A", "galeria", "Ten sam środek, inny ogon",
  lc_sf_figure("z6_modele",
    control = lc_segmented("z6p14_view", "Widok",
      choices = c("Cały rozkład" = "all", "Ogon powyżej 88°C" = "tail")),
    ratio = "2.4/1", max_height = "360px", legend = TRUE,
    after = tagList(
      tags$div(`data-role` = "table"),
      lc_caption("Przy progu 1σ (85°C) trzy modele dają podobne ryzyko, przy progu 3σ (91°C) różnią się 5–9 razy. Ten sam środek, a różni je tylko ogon.")
    )))

z6p_14b_ui <- z6p_panel("P14B", "galeria", "Trzy modele obok siebie",
  lc_sf_figure("z6_modele_b", charts = 3,
    headings = c("Normalny", "Skośny (gamma)", "Ciężki ogon (t₃)"),
    ratio = "1.5/1", max_height = "320px",
    after = lc_caption("Przy progu 91°C pole nad krzywą jest u obu modeli nienormalnych wyraźnie większe niż u normalnego, mimo tego samego środka i rozrzutu.")))

# ============================================================================
# P15A i P15B — JAK POWSTAJE PUNKT WYKRESU Q–Q
# ============================================================================

# Dane z przykładu 6.11: pomiary w kolejności zapisu, posortowane i kwantyle modelu.
z6rf_meas <- c(82.3, 79.4, 88.6, 80.9, 83.1)
z6rf_sorted <- sort(z6rf_meas)
z6rf_q <- round(qnorm((1:5 - 0.5) / 5, z6p_mu, z6p_sd), 1)   # 78.2 … 85.8
z6rf_zq <- qnorm((1:5 - 0.5) / 5)                              # −1.28 … 1.28
z6rf_diff <- round(z6rf_sorted - z6rf_q, 1)                    # +1.2 … +2.8

# Pomocniki rysunku (przedrostek z6rf_ jest tylko w tym pliku).
z6rf_fmt <- function(x) sprintf("%.1f", x)
z6rf_sgn <- function(x) sub("-", "−", sprintf("%+.1f", x), fixed = TRUE)
z6rf_cls <- function(t) ifelse(t == max(z6rf_meas), "z6p-dot-bad", "z6p-dot")

# Skala temperatury słupków: 76°C na dole (y = 350), 90°C na górze (y = 40).
z6rf_y <- function(t) 350 - (t - 76) / 14 * 310

# Słupek-termometr o środku xc i wysokości do temperatury t (wektory dozwolone).
z6rf_bar <- function(xc, t, cls = z6rf_cls(t), w = 44) {
  paste(sprintf('<rect class="%s" x="%g" y="%g" width="%g" height="%g" rx="3"/>',
    cls, xc - w / 2, z6rf_y(t), w, 350 - z6rf_y(t)), collapse = "")
}

# Kontur kwantyla modelu (przerywany) w tym samym kształcie co słupek.
z6rf_ghost <- function(xc, t, w = 44) {
  paste(sprintf('<rect class="z6p-curve-ghost" x="%g" y="%g" width="%g" height="%g" rx="3"/>',
    xc - w / 2, z6rf_y(t), w, 350 - z6rf_y(t)), collapse = "")
}

# Wartość nad słupkiem (mono).
z6rf_vlabel <- function(xc, t) {
  paste(sprintf('<text class="z6p-mono" x="%g" y="%g" font-size="14" text-anchor="middle">%s</text>',
    xc, z6rf_y(t) - 8, z6rf_fmt(t)), collapse = "")
}

# Oś temperatury po lewej stronie słupków, podziałka co 2°C.
z6rf_axis <- function() {
  t <- seq(76L, 90L, 2L)
  paste0(
    '<line class="z6p-axis" x1="110" x2="110" y1="40" y2="350"/>',
    '<line class="z6p-axis" x1="110" x2="760" y1="350" y2="350"/>',
    paste(sprintf('<line class="z6p-axis" x1="105" x2="110" y1="%g" y2="%g"/><text class="z6p-tick" style="text-anchor:end" x="100" y="%g">%d</text>',
      z6rf_y(t), z6rf_y(t), z6rf_y(t) + 4, t), collapse = ""),
    '<text class="z6p-subtle" x="110" y="26" font-size="12" text-anchor="middle">°C</text>'
  )
}

# Numery pod słupkami (kolejność rosnąca).
z6rf_numbers <- function(xs, y = 372) {
  paste(sprintf('<text class="z6p-mono" x="%g" y="%g" font-size="14" text-anchor="middle">%d</text>',
    xs, y, seq_along(xs)), collapse = "")
}

# Wykres Q–Q: oś pozioma z (kwantyl modelu) od −2 do 2 między x0 i x1, oś pionowa
# z pomiarem od 76 do 90°C między yb (76) i yt (90). Prosta modelu y = 82 + 3z
# jako przerywana linia. segments = "all" rysuje odcinki do prostej dla wszystkich
# punktów, "last" tylko dla największego pomiaru (z napisem różnicy).
z6rf_qq <- function(x0, x1, yb, yt, segments = "last") {
  xz <- function(z) x0 + (z + 2) / 4 * (x1 - x0)
  yv <- function(t) yb - (t - 76) / 14 * (yb - yt)
  last <- length(z6rf_sorted)
  px <- xz(z6rf_zq); py <- yv(z6rf_sorted); ly <- yv(z6p_mu + z6p_sd * z6rf_zq)
  zt <- -2:2
  tt <- seq(76L, 90L, 2L)
  seg_idx <- if (segments == "all") seq_len(last) else last
  paste0(
    sprintf('<line class="z6p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x0, x1, yb, yb),
    sprintf('<line class="z6p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/>', x0, x0, yt, yb),
    paste(sprintf('<line class="z6p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="z6p-mono z6p-subtle" x="%g" y="%g" font-size="12" text-anchor="middle">%s</text>',
      xz(zt), xz(zt), yb, yb + 5, xz(zt), yb + 18, c("−2", "−1", "0", "1", "2")), collapse = ""),
    paste(sprintf('<line class="z6p-axis" x1="%g" x2="%g" y1="%g" y2="%g"/><text class="z6p-tick" style="text-anchor:end" x="%g" y="%g">%d</text>',
      x0 - 5, x0, yv(tt), yv(tt), x0 - 8, yv(tt) + 4, tt), collapse = ""),
    sprintf('<text class="z6p-subtle" x="%g" y="%g" font-size="12" text-anchor="end">kwantyl modelu (z)</text>', x1, yb + 40),
    sprintf('<text class="z6p-subtle" x="%g" y="%g" font-size="12">pomiar (°C)</text>', x0, yt - 14),
    sprintf('<line class="z6p-curve-ghost" x1="%g" x2="%g" y1="%g" y2="%g"/>', xz(-2), xz(2), yv(z6p_mu - 2 * z6p_sd), yv(z6p_mu + 2 * z6p_sd)),
    paste(sprintf('<line x1="%g" x2="%g" y1="%g" y2="%g" style="stroke:var(--upwr-accent);stroke-width:2"/>',
      px[seg_idx], px[seg_idx], py[seg_idx], ly[seg_idx]), collapse = ""),
    paste(sprintf('<circle class="z6p-dot" cx="%g" cy="%g" r="6"/>', px[-last], py[-last]), collapse = ""),
    sprintf('<circle class="z6p-dot-bad" cx="%g" cy="%g" r="7"/>', px[last], py[last]),
    sprintf('<text class="z6p-mono z6p-bad" x="%g" y="%g" font-size="15" font-weight="700" text-anchor="end">%s°C</text>',
      px[last] - 12, py[last] - 6, z6rf_sgn(z6rf_diff[last]))
  )
}

# Kadry P15A. Każdy kadr to SVG 800 × 400 policzony tutaj.
z6rf_svg <- function(i) {
  xs <- c(200, 300, 400, 500, 600)
  aria <- c(
    "Pięć pomiarów temperatury łożyska w kolejności zapisu, od 79.4 do 88.6 stopni.",
    "Te same pięć pomiarów posortowane rosnąco, z numerami od 1 do 5.",
    "Pary: pomiar i kwantyl modelu normalnego tego samego rzędu, z różnicą nad każdą parą.",
    "Wykres kwantylowy: pięć punktów nad prostą modelu, ostatni odstaje o 2.8 stopnia.")[i]
  body <- switch(i,
    paste0(z6rf_axis(), z6rf_bar(xs, z6rf_meas), z6rf_vlabel(xs, z6rf_meas),
      '<text class="z6p-subtle" x="760" y="392" font-size="12" text-anchor="end">kolejność zapisu · oś pionowa: temperatura (°C)</text>'),
    paste0(z6rf_axis(), z6rf_bar(xs, z6rf_sorted), z6rf_vlabel(xs, z6rf_sorted), z6rf_numbers(xs),
      '<text class="z6p-subtle" x="760" y="392" font-size="12" text-anchor="end">posortowane rosnąco · oś pionowa: temperatura (°C)</text>'),
    {
      ptop <- pmin(z6rf_y(z6rf_sorted), z6rf_y(z6rf_q))
      diff_cls <- ifelse(z6rf_sorted == max(z6rf_meas), "z6p-mono z6p-bad", "z6p-mono")
      paste0(z6rf_axis(),
        z6rf_bar(xs - 24, z6rf_sorted, w = 40), z6rf_ghost(xs + 24, z6rf_q, w = 40),
        paste(sprintf('<text class="%s" x="%g" y="%g" font-size="15" font-weight="700" text-anchor="middle">%s</text>',
          diff_cls, xs, ptop - 12, z6rf_sgn(z6rf_diff)), collapse = ""),
        z6rf_numbers(xs),
        '<text class="z6p-subtle" x="760" y="392" font-size="12" text-anchor="end">pomiar (wypełniony) · kwantyl modelu N(82, 3) (kontur) · różnica pomiar − model</text>')
    },
    paste0(
      # Zmniejszone pary po lewej: ta sama skala pionowa co wykres po prawej.
      sprintf('<line class="z6p-axis" x1="40" x2="290" y1="340" y2="340"/>'),
      paste(sprintf('<rect class="%s" x="%g" y="%g" width="18" height="%g" rx="2"/>',
        ifelse(z6rf_sorted == max(z6rf_meas), "z6p-dot-bad", "z6p-dot"),
        c(60, 108, 156, 204, 252) - 11, 340 - (z6rf_sorted - 76) / 14 * 300,
        (z6rf_sorted - 76) / 14 * 300), collapse = ""),
      paste(sprintf('<rect class="z6p-curve-ghost" x="%g" y="%g" width="18" height="%g" rx="2"/>',
        c(60, 108, 156, 204, 252) + 11, 340 - (z6rf_q - 76) / 14 * 300,
        (z6rf_q - 76) / 14 * 300), collapse = ""),
      paste(sprintf('<text class="z6p-mono" x="%g" y="356" font-size="11" text-anchor="middle">%d</text>',
        c(60, 108, 156, 204, 252), 1:5), collapse = ""),
      '<text class="z6p-subtle" x="40" y="30" font-size="12">pary pomiar i model</text>',
      z6rf_qq(500, 760, 340, 40, segments = "last")
    )
  )
  z6p_svg(800, 400, aria, body)
}

z6rf_frames <- list(
  list(name = "Pomiary",
    text = "Pięć pomiarów temperatury łożyska, w kolejności, w jakiej je zapisano. Każdy słupek to jeden pomiar, nad nim wartość w °C.",
    read = NULL),
  list(name = "Rosnąco",
    text = "Te same słupki posortowane rosnąco. Numer pod słupkiem to miejsce pomiaru w rzędzie.",
    read = NULL),
  list(name = "Kwantyle modelu",
    text = "Obok każdego pomiaru przerywany kontur: kwantyl N(82, 3) tego samego rzędu. Nad parą różnica pomiar − model.",
    read = NULL),
  list(name = "Wykres Q–Q",
    text = "Każda para daje jeden punkt: poziomo kwantyl modelu, pionowo pomiar. Cztery punkty leżą blisko prostej, piąty odstaje.",
    read = sprintf("największy pomiar: %s°C · model: %s°C · różnica %s°C",
      z6rf_fmt(max(z6rf_sorted)), z6rf_fmt(z6rf_q[5]), z6rf_sgn(z6rf_diff[5])))
)

z6p_15a_ui <- z6p_frames_ui("P15A", "pojęcie", "Pięć pomiarów obok modelu", z6rf_frames, z6rf_svg)

# P15B: jeden statyczny rysunek — tabela po lewej, wykres Q–Q po prawej.
z6rf_table_svg <- function() {
  row_y <- 84 + (0:4) * 30
  cls <- ifelse(seq_len(5) == 5, "z6p-mono z6p-bad", "z6p-mono")
  paste0(
    '<text class="z6p-subtle" x="40" y="40" font-size="12" font-weight="700">i</text>',
    '<text class="z6p-subtle" x="90" y="40" font-size="12" font-weight="700">pomiar</text>',
    '<text class="z6p-subtle" x="180" y="40" font-size="12" font-weight="700">kwantyl modelu</text>',
    '<text class="z6p-subtle" x="320" y="40" font-size="12" font-weight="700">różnica</text>',
    '<line class="z6p-axis" x1="36" x2="380" y1="52" y2="52"/>',
    paste(sprintf('<text class="z6p-mono" x="40" y="%g" font-size="14">%d</text>', row_y, 1:5), collapse = ""),
    paste(sprintf('<text class="z6p-mono" x="90" y="%g" font-size="14">%s</text>', row_y, z6rf_fmt(z6rf_sorted)), collapse = ""),
    paste(sprintf('<text class="z6p-mono" x="180" y="%g" font-size="14">%s</text>', row_y, z6rf_fmt(z6rf_q)), collapse = ""),
    paste(sprintf('<text class="%s" x="320" y="%g" font-size="14">%s</text>', cls, row_y, z6rf_sgn(z6rf_diff)), collapse = "")
  )
}

z6p_15b_ui <- z6p_panel("P15B", "pojęcie", "Wykres Q–Q z pięciu pomiarów", z6p_css,
  z6p_svg(800, 360, "Tabela pięciu pomiarów z kwantylami modelu i różnicami oraz wykres Q–Q: cztery punkty blisko prostej, ostatni odstaje o 2.8 stopnia.",
    z6rf_table_svg(), z6rf_qq(460, 760, 300, 40, segments = "all")),
  lc_caption("Cztery pierwsze punkty leżą blisko prostej, ostatni odstaje o prawie 1σ. Przy pięciu pomiarach to tylko sygnał, nie dowód o ogonie."))
