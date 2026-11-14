# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa re: P9, P10, P11B, P12 (wykład 10) -----
#
#   i10p_9_ui   — Pojęcie (kadry) „Redukcja schematu” (P9): schemat P – C – (A ∥ B) redukowany
#                 krok po kroku, 5 kadrów; liczby z i10p_mission() dla modelu wykładniczego
#                 i Weibulla. Bloki i schemat rysuje R (i10re_block, i10re_scheme).
#   i10p_10_ui  — Piaskownica „Wspólny czas misji” (P10): suwak czasu i przełącznik modelu
#                 przeliczają wszystkie elementy naraz. Scena „uklad” w modules/proto_re.js.
#   i10p_11b_ui — Pojęcie (kadry) „Lejek 10 000 misji” (P11B): 5 kadrów, trzy poziomy pasków,
#                 każdy niższy jest powiększeniem fragmentu wyższego.
#   i10p_12_ui  — Przypadek brzegowy (kadry) „Tysiąc lat po trzy misje” (P12): siatka 1000 lat,
#                 4 kadry; położenia kratek losowane raz, ze stałym ziarnem (set.seed(10)).
# Wymaga: stop_proto_common.R (i10p_cfg, i10p_mission, i10p_panel, i10p_frames_ui, i10p_scene_ui,
# i10p_svg, i10p_p, i10p_num) oraz risk_math.R / risk_block.R (risk_fmt_p).
# Liczby modelu tylko z i10p_cfg i i10p_mission(); w tekstach i odczytach sprintf z policzonych
# wartości. Jedyne stałe w tym pliku to wymiary rysunku (px) i układ siatki 50 × 20 = 1000 lat.

# ============================================================================
# WSPÓLNE: rachunek misji i rysowanie schematu
# ============================================================================

i10re_m_exp <- i10p_mission()
i10re_m_wei <- i10p_mission(model = "weibull")

# Liczba z kropką, bez końcowych zer (do tekstów); całe liczby bez części dziesiętnej.
i10re_fmt <- function(x) if (abs(x - round(x)) < 1e-9) sprintf("%d", round(x)) else sprintf("%.1f", x)

# Tekst SVG: x, y, anchor („start”, „middle”, „end”), klasa opcjonalna, rozmiar w px.
i10re_txt <- function(x, y, text, anchor = "middle", cls = NULL, size = 14, extra = "") {
  sprintf('<text%s x="%g" y="%g" font-size="%g" text-anchor="%s"%s>%s</text>',
    if (is.null(cls)) "" else sprintf(' class="%s"', cls), x, y, size, anchor, extra, text)
}

# Połączenie schematu (klasa i10p-link) i kropka wejścia/wyjścia.
i10re_link <- function(x1, y1, x2, y2) sprintf('<line class="i10p-link" x1="%g" y1="%g" x2="%g" y2="%g"/>', x1, y1, x2, y2)
i10re_dot <- function(x, y) sprintf('<circle class="i10p-ink" cx="%g" cy="%g" r="6"/>', x, y)

# Blok schematu 130 × 64: nazwa 13 px, wartość R mono pogrubiona 20 px.
i10re_block <- function(x, y, name, value, cls = "i10p-box") {
  cx <- x + 65
  paste0(
    sprintf('<rect class="%s" x="%g" y="%g" width="130" height="64" rx="6"/>', cls, x, y),
    i10re_txt(cx, y + 22, name, size = 13),
    i10re_txt(cx, y + 50, value, size = 20, cls = "i10p-mono", extra = ' font-weight="700"'))
}

# Schemat z listy wartości v (P, C, A, B, par, sys, one — napisy liczb).
# mode: „full” — P, C, rozgałęzienie na A i B; „par” — P, C, blok A ∥ B;
# „sys” — jeden blok układu chłodzenia; „one” — P, C, A bez B i wynik pod spodem.
i10re_scheme <- function(v, mode = "full") {
  yc <- 120
  dot_in <- i10re_dot(30, yc)
  dot_out <- i10re_dot(770, yc)
  if (mode == "sys") return(paste0(
    i10re_link(36, yc, 335, yc), dot_in,
    i10re_block(335, 88, "Układ chłodzenia", v$sys, "i10p-box-ok"),
    i10re_link(465, yc, 764, yc), dot_out))
  if (mode == "par") return(paste0(
    i10re_link(36, yc, 70, yc), dot_in,
    i10re_block(70, 88, "Zasilanie P", v$P),
    i10re_link(200, yc, 240, yc),
    i10re_block(240, 88, "Sterownik C", v$C),
    i10re_link(370, yc, 450, yc),
    i10re_block(450, 88, "A ∥ B", v$par, "i10p-box-ok"),
    i10re_link(580, yc, 764, yc), dot_out))
  if (mode == "one") return(paste0(
    i10re_link(46, yc, 90, yc), i10re_dot(40, yc),
    i10re_block(90, 88, "Zasilanie P", v$P),
    i10re_link(220, yc, 270, yc),
    i10re_block(270, 88, "Sterownik C", v$C),
    i10re_link(400, yc, 450, yc),
    i10re_block(450, 88, "Wentylator A", v$A),
    i10re_link(580, yc, 714, yc), i10re_dot(720, yc),
    sprintf('<rect class="i10p-box-dim" x="270" y="186" width="260" height="44" rx="6"/>'),
    i10re_txt(400, 214, sprintf("R układu bez B = %s", v$one), size = 18, cls = "i10p-mono", extra = ' font-weight="700"')))
  # mode „full”
  paste0(
    i10re_link(36, yc, 70, yc), dot_in,
    i10re_block(70, 88, "Zasilanie P", v$P),
    i10re_link(200, yc, 240, yc),
    i10re_block(240, 88, "Sterownik C", v$C),
    i10re_link(370, yc, 410, yc),
    i10re_link(410, 56, 410, 184),
    i10re_link(410, 56, 450, 56), i10re_link(410, 184, 450, 184),
    i10re_block(450, 24, "Wentylator A", v$A),
    i10re_block(450, 152, "Wentylator B", v$B),
    i10re_link(580, 56, 620, 56), i10re_link(580, 184, 620, 184),
    i10re_link(620, 56, 620, 184),
    i10re_link(620, yc, 764, yc), dot_out)
}

# ============================================================================
# P9 — POJĘCIE (KADRY) „REDUKCJA SCHEMATU”
# ============================================================================

i10re_9_vals <- function(m) list(
  P = i10p_p(m$power_r), C = i10p_p(m$controller_r),
  A = i10p_p(m$fan_r), B = i10p_p(m$fan_r),
  par = i10p_p(m$parallel_r), sys = i10p_p(m$system_r),
  one = i10p_p(m$power_r * m$controller_r * m$fan_r))
i10re_9_x <- i10re_9_vals(i10re_m_exp)
i10re_9_w <- i10re_9_vals(i10re_m_wei)

# Wartości liczbowe do tekstów kadrów (ten sam rachunek, co w schemacie).
i10re_9_num <- list(
  fan = i10re_m_exp$fan_r, par = i10re_m_exp$parallel_r, sys = i10re_m_exp$system_r,
  one = i10re_m_exp$power_r * i10re_m_exp$controller_r * i10re_m_exp$fan_r,
  wpar = i10re_m_wei$parallel_r, wsys = i10re_m_wei$system_r)

i10re_9_frames <- list(
  list(name = "Schemat",
    text = sprintf("Wszystkie cztery elementy liczymy dla tej samej misji %s %s. Zasilanie i sterownik są konieczne, z dwóch wentylatorów wystarcza jeden.",
      i10p_cfg$time, i10p_cfg$unit),
    read = NULL),
  list(name = "Gałąź równoległa",
    text = "Gałąź równoległa zawodzi tylko wtedy, gdy zawiodą oba wentylatory.",
    read = sprintf("1 − (1 − %s)² = %s", i10p_p(i10re_9_num$fan), i10p_p(i10re_9_num$par))),
  list(name = "Szereg",
    text = "Elementy szeregowe mnożymy.",
    read = sprintf("R_sys = %s · %s · %s = %s, czyli P(S | I) = %s",
      i10p_p(i10re_m_exp$power_r), i10p_p(i10re_m_exp$controller_r), i10p_p(i10re_m_exp$parallel_r),
      i10p_p(i10re_m_exp$system_r), i10p_p(1 - i10re_m_exp$system_r))),
  list(name = "Jeden wentylator",
    text = sprintf("Bez redundancji ten sam układ miałby niezawodność %s. Drugi wentylator podnosi ją o ponad %s.",
      i10p_p(i10re_9_num$one), i10p_num(i10re_m_exp$system_r - i10re_9_num$one, 2)),
    read = sprintf("%s · %s · %s = %s",
      i10p_p(i10re_m_exp$power_r), i10p_p(i10re_m_exp$controller_r), i10p_p(i10re_m_exp$fan_r),
      i10p_p(i10re_9_num$one))),
  list(name = "Weibull",
    text = sprintf("Przy drugiej hipotezie o czasie życia wentylatora te same kroki dają %s.", i10p_p(i10re_9_num$wsys)),
    read = sprintf("gałąź %s · R_sys = %s", i10p_p(i10re_9_num$wpar), i10p_p(i10re_9_num$wsys)))
)

i10re_9_svg <- function(i) {
  svg <- switch(i,
    i10re_scheme(i10re_9_x, "full"),
    i10re_scheme(i10re_9_x, "par"),
    i10re_scheme(i10re_9_x, "sys"),
    i10re_scheme(i10re_9_x, "one"),
    i10re_scheme(i10re_9_w, "full"))
  aria <- c(
    sprintf("Cztery bloki: zasilanie %s, sterownik %s, wentylatory A i B po %s.",
      i10re_9_x$P, i10re_9_x$C, i10re_9_x$A),
    sprintf("Gałąź równoległa zastąpiona jednym blokiem A ∥ B o wartości %s.", i10re_9_x$par),
    sprintf("Cały układ chłodzenia jako jeden blok o wartości %s.", i10re_9_x$sys),
    sprintf("Schemat bez wentylatora B: P, C, A i wynik %s.", i10re_9_x$one),
    sprintf("Schemat dla Weibulla: wentylatory %s, gałąź %s, układ %s.",
      i10re_9_w$A, i10p_p(i10re_9_num$wpar), i10re_9_w$sys))[i]
  i10p_svg(800, 260, aria, svg)
}

i10p_9_ui <- i10p_frames_ui("P9", "pojęcie (kadry)", "Redukcja schematu", i10re_9_frames, i10re_9_svg)

# ============================================================================
# P10 — PIASKOWNICA „WSPÓLNY CZAS MISJI”
# ============================================================================
# Scena „uklad” (modules/proto_re.js) czyta suwak i10p10_t i przełącznik model
# z kontenera i przelicza U.mission() w przeglądarce; R podaje tylko config i sloty.

# Suwak jak i10p_slider(): etykieta, wartość i jednostka w jednym wierszu (output ma globalnie display: block).
i10re_slider <- function(id, label, min, max, value, step, suffix = NULL) {
  tags$div(class = "lc-grp i10p-slider",
    tags$label(`for` = id, class = "lc-grp-l",
      tags$span(label, " ", tags$output(`data-value` = id, style = "display: inline;", value), suffix)),
    tags$input(id = id, type = "range", min = min, max = max, value = value, step = step))
}

i10p_10_ui <- i10p_scene_ui("P10", "piaskownica", "Wspólny czas misji", "uklad", i10p_cfg,
  tags$div(class = "i10p-nav",
    i10re_slider("i10p10_t", "Czas misji", 100, i10p_cfg$time_max, i10p_cfg$time, 50,
      suffix = paste0(" ", i10p_cfg$unit)),
    i10p_seg("model", "Model wentylatora", c("Wykładniczy" = "exp", "Weibull" = "weibull"))),
  i10p_out("stage"),
  i10p_out("read"))

# ============================================================================
# P11B — POJĘCIE (KADRY) „LEJEK 10 000 MISJI”
# ============================================================================

i10p11_n <- 10000

# Lejek: liczby misji na każdym poziomie (ten sam rachunek co w przykładzie 10.8).
i10re_11 <- local({
  m <- i10p_mission()
  dem <- i10p11_n * i10p_cfg$init
  miss <- dem * m$miss
  det <- dem - miss
  cool <- det * m$cooling_failure
  ok <- det * m$system_r
  list(dem = dem, miss = miss, det = det, cool = cool, ok = ok,
       loss = miss + cool, top = m$top, miss_p = m$miss, sys = m$system_r,
       init = i10p_cfg$init)
})

i10re_11_frames <- list(
  list(name = "10 000 misji",
    text = "Jednostką rachunku jest misja. Zaczynamy od 10 000 porównywalnych misji.",
    read = NULL),
  list(name = "Zapotrzebowanie",
    text = sprintf("Zapotrzebowanie na chłodzenie pojawia się w %s z nich. Tylko w tych misjach można utracić ochronę.",
      i10re_fmt(i10re_11$dem)),
    read = NULL),
  list(name = "Przeoczenia",
    text = sprintf("Detektor przeocza %s zapotrzebowań, czyli %s misji.",
      sprintf("%g%%", round(100 * i10re_11$miss_p, 6)), i10re_fmt(i10re_11$miss)),
    read = NULL),
  list(name = "Chłodzenie",
    text = sprintf("W %s wykrytych misjach układ musi przepracować całą misję. Nie udaje się to w %s z nich.",
      i10re_fmt(i10re_11$det), i10re_fmt(i10re_11$cool)),
    read = NULL),
  list(name = "Razem",
    text = sprintf("Ochronę tracimy w około %d misjach na %s.", round(i10re_11$loss), format(i10p11_n, big.mark = " ", scientific = FALSE)),
    read = sprintf("P(TOP) = %s · [1 − %s · %s] = %s",
      i10p_num(i10re_11$init, 3), i10p_num(1 - i10re_11$miss_p, 3), i10p_num(i10re_11$sys, 3),
      i10p_p(i10re_11$top)))
)

# Rysunek kadru k (1–5): paski na trzech poziomach, połączenia jak w lupie.
i10re_11_svg <- function(k) {
  R <- i10re_11
  W <- 720; x0 <- 40; h <- 34
  y1 <- 30; y2 <- 110; y3 <- 190
  frag <- max(6, W * R$dem / i10p11_n)      # fragment poziomu 1 (min. 6 px)
  miss_w <- max(36, W * R$miss / R$dem)     # przeoczone na poziomie 2 (min. 36 px)
  cool_w <- W * R$cool / R$det              # awarie chłodzenia na poziomie 3
  p <- character(0)
  add <- function(...) p <<- c(p, ...)

  # Poziom 1: 10 000 misji z wyróżnionym fragmentem.
  add(sprintf('<rect class="i10p-fill-idle" x="%g" y="%g" width="%g" height="%g"/>', x0, y1, W, h),
    sprintf('<rect class="i10p-fill-warn" x="%g" y="%g" width="%g" height="%g"/>', x0, y1, frag, h),
    i10re_txt(x0 + frag + 10, y1 + 22, "50 z zapotrzebowaniem", anchor = "start"),
    i10re_txt(x0 + W - 10, y1 + 22, "10 000 misji", anchor = "end"))

  # Poziom 2: 50 z zapotrzebowaniem, od kadru 3 podzielone na przeoczone i wykryte.
  if (k >= 2) {
    add(i10re_link(x0, y1 + h, x0, y2), i10re_link(x0 + frag, y1 + h, x0 + W, y2))
    if (k == 2) {
      add(sprintf('<rect class="i10p-fill-warn" x="%g" y="%g" width="%g" height="%g"/>', x0, y2, W, h),
        i10re_txt(x0 + W / 2, y2 + 22, "50 misji z zapotrzebowaniem"))
    } else {
      add(sprintf('<rect class="i10p-fill-bad" x="%g" y="%g" width="%g" height="%g"/>', x0, y2, miss_w, h),
        sprintf('<rect class="i10p-fill-warn" x="%g" y="%g" width="%g" height="%g"/>', x0 + miss_w, y2, W - miss_w, h),
        i10re_txt(x0 + 8, y2 - 10, "2.5 przeoczone", anchor = "start"),
        i10re_txt(x0 + miss_w + (W - miss_w) / 2, y2 + 22, "47.5 wykryte"))
    }
  }

  # Poziom 3: wykryte misje, od kadru 4 podzielone na awarie chłodzenia i utrzymaną ochronę.
  if (k >= 4) {
    add(i10re_link(x0 + miss_w, y2 + h, x0, y3), i10re_link(x0 + W, y2 + h, x0 + W, y3),
      sprintf('<rect class="i10p-fill-bad" x="%g" y="%g" width="%g" height="%g"/>', x0, y3, cool_w, h),
      sprintf('<rect class="i10p-fill-ok" x="%g" y="%g" width="%g" height="%g"/>', x0 + cool_w, y3, W - cool_w, h),
      i10re_txt(x0 + cool_w / 2, y3 + 22, sprintf("%s chłodzenie zawiodło", i10re_fmt(R$cool)), cls = "i10p-inv"),
      i10re_txt(x0 + cool_w + (W - cool_w) / 2, y3 + 22, sprintf("%s ochrona utrzymana", i10re_fmt(R$ok))),
      i10re_txt(x0 + W, y3 - 10, sprintf("%s wykrytych", i10re_fmt(R$det)), anchor = "end"))
  }

  # Kadr 5: wynik.
  if (k == 5) {
    add(sprintf('<rect class="i10p-box-hit" x="200" y="250" width="400" height="48" rx="6"/>'),
      i10re_txt(400, 280, sprintf("%s + %s ≈ %d misji bez ochrony", i10re_fmt(R$miss), sprintf("%.1f", R$cool), round(R$loss)),
        size = 18, extra = ' font-weight="700"'))
  }
  i10p_svg(800, 320, c(
    sprintf("Lejek: z %s misji %s ma zapotrzebowanie.", format(i10p11_n, big.mark = " ", scientific = FALSE), i10re_fmt(R$dem)),
    sprintf("Poziom zapotrzebowania: %s misji.", i10re_fmt(R$dem)),
    sprintf("Poziom zapotrzebowania podzielony na przeoczone %s i wykryte %s.", i10re_fmt(R$miss), i10re_fmt(R$det)),
    sprintf("Poziom wykrytych misji podzielony na awarie chłodzenia %s i ochronę utrzymaną %s.", i10re_fmt(R$cool), i10re_fmt(R$ok)),
    sprintf("Wynik: około %d misji na %s bez ochrony.", round(R$loss), format(i10p11_n, big.mark = " ", scientific = FALSE)))[k], p)
}

i10p_11b_ui <- i10p_frames_ui("P11B", "pojęcie (kadry)", "Lejek 10 000 misji", i10re_11_frames,
  function(i) i10re_11_svg(i))

# ============================================================================
# P12 — PRZYPADEK BRZEGOWY „TYSIĄC LAT PO TRZY MISJE”
# ============================================================================

i10re_12_m <- i10p_mission()
i10re_12_k <- i10p_cfg$missions
i10re_12_p <- list(
  sys = 1 - i10re_12_m$system_r^i10re_12_k,            # układ by zawiódł w co najmniej jednej misji
  dem = 1 - (1 - i10p_cfg$init)^i10re_12_k,            # zapotrzebowanie w co najmniej jednej misji
  year = 1 - (1 - i10re_12_m$top)^i10re_12_k)          # utrata ochrony w roku
i10re_12_n <- lapply(i10re_12_p, function(p) round(1000 * p))   # liczba lat na 1000

# Losowanie kratek: 1000 lat = siatka 50 × 20. Ziarno stałe, losowanie raz.
i10re_12_sel <- local({
  set.seed(10)
  fail <- sample(1000L, i10re_12_n$sys)
  dem <- sample(1000L, i10re_12_n$dem)
  loss <- sample(dem, i10re_12_n$year)
  list(fail = fail, dem = dem, loss = loss)
})

# Kratki jako wektor: indeks 1..1000 → pozycja w siatce 50 × 20 (kratka 13 px, odstęp 2 px).
i10re_cells <- function(idx, cls, x0 = 26, y0 = 16) {
  if (!length(idx)) return("")
  col <- (idx - 1L) %% 50L
  row <- (idx - 1L) %/% 50L
  paste(sprintf('<rect class="%s" x="%g" y="%g" width="13" height="13" rx="2"/>', cls, x0 + 15 * col, y0 + 15 * row),
    collapse = "")
}

# Legenda pod siatką: element pojawia się razem z kadrem, w którym wchodzi.
i10re_12_legend <- function(k) {
  item <- function(x, cls, text) paste0(
    sprintf('<rect class="%s" x="%g" y="342" width="13" height="13" rx="2"/>', cls, x),
    i10re_txt(x + 22, 353, text, anchor = "start"))
  paste0(
    if (k >= 2) item(200, "i10p-fill-warn", "układ by zawiódł") else "",
    if (k >= 3) item(390, "i10p-fill-idle i10re-dem", "było zapotrzebowanie") else "",
    if (k >= 4) item(580, "i10p-fill-bad i10re-dem", "utrata ochrony") else "")
}

i10re_12_svg <- function(k) {
  s <- i10re_12_sel
  fail <- if (k >= 2) s$fail else integer(0)
  rest <- setdiff(seq_len(1000L), fail)
  body <- if (k == 1) {
    i10re_cells(seq_len(1000L), "i10p-fill-idle")
  } else if (k == 2) {
    paste0(i10re_cells(rest, "i10p-fill-idle"), i10re_cells(fail, "i10p-fill-warn"))
  } else {
    loss_idx <- if (k == 4) s$loss else integer(0)
    dem_cells <- vapply(s$dem, function(d) {
      cls <- if (d %in% loss_idx) "i10p-fill-bad" else if (d %in% fail) "i10p-fill-warn" else "i10p-fill-idle"
      i10re_cells(d, paste(cls, "i10re-dem"))
    }, character(1))
    paste0(
      i10re_cells(rest, "i10p-fill-idle"),
      sprintf('<g opacity=".35">%s</g>', i10re_cells(fail, "i10p-fill-warn")),
      paste(dem_cells, collapse = ""))
  }
  i10p_svg(800, 380, c("Siatka 1000 kratek: każda to jeden rok.",
    sprintf("Kratki, w których układ by zawiódł: %d z 1000.", i10re_12_n$sys),
    sprintf("Kratki z zapotrzebowaniem: %d z 1000, obwiedzione.", i10re_12_n$dem),
    sprintf("Kratki z utratą ochrony: %d z 1000, wypełnione.", i10re_12_n$year))[k],
    body, i10re_12_legend(k))
}

i10re_12_frames <- list(
  list(name = "1000 lat",
    text = sprintf("Każda kratka to jeden rok z trzema misjami po %s %s.", i10p_cfg$time, i10p_cfg$unit),
    read = NULL),
  list(name = "Układ by zawiódł",
    text = sprintf("Gdyby układ chłodzenia musiał pracować w każdej misji, zawiódłby w co najmniej jednej z trzech w %d latach na 1000.",
      i10re_12_n$sys),
    read = sprintf("1 − %s³ = %s", i10p_num(i10re_12_m$system_r, 3), i10p_p(i10re_12_p$sys))),
  list(name = "Zapotrzebowanie",
    text = sprintf("Układ jest potrzebny tylko wtedy, gdy pojawi się zapotrzebowanie. Zdarza się to w %d latach na 1000.",
      i10re_12_n$dem),
    read = sprintf("1 − %s³ = %s", i10p_num(1 - i10p_cfg$init, 3), i10p_p(i10re_12_p$dem))),
  list(name = "Utrata ochrony",
    text = sprintf("Ochronę tracimy w %d latach na 1000, nie w %d. Liczba %.2f opisuje niezawodność sprzętu i gubi warunek zapotrzebowania.",
      i10re_12_n$year, i10re_12_n$sys, i10re_12_p$sys),
    read = sprintf("P_rok = 1 − (1 − %s)³ = %s", i10p_p(i10re_12_m$top), i10p_p(i10re_12_p$year)))
)

i10p_12_ui <- tagList(
  tags$style(HTML(".i10p-svg .i10re-dem { stroke: var(--upwr-ink); stroke-width: 2.5; }")),
  i10p_frames_ui("P12", "przypadek brzegowy", "Tysiąc lat po trzy misje", i10re_12_frames,
    function(i) i10re_12_svg(i)))
