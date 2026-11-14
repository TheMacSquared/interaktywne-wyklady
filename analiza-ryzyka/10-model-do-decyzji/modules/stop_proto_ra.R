# PROTOTYPY PUNKTÓW STOP (2026-11-14) — P1 i P7 (wzorzec dla pozostałych) ---------
#
#   i10p_1_ui — Scena „Cztery liczby na biurku” (P1), 4 kadry policzone w R: kartki →
#               pytania → warunki → która odpowiada zarządowi. Liczby z i10p_cfg:
#               1 − P(I | alarm), P(S | I), 1 − R_sys³ i P(TOP) na 10 000 misji.
#               Dane kartek (i10p1_notes) wracają w domknięciu P18B.
#   i10p_7_ui — Galeria „Ta sama średnia, inne ryzyko” (P7): rycina lcSF i10_modele
#               (modules/proto_ra.js), oba modele czasu życia na stałych osiach.
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R.

# ============================================================================
# P1 — SCENA „CZTERY LICZBY NA BIURKU”
# ============================================================================

i10p1_base <- i10p_mission()
i10p1_post <- risk_bayes(i10p_cfg$init, i10p_cfg$sens, i10p_cfg$fpr)
i10p1_year_sys <- 1 - i10p1_base$system_r^i10p_cfg$missions
i10p1_pct <- function(p) paste0(round(100 * p), "%")

# Kartki: kto pisze, liczba, zdanie z kartki, pytanie, warunek, gdzie wraca.
i10p1_notes <- list(
  list(who = "OPERATOR", num = i10p1_pct(1 - i10p1_post), says = c("alarmów jest", "fałszywych"),
    ask = c("Czy wierzyć", "alarmowi?"), cond = "wśród alarmów", back = "wraca: rozdz. 2"),
  list(who = "SERWIS", num = i10p1_pct(i10p1_base$cooling_failure), says = c("misji kończy się", "awarią chłodzenia"),
    ask = c("Czy układ wytrzyma", "misję, gdy jest potrzebny?"), cond = "gdy jest zapotrzebowanie", back = "wraca: rozdz. 3"),
  list(who = "UTRZYMANIE", num = i10p1_pct(i10p1_year_sys), says = c("rocznie: ryzyko", "awarii chłodzenia"),
    ask = c("Czy układ przepracuje", "trzy misje bez awarii?"), cond = "zapotrzebowanie pominięte", back = "pułapka: rozdz. 3"),
  list(who = "ANALITYK", num = as.character(round(1e4 * i10p1_base$top)), says = c("na 10 000 misji", "kończy się bez ochrony"),
    ask = c("Jak często misja", "zostaje bez ochrony?"), cond = "wśród wszystkich misji", back = "odpowiada zarządowi")
)

i10p1_card <- function(j, step) {
  n <- i10p1_notes[[j]]; w <- 184; x <- 14 + (j - 1) * (w + 12); cx <- x + w / 2
  last <- j == length(i10p1_notes)
  paste0(
    sprintf('<g%s>', if (step == 4 && !last) ' opacity=".5"' else ""),
    sprintf('<rect class="%s" x="%g" y="10" width="%g" height="%g" rx="6"/>',
      if (step == 4 && last) "i10p-box-ok" else "i10p-paper", x, w, if (step >= 4) 296 else if (step == 3) 262 else if (step == 2) 212 else 136),
    sprintf('<text class="i10p-mono i10p-subtle" x="%g" y="34" font-size="12" text-anchor="middle" letter-spacing="1">%s</text>', cx, n$who),
    sprintf('<text class="i10p-mono" x="%g" y="78" font-size="34" font-weight="700" text-anchor="middle">%s</text>', cx, n$num),
    paste(sprintf('<text x="%g" y="%g" font-size="14" text-anchor="middle">%s</text>', cx, 104 + 18 * (seq_along(n$says) - 1), n$says), collapse = ""),
    if (step >= 2) paste0(
      sprintf('<line class="i10p-grid" x1="%g" x2="%g" y1="146" y2="146"/>', x + 14, x + w - 14),
      sprintf('<text class="i10p-subtle" x="%g" y="166" font-size="11" text-anchor="middle">pytanie</text>', cx),
      paste(sprintf('<text x="%g" y="%g" font-size="13" font-style="italic" text-anchor="middle">%s</text>', cx, 185 + 17 * (seq_along(n$ask) - 1), n$ask), collapse = "")),
    if (step >= 3) paste0(
      sprintf('<rect class="i10p-fill-idle" x="%g" y="224" width="%g" height="28" rx="14"/>', x + 8, w - 16),
      sprintf('<text x="%g" y="243" font-size="12.5" text-anchor="middle">%s</text>', cx, n$cond)),
    if (step >= 4) sprintf('<text class="i10p-mono %s" x="%g" y="286" font-size="12" font-weight="%s" text-anchor="middle">%s</text>',
      if (last) "i10p-ok" else "i10p-subtle", cx, if (last) "700" else "400", n$back),
    "</g>")
}

i10p1_frames <- list(
  list(name = "Biurko",
    text = "Zarząd pyta, czy ryzyko w komorze chłodniczej jest akceptowalne. Dostaje cztery kartki o tej samej komorze, każdą od kogoś innego.",
    read = NULL),
  list(name = "Pytania",
    text = "Każda liczba jest poprawnie policzona. Każda odpowiada jednak na inne pytanie.",
    read = NULL),
  list(name = "Warunki",
    text = "Liczby różni warunek: wśród których misji je liczono. Tego na kartkach nie było.",
    read = NULL),
  list(name = "Która liczba",
    text = "Na pytanie zarządu odpowiada tylko ostatnia kartka. Pozostałe trzy wrócą jako składniki rachunku albo jako pułapki.",
    read = sprintf("P(TOP) ≈ %s na misję %s %s", i10p_p(i10p1_base$top), i10p_cfg$time, i10p_cfg$unit))
)

i10p1_svg <- function(step) {
  aria <- c("Cztery kartki z liczbami o tej samej komorze: 91 procent, 29 procent, 64 procent i 16 na 10 000.",
    "Te same kartki z pytaniem, na które odpowiada każda liczba.",
    "Te same kartki z warunkiem, przy którym policzono każdą liczbę.",
    "Kartka analityka wyróżniona: tylko ona odpowiada na pytanie zarządu.")[step]
  i10p_svg(800, c(156, 232, 282, 316)[step], aria,
    paste(vapply(seq_along(i10p1_notes), i10p1_card, character(1), step = step), collapse = ""))
}

i10p_1_ui <- i10p_frames_ui("P1", "scena", "Cztery liczby na biurku", i10p1_frames, i10p1_svg)

# ============================================================================
# P7 — GALERIA „TA SAMA ŚREDNIA, INNE RYZYKO”
# ============================================================================

# Przecięcie krzywych: t/mttf = (t/η)^β.
i10p7_cross <- (i10p_cfg$eta^i10p_cfg$beta / i10p_cfg$mttf)^(1 / (i10p_cfg$beta - 1))
i10p7_mttf_w <- i10p_cfg$eta * gamma(1 + 1 / i10p_cfg$beta)

i10p_7_ui <- i10p_panel("P7", "galeria", "Ta sama średnia, inne ryzyko",
  lc_sf_figure("i10_modele",
    control = list(lc_sf_slider("i10p7_t", "Czas misji", 100, i10p_cfg$time_max, i10p_cfg$time, 50, suffix = paste0(" ", i10p_cfg$unit))),
    toolbar = lc_readouts(i10p_read("re", "R wykładniczy", upwr_secondary), i10p_read("rw", "R Weibull", upwr_accent),
      i10p_read("d", "Różnica")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    config = c(i10p_cfg, list(cross = i10p7_cross)),
    after = lc_caption(sprintf("Średni czas życia: %s %s (wykładniczy) i %s %s (Weibull). Krzywe przecinają się przy %s %s; osie są stałe.",
      i10p_num(i10p_cfg$mttf, 0), i10p_cfg$unit, i10p_num(i10p7_mttf_w, 0), i10p_cfg$unit, i10p_num(i10p7_cross, 0), i10p_cfg$unit)))
)
