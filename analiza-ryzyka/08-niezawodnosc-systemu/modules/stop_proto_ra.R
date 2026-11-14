# PROTOTYPY PUNKTÓW STOP (2026-11-14) — P1 i P7 (wzorzec dla pozostałych) ---------
#
#   s8p_1_ui — Scena „Nocna awaria, trzy raporty” (P1), 5 kadrów policzonych w R:
#              noc w komorze → raport serwisanta (szereg) → kierowniczki zmiany
#              (układ równoległy) → automatyka (jeden element) → trzy liczby
#              0.81 / 0.99 / 0.90 z przykładu 8.1.
#   s8p_7_ui — Piaskownica „Długi łańcuch” (P7): rycina lcSF s8_lancuch
#              (modules/proto_ra.js), R systemu jako Rⁿ, próg 0.90 z przykładu 8.3.
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R.

# ============================================================================
# P1 — SCENA „NOCNA AWARIA, TRZY RAPORTY”
# ============================================================================

s8p1_r <- 0.9
s8p1_lab <- c("E₁", "E₂")
s8p1_sub <- rep("R = 0.9", 2)
s8p1_night <- c(TRUE, FALSE)            # tej nocy staje E₂
# Trzy logiki sukcesu: etapy schematu i niezawodność systemu.
s8p1_logic <- list(
  series = list(who = "serwisant", stages = list(1, 2), r = risk_series_reliability(c(s8p1_r, s8p1_r))),
  parallel = list(who = "kierowniczka zmiany", stages = list(c(1, 2)), r = risk_parallel_reliability(c(s8p1_r, s8p1_r))),
  single = list(who = "automatyk", stages = list(1), r = s8p1_r)
)

# Komora nocą: dwa wentylatory w hali, zegar w rogu.
s8p1_hall <- function(x, y, w, h, r = 34, labels = TRUE) {
  paste0(
    sprintf('<rect class="s8p-sunken" x="%g" y="%g" width="%g" height="%g" rx="8"/>', x, y, w, h),
    s8p_text(x + 14, y + 24, "Komora dojrzewalni", 13, "s8p-subtle"),
    s8p_text(x + w - 14, y + 24, "02:40", 13, "s8p-mono s8p-subtle", "end"),
    s8p_fan(x + w * .3, y + h * .5 + 4, r, s8p1_night[1], if (labels) "E₁ pracuje", if (labels) "R = 0.9"),
    s8p_fan(x + w * .7, y + h * .5 + 4, r, s8p1_night[2], if (labels) "E₂ stoi", if (labels) "R = 0.9")
  )
}

# Kadry 2–4: po lewej mała komora, po prawej raport z werdyktem, niżej schemat autora raportu.
s8p1_report <- function(key, verdict, tone) {
  lg <- s8p1_logic[[key]]
  paste0(
    s8p1_hall(40, 16, 250, 120, r = 26, labels = FALSE),
    '<rect class="s8p-paper" x="330" y="16" width="430" height="120" rx="6"/>',
    s8p_text(352, 46, paste0("RAPORT · ", lg$who), 14, weight = 700),
    s8p_text(352, 76, "Zdarzenie: postój wentylatora E₂", 14, "s8p-subtle"),
    s8p_text(352, 112, "Ocena:", 14, "s8p-subtle"),
    s8p_chip(410, 112, verdict, tone, w = 250),
    s8p_rbd_center(400, 218, stages = lg$stages, labels = s8p1_lab, sub = s8p1_sub, up = s8p1_night,
      off = if (key == "single") 2)
  )
}

# Kadr 5: trzy schematy (wszystkie bloki sprawne) i trzy niezawodności.
s8p1_three <- function() {
  keys <- c("series", "parallel", "single")
  paste(vapply(seq_along(keys), function(i) {
    lg <- s8p1_logic[[keys[i]]]; y <- 62 + (i - 1) * 118
    paste0(
      if (i > 1) sprintf('<line class="s8p-rule" x1="30" x2="770" y1="%g" y2="%g"/>', y - 58, y - 58) else "",
      s8p_text(30, y + 5, lg$who, 15, weight = 600),
      s8p_rbd(lg$stages, s8p1_lab, x = 230, y = y, scale = .78, lamp = NULL),
      s8p_text(770, y + 10, s8p_fix(lg$r, 2), 30, "s8p-mono", "end", 700),
      s8p_text(770, y + 30, "R systemu", 12, "s8p-subtle", "end")
    )
  }, character(1)), collapse = "")
}

s8p1_frames <- list(
  list(name = "Noc",
    text = "Dojrzewalnia, 02:40. Komorę chłodzą dwa wentylatory, każdy o niezawodności 0.9. Wentylator E₂ staje, E₁ pracuje dalej.",
    read = NULL),
  list(name = "Raport serwisanta",
    text = "Serwisant: przy pełnym załadunku komora potrzebuje obu wentylatorów. Na jego schemacie droga jest przerwana i lampka gaśnie.",
    read = NULL),
  list(name = "Raport kierowniczki",
    text = "Kierowniczka zmiany: jeden wentylator wystarcza, drugi jest zapasem. Na jej schemacie droga przez E₁ nadal świeci.",
    read = NULL),
  list(name = "Raport automatyka",
    text = "Automatyk: komorę chłodzi E₁, a E₂ tylko przewietrza przedsionek. Na jego schemacie E₂ w ogóle nie ma.",
    read = NULL),
  list(name = "Trzy liczby",
    text = "Ta sama noc i te same dwa wentylatory. Każde z trojga policzy inną niezawodność instalacji, bo każde przyjęło inną definicję sukcesu.",
    read = "0.9 · 0.9 = 0.81 · 1 − 0.1 · 0.1 = 0.99 · 0.9 = 0.90")
)

s8p1_svg <- function(step) {
  switch(step,
    s8p_svg(800, 250, "Komora dojrzewalni nocą: wentylator E1 pracuje, wentylator E2 stoi.",
      s8p1_hall(150, 14, 500, 222, r = 46)),
    s8p_svg(800, 290, "Raport serwisanta: schemat szeregowy, blok E2 zepsuty, lampka zgaszona. Ocena: system zawiódł.",
      s8p1_report("series", "system zawiódł", "bad")),
    s8p_svg(800, 290, "Raport kierowniczki zmiany: schemat równoległy, droga przez E1 świeci. Ocena: system działał.",
      s8p1_report("parallel", "system działał", "ok")),
    s8p_svg(800, 330, "Raport automatyka: w schemacie jest tylko E1, lampka świeci. Ocena: zdarzenie nie dotyczy systemu.",
      s8p1_report("single", "nie dotyczy systemu", "idle")),
    s8p_svg(800, 360, "Trzy schematy tych samych dwóch wentylatorów i trzy niezawodności: 0.81, 0.99 i 0.90.",
      s8p1_three())
  )
}

s8p_1_ui <- s8p_frames_ui("P1", "scena", "Nocna awaria, trzy raporty", s8p1_frames, s8p1_svg)

# ============================================================================
# P7 — PIASKOWNICA „DŁUGI ŁAŃCUCH”
# ============================================================================

s8p_7_ui <- s8p_panel("P7", "piaskownica", "Długi łańcuch",
  lc_sf_figure("s8_lancuch",
    control = list(lc_sf_slider("s8p7_n", "Liczba ogniw n", 1, 20, 10, 1),
      lc_sf_slider("s8p7_r", "R jednego ogniwa", 0.9, 0.995, 0.95, 0.005)),
    toolbar = lc_readouts(s8p_read("rs", "R systemu", upwr_accent), s8p_read("risk", "Ryzyko systemu"),
      s8p_read("sum", "Suma ryzyk ogniw"), s8p_read("max", "Ogniw przy R ≥ 0.90")),
    ratio = "2.4/1", max_height = "340px", legend = TRUE,
    config = list(limit = 0.90, nmax = 20),
    after = lc_caption("Każdy słupek to szereg o jedno ogniwo dłuższy. Oś pionowa jest stała; przerywana kreska to próg 0.90 z przykładu 8.3."))
)
