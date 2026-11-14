# PROTOTYPY PUNKTÓW STOP (2026-11-14) — P1 i P12 (wzorzec dla pozostałych) --------
#
#   c7p_1_ui  — Scena „Dwie oferty, jedna średnia” (P1), 5 kadrów policzonych w R:
#               karty katalogowe → sto linii życia → od najkrótszej → misja 1000 h
#               → misja 3000 h. Park to kwantyle modelu (c7p_park), więc po 1000 h
#               działa 51 i 71 ze 100, po 3000 h 14 i 4 — jak w przykładzie 7.1.
#   c7p_12_ui — Piaskownica „Stały hazard” (P12): rycina lcSF c7_staly
#               (modules/proto_ra.js), stałe osie, kreska misji 1000 h,
#               punkt (MTTF, 0.37).
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R.

# ============================================================================
# P1 — SCENA „DWIE OFERTY, JEDNA ŚREDNIA”
# ============================================================================

c7p1_n <- 100L
c7p1_life <- list(a = c7p_park(c7p1_n, 1, c7p_mttf), b = c7p_park(c7p1_n, c7p_beta, c7p_eta))
# Kolejność „z dziennika montażu”: stałe przetasowanie tych samych stu czasów.
c7p1_order <- local({ set.seed(7); list(a = sample(c7p1_n), b = sample(c7p1_n)) })
c7p1_alive <- function(t) vapply(c7p1_life, function(x) sum(x > t), integer(1))

# Geometria: dwa panele na wspólnej skali czasu 0–4500 h.
c7p1_tmax <- 4500
c7p1_x0 <- c(a = 40, b = 470); c7p1_w <- 350
c7p1_top <- 52; c7p1_dy <- 3
c7p1_base <- c7p1_top + c7p1_n * c7p1_dy + 6
c7p1_xof <- function(k) function(t) c7p1_x0[[k]] + pmin(t, c7p1_tmax) / c7p1_tmax * c7p1_w

c7p1_panel <- function(k, step) {
  x_of <- c7p1_xof(k)
  t <- c7p1_life[[k]]
  if (step == 2) t <- t[c7p1_order[[k]]]
  y <- c7p1_top + (seq_len(c7p1_n) - 0.5) * c7p1_dy
  cut <- c(NA, NA, NA, 1000, 3000)[step]
  cls <- paste0("c7p-life-", k)
  if (!is.na(cut)) cls <- ifelse(t > cut, cls, paste(cls, "c7p-life-dim"))
  name <- c(a = "Oferta A · MTTF 1500 h", b = "Oferta B · MTTF 1507 h")[[k]]
  paste0(
    sprintf('<rect class="c7p-fill-%s" x="%g" y="18" width="14" height="14" rx="2"/>', k, c7p1_x0[[k]]),
    sprintf('<text x="%g" y="30" font-size="15" font-weight="700">%s</text>', c7p1_x0[[k]] + 22, name),
    c7p_lifelines(t, y, x_of, cls = cls, tmax = c7p1_tmax, tick = 1.3, width = 1.8),
    c7p_axis(x_of, c7p1_base, seq(0, 4000, 1000), "czas pracy (h)"),
    if (!is.na(cut)) paste0(
      c7p_vmark(x_of(cut), c7p1_top - 12, c7p1_base, NULL),
      # Liczba stoi w pustym rogu nad liniami; przy misji 3000 h po lewej stronie kreski.
      sprintf('<text class="c7p-mono c7p-ink" x="%g" y="%g" font-size="34" font-weight="700" text-anchor="end">%d</text>',
        if (cut > 2000) x_of(cut) - 10 else c7p1_x0[[k]] + c7p1_w, c7p1_top + 44, c7p1_alive(cut)[[k]]),
      sprintf('<text class="c7p-mono c7p-subtle" x="%g" y="%g" font-size="13" text-anchor="end">ze 100 działa po %d h</text>',
        if (cut > 2000) x_of(cut) - 10 else c7p1_x0[[k]] + c7p1_w, c7p1_top + 64, cut))
  )
}

# Kadr 1: dwie karty katalogowe z tym samym MTTF.
c7p1_cards <- function() {
  card <- function(x, k, name, model, mttf) paste0(
    sprintf('<rect class="c7p-paper" x="%g" y="30" width="330" height="230" rx="6"/>', x),
    sprintf('<rect class="c7p-fill-%s" x="%g" y="30" width="330" height="8"/>', k, x),
    sprintf('<text class="c7p-subtle" x="%g" y="70" font-size="12" font-weight="700">KARTA KATALOGOWA</text>', x + 24),
    sprintf('<text x="%g" y="102" font-size="21" font-weight="700">%s</text>', x + 24, name),
    sprintf('<text class="c7p-subtle" x="%g" y="126" font-size="14">%s</text>', x + 24, model),
    sprintf('<line class="c7p-grid" x1="%g" x2="%g" y1="146" y2="146"/>', x + 24, x + 306),
    sprintf('<text class="c7p-subtle" x="%g" y="178" font-size="14">średni czas do awarii (MTTF)</text>', x + 24),
    sprintf('<text class="c7p-mono" x="%g" y="226" font-size="38" font-weight="700">%s h</text>', x + 24, mttf))
  paste0(card(70, "a", "Wentylator A", "dojrzewalnia, praca ciągła", "1500"),
    card(460, "b", "Wentylator B", "dojrzewalnia, praca ciągła", "1507"),
    '<text class="c7p-subtle" x="430" y="300" font-size="15" text-anchor="middle">Pytanie kierownika: ile wentylatorów dotrwa do przeglądu po 1000 h?</text>')
}

c7p1_frames <- list(
  list(name = "Dwie karty",
    text = "Dział zakupów porównuje dwie oferty. Obie karty katalogowe podają niemal ten sam średni czas do awarii.",
    read = "MTTF: 1500 h i 1507 h"),
  list(name = "Sto wentylatorów",
    text = "Z każdej oferty uruchamiamy sto wentylatorów. Linia to czas pracy jednego egzemplarza, kreska na końcu to awaria.",
    read = NULL),
  list(name = "Od najkrótszej",
    text = "Te same linie ułożone od najkrócej do najdłużej żyjącego. Oferta A ma wiele bardzo krótkich i kilka bardzo długich żyć; oferta B trzyma się środka.",
    read = NULL),
  list(name = "Misja 1000 h",
    text = "Kreska przeglądu po 1000 h. Linie, które jej nie sięgają, to wentylatory, które zawiodły wcześniej.",
    read = sprintf("po 1000 h działa: A %d ze 100 · B %d ze 100", c7p1_alive(1000)[["a"]], c7p1_alive(1000)[["b"]])),
  list(name = "Misja 3000 h",
    text = "Ta sama kreska przesunięta na 3000 h. Kolejność ofert się odwraca, choć średnie się nie zmieniły.",
    read = sprintf("po 3000 h działa: A %d ze 100 · B %d ze 100", c7p1_alive(3000)[["a"]], c7p1_alive(3000)[["b"]]))
)

c7p1_svg <- function(step) {
  if (step == 1) return(c7p_svg(860, 320,
    "Dwie karty katalogowe wentylatorów: średni czas do awarii 1500 i 1507 godzin.", c7p1_cards()))
  aria <- c("",
    "Sto linii życia z każdej oferty w kolejności montażu.",
    "Sto linii życia z każdej oferty ułożonych od najkrótszej do najdłuższej.",
    sprintf("Po 1000 godzinach działa %d wentylatorów oferty A i %d oferty B.", c7p1_alive(1000)[["a"]], c7p1_alive(1000)[["b"]]),
    sprintf("Po 3000 godzinach działa %d wentylatorów oferty A i %d oferty B.", c7p1_alive(3000)[["a"]], c7p1_alive(3000)[["b"]]))[step]
  c7p_svg(860, c7p1_base + 40, aria, c7p1_panel("a", step), c7p1_panel("b", step))
}

c7p_1_ui <- c7p_frames_ui("P1", "scena", "Dwie oferty, jedna średnia", c7p1_frames, c7p1_svg)

# ============================================================================
# P12 — PIASKOWNICA „STAŁY HAZARD”
# ============================================================================

c7p_12_ui <- c7p_panel("P12", "piaskownica", "Stały hazard: jedna krzywa, inna skala",
  lc_sf_figure("c7_staly",
    control = list(lc_sf_slider("c7p12_mttf", "MTTF", 300, 4000, c7p_mttf, 50, suffix = " h")),
    toolbar = lc_readouts(c7p_read("r", "R(1000 h)", upwr_accent), c7p_read("med", "Mediana"),
      c7p_read("fm", "Zawodzi przed MTTF")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    config = list(mttf = c7p_mttf, mission = c7p_mission),
    after = lc_caption("Osie są stałe. Przerywany kontur to oferta A (MTTF 1500 h). Punkt na krzywej leży zawsze na wysokości 0.37: do chwili t = MTTF zawodzi 63% wentylatorów."))
)
