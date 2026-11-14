# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa wy: P10, P12, P16A, P16B, P18 ----------
#
# Ryciny lcSF liczone w przeglądarce (modules/proto_wy.js, window.lcSF); R składa
# tylko kontenery lc_sf_figure() i odczyty. Bez renderUI i bez input$.
#
#   f9p_10_ui  — przypadek brzegowy „Suma liczy narożnik dwa razy” (P10): suwaki d i s,
#                rycina f9_suma, stałe osie 0–0.6, pasy detekcji i tłumienia, narożnik.
#   f9p_12_ui  — przypadek brzegowy „Dwa wystąpienia, jedno źródło” (P12): suwak q,
#                rycina f9_powtorzenie, prawdziwy wynik i błędne OR / AND na jednej osi.
#   f9p_16a_ui — piaskownica „Ta sama redukcja każdego liścia” (P16A): suwak r,
#                rycina f9_ranking, trzy słupki spadku na stałej osi 0–100%.
#   f9p_16b_ui — piaskownica „Ryzyko jako pole prostokąta” (P16B): wybór liścia i suwak r,
#                rycina f9_pole, pole P(TOP) z usuniętym pasem.
#   f9p_18_ui  — pojęcie „Przedział zamiast punktu” (P18): statyczna rycina f9_przedzial,
#                pas d od 0.02 do 0.10 i przedział wyniku.
# Wymaga: stop_proto_common.R (f9p_panel, f9p_read, f9p_pi, f9p_d, f9p_s, f9p_top),
# R/lc_stat_figures.R, R/lecture_layout.R oraz modules/proto_wy.js.

# ============================================================================
# P10 — SUMA LICZY NAROŻNIK DWA RAZY
# ============================================================================

f9p_10_ui <- f9p_panel("P10", "przypadek brzegowy", "Suma liczy narożnik dwa razy",
  lc_sf_figure("f9_suma",
    control = list(
      lc_sf_slider("f9p10_d", "d = P(D | I)", 0.01, 0.5, 0.05, 0.01),
      lc_sf_slider("f9p10_s", "s = P(S | I)", 0.01, 0.5, 0.08, 0.01)),
    toolbar = lc_readouts(f9p_read("sum", "Suma d + s"), f9p_read("ex", "Dokładnie"),
      f9p_read("cor", "Narożnik d · s", upwr_accent), f9p_read("err", "Błąd sumy")),
    ratio = "1.5/1", max_height = "400px", legend = TRUE,
    after = lc_caption("Pole całego kwadratu to 1; oś pokazuje jego róg do 0.6. Suma dodaje oba pasy, więc narożnik wchodzi do niej dwa razy."))
)

# ============================================================================
# P12 — DWA WYSTĄPIENIA, JEDNO ŹRÓDŁO
# ============================================================================

f9p_12_ui <- f9p_panel("P12", "przypadek brzegowy", "Dwa wystąpienia, jedno źródło",
  lc_sf_figure("f9_powtorzenie",
    control = lc_sf_slider("f9p12_q", "q", 0.01, 0.2, 0.05, 0.01),
    toolbar = lc_readouts(f9p_read("q1", "Jedno zdarzenie"),
      f9p_read("or", "Błędne OR kopii"),
      f9p_read("and", "Błędne AND kopii", upwr_accent)),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    after = lc_caption("Pod OR błąd zawyża wynik najwyżej dwukrotnie. Pod AND krzywa przykleja się do zera: im rzadsze zdarzenie, tym większe zaniżenie."))
)

# ============================================================================
# P16A — TA SAMA REDUKCJA KAŻDEGO LIŚCIA (piaskownica, słupki)
# ============================================================================

f9p_16a_ui <- f9p_panel("P16A", "piaskownica", "Ta sama redukcja każdego liścia",
  lc_sf_figure("f9_ranking",
    control = lc_sf_slider("f9p16_r", "Redukcja parametru r", 0, 0.9, 0.5, 0.05),
    toolbar = lc_readouts(f9p_read("ri", "Inicjacja"), f9p_read("rs", "Tłumienie"),
      f9p_read("rd", "Detekcja")),
    ratio = "2.2/1", max_height = "360px",
    config = list(pi = f9p_pi, d = f9p_d, s = f9p_s, top = f9p_top),
    after = lc_caption("Oś jest stała: 100% to całe wyjściowe P(TOP) = 0.00063. Zmiana r zmienia wysokość słupków, ale nie ich kolejność."))
)

# ============================================================================
# P16B — RYZYKO JAKO POLE PROSTOKĄTA (piaskownica)
# ============================================================================

f9p_16b_ui <- f9p_panel("P16B", "piaskownica", "Ryzyko jako pole prostokąta",
  lc_sf_figure("f9_pole",
    control = list(
      lc_segmented("f9p16b_leaf", "Poprawiany liść", c("Inicjacja" = "I", "Tłumienie" = "S", "Detekcja" = "D"),
        selected = "I"),
      lc_sf_slider("f9p16b_r", "Redukcja parametru r", 0, 0.9, 0.5, 0.05)),
    toolbar = lc_readouts(f9p_read("after", "P(TOP) po poprawie"),
      f9p_read("drop", "Spadek", upwr_accent), f9p_read("part", "Część wyjściowego P(TOP)")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    config = list(pi = f9p_pi, d = f9p_d, s = f9p_s, top = f9p_top),
    after = lc_caption("Pole prostokąta to P(TOP) = 0.005 · 0.126 = 0.00063. Osie mają różne skale; liczy się, jaką część pola zabiera ta sama względna redukcja."))
)

# ============================================================================
# P18 — PRZEDZIAŁ ZAMIAST PUNKTU (pojęcie, bez kontrolek)
# ============================================================================

f9p_18_ui <- f9p_panel("P18", "pojęcie", "Przedział zamiast punktu",
  lc_sf_figure("f9_przedzial",
    ratio = "2.2/1", max_height = "360px",
    after = tagList(
      lc_readouts(
        lc_readout("Wartość punktowa", "0.00063"),
        lc_readout("Przedział P(TOP)", "0.000492 – 0.00086", color = upwr_accent),
        lc_readout("Przedział d", "0.02 – 0.10")),
      lc_caption("Pas to wszystko, co wiemy o d. Pionowa rozpiętość linii nad pasem to przedział wyniku: od 4.9 do 8.6 nieopanowanych pożarów na 10 000 magazyno-lat.")))
)
