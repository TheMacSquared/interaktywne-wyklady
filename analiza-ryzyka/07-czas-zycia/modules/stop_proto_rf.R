# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rf: P14, P16A, P16B, P17A ----------
#
#   c7p_14_ui  — Piaskownica „Czas do k-tej awarii” (P14): rycina c7_gamma
#                (modules/proto_rf.js), suwak kształtu k, stałe osie, pole od 1000 h.
#   c7p_16a_ui — Piaskownica „β obraca hazard, η rozciąga oś” (P16A): rycina c7_weibull,
#                suwaki β i η, R(t) i h(t) obok siebie, stałe osie.
#   c7p_16b_ui — Galeria „Rodzina Weibulla” (P16B): rycina c7_rodzina, cztery kształty
#                β przy η = 1700 h, bez kontrolek.
#   c7p_17a_ui — Piaskownica „Ten sam MTTF, inny ranking” (P17A): rycina c7_ranking,
#                suwak czasu misji, pasek „kto prowadzi” przy osi.
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R, R/lc_dist.js (window.lcDist),
# modules/sceny.js (window.lcC7.util), modules/proto_rf.js.

# ============================================================================
# P14 — PIASKOWNICA „CZAS DO K-TEJ AWARII”
# ============================================================================

c7p_14_ui <- c7p_panel("P14", "piaskownica", "Czas do k-tej awarii",
  lc_sf_figure("c7_gamma",
    control = list(lc_sf_slider("c7p14_k", "Kształt k", 0.5, 8, 3, 0.5)),
    toolbar = lc_readouts(c7p_read("mean", "Średni czas E(T)"), c7p_read("mode", "Szczyt gęstości"),
      c7p_read("p", "P(T > 1000 h)", upwr_secondary), c7p_read("fam", "Rodzina")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    after = lc_caption("Osie są stałe. Dla całkowitego k to czas do k-tej awarii w hali, w której awarie przychodzą średnio co 500 h; pole za kreską to szansa, że k-ta awaria przyjdzie po 1000 h."))
)

# ============================================================================
# P16A — PIASKOWNICA „β OBRACA HAZARD, η ROZCIĄGA OŚ”
# ============================================================================

c7p_16a_ui <- c7p_panel("P16A", "piaskownica", "β obraca hazard, η rozciąga oś",
  lc_sf_figure("c7_weibull",
    control = list(lc_sf_slider("c7p16_beta", "Kształt β", 0.4, 4, 2, 0.1),
      lc_sf_slider("c7p16_eta", "Skala η", 300, 4000, 1700, 50, suffix = " h")),
    toolbar = lc_readouts(c7p_read("dir", "Kierunek hazardu"), c7p_read("r", "R(1000 h)", upwr_accent),
      c7p_read("h", "h(1000 h) [1/h]"), c7p_read("m", "MTTF")),
    legend = TRUE,
    body = c7p_stack(c("Niezawodność R(t)", "Hazard h(t) w jednostkach 1/h")),
    after = lc_caption("Osie są stałe. Zmiana η nie zmienia kształtu krzywych, tylko rozciąga je wzdłuż osi czasu; punkt t = η leży zawsze na wysokości 0.37."))
)

# ============================================================================
# P16B — GALERIA „RODZINA WEIBULLA”
# ============================================================================

c7p_16b_ui <- c7p_panel("P16B", "galeria", "Rodzina Weibulla: cztery kształty, jedna skala",
  lc_sf_figure("c7_rodzina",
    legend = TRUE,
    body = c7p_stack(c("Niezawodność R(t)", "Hazard h(t) w jednostkach 1/h")),
    after = lc_caption("Skala η = 1700 h jest wspólna. Kształt β decyduje, czy ryzyko dla działającego elementu maleje, stoi czy rośnie."))
)

# ============================================================================
# P17A — PIASKOWNICA „TEN SAM MTTF, INNY RANKING”
# ============================================================================

c7p_17a_ui <- c7p_panel("P17A", "piaskownica", "Ten sam MTTF, inny ranking",
  lc_sf_figure("c7_ranking",
    control = list(lc_sf_slider("c7p17_t", "Czas misji", 100, 3500, 1000, 50, suffix = " h")),
    toolbar = lc_readouts(c7p_read("r1", "R(t), β = 0.7"), c7p_read("r2", "R(t), β = 1"),
      c7p_read("r3", "R(t), β = 2.5"), c7p_read("best", "Prowadzi")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    after = lc_caption("Wszystkie trzy modele mają MTTF = 1500 h. Model zużyciowy prowadzi do około 1830 h, potem model wykładniczy, a od około 2600 h model z malejącym hazardem."))
)
