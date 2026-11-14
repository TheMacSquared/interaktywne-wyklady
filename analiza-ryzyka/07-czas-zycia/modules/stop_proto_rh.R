# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rh: P19, P20, P21A, P21B, P22 ----
#
#   c7p_19_ui  — Piaskownica „Wanna: suma trzech mechanizmów” (P19): rycina lcSF
#                c7_wanna (modules/proto_rh.js), suwak nasilenia zużycia, pasy
#                dominującego składnika, dno wanny.
#   c7p_20_ui  — Przypadek brzegowy „Jeden Weibull nie zrobi wanny” (P20): rycina
#                c7_jeden_weibull, suwak kształtu β, wanna dla w = 1 na tle.
#   c7p_21a_ui — Piaskownica „Termin przeglądu na krzywej R(t)” (P21A): rycina c7_termin,
#                suwak czasu przeglądu i przełącznik wymaganej niezawodności R*.
#   c7p_21b_ui — Piaskownica „Ryzyko jako pole przed kreską przeglądu” (P21B): rycina
#                c7_termin_pole, suwak czasu przeglądu.
#   c7p_22_ui  — Piaskownica „Używany czy nowy na kolejne 500 h?” (P22): rycina
#                c7_stary_nowy, przełącznik mechanizmu i suwak wieku s.
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R (lc_sf_figure, lc_sf_slider, lc_sf_value).

# ============================================================================
# P19 — PIASKOWNICA „WANNA: SUMA TRZECH MECHANIZMÓW”
# ============================================================================

c7p_19_ui <- c7p_panel("P19", "piaskownica", "Wanna: suma trzech mechanizmów",
  lc_sf_figure("c7_wanna",
    control = lc_sf_slider("c7p19_w", "Nasilenie zużycia", 0.2, 2, 1, 0.1),
    toolbar = lc_readouts(c7p_read("t", "Dno wanny", upwr_accent), c7p_read("h", "Hazard w dnie")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    after = lc_caption("Oś pionowa jest stała. Kolor tła pokazuje, który mechanizm ma największy hazard; nad nim stoi interwencja, która na niego działa."))
)

# ============================================================================
# P20 — PRZYPADEK BRZEGOWY „JEDEN WEIBULL NIE ZROBI WANNY”
# ============================================================================

c7p_20_ui <- c7p_panel("P20", "przypadek brzegowy", "Jeden Weibull nie zrobi wanny",
  lc_sf_figure("c7_jeden_weibull",
    control = lc_sf_slider("c7p20_beta", "Kształt β", 0.3, 4, 1, 0.1),
    toolbar = lc_readouts(c7p_read("dir", "Hazard Weibulla"), c7p_read("fit", "Pasuje do")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    after = lc_caption("Żadne położenie suwaka nie daje krzywej, która najpierw maleje, a potem rośnie. Wanna wymaga sumy hazardów kilku mechanizmów."))
)

# ============================================================================
# P21A — PIASKOWNICA „TERMIN PRZEGLĄDU NA KRZYWEJ R(t)”
# ============================================================================

c7p_21a_ui <- c7p_panel("P21A", "piaskownica", "Termin przeglądu na krzywej R(t)",
  lc_sf_figure("c7_termin",
    control = list(
      lc_sf_slider("c7p21a_t", "Czas do przeglądu", 100, 3000, 1000, 50, suffix = " h"),
      lc_segmented("c7p21a_req", "Wymagana niezawodność R*",
        c("0.95" = "0.95", "0.90" = "0.90", "0.80" = "0.80"), selected = "0.90")
    ),
    toolbar = lc_readouts(c7p_read("r", "R(t)"), c7p_read("risk", "Ryzyko awarii", upwr_accent),
      c7p_read("tw", "Termin (Weibull)"), c7p_read("te", "Termin (wykładniczy)")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    after = lc_caption("Termin to miejsce, w którym krzywa przecina wymaganie. Odcinek w kolorze akcentu to ryzyko awarii przed przeglądem dla wybranego czasu."))
)

# ============================================================================
# P21B — PIASKOWNICA „RYZYKO JAKO POLE PRZED KRESKĄ PRZEGLĄDU”
# ============================================================================

c7p_21b_ui <- c7p_panel("P21B", "piaskownica", "Ryzyko jako pole przed kreską przeglądu",
  lc_sf_figure("c7_termin_pole",
    control = lc_sf_slider("c7p21b_t", "Czas do przeglądu", 100, 3000, 1000, 50, suffix = " h"),
    toolbar = lc_readouts(c7p_read("risk", "Ryzyko awarii przed przeglądem", upwr_accent),
      c7p_read("r", "R(t)"), c7p_read("n", "Na 100 wentylatorów")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    after = lc_caption("Jak w wykładzie 06: prawdopodobieństwo to pole pod gęstością. Tu próg stoi na osi czasu, a ryzyko to pole przed kreską."))
)

# ============================================================================
# P22 — PIASKOWNICA „UŻYWANY CZY NOWY NA KOLEJNE 500 H?”
# ============================================================================

c7p_22_ui <- c7p_panel("P22", "piaskownica", "Używany czy nowy na kolejne 500 h?",
  lc_sf_figure("c7_stary_nowy",
    control = list(
      lc_segmented("c7p22_model", "Mechanizm",
        c("zużycie (β = 2)" = "wear", "stały hazard (β = 1)" = "const", "wczesne defekty (β = 0.7)" = "early"),
        selected = "wear"),
      lc_sf_slider("c7p22_s", "Wiek używanego wentylatora s", 0, 2500, 1000, 50, suffix = " h")
    ),
    toolbar = lc_readouts(c7p_read("used", "Używany: +500 h"), c7p_read("new", "Nowy: +500 h"),
      c7p_read("verdict", "Wymiana", upwr_accent)),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    after = lc_caption("Przy stałym hazardzie obie krzywe się pokrywają dla każdego wieku: brak pamięci. Przy wczesnych defektach używany egzemplarz ma najgorsze już za sobą."))
)
