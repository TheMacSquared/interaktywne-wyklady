# PROTOTYPY PUNKTÓW STOP (2026-11-14) — drzewo, interwencje, scenariusze, kryterium ----
#
#   i10p_11a_ui — Galeria „Drzewo z liczbami” (P11A): scena JS „drzewo”, warianty:
#                 stan bazowy, idealny detektor, model Weibulla.
#   i10p_13b_ui — Galeria „Który liść się zmienia” (P13B): ta sama scena „drzewo”,
#                 warianty: bez działania i cztery interwencje.
#   i10p_13a_ui — Galeria „Cztery interwencje na jednej osi” (P13A): rycina lcSF i10_opcje.
#   i10p_14_ui  — Piaskownica „Wynik to przedział” (P14): rycina lcSF i10_scen.
#   i10p_15_ui  — Przypadek brzegowy „Ranking się odwraca” (P15): rycina lcSF i10_odwrocenie.
#   i10p_16_ui  — Piaskownica „Ranking to nie kryterium” (P16): rycina lcSF i10_kryterium.
# Wszystkie liczby liczy JS (lcI10.util.mission) z parametrów i10p_cfg w data-config.
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R, modules/proto_rd.js.

i10pd_model_seg <- function(id, selected = "exp") lc_segmented(id, "Model wentylatora",
  c("Wykładniczy" = "exp", "Weibull" = "weibull"), selected = selected)

# ============================================================================
# P11A i P13B — DRZEWO Z LICZBAMI (scena „drzewo”)
# ============================================================================

i10pd_tree_ui <- function(num, title, variants, seg_label, caption) {
  choices <- setNames(as.character(seq_along(variants) - 1L), vapply(variants, function(v) v$label, ""))
  i10p_scene_ui(num, "galeria", title, "drzewo", c(i10p_cfg, list(variants = variants)),
    tags$div(class = "i10p-nav", i10p_seg("variant", seg_label, choices)),
    i10p_out("stage"), i10p_out("read"), lc_caption(caption))
}

i10p_11a_ui <- i10pd_tree_ui("P11A", "Drzewo z liczbami", list(
    list(label = "Stan bazowy"),
    list(label = "Idealny detektor", sens = 1),
    list(label = "Wentylator Weibulla", model = "weibull")),
  "Wariant",
  "Liść wyróżniony to ten, który zmienił się względem stanu bazowego. Idealny detektor usuwa mały składnik sumy; model czasu życia zmienia duży.")

i10p_13b_ui <- i10pd_tree_ui("P13B", "Który liść się zmienia", c(
    list(list(label = "Bez działania")),
    lapply(i10p_cfg$interventions, function(x) list(label = x$label, intervention = x$id, cost = x$cost))),
  "Działanie",
  "Każda interwencja zmienia jeden liść. Ograniczenie źródła ciepła zmienia P(I), czyli czynnik, który mnoży cały wzór (10.8).")

# ============================================================================
# P13A — CZTERY INTERWENCJE NA JEDNEJ OSI
# ============================================================================

i10p_13a_ui <- i10p_panel("P13A", "galeria", "Cztery interwencje na jednej osi",
  lc_sf_figure("i10_opcje",
    control = list(lc_sf_slider("i10p13a_budget", "Budżet", 1, max(vapply(i10p_cfg$interventions, function(x) x$cost, 0)), i10p_cfg$budget, 1),
      i10pd_model_seg("i10p13a_model")),
    toolbar = lc_readouts(i10p_read("best", "Najlepsza w budżecie"), i10p_read("top", "P(TOP) po działaniu", upwr_accent),
      i10p_read("cut", "Redukcja")),
    ratio = "2.4/1", max_height = "340px",
    config = c(i10p_cfg, list(axis = i10p_cfg$top_axis / 2)),
    after = lc_caption("Przerywana kreska to stan bez działania. Wyblakłe słupki są poza budżetem. Oś jest stała."))
)

# ============================================================================
# P14 — WYNIK TO PRZEDZIAŁ
# ============================================================================

i10p_14_ui <- i10p_panel("P14", "piaskownica", "Wynik to przedział",
  lc_sf_figure("i10_scen",
    control = list(lc_sf_slider("i10p14_u", "Zakres niepewności u", 0, i10p_cfg$u_max, i10p_cfg$u, 0.05),
      i10pd_model_seg("i10p14_model")),
    toolbar = lc_readouts(i10p_read("spread", "Rozrzut scenariuszy najlepszej opcji"),
      i10p_read("gap", "Różnica między opcjami (bazowo)")),
    ratio = "2.2/1", max_height = "380px", legend = TRUE,
    config = i10p_cfg,
    after = lc_caption("Odcinek łączy scenariusz optymistyczny z ostrożnym, kropka to scenariusz bazowy. Scenariusze są eksperymentem na założeniach, nie przedziałem ufności. Oś jest stała."))
)

# ============================================================================
# P15 — RANKING SIĘ ODWRACA
# ============================================================================

i10p_15_ui <- i10p_panel("P15", "przypadek brzegowy", "Ranking się odwraca",
  lc_sf_figure("i10_odwrocenie",
    control = list(lc_sf_slider("i10p15_u", "Zakres niepewności u", 0, i10p_cfg$u_max, i10p_cfg$u, 0.01),
      i10pd_model_seg("i10p15_model", "weibull")),
    toolbar = lc_readouts(i10p_read("prev", "Ograniczenie źródła ciepła"), i10p_read("fan", "Dodatkowy wentylator"),
      i10p_read("win", "Wygrywa w scenariuszu ostrożnym")),
    ratio = "2.2/1", max_height = "380px", legend = TRUE,
    config = c(i10p_cfg, list(axis = i10p_cfg$top_axis * 0.75, pair = c("prevention", "fan"))),
    after = lc_caption("Linie pokazują P(TOP) po działaniu w scenariuszu ostrożnym (m = 1 + u). Scenariusz osłabia skuteczność działań z założoną skutecznością; redundancja takiego parametru nie ma."))
)

# ============================================================================
# P16 — RANKING TO NIE KRYTERIUM
# ============================================================================

i10p_16_ui <- i10p_panel("P16", "piaskownica", "Ranking to nie kryterium",
  lc_sf_figure("i10_kryterium",
    control = list(lc_sf_slider("i10p16_limit", "Demonstracyjny limit P(TOP) na misję", 0.0005, 0.003, i10p_cfg$limit, 0.0001)),
    toolbar = lc_readouts(i10p_read("best", "Najlepsza w budżecie"), i10p_read("worst", "Jej najgorszy scenariusz"),
      i10p_read("verdict", "Kryterium", upwr_accent)),
    ratio = "2.4/1", max_height = "340px", legend = TRUE,
    config = i10p_cfg,
    after = lc_caption(sprintf("Model wykładniczy, misja %s %s, budżet %s, zakres u = %s. Z limitem porównujemy najgorszy z rozpatrzonych scenariuszy, czyli prawy koniec odcinka.",
      i10p_cfg$time, i10p_cfg$unit, i10p_cfg$budget, i10p_cfg$u)))
)
