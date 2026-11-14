# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rb: P2A, P2B, P8, P10 ----------
#
#   c7p_2a_ui  — galeria „Dwie gęstości, jedna średnia” (P2A): gęstości A i B
#                na wspólnym wykresie, pola za kreską misji, stałe osie.
#   c7p_2b_ui  — galeria „Dwie oferty, jedna średnia: gęstości osobno” (P2B):
#                to samo dwa razy, pole przed kreską w kolorze akcentu.
#   c7p_8_ui   — pojęcie „Cztery funkcje, jedna chwila” (P8): F i R na górze,
#                h i f na dole; suwak czasu i wybór modelu.
#   c7p_10_ui  — pojęcie „Pole pod hazardem” (P10): pole H(t) pod hazardem
#                na górze, R(t) = exp(−H(t)) na dole.
# Ryciny lcSF liczone w przeglądarce: modules/proto_rb.js (window.lcSF).
# Wymaga: stop_proto_common.R (c7p_panel, c7p_read, c7p_mttf, c7p_beta, c7p_eta),
# R/lc_stat_figures.R, R/lecture_layout.R.

# Parametry obu ofert dla JS (jedno źródło: stałe z stop_proto_common.R).
c7p_rb_cfg <- list(mttf = c7p_mttf, beta = c7p_beta, eta = c7p_eta)

# Wybór czasu misji dla P2A i P2B (ten sam zestaw wartości).
c7p_rb_mission <- c("500 h" = "500", "1000 h" = "1000", "3000 h" = "3000")

# ============================================================================
# P2A — DWIE GĘSTOŚCI, JEDNA ŚREDNIA
# ============================================================================

c7p_2a_ui <- c7p_panel("P2A", "galeria", "Dwie gęstości, jedna średnia",
  lc_sf_figure("c7_gestosci",
    control = lc_segmented("c7p2a_t", "Czas misji", c7p_rb_mission, selected = "1000"),
    toolbar = lc_readouts(c7p_read("ra", "R(t) oferty A"), c7p_read("rb", "R(t) oferty B")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    config = c7p_rb_cfg,
    after = lc_caption("Pole pod gęstością za kreską misji to R(t). Obie gęstości mają tę samą średnią, ale oferta A jest najwyższa w zerze, a oferta B ma garb przy 1200 h."))
)

# ============================================================================
# P2B — TO SAMO OSOBNO: PRZED KREŚKĄ I ZA KREŚKĄ
# ============================================================================

c7p_2b_ui <- c7p_panel("P2B", "galeria", "Dwie oferty, jedna średnia: gęstości osobno",
  lc_sf_figure("c7_gestosci_b",
    control = lc_segmented("c7p2b_t", "Czas misji", c7p_rb_mission, selected = "1000"),
    toolbar = lc_readouts(c7p_read("ra", "R(t) oferty A"), c7p_read("rb", "R(t) oferty B")),
    body = c7p_stack(c("Oferta A: rozkład wykładniczy", "Oferta B: rozkład Weibulla"), "4.2/1", "200px"),
    config = c7p_rb_cfg,
    after = lc_caption("Pole w kolorze akcentu to wentylatory, które zawiodły przed końcem misji; pole w kolorze oferty to te, które dotrwały."))
)

# ============================================================================
# P8 — CZTERY FUNKCJE, JEDNA CHWILA
# ============================================================================

c7p_8_ui <- c7p_panel("P8", "pojęcie", "Cztery funkcje, jedna chwila",
  lc_sf_figure("c7_cztery",
    control = list(
      lc_sf_slider("c7p8_t", "Czas t", 0, 4000, 1000, 50, suffix = " h"),
      lc_segmented("c7p8_model", "Model", c("A: stały hazard" = "a", "B: zużycie" = "b"))),
    toolbar = lc_readouts(c7p_read("F", "F(t)", upwr_accent), c7p_read("R", "R(t)"),
      c7p_read("f", "f(t) [1/h]"), c7p_read("h", "h(t) [1/h]")),
    legend = TRUE,
    body = c7p_stack(c("Prawdopodobieństwa F(t) i R(t)", "Tempa f(t) i h(t) w jednostkach 1/h")),
    config = c7p_rb_cfg,
    after = lc_caption("Gęstość to hazard pomnożony przez R(t): f(t) = h(t) · R(t). Gdy ubywa działających, gęstość odkleja się od hazardu w dół."))
)

# ============================================================================
# P10 — POLE POD HAZARDEM
# ============================================================================

c7p_10_ui <- c7p_panel("P10", "pojęcie", "Pole pod hazardem",
  lc_sf_figure("c7_pole_hazardu",
    control = lc_sf_slider("c7p10_t", "Czas t", 100, 3000, 1000, 50, suffix = " h"),
    toolbar = lc_readouts(c7p_read("ha", "Pole H(t), oferta A"), c7p_read("hb", "Pole H(t), oferta B"),
      c7p_read("ra", "R(t), oferta A"), c7p_read("rb", "R(t), oferta B")),
    legend = TRUE,
    body = c7p_stack(c("Hazard h(t) i jego pole do chwili t", "Niezawodność R(t) = exp(−pole)")),
    config = c7p_rb_cfg,
    after = lc_caption("Przy 1000 h hazardy obu ofert są prawie równe, ale oferta B dochodzi do tej wartości od zera. Jej pole jest o połowę mniejsze, więc niezawodność wyższa."))
)
