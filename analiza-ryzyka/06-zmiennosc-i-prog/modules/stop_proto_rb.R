# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rb: P2B, P3, P5 ---------------------
#
#   z6p_2b_ui — Symulacja „Ogon dochodzi ostatni” (P2B): histogram z ogonem ponad
#               85°C i ścieżka odsetka pomiarów ponad progiem w funkcji ich liczby.
#   z6p_3_ui  — Pojęcie „Pole między dwiema kreskami” (P3): P(a < T ≤ b) jako pole
#               pod N(82, 3) między dwoma suwakami, gotowe ustawienia przedziału.
#   z6p_5_ui  — Pojęcie „Linijka σ” (P5): przedziały μ ± kσ nad osią i linijka odchyleń.
# Rysunki w przeglądarce (lcSF, ECharts): z6_hist_b, z6_pole, z6_linijka
# w modules/proto_rb.js. Wymaga: stop_proto_common.R, R/lc_stat_figures.R.
# Kontrolki i odczyty mają unikalne identyfikatory z prefiksem z6p<nr>_.

# Konfiguracja wspólna dla rycin: stan bazowy i próg (jak w stop_proto_common.R).
z6p_rb_config <- list(mu = z6p_mu, sd = z6p_sd, c = z6p_c)

# ============================================================================
# P2B — SYMULACJA „OGON DOCHODZI OSTATNI” (przyjęta 17.11.2026, zamiast P2A
# i dawnego histogramu z suwakiem liczebności)
# ============================================================================

z6p_2b_ui <- z6p_panel("P2B", "symulacja", "Ogon dochodzi ostatni", proto = FALSE,
  lc_sf_figure("z6_hist_b",
    charts = 2,
    headings = c("Histogram pomiarów", "Odsetek pomiarów ponad 85°C"),
    toolbar = tagList(
      lc_sf_action_group("Dopisz pomiary", add10 = "+10", add100 = "+100", add1000 = "+1000"),
      lc_sf_action("reset", "z6p2b_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
      lc_readouts(
        z6p_read("n", "Pomiarów"),
        z6p_read("over85", "Ponad 85°C", upwr_accent),
        z6p_read("over91", "Ponad 91°C"))),
    ratio = "1.6/1", max_height = "340px",
    config = z6p_rb_config,
    after = lc_caption("Środek histogramu ustala się po kilkudziesięciu pomiarach, a odsetek ponad progiem jeszcze przy kilkuset waha się o kilka punktów procentowych. Dla progu 91°C mała próba nie mówi prawie nic.")))

# ============================================================================
# P3 — POJĘCIE „POLE MIĘDZY DWIEMA KRESKAMI”
# ============================================================================

z6p_3_ui <- z6p_panel("P3", "pojęcie", "Pole między dwiema kreskami", proto = FALSE,
  lc_sf_figure("z6_pole",
    control = list(
      lc_sf_slider("z6p3_a", "Lewa kreska a", 76, 88, 79, 0.05, suffix = "°C"),
      lc_sf_slider("z6p3_b", "Prawa kreska b", 76, 88, 85, 0.05, suffix = "°C")),
    toolbar = tagList(
      lc_sf_action_group("Ustaw przedział",
        wide = "79–85°C", ex = "80–85°C", narrow = "81.95–82.05°C", point = "82°C"),
      lc_readouts(
        z6p_read("p", "P(a < T ≤ b)"),
        z6p_read("w", "Szerokość b − a"),
        z6p_read("h", "Wysokość krzywej w środku"))),
    ratio = "2.2/1", max_height = "340px",
    config = z6p_rb_config,
    after = lc_caption("Gdy kreski się zejdą, pole maleje do zera, choć krzywa w 82°C nadal ma wysokość 0.133: gęstość to nie prawdopodobieństwo.")))

# ============================================================================
# P5 — POJĘCIE „LINIJKA σ”
# ============================================================================

z6p_5_ui <- z6p_panel("P5", "pojęcie", "Linijka σ", proto = FALSE,
  lc_sf_figure("z6_linijka",
    control = lc_segmented("z6p5_k", "Przedział",
      choices = c("μ ± 1σ" = "1", "μ ± 2σ" = "2", "μ ± 3σ" = "3"), selected = "1"),
    toolbar = lc_readouts(
      z6p_read("range", "Przedział"),
      z6p_read("inside", "W przedziale"),
      z6p_read("outside", "Ponad górnym końcem", upwr_accent)),
    ratio = "2.2/1", max_height = "360px",
    config = z6p_rb_config,
    after = lc_caption("Próg 85°C leży dokładnie jedno σ nad średnią, więc ponad nim jest połowa z pozostałych 32%.")))
