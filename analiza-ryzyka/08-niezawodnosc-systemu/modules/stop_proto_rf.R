# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rf: P16A, P16B, P17 ----------
#
#   s8p_16a_ui — piaskownica „Ile zostaje z obietnicy 0.996” (P16A): R(q) = (1 − q)·0.996
#                (wzór 8.10) przy wspólnym zasilaniu q; ryciny lcSF s8_wspolna.
#   s8p_16b_ui — piaskownica „Z czego składa się ryzyko” (P16B): ryzyko rozłożone na
#                wspólne zasilanie i awarię obu wentylatorów; ryciny lcSF s8_sklad.
#   s8p_17_ui  — piaskownica „β: awarie własne i wspólne” (P17): model beta-factor
#                (wzór 8.12) dla dwóch wentylatorów o R = 0.92; ryciny lcSF s8_beta.
# Wymaga: stop_proto_common.R (s8p_panel, s8p_read), R/lc_stat_figures.R
# (lc_sf_figure, lc_sf_slider) i modules/proto_rf.js (window.lcS8 z sceny.js).
# Wszystkie liczby rysuje JS; R składa tylko kontenery i odczyty.

# ============================================================================
# P16A — ILE ZOSTAJE Z OBIETNICY 0.996
# ============================================================================

s8p_16a_ui <- s8p_panel("P16A", "piaskownica", "Ile zostaje z obietnicy 0.996",
  lc_sf_figure("s8_wspolna",
    control = lc_sf_slider("s8p16a_q", "P(utraty wspólnego zasilania) q", 0, 0.15, 0.01, 0.005),
    toolbar = lc_readouts(
      s8p_read("s8p16a_out_r", "R układu", upwr_accent),
      s8p_read("s8p16a_out_risk", "Ryzyko"),
      s8p_read("s8p16a_out_share", "Udział wspólnej przyczyny"),
      s8p_read("s8p16a_out_ratio", "Ryzyko względem obiecanego")),
    ratio = "2.2/1", max_height = "340px",
    after = lc_caption("Przerywana kreska to obietnica samego układu równoległego. Pionowy odcinek to niezawodność, którą zabiera wspólne zasilanie.")))

# ============================================================================
# P16B — Z CZEGO SKŁADA SIĘ RYZYKO
# ============================================================================

s8p_16b_ui <- s8p_panel("P16B", "piaskownica", "Z czego składa się ryzyko",
  lc_sf_figure("s8_sklad",
    control = lc_sf_slider("s8p16b_q", "P(utraty wspólnego zasilania) q", 0, 0.15, 0.01, 0.005),
    toolbar = lc_readouts(
      s8p_read("s8p16b_out_q", "Wspólne zasilanie"),
      s8p_read("s8p16b_out_both", "Oba wentylatory"),
      s8p_read("s8p16b_out_risk", "Ryzyko razem", upwr_accent),
      s8p_read("s8p16b_out_share", "Udział wspólnej przyczyny")),
    legend = TRUE, ratio = "2.2/1", max_height = "340px",
    after = lc_caption("Ciemny pasek na dole to ryzyko, które obiecywał sam układ równoległy. Cała reszta to jedno zdarzenie: utrata zasilania.")))

# ============================================================================
# P17 — β: AWARIE WŁASNE I WSPÓLNE
# ============================================================================

s8p_17_ui <- s8p_panel("P17", "piaskownica", "β: awarie własne i wspólne",
  lc_sf_figure("s8_beta",
    control = lc_sf_slider("s8p17_beta", "Udział awarii wspólnych β", 0, 0.2, 0.1, 0.01),
    toolbar = lc_readouts(
      s8p_read("s8p17_out_r", "R układu"),
      s8p_read("s8p17_out_risk", "Ryzyko", upwr_accent),
      s8p_read("s8p17_out_common", "Część wspólna"),
      s8p_read("s8p17_out_ratio", "Względem niezależnych")),
    legend = TRUE, ratio = "2.2/1", max_height = "340px",
    after = lc_caption("Każdy wentylator ma przez cały czas R = 0.92. Zmienia się tylko to, jaka część jego awarii zdarza się razem z awarią drugiego.")))
