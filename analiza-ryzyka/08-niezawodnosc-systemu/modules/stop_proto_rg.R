# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rg: P19A, P19B, P23 ------------------
#
#   s8p_19a_ui — piaskownica „Krzywa nasycenia z sufitem” (P19A): rycina s8_galezie
#                (modules/proto_rg.js), R układu dla n gałęzi o niezawodności r,
#                ze wspólnym zasilaniem q: sufit 1 − q.
#   s8p_19b_ui — piaskownica „Ryzyko gałąź po gałęzi” (P19B): rycina s8_galezie_log,
#                ryzyko na osi log10, prosta dla niezależnych gałęzi, podłoga q.
#   s8p_23_ui  — piaskownica „Ta sama poprawa, trzy miejsca” (P23): rycina s8_poprawa,
#                zysk z poprawy o 0.02 każdego elementu szeregu Bananpolu.
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R, R/lecture_layout.R.

# ============================================================================
# P19A — KRZYWA NASYCENIA Z SUFITEM
# ============================================================================

s8p_19a_ui <- s8p_panel("P19A", "piaskownica", "Krzywa nasycenia z sufitem",
  lc_sf_figure("s8_galezie",
    control = list(
      lc_sf_slider("s8p19a_n", "Liczba gałęzi", 1, 6, 2, 1),
      lc_sf_slider("s8p19a_r", "R jednej gałęzi", 0.5, 0.99, 0.9, 0.01),
      lc_segmented("s8p19a_q", "Wspólne zasilanie", c("brak" = "0", "q = 0.01" = "0.01"), selected = "0")
    ),
    toolbar = lc_readouts(
      s8p_read("rs", "R układu", upwr_accent),
      s8p_read("gain", "Przyrost od poprzedniej gałęzi"),
      s8p_read("risk", "Ryzyko"),
      s8p_read("cost", "Koszt")
    ),
    ratio = "2.4/1", max_height = "340px", legend = TRUE,
    after = lc_caption("Oś pionowa jest stała. Przy słabszej gałęzi krzywa rośnie dłużej; wspólne zasilanie zatrzymuje ją pod sufitem.")
  )
)

# ============================================================================
# P19B — RYZYKO GAŁĄŹ PO GAŁĘZI
# ============================================================================

s8p_19b_ui <- s8p_panel("P19B", "piaskownica", "Ryzyko gałąź po gałęzi",
  lc_sf_figure("s8_galezie_log",
    control = list(
      lc_sf_slider("s8p19b_r", "R jednej gałęzi", 0.5, 0.99, 0.9, 0.01),
      lc_segmented("s8p19b_q", "Wspólne zasilanie", c("brak" = "0", "q = 0.01" = "0.01"), selected = "0")
    ),
    toolbar = lc_readouts(
      s8p_read("goal", "Gałęzi do celu (niezależne)"),
      s8p_read("goalc", "Gałęzi do celu (wspólne zasilanie)"),
      s8p_read("risk6", "Ryzyko przy 6 gałęziach", upwr_accent)
    ),
    ratio = "2.4/1", max_height = "340px", legend = TRUE,
    after = lc_caption("Każda gałąź mnoży ryzyko przez 1 − r, więc na tej osi punkty leżą na prostej. Wspólne zasilanie zatrzymuje je na wysokości q.")
  )
)

# ============================================================================
# P23 — TA SAMA POPRAWA, TRZY MIEJSCA
# ============================================================================

s8p_23_ui <- s8p_panel("P23", "piaskownica", "Ta sama poprawa, trzy miejsca",
  lc_sf_figure("s8_poprawa",
    control = list(
      lc_sf_slider("s8p23_r1", "R wentylatora", 0.8, 0.98, 0.92, 0.01),
      lc_sf_slider("s8p23_r2", "R czujnika", 0.8, 0.98, 0.95, 0.01),
      lc_sf_slider("s8p23_r3", "R zasilania", 0.8, 0.98, 0.98, 0.01)
    ),
    toolbar = lc_readouts(
      s8p_read("rs", "R szeregu", upwr_accent),
      s8p_read("best", "Najwięcej daje"),
      s8p_read("birn", "Istotność Birnbauma")
    ),
    ratio = "2.4/1", max_height = "340px",
    after = lc_caption("Każdy słupek to ta sama poprawa o 0.02 w innym miejscu szeregu. Najwyższy jest zawsze słupek najsłabszego elementu.")
  )
)
