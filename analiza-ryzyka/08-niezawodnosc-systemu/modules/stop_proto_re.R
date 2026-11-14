# PROTOTYPY PUNKTÓW STOP (2026-11-14) — wykład 08, grupa RE: piaskownice z ryciny lcSF ----
#
#   s8p_4b_ui  — P4B „Wspólny czas misji”: wentylator, czujnik i system w czasie (s8_horyzont)
#   s8p_9a_ui  — P9A „Kwadrat dwóch elementów”: pola R₁ × R₂ dla szeregu i równoległego (s8_kwadrat)
#   s8p_9b_ui  — P9B „Te same R, dwa systemy”: elementy, szereg i układ równoległy (s8_architektury)
#   s8p_11a_ui — P11A „System pod najsłabszym szeregiem”: wentylatory A, B i sterownik C (s8_czas)
#   s8p_11b_ui — P11B „Ile daje druga gałąź”: ryzyko bloku wentylatorów (s8_przewaga)
#
# Rysunki liczy modules/proto_re.js (window.lcSF, ECharts); R tylko składa kontenery,
# suwaki, przełączniki i odczyty. Wymaga: stop_proto_common.R, R/lc_stat_figures.R.

# ============================================================================
# P4B — WSPÓLNY CZAS MISJI
# ============================================================================

s8p_4b_ui <- s8p_panel("P4B", "piaskownica", "Wspólny czas misji",
  lc_sf_figure("s8_horyzont",
    control = list(lc_sf_slider("s8p4b_t", "Czas misji t", 200, 8760, 1000, 20, suffix = " h")),
    toolbar = lc_readouts(
      s8p_read("fan", "R wentylatora"),
      s8p_read("sens", "R czujnika"),
      s8p_read("sys", "R systemu", upwr_accent),
      s8p_read("naive", "Naiwny iloczyn 0.92 · 0.95", text = "0.874")
    ),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    after = lc_caption("Liczby z karty leżą w dwóch różnych chwilach. Mnożyć wolno tylko wartości z jednej pionowej kreski.")
  )
)

# ============================================================================
# P9A — KWADRAT DWÓCH ELEMENTÓW
# ============================================================================

s8p_9a_ui <- s8p_panel("P9A", "piaskownica", "Kwadrat dwóch elementów",
  lc_sf_figure("s8_kwadrat",
    control = list(
      lc_segmented("s8p9a_arch", "Układ", c("Szeregowy" = "series", "Równoległy" = "parallel")),
      lc_sf_slider("s8p9a_r1", "R₁", 0.5, 1, 0.92, 0.01),
      lc_sf_slider("s8p9a_r2", "R₂", 0.5, 1, 0.95, 0.01)
    ),
    toolbar = lc_readouts(
      s8p_read("both", "Oba działają R₁R₂"),
      s8p_read("one", "Tylko jeden R₁(1−R₂)+(1−R₁)R₂"),
      s8p_read("none", "Oba zawodzą (1−R₁)(1−R₂)"),
      s8p_read("sys", "R systemu", upwr_accent)
    ),
    ratio = "1.3/1", max_height = "440px", legend = TRUE,
    after = lc_caption("Pole to prawdopodobieństwo. Suwaki przesuwają linie podziału, osie stoją w miejscu.")
  )
)

# ============================================================================
# P9B — TE SAME R, DWA SYSTEMY
# ============================================================================

s8p_9b_ui <- s8p_panel("P9B", "piaskownica", "Te same R, dwa systemy",
  lc_sf_figure("s8_architektury",
    control = list(
      lc_sf_slider("s8p9b_r1", "R₁", 0.5, 1, 0.92, 0.01),
      lc_sf_slider("s8p9b_r2", "R₂", 0.5, 1, 0.95, 0.01)
    ),
    toolbar = lc_readouts(
      s8p_read("r_ser", "R szeregu", upwr_accent),
      s8p_read("k_ser", "Ryzyko szeregu"),
      s8p_read("r_par", "R równoległego"),
      s8p_read("k_par", "Ryzyko równoległego")
    ),
    ratio = "2/1", max_height = "380px",
    after = lc_caption("Przerywane kreski to słabszy i lepszy element. Szereg zawsze kończy pod dolną, układ równoległy nad górną.")
  )
)

# ============================================================================
# P11A — SYSTEM POD NAJSŁABSZYM SZEREGIEM
# ============================================================================

s8p_11a_ui <- s8p_panel("P11A", "piaskownica", "System pod najsłabszym szeregiem",
  lc_sf_figure("s8_czas",
    control = list(lc_sf_slider("s8p11a_t", "Czas misji", 100, 3000, 1000, 50, suffix = " h")),
    toolbar = lc_readouts(
      s8p_read("ra", "R wentylatora A"),
      s8p_read("rb", "R wentylatora B"),
      s8p_read("rc", "R sterownika C"),
      s8p_read("rs", "R systemu", upwr_accent)
    ),
    ratio = "2.2/1", max_height = "380px", legend = TRUE,
    after = lc_caption("Sterownik leży na każdej drodze sukcesu, więc system nigdy nie jest od niego lepszy.")
  )
)

# ============================================================================
# P11B — ILE DAJE DRUGA GAŁĄŹ
# ============================================================================

s8p_11b_ui <- s8p_panel("P11B", "piaskownica", "Ile daje druga gałąź",
  lc_sf_figure("s8_przewaga",
    control = list(lc_sf_slider("s8p11b_t", "Czas misji", 100, 3000, 1000, 50, suffix = " h")),
    toolbar = lc_readouts(
      s8p_read("risk_a", "Ryzyko samego A"),
      s8p_read("risk_ab", "Ryzyko bloku A/B", upwr_accent),
      s8p_read("mult", "Mnożnik 1 − R_B(t)")
    ),
    ratio = "2.2/1", max_height = "380px", legend = TRUE,
    after = lc_caption("Druga gałąź mnoży ryzyko przez 1 − R_B(t). Im dłuższa misja, tym bliżej jedynki jest ten mnożnik.")
  )
)
