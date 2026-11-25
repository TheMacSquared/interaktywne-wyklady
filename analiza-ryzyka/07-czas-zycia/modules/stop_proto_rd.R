# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rd: P4 (rozdział 2, obserwacja cenzorowana)
#
#   c7p_4_ui — Piaskownica „Koniec badania a średnia z awarii” (P4): rycina lcSF c7_koniec
#              (modules/proto_rd.js). Osiem wentylatorów o stałych czasach życia, suwak końca
#              badania c i pole „pokaż prawdziwe czasy życia”. Naiwna średnia z awarii rośnie
#              z c, szacunek (7.2) liczony jest z tego samego zbioru danych.
# (Symulacja P6 „To samo badanie powtórzone wiele razy” usunięta 2026-11-25: nie pasowała do narracji.)
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R.

# ============================================================================
# P4 — PIASKOWNICA „KONIEC BADANIA A ŚREDNIA Z AWARII”
# ============================================================================

c7p4_times <- c(220, 480, 760, 990, 1350, 1750, 2300, 3100)

c7p_4_ui <- c7p_panel("P4", "piaskownica", "Koniec badania a średnia z awarii",
  lc_sf_figure("c7_koniec",
    control = list(
      lc_sf_slider("c7p4_c", "Koniec badania", 300, 3300, 1200, 50, suffix = " h"),
      checkboxInput("c7p4_truth", "Pokaż prawdziwe czasy życia", FALSE)
    ),
    toolbar = lc_readouts(
      c7p_read("d", "Awarie"),
      c7p_read("sum", "Łączny czas obserwacji"),
      c7p_read("naive", "Średnia z samych awarii", upwr_accent),
      c7p_read("est", "Szacunek MTTF (7.2)")
    ),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    config = list(times = c7p4_times),
    after = lc_caption("Wentylatory są te same przy każdym położeniu suwaka. Zmienia się tylko to, które czasy życia zdążyły zostać awariami.")
  )
)
