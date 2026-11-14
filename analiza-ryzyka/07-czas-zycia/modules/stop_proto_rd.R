# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rd: P4 i P6 (rozdział 2, obserwacja cenzorowana)
#
#   c7p_4_ui — Piaskownica „Koniec badania a średnia z awarii” (P4): rycina lcSF c7_koniec
#              (modules/proto_rd.js). Osiem wentylatorów o stałych czasach życia, suwak końca
#              badania c i pole „pokaż prawdziwe czasy życia”. Naiwna średnia z awarii rośnie
#              z c, szacunek (7.2) liczony jest z tego samego zbioru danych.
#   c7p_6_ui — Symulacja „To samo badanie powtórzone wiele razy” (P6): rycina lcSF
#              c7_powtorki, dwa histogramy (średnia z awarii i szacunek 7.2) z modelu
#              wykładniczego MTTF 1500 h, badanie przerwane po 1200 h.
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R, R/lc_dist.js (window.lcDist.rng).

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

# ============================================================================
# P6 — SYMULACJA „TO SAMO BADANIE POWTÓRZONE WIELE RAZY”
# ============================================================================

c7p_6_ui <- c7p_panel("P6", "symulacja", "To samo badanie powtórzone wiele razy",
  lc_sf_figure("c7_powtorki",
    toolbar = tagList(
      lc_sf_action_group("Powtórz badanie", add1 = "+1", add10 = "+10", add100 = "+100", add1000 = "+1000"),
      lc_sf_action_group("Symulacja", reset = "Od nowa"),
      lc_readouts(
        c7p_read("n", "Badań"),
        c7p_read("naive", "Mediana średnich z awarii", upwr_accent),
        c7p_read("est", "Mediana szacunków (7.2)"),
        c7p_read("below", "Średnia z awarii poniżej 1500 h"),
        c7p_read("out", "Szacunki poza osią")
      )
    ),
    body = c7p_stack(c("Średnia z samych awarii", "Szacunek ze wzoru (7.2)"), "4.2/1", "200px"),
    config = list(mttf = c7p_mttf, n = 8, c = 1200, seed = 2026),
    after = lc_caption("Każde powtórzenie to nowe osiem wentylatorów z tego samego modelu (MTTF 1500 h), badanie przerwane po 1200 h. Średnia z samych awarii nie może przekroczyć 1200 h, więc myli się zawsze w tę samą stronę.")
  )
)
