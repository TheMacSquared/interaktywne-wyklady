# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rd: P4 (rozdział 2, obserwacja cenzorowana)
#
#   c7p_4_ui — Piaskownica „Trzy rachunki a koniec badania” (P4): rycina lcSF c7_koniec
#              (modules/proto_rd.js). Osiem wentylatorów o stałych czasach życia, suwak końca
#              badania c i pole „pokaż prawdziwe czasy życia”. Trzy liczby z tych samych
#              danych: średnia z rejestru awarii, dolna granica (łączny czas / liczba
#              wentylatorów) i szacunek (7.2) (łączny czas / liczba awarii). Stoi po
#              przykładzie 7.2, gdy wszystkie trzy rachunki są już wprowadzone.
# (Symulacja P6 „To samo badanie powtórzone wiele razy” usunięta 2026-11-25: nie pasowała do narracji.)
# Wymaga: stop_proto_common.R, R/lc_stat_figures.R.

# ============================================================================
# P4 — PIASKOWNICA „KONIEC BADANIA A ŚREDNIA Z AWARII”
# ============================================================================

c7p4_times <- c(220, 480, 760, 990, 1350, 1750, 2300, 3100)

c7p_4_ui <- c7p_panel("P4", "piaskownica", "Trzy rachunki a koniec badania",
  lc_sf_figure("c7_koniec",
    control = list(
      lc_sf_slider("c7p4_c", "Koniec badania", 300, 3300, 1200, 50, suffix = " h"),
      checkboxInput("c7p4_truth", "Pokaż prawdziwe czasy życia", FALSE)
    ),
    toolbar = lc_readouts(
      c7p_read("d", "Awarie"),
      c7p_read("naive", "Średnia z rejestru awarii", upwr_accent),
      c7p_read("lower", "Dolna granica", upwr_cat[["szalwia"]]),
      c7p_read("est", "Szacunek \\(1/\\hat\\lambda\\)", upwr_cat[["niebo"]]),
      c7p_read("truth", "Prawdziwa średnia", "var(--upwr-ink)", text = "nieznana")
    ),
    ratio = "2.2/1", max_height = "360px",
    config = list(times = c7p4_times),
    after = lc_caption("Wentylatory są te same przy każdym położeniu suwaka. Zmienia się tylko to, które czasy życia zdążyły zostać awariami. Dolna granica nigdy nie przekracza prawdziwej średniej.")
  )
)
