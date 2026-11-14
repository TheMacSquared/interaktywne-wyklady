# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rd: P9A, P9B, P10A, P10B, P13 ----------
#
#   z6p_9a_ui  — galeria „Trzy interwencje” (P9A): pięć wariantów w przełączniku,
#                rycina z6_interwencje, stałe osie, strzałki zmian i odczyty.
#   z6p_9b_ui  — galeria „Trzy interwencje obok siebie” (P9B): cztery małe wykresy,
#                rycina z6_interwencje_b, każdy z wynikiem nad wykresem.
#   z6p_10a_ui — przypadek brzegowy „Próg poniżej średniej” (P10A): suwak σ i wybór
#                progu, rycina z6_prog_ponizej.
#   z6p_10b_ui — ta sama galeria co P9A dla progu 80°C (P10B): rycina z6_interwencje
#                z inną konfiguracją i innym id kontrolki.
#   z6p_13_ui  — przypadek brzegowy „Pole nakładania to nie ryzyko” (P13): dwie gęstości
#                i margines D, rycina z6_nakladanie.
# Ryciny lcSF liczone w przeglądarce: modules/proto_rd.js (window.lcSF).
# Wymaga: stop_proto_common.R (z6p_panel, z6p_read), R/lc_stat_figures.R, R/lecture_layout.R.

# ============================================================================
# P9A / P10B — WARIANTY INTERWENCJI (jedna rycina, konfiguracja w config)
# ============================================================================

# Warianty stanu łożyska: moved mówi, co się przesunęło (odczyt „Co się ruszyło”).
z6p9_variants <- list(
  list(key = "base", label = "Stan bazowy", mu = 82, sd = 3, c = 85, moved = "nic"),
  list(key = "mean", label = "Chłodzenie: μ − 2°C", mu = 80, sd = 3, c = 85, moved = "krzywa"),
  list(key = "sd", label = "Stabilizacja: σ − 1°C", mu = 82, sd = 2, c = 85, moved = "krzywa"),
  list(key = "thr", label = "Wyższy próg: c + 2°C", mu = 82, sd = 3, c = 87, moved = "kreska progu"),
  list(key = "both", label = "Chłodzenie i stabilizacja", mu = 80, sd = 2, c = 85, moved = "krzywa")
)

z6p9a_cfg <- list(base = list(mu = z6p_mu, sd = z6p_sd, c = z6p_c), variants = z6p9_variants, ymax = 0.21)

z6p_9a_ui <- z6p_panel("P9A", "galeria", "Trzy interwencje",
  lc_sf_figure("z6_interwencje",
    control = lc_segmented("z6p9a_v", "Działanie", c(
      "Stan bazowy" = "base", "Chłodzenie: μ − 2°C" = "mean", "Stabilizacja: σ − 1°C" = "sd",
      "Wyższy próg: c + 2°C" = "thr", "Chłodzenie i stabilizacja" = "both")),
    toolbar = lc_readouts(z6p_read("musp", "μ / σ / próg"), z6p_read("z", "z progu"),
      z6p_read("p", "P(T > c)", upwr_accent), z6p_read("f", "Częstość"),
      z6p_read("moved", "Co się ruszyło")),
    ratio = "2.2/1", max_height = "360px", config = z6p9a_cfg,
    after = lc_caption("Przerywany kontur i przerywana szara kreska to stan bazowy. Chłodzenie i wyższy próg dają to samo P(T > c) = 0.048, ale tylko chłodzenie przesuwa krzywą: przy wyższym progu rusza się kreska, a łożysko pracuje jak przedtem."))
)

# P10B: ten sam rysunek (z6_interwencje), inny przypadek: próg 80°C, tylko σ się zmienia.
z6p10b_cfg <- list(base = list(mu = z6p_mu, sd = z6p_sd, c = 80), ymax = 0.28,
  variants = list(
    list(key = "sd3", label = "σ = 3°C", mu = 82, sd = 3, c = 80, moved = "nic"),
    list(key = "sd2", label = "σ = 2°C", mu = 82, sd = 2, c = 80, moved = "krzywa"),
    list(key = "sd15", label = "σ = 1.5°C", mu = 82, sd = 1.5, c = 80, moved = "krzywa")
  ))

z6p_10b_ui <- z6p_panel("P10B", "przypadek brzegowy", "Stabilizacja przy progu 80°C",
  lc_sf_figure("z6_interwencje",
    control = lc_segmented("z6p10b_v", "Stabilizacja", c(
      "σ = 3°C" = "sd3", "σ = 2°C" = "sd2", "σ = 1.5°C" = "sd15")),
    toolbar = lc_readouts(z6p_read("musp", "μ / σ / próg"), z6p_read("z", "z progu"),
      z6p_read("p", "P(T > c)", upwr_accent), z6p_read("f", "Częstość"),
      z6p_read("moved", "Co się ruszyło")),
    ratio = "2.2/1", max_height = "360px", config = z6p10b_cfg,
    after = lc_caption("Ta sama galeria co P9A dla progu 80°C: zwężenie rozkładu tu podnosi ryzyko, z 0.748 przez 0.841 do 0.909."))
)

# P9B: cztery małe wykresy obok siebie, każdy z wynikiem nad wykresem.
z6p9b_cfg <- list(base = list(mu = z6p_mu, sd = z6p_sd, c = z6p_c), grid = TRUE, ymax = 0.21,
  variants = z6p9_variants[1:4])

z6p_9b_ui <- z6p_panel("P9B", "galeria", "Trzy interwencje obok siebie",
  lc_sf_figure("z6_interwencje_b", charts = 4,
    headings = c("Stan bazowy", "Chłodzenie: μ − 2°C", "Stabilizacja: σ − 1°C", "Wyższy próg: c + 2°C"),
    ratio = "1.7/1", config = z6p9b_cfg,
    after = lc_caption("Drugi i czwarty wykres mają to samo pole, 0.048: chłodzenie przesuwa krzywą, wyższy próg tylko kreskę."))
)

# ============================================================================
# P10A — PRÓG PONIŻEJ ŚREDNIEJ (suwak σ, wybór progu)
# ============================================================================

z6p_10a_ui <- z6p_panel("P10A", "przypadek brzegowy", "Próg poniżej średniej",
  lc_sf_figure("z6_prog_ponizej",
    control = list(
      lc_segmented("z6p10_c", "Próg", c("80°C (pod średnią)" = "80", "82°C (w średniej)" = "82",
        "85°C (nad średnią)" = "85"), selected = "80"),
      lc_sf_slider("z6p10_sd", "Odchylenie σ", 1.5, 6, 3, 0.25, suffix = "°C")),
    toolbar = lc_readouts(z6p_read("z", "z progu"), z6p_read("p", "P(T > c)", upwr_accent),
      z6p_read("zw", "Zwężenie rozkładu")),
    ratio = "2.2/1", max_height = "360px",
    after = lc_caption("Zwężenie rozkładu pomaga tylko wtedy, gdy próg leży nad średnią. Dla progu 80°C zmniejszenie σ z 3 do 2 podnosi P(T > 80) z 0.748 do 0.841; dla progu równego średniej zostaje 0.5."))
)

# ============================================================================
# P13 — POLE NAKŁADANIA TO NIE RYZYKO (dwie gęstości i margines D)
# ============================================================================

z6p13_cfg <- list(sdL = 8, sdS = 7)

z6p_13_ui <- z6p_panel("P13", "przypadek brzegowy", "Pole nakładania to nie ryzyko",
  lc_sf_figure("z6_nakladanie",
    control = lc_segmented("z6p13_v", "Układ", c("L = 85, S = 95" = "a", "Zamienione: L = 95, S = 85" = "b")),
    toolbar = lc_readouts(z6p_read("ov", "Pole wspólne"), z6p_read("pl", "P(L > S)", upwr_accent),
      z6p_read("f", "Częstość")),
    charts = 2, headings = c("Dwie gęstości na jednej osi", "Margines D = S − L"),
    ratio = "1.7/1", legend = TRUE, config = z6p13_cfg,
    after = lc_caption("Pole wspólne dwóch krzywych wynosi 0.50 w obu układach, a P(L > S) skacze z 0.173 do 0.827. Pole nakładania nie wie, która krzywa jest która."))
)
