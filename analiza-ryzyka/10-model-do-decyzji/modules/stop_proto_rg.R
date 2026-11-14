# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rg: karta 2 (kontrola partii) i odnowa ----
#
#   i10p_6a_ui — Piaskownica „Zero wad w n kontrolach” (P6A): krzywa (1 − p)ⁿ, poziom
#                1 − conf i górna granica p przy zerze wad (wzór 10.3).
#   i10p_6b_ui — Pojęcie „Dziesięć razy niżej, dziesięć razy więcej” (P6B): granica
#                jako funkcja n na osiach logarytmicznych; prosta reguły trzech.
#   i10p_8_ui  — Galeria „Trzy polityki odnowy” (P8): trzy polityki wymiany wentylatora
#                na osi 0–3000 h, model Weibulla i wykładniczy.
# Ryciny lcSF (ECharts) rejestrowane w modules/proto_rg.js: i10_zero, i10_zero_n, i10_odnowa.
# Wymaga: stop_proto_common.R (i10p_cfg, i10p_panel, i10p_read), R/lc_stat_figures.R,
# R/palette.R (upwr_cat).

# Poziom ufności granicy (wzór 10.3). Brak go w i10p_cfg, więc trzymany tu,
# a przekazywany do JS w config (nie wpisujemy go w kodzie rysunku w JS).
i10p6_conf <- 0.95
# Zakres osi p w P6A (stała oś; górna granica osi p).
i10p6a_pmax <- 0.3
# Zakres osi p w P6B: od 0.001 do 1 (granice osi logarytmicznej).
i10p6b_p_axis <- c(0.001, 1)

# ============================================================================
# P6A — PIASKOWNICA „ZERO WAD W n KONTROLACH”
# ============================================================================

i10p_6a_ui <- i10p_panel("P6A", "piaskownica", "Zero wad w n kontrolach",
  lc_sf_figure("i10_zero",
    control = list(lc_sf_slider("i10p6a_n", "Liczba kontroli bez wady n", 10, 1000, 100, 10)),
    toolbar = lc_readouts(
      i10p_read("up", "Górna granica 95% dla p", upwr_accent),
      i10p_read("r3", "Reguła trzech 3/n")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    config = list(conf = i10p6_conf, p_max = i10p6a_pmax),
    after = lc_caption("Im więcej kontroli bez wady, tym węższy pas wartości p, których wynik nie wyklucza. Zero wad nigdy nie daje granicy równej zero.")))

# ============================================================================
# P6B — POJĘCIE „DZIESIĘĆ RAZY NIŻEJ, DZIESIĘĆ RAZY WIĘCEJ”
# ============================================================================

i10p_6b_ui <- i10p_panel("P6B", "pojęcie", "Dziesięć razy niżej, dziesięć razy więcej",
  lc_sf_figure("i10_zero_n",
    control = list(lc_sf_slider("i10p6b_n", "Liczba kontroli bez wady n", 10, 1000, 100, 10)),
    toolbar = lc_readouts(
      i10p_read("up", "Górna granica 95% dla p", upwr_accent),
      i10p_read("r3", "Reguła trzech 3/n")),
    ratio = "2.2/1", max_height = "360px", legend = TRUE,
    config = list(conf = i10p6_conf, marks = c(10, 100, 1000), p_axis = i10p6b_p_axis),
    after = lc_caption("Obie osie są logarytmiczne. Dziesięć razy niższa granica wymaga dziesięć razy większej próby.")))

# ============================================================================
# P8 — GALERIA „TRZY POLITYKI ODNOWY”
# ============================================================================

i10p_8_ui <- i10p_panel("P8", "galeria", "Trzy polityki odnowy",
  lc_sf_figure("i10_odnowa",
    control = lc_segmented("i10p8_model", "Model wentylatora",
      c("Weibull (zużycie)" = "weibull", "Wykładniczy (stały hazard)" = "exp")),
    toolbar = lc_readouts(
      i10p_read("a", "Bez wymiany", upwr_secondary),
      i10p_read("b", paste0("Wymiana co ", i10p_cfg$time, " ", i10p_cfg$unit), upwr_cat[["niebo"]]),
      i10p_read("c", paste0("Wymiana co ", i10p_num(i10p_cfg$time / 2, 0), " ", i10p_cfg$unit), upwr_cat[["szalwia"]])),
    ratio = "2.2/1", max_height = "360px", legend = TRUE, selected = "weibull",
    notes = list(
      weibull = lc_caption("Przy zużyciu każda wymiana zaczyna krzywą od nowa, więc częstsza wymiana daje większą szansę przepracowania całego czasu bez awarii."),
      exp = lc_caption("Przy stałym hazardzie trzy krzywe pokrywają się: wymiana niczego nie zmienia.")),
    config = c(i10p_cfg, list(periods = c(i10p_cfg$time, i10p_cfg$time / 2)))))
