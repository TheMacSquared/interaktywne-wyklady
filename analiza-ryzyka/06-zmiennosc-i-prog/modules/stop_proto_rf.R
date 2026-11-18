# PROTOTYPY PUNKTÓW STOP (2026-11-14) — grupa rf: P14A, P15B ----------------------
#
#   z6p_14a_ui — galeria „Ten sam środek, inny ogon” (P14A): trzy modele na jednej
#                osi, widok „Cały rozkład” / „Ogon powyżej 88°C”; ryciny lcSF z6_modele
#                (modules/proto_rf.js), tabela pod wykresem wypełniana przez JS.
#   z6p_15b_ui — pojęcie „Jak powstaje wykres kwantylowy” (P15B): histogram, krzywa
#                modelu i wykres Q–Q krok po kroku (modules/proto_rf.js, scena „qq_budowa”).
# Wymaga: stop_proto_common.R (z6p_panel, z6p_css, z6p_svg, z6p_frames_ui),
# R/lc_stat_figures.R (lc_sf_figure, lc_segmented) i modules/proto_rf.js.

# ============================================================================
# P14A — GALERIA TRZECH MODELI (przyjęta 17.11.2026)
# ============================================================================

z6p_14a_ui <- z6p_panel("P14A", "galeria", "Ten sam środek, inny ogon", proto = FALSE,
  lc_sf_figure("z6_modele",
    control = lc_segmented("z6p14_view", "Widok",
      choices = c("Cały rozkład" = "all", "Ogon powyżej 88°C" = "tail")),
    ratio = "2.4/1", max_height = "360px", legend = TRUE,
    after = tagList(
      tags$div(`data-role` = "table"),
      lc_caption("Przy progu 1σ (85°C) trzy modele dają podobne ryzyko, przy progu 3σ (91°C) różnią się 5–9 razy. Ten sam środek, a różni je tylko ogon.")
    )))

# ============================================================================
# P15B — JAK POWSTAJE WYKRES KWANTYLOWY (18.11.2026; zastępuje kadry P15A)
# ============================================================================
# Krok po kroku obok definicji 6.8: histogram pomiarów i krzywa modelu N(82, 3) na
# wspólnej osi temperatury, obok wykres Q–Q. Kroki: dane → kwantyl empiryczny
# (i-ty pomiar rosnąco) → kwantyl teoretyczny (to samo pole pod krzywą) → para jako
# punkt → wszystkie pary. Przełącznik komory (jak w scenie P19B) i suwak numeru
# pomiaru. Rysunek: modules/proto_rf.js (scena „qq_budowa”).

# 40 pomiarów na komorę: kwantyle modelu komory z niewielkim szumem, w losowej
# kolejności zapisu (stałe ziarno). Komora 1: N(82, 3); komora 2: ten sam model
# skośny co w P14A i P19B (76.8 + Gamma(3, √3)).
z6rf_n <- 40L
z6rf_data <- local({
  set.seed(615)
  p <- (seq_len(z6rf_n) - 0.5) / z6rf_n
  a <- qnorm(p, z6p_mu, z6p_sd) + rnorm(z6rf_n, 0, 0.5)
  b <- (z6p_mu - sqrt(3) * z6p_sd) + qgamma(p, 3, scale = z6p_sd / sqrt(3)) + rnorm(z6rf_n, 0, 0.3)
  b <- pmax(b, z6p_mu - sqrt(3) * z6p_sd + 0.1)
  list(a = round(sample(a), 1), b = round(sample(b), 1))
})

z6rf_qq_cfg <- list(
  mu = z6p_mu, sd = z6p_sd, n = z6rf_n, i0 = 36L, data = z6rf_data,
  sets = list(a = "Komora 1", b = "Komora 2"),
  steps = list(
    list(name = "Dane", text = "Czterdzieści pomiarów temperatury łożyska z wybranej komory. U góry ich histogram, pod nim krzywa modelu normalnego N(82, 3) na tej samej osi."),
    list(name = "Kwantyl empiryczny", text = "Ustawiamy pomiary rosnąco. Pomiar nr i to kwantyl empiryczny: poniżej niego leży ułamek (i − 0.5)/n danych."),
    list(name = "Kwantyl teoretyczny", text = "Model odpowiada na to samo pytanie: gdzie krzywa N(82, 3) odcina z lewej to samo pole? To kwantyl teoretyczny, w jednostkach z."),
    list(name = "Para na wykres", text = "Oba kwantyle tworzą jeden punkt: poziomo kwantyl modelu z, pionowo pomiar. Gdy dane zgadzają się z modelem, punkt leży na prostej 82 + 3z."),
    list(name = "Wszystkie pomiary", text = "To samo dla każdego z czterdziestu pomiarów daje wykres kwantylowy.")
  ),
  end = list(
    a = "Komora 1: punkty idą wzdłuż prostej, a odchylenia są małe i bez wzoru. Model normalny pasuje.",
    b = "Komora 2: punkty układają się w łuk. Lewy koniec leży nad prostą, bo łożysko świeżo po smarowaniu nie schodzi niżej, a prawy ucieka w górę przez spóźnione smarowania."
  )
)

z6p_15b_ui <- z6p_scene_ui("P15B", "pojęcie", "Jak powstaje wykres kwantylowy", "qq_budowa", z6rf_qq_cfg, proto = FALSE,
  z6p_out("head"),
  tags$div(class = "z6p-nav",
    z6p_seg("set", "Dane", c("Komora 1" = "a", "Komora 2" = "b")),
    lc_sf_slider("z6p15b_i", "Pomiar nr i", 1, z6rf_n, 36, 1)),
  z6p_out("stage"),
  z6p_out("body"),
  tags$div(class = "z6p-nav", z6p_prev_next()))
