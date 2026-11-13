# RYCINY — rozdział 5 -----------------------------------------------------------
#   fig_5a_ui — Ryc. 5.1 scena „Pracownia dokupuje ankiety” (otwiera rozdział).
#               Puenta: każde zmniejszenie marginesu o połowę kosztuje cztery razy więcej ankiet.
#   fig_D1_ui — Ryc. 5.5 domknięcie „Anatomia przedziału” (przed ostatnim akapitem rozdziału).
#               Puenta: cały wykład w jednym przedziale; przecina próg, więc nie rozstrzyga.
# Obie ryciny liczy i rysuje przeglądarka (sf_ch5.js: c5-poll, c5-anatomy); serwer jest pusty.

# ============================================================================
# (5a) Ryc. 5.1 „PRACOWNIA DOKUPUJE ANKIETY”
# ============================================================================
# Górny wykres: bieżący przedział 50% ± ME (gruby) i poprzednie jako ślady (max 12).
# Dolny wykres: ścieżka kolejnych zakupów (koszt w tys. zł, ME w pkt proc.).
# ME = 1.96 · √(0.25 / n); start 100 ankiet po 20 zł.

fig_5a_ui <- fig_panel("5a", "Ryc. 5.1", "Pracownia dokupuje ankiety", width_mode = "wide",
  lc_sf_figure("c5-poll",
    toolbar = tagList(
      lc_sf_action("dokup", "fig_5a_dokup", "Dokup 100 ankiet", variant = "solid"),
      lc_sf_action("double", "fig_5a_double", "Podwój próbę", variant = "outline"),
      lc_sf_action("reset", "fig_5a_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
      lc_readouts(
        lc_readout("Ankiet", lc_sf_value("poll_n", "100")),
        lc_readout("Margines błędu", lc_sf_value("poll_me", "± 9.8 pkt proc.")),
        lc_readout("Koszt", lc_sf_value("poll_cost", "2 000 zł")))),
    body = tagList(
      lc_sf_chart(0, ratio = "3.2/1", max_height = "340px"),
      lc_sf_chart(1, ratio = "2.6/1", max_height = "340px")),
    after = lc_caption("Połowa marginesu kosztuje cztery razy więcej ankiet."))
)

# ============================================================================
# (D1) DOMKNIĘCIE „ANATOMIA PRZEDZIAŁU” — kroki lc_step_widget
# ============================================================================
# Kroki dokładają warstwy: 1 estymata, 2 ±1 SE, 3 95%, 4 warianty, 5 próg 26.

fig_D1_texts <- c(
  "Rozdz. 1: średnia z 40 pomiarów wynosi x̄ = 28.5 min. To najlepsza pojedyncza wartość, ale nie mówi, jak bardzo może się mylić.",
  "Rozdz. 1: pasek to ±1 SE = 8/√40 = 1.26 min. Tyle średnia typowo odchodzi od prawdziwej wartości.",
  "Rozdz. 2–3: x̄ ± t*·SE, z t* = 2.023 dla 95% i 39 stopni swobody. Przedział to 25.9–31.1 min.",
  "Rozdz. 5: większa próba (n × 4), mniejsze s (s / 2) i niższa ufność (90%) zawężają przedział. Dwa pierwsze działają przez SE, trzeci przez t*.",
  "Rozdz. 5: próg 26 min leży wewnątrz przedziału, więc dane tego nie rozstrzygają. Wykład 05 wróci do takich pytań."
)

fig_D1_ui <- fig_panel("D1", "Ryc. 5.5", NULL, width_mode = "wide",
  lc_sf_figure("c5-anatomy", bare = TRUE, config = list(texts = fig_D1_texts),
    body = lc_step_widget("fig_D1_step",
      title = "Anatomia przedziału",
      steps = c("Estymata", "Błąd standardowy", "Przedział", "Co go zwęża", "Odczyt"),
      toolbar = lc_toolbar(lc_chart_zoom_button()),
      body = lc_sf_chart(0, ratio = "2.6/1", max_height = "400px")))
)
