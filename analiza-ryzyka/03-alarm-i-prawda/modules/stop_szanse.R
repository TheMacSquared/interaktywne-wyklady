# Punkt stop rozdziału „Częstość bazowa”: wykres podwójnych alarmów
# do piaskownicy „Dwa alarmy”.
# Wymaga: palette.R, theme_upwr.R, shared.R, lecture_layout.R, risk_math.R.

# ============================================================================
# DWA ALARMY: PODWÓJNE ALARMY NA 10 000 ZMIAN
# ============================================================================

# Model jak w risk_two_alarm_posterior(): P(oba | stan) = d·p + (1 − d)·p².
# Wykres i odczyty liczy modules/detektor.js (rycina lcSF „a3_double”); parametry
# detektora czyta z suwaków tablicy 2×2, kopiowanie z własnego suwaka.
# Para słupków w każdej grupie: przerywany kontur — czujniki niezależne (d = 0),
# pełny słupek — bieżące kopiowanie. Oś y stała dla danych parametrów detektora.
a3_stop_double_plot_ui <- lc_sf_figure("a3_double",
  control = lc_sf_slider("a3_dependence", "Prawdopodobieństwo skopiowania pierwszego alarmu", 0, 1, 0, 0.05),
  toolbar = lc_readouts(
    lc_readout("Po jednym alarmie", lc_sf_value("p1"), color = upwr_secondary),
    lc_readout("Po dwóch alarmach", lc_sf_value("p2"), color = upwr_accent)),
  ratio = "1.8/1", max_height = "380px",
  after = lc_caption("Przerywany kontur: czujniki niezależne. Pełny słupek: bieżące kopiowanie."))
