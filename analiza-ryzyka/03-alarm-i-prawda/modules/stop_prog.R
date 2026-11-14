# Punkty stop rozdziału „Próg reakcji”:
#   a3_stop_matrix_ui — pojęcie „Macierz konsekwencji” (statyczna tablica 2×2),
#   a3_stop_cost_ui   — piaskownica „Koszt reakcji a koszt zaniechania”.
# Oba działają bez serwera (wykres kosztów: modules/detektor.js).

# ============================================================================
# MACIERZ KONSEKWENCJI (pojęcie)
# ============================================================================

# Układ z risk_confusion_matrix() (klasy .lc-cm*), komórki z kwotą zamiast skrótu.
# Strata przy zignorowanej awarii — akcent; koszt reakcji — złoto; zero — neutralnie.
a3_stop_matrix_css <- "
.a3-cmx { max-width: 760px; gap: 8px; }
.a3-cmx .lc-cm-head, .a3-cmx .lc-cm-side { font-size: calc(14px * var(--lc-font-scale)); }
.a3-cmx .lc-cm-cell { padding: 18px 20px; }
.a3-cmx-value {
  font-family: var(--upwr-mono);
  font-size: calc(30px * var(--lc-font-scale));
  font-weight: 700;
  line-height: 1.1;
  font-variant-numeric: tabular-nums;
}
.a3-cmx-loss { background: var(--upwr-accent-tint); border-color: var(--upwr-accent); border-left-color: var(--upwr-accent); }
.a3-cmx-loss .a3-cmx-value { color: var(--upwr-accent); }
.a3-cmx-act  { background: var(--upwr-single-alt-tint); border-color: var(--upwr-rule); border-left-color: var(--upwr-single-alt); }
.a3-cmx-act .a3-cmx-value { color: var(--upwr-ink); }
.a3-cmx-zero { background: var(--upwr-surface-sunken); border-color: var(--upwr-rule); border-left-color: var(--upwr-rule); }
.a3-cmx-zero .a3-cmx-value { color: var(--upwr-ink-subtle); }
"

a3_stop_matrix_cell <- function(kind, value, desc) {
  tags$div(
    class = paste("lc-cm-cell", paste0("a3-cmx-", kind)),
    tags$div(class = "a3-cmx-value", value),
    tags$div(class = "lc-cm-desc", desc)
  )
}

a3_stop_matrix_ui <- tagList(
  tags$style(HTML(a3_stop_matrix_css)),
  figure_panel(label = "Pojęcie", title = "Macierz konsekwencji",
    tags$div(
      class = "lc-cm a3-cmx",
      role = "table",
      `aria-label` = "Macierz konsekwencji: koszt każdej pary stan instalacji i decyzja dyżurnego",
      tags$div(class = "lc-cm-corner"),
      tags$div(class = "lc-cm-head", "Reagować"),
      tags$div(class = "lc-cm-head", "Nie reagować"),
      tags$div(class = "lc-cm-side", "Awaria (A)"),
      a3_stop_matrix_cell("act", "100 zł", "szkoda uniknięta"),
      a3_stop_matrix_cell("loss", "2000 zł", "strata"),
      tags$div(class = "lc-cm-side", "Brak awarii (Aᶜ)"),
      a3_stop_matrix_cell("act", "100 zł", "zbędny wyjazd"),
      a3_stop_matrix_cell("zero", "0 zł", "spokojna zmiana")
    )
  )
)

# ============================================================================
# KOSZT REAKCJI A KOSZT ZANIECHANIA (piaskownica)
# ============================================================================

# Trzy sytuacje z przykładu 3.8: czułość 0.95, FPR 0.05. Dwa warunkowo
# niezależne alarmy: czułość 0.95², FPR 0.05², czyli szanse 0.002/0.998 · 361.
a3_cost_cases <- data.frame(
  label = c("hala 0.002", "hala 0.01", "dwa alarmy"),
  q = c(
    risk_bayes(0.002, 0.95, 0.05),
    risk_bayes(0.01, 0.95, 0.05),
    risk_bayes(0.002, 0.95^2, 0.05^2)
  )
)

# Wykres: modules/detektor.js (rycina lcSF „a3_cost”): koszt reakcji c (linia
# ciągła), oczekiwany koszt braku reakcji L · q (przerywana), próg q* = c/L.
a3_stop_cost_ui <- figure_panel(label = "Decyzja", title = "Koszt reakcji a koszt zaniechania", width_mode = "wide",
  lc_sf_figure("a3_cost",
    control = list(
      lc_sf_slider("a3_cost_c", "Koszt reakcji c (zł)", 20, 500, 100, 10),
      lc_sf_slider("a3_cost_L", "Strata przy zignorowanej awarii L (zł)", 1000, 10000, 2000, 100)),
    toolbar = lc_readouts(
      lc_readout("Próg q* = c/L", lc_sf_value("qs"), color = upwr_reference),
      lc_readout(sprintf("Hala 0.01 (q = %.3f)", a3_cost_cases$q[a3_cost_cases$label == "hala 0.01"]),
        lc_sf_value("hall"), color = upwr_accent)),
    ratio = "1.9/1", max_height = "400px",
    config = list(cases = a3_cost_cases),
    after = lc_caption("Linia ciągła: koszt reakcji c. Linia przerywana: oczekiwany koszt braku reakcji L · q. Punkty: sytuacje z przykładu 3.8."))
)
