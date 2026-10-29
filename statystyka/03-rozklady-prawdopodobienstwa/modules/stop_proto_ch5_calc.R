# RYC. 5.4 KALKULATOR Z-SCORE (prototyp 5.4-A, zaakceptowany 2026-10-29) ------------
# Chart.js (zscore_chart.js, data-mode = "two"): pola μ, σ, x w jednej linii, dwa
# wyrównane wykresy (skala x i skala z), jedna linia wzoru z = (x − μ)/σ. Bez serwera.
# Usunięte warianty: 5.4-B „Jedna krzywa, dwie osie”, 5.4-C „Odchudzony ggplot”.

# Pole liczbowe bez Shiny, czytane przez zscore_chart.js.
s3p_54_field <- function(label, role, value) {
  lc_group(label, tags$input(type = "number", class = "form-control", `data-role` = role,
                             value = value, step = "any", style = "width: 7em;"))
}

s3p_54_js <- function(num, title, mode, canvases) {
  s3p_panel(num, "Ryc. 5.4", title, proto = FALSE,
    tags$div(class = "lc-zcalc", `data-mode` = mode,
      lc_toolbar(
        s3p_54_field("μ", "mu", 65), s3p_54_field("σ", "sd", 10), s3p_54_field("x", "x", 80),
        lc_readouts(lc_readout("z", tags$span(`data-role` = "zval", "1.50"), color = "var(--upwr-accent)"))
      ),
      canvases,
      tags$div(class = "s3p-54-calc", tags$span(`data-role` = "calc", "z = (80 − 65) / 10 = 1.50"))
    )
  )
}

s3p_54a_ui <- s3p_54_js("5.4-A", "Kalkulator z-score", "two", tagList(
  tags$div(class = "s3p-54-canvas", tags$canvas(`data-role` = "x")),
  tags$div(class = "s3p-54-canvas", tags$canvas(`data-role` = "z"))))

s3p_54_css <- tags$style(HTML("
  .s3p-54-canvas { position: relative; aspect-ratio: 3.2 / 1; margin-top: .4em; }
  .s3p-54-canvas.is-dual { aspect-ratio: 2.6 / 1; }
  .s3p-54-calc { font-family: var(--upwr-mono); font-size: 1.35em; font-weight: 700; text-align: center;
    color: var(--upwr-ink); margin: .6em 0 .2em; }
"))
