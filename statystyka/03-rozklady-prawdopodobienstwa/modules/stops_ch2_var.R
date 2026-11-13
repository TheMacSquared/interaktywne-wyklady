# PUNKT STOP — rozdział 2, wariancja --------------------------------------------
#   s3p_24b_ui — Ryc. 2.5 „Pasy wypłat”: trzy loterie o tym samym E(X) = 50 zł
#   i różnym rozrzucie; C dyskretna (kostka).
# Serwer: s3p_ch2_var_server(input, output, session).

s3p_24_lots <- list(
  A = list(name = "A: pewne 50 zł", x = 50, p = 1),
  B = list(name = "B: 0 albo 100 zł", x = c(0, 100), p = c(0.5, 0.5)),
  C = list(name = "C: kostka × 20 zł", x = seq(0, 100, 20), p = rep(1/6, 6))
)
s3p_24_sd <- vapply(s3p_24_lots, function(l) sqrt(sum((l$x - 50)^2 * l$p)), numeric(1))

# ============================================================================
# (2.4-B) PASY WYPŁAT
# ============================================================================

# Gry, chmury punktów, klamry ±SD i odczyty liczy przeglądarka (stops12_charts.js, d03-pays).
s3p_24b_ui <- s3p_panel("Ryc. 2.5", "Pasy wypłat",
  lc_sf_figure("d03-pays", ratio = "2.4/1", max_height = "440px",
    config = list(
      lots = lapply(s3p_24_lots, function(l) list(name = l$name, x = I(l$x), p = I(l$p))),
      sd = as.list(s3p_24_sd),
      captions = list(
        start = "Każde kliknięcie to jedna gra w każdą z trzech loterii. Kropka to wypłata.",
        played = "Chmury mają ten sam środek, 50 zł, ale różną szerokość. Włącz klamry: SD mierzy, jak daleko od E(X) typowo ląduje pojedyncza wypłata.")),
    toolbar = tagList(
      lc_sf_action_group("Zagraj w każdą loterię", g1 = "+1", g10 = "+10", g100 = "+100"),
      lc_sf_action("reset", "s3p_24b_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
      lc_sf_ctl(checkboxInput("s3p_24b_sd", "Klamry ±SD", value = FALSE)),
      lc_readouts(
        lc_readout("Gier", lc_sf_value("n", "0")),
        lc_readout("Średnie A / B / C", lc_sf_value("means"), color = upwr_accent))),
    after = lc_caption(lc_sf_value("cap", "Każde kliknięcie to jedna gra w każdą z trzech loterii. Kropka to wypłata.")))
)

# ============================================================================
# SERWER
# ============================================================================

# Ryc. 2.5 liczy przeglądarka (stops12_charts.js); funkcję wywołuje app.R.
s3p_ch2_var_server <- function(input, output, session) {
  # Ryc. 2.5 liczy przeglądarka (stops12_charts.js); app.R wywołuje tę funkcję.
}
