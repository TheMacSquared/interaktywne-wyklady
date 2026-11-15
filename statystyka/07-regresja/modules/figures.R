# ============================================================================
# Ryciny wykładu 07: opisy liczone w R przy budowaniu strony
# ============================================================================
# Ryciny na stałych danych i o skończonej liczbie ustawień opisuje R i rysuje
# wbudowana rycina lc_spec (lc_sf_spec() w shared/lc_stat_figures.R). To, co zależy
# od losowania, suwaków i kliknięć, liczy modules/figures.js.
# Wymaga: helpers.R, lc_stat_figures.R.

# Rycina bez wykresu (same odczyty albo tekst): kontener lcSF z atrybutem data-lc-nochart.
s07_reads_figure <- function(fig, ..., config = NULL) {
  tags$div(class = "lc-sf", `data-fig` = fig, `data-lc-nochart` = NA,
    `data-config` = if (!is.null(config)) as.character(jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA)),
    ...)
}
s07_html <- function(...) as.character(tagList(...))

# ---------------------------------------------------------------------------
# Rozdział 1
# ---------------------------------------------------------------------------

# Ryc. 1.1: anatomia prostej — wyraz wolny przy X = 0, trójkąt nachylenia o podstawie 1
# i pionowy odcinek błędu jednego punktu.
ch1_anatomy_fig <- function() {
  b0 <- 1.5; b1 <- 0.8
  pts <- data.frame(x = c(0.7, 1.4, 2.2, 2.9, 3.6, 4.4, 5.1, 5.7), e = c(0.5, -0.6, 0.3, 1.3, -0.4, -0.7, 0.6, 0.2))
  pts$y <- b0 + b1 * pts$x + pts$e
  hi <- pts[4, ]
  tx <- 4.5  # początek trójkąta nachylenia
  lc_sf_spec(sf_spec(c(-0.3, 6.2), c(0, 7.2), list(
      sf_vline(0, color = "@ref", width = 0.8, dash = NULL),
      sf_line(c(-0.3, 6.2), b0 + b1 * c(-0.3, 6.2), color = "@secondary", width = 2),
      sf_points(pts$x[-4], pts$y[-4], color = "@niebo", size = 9, alpha = 0.8),
      # ε: pionowy odcinek od prostej do punktu
      sf_vline(hi$x, color = "@bursztyn", width = 2.2, dash = NULL, from = b0 + b1 * hi$x, to = hi$y),
      sf_points(hi$x, hi$y, color = "@bursztyn", size = 11),
      sf_text(hi$x - 0.12, b0 + b1 * hi$x + hi$e / 2, "ε", color = "@bursztyn", align = "right", valign = "middle", size = 1.6),
      # β₁: krok o 1 w poziomie i β₁ w pionie
      sf_line(c(tx, tx + 1), rep(b0 + b1 * tx, 2), color = "@indygo", width = 1.8),
      sf_vline(tx + 1, color = "@indygo", width = 2.2, dash = NULL, from = b0 + b1 * tx, to = b0 + b1 * (tx + 1)),
      sf_text(tx + 0.5, b0 + b1 * tx - 0.3, "1", color = "@indygo", mono = TRUE, valign = "middle", size = 1.3),
      sf_text(tx + 1.12, b0 + b1 * (tx + 0.5), "β₁", color = "@indygo", align = "left", valign = "middle", size = 1.6),
      # β₀: wysokość prostej przy X = 0
      sf_vline(0, color = "@terakota", width = 2.6, dash = NULL, from = 0, to = b0),
      sf_points(0, b0, color = "@terakota", size = 12),
      sf_text(0.15, b0 / 2, "β₀", color = "@terakota", align = "left", valign = "middle", size = 1.6)),
    "X", "Y", aria = "Prosta regresji z zaznaczonym wyrazem wolnym przy X równym zero, trójkątem nachylenia o podstawie 1 i pionowym odcinkiem błędu jednego punktu.",
    xTicks = list(0), hideY = TRUE, noYGrid = TRUE, noXGrid = TRUE),
    ratio = "1.6/1", max_height = "340px")
}

# Ryc. 1.2: te same losowania co dawniej na serwerze (set.seed(101); rnorm(80) razy σ).
ch1_beta_config <- local({ set.seed(101); list(z = round(rnorm(80), 5)) })
