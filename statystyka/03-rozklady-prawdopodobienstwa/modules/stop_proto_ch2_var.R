# RYC. 2.5 „PASY WYPŁAT” (prototyp 2.4-B, zaakceptowany 2026-10-29) -----------
# Trzy loterie o tym samym E(X) = 50 zł i różnym rozrzucie; C dyskretna (kostka).
#   s3p_24b_ui — Ryc. 2.5 „Pasy wypłat” (zaakceptowany 2026-10-29; zastąpił histogramy loterii)
#   Usunięte warianty: 2.4-A „Portfele graczy”, 2.4-C „Model bez symulacji”.
# Serwer: s3p_ch2_var_server(input, output, session).

s3p_24_lots <- list(
  A = list(name = "A: pewne 50 zł", x = 50, p = 1),
  B = list(name = "B: 0 albo 100 zł", x = c(0, 100), p = c(0.5, 0.5)),
  C = list(name = "C: kostka × 20 zł", x = seq(0, 100, 20), p = rep(1/6, 6))
)
s3p_24_sd <- vapply(s3p_24_lots, function(l) sqrt(sum((l$x - 50)^2 * l$p)), numeric(1))
s3p_24_cols <- c(A = upwr_cat[["grafit"]], B = upwr_cat[["terakota"]], C = upwr_cat[["indygo"]])
s3p_24_draw <- function(key, n) {
  l <- s3p_24_lots[[key]]
  if (length(l$x) == 1) rep(l$x, n) else sample(l$x, n, replace = TRUE, prob = l$p)
}

# ============================================================================
# (2.4-B) PASY WYPŁAT
# ============================================================================

s3p_24b_ui <- s3p_panel("2.4-B", "Ryc. 2.5", "Pasy wypłat", proto = FALSE,
  lc_toolbar(
    lc_action_group(label = "Zagraj w każdą loterię", s3p_24b_g1 = "+1", s3p_24b_g10 = "+10", s3p_24b_g100 = "+100"),
    lc_action("s3p_24b_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
    tags$div(class = "lc-checks", checkboxInput("s3p_24b_sd", "Klamry ±SD", value = FALSE)),
    lc_readouts(uiOutput("s3p_24b_read"))
  ),
  lc_plot("s3p_24b_plot", ratio = "2.4/1"),
  uiOutput("s3p_24b_caption")
)

# ============================================================================
# SERWER
# ============================================================================

s3p_ch2_var_server <- function(input, output, session) {

  # --- 2.4-B ----------------------------------------------------------------
  pays <- reactiveVal(list(A = numeric(0), B = numeric(0), C = numeric(0)))
  play_b <- function(k) { p <- pays(); pays(setNames(lapply(names(p), function(key) c(p[[key]], s3p_24_draw(key, k))), names(p))) }
  observeEvent(input$s3p_24b_g1, play_b(1))
  observeEvent(input$s3p_24b_g10, play_b(10))
  observeEvent(input$s3p_24b_g100, play_b(100))
  observeEvent(input$s3p_24b_reset, pays(list(A = numeric(0), B = numeric(0), C = numeric(0))))

  output$s3p_24b_read <- renderUI({
    p <- pays(); n <- length(p$A)
    tagList(lc_readout("Gier", n),
            lc_readout("Średnie A / B / C", if (n) paste(lc_fmt(vapply(p, mean, 0), 0), collapse = " / ") else "–",
                       color = upwr_accent))
  })

  zoom_plot_server("s3p_24b_plot", reactive({
    p <- pays(); n <- length(p$A); rows <- c(A = 3, B = 2, C = 1)
    g <- ggplot() +
      geom_vline(xintercept = 50, colour = upwr_accent, linetype = "22", linewidth = 0.9) +
      annotate("text", x = 50, y = 3.75, label = "E = 50 zł", colour = upwr_accent, fontface = "bold", vjust = 0) +
      scale_y_continuous(breaks = rows, labels = vapply(s3p_24_lots, `[[`, "", "name"), limits = c(0.5, 3.85)) +
      scale_x_continuous(limits = c(-5, 105), breaks = seq(0, 100, 20)) +
      labs(x = "Wypłata w jednej grze (zł)", y = NULL) + theme_upwr(grid = "x")
    if (n > 0) {
      set.seed(n)
      d <- do.call(rbind, lapply(names(p), function(key)
        data.frame(key = key, x = p[[key]] + runif(n, -2.5, 2.5), y = rows[[key]] + runif(n, -0.22, 0.22))))
      g <- g + geom_point(data = d, aes(x, y, colour = key), alpha = if (n > 50) 0.35 else 0.8, size = 2.2) +
        scale_colour_manual(values = s3p_24_cols, guide = "none")
    }
    if (isTRUE(input$s3p_24b_sd)) {
      sdv <- if (n > 1) vapply(p, function(v) sqrt(mean((v - mean(v))^2)), 0) else s3p_24_sd
      b <- data.frame(y = rows - 0.33, lo = 50 - sdv, hi = 50 + sdv, lab = sprintf("SD ≈ %s zł", lc_fmt(sdv, 0)))
      g <- g + geom_errorbar(data = b, aes(xmin = lo, xmax = hi, y = y), width = 0.08, colour = upwr_ink, linewidth = 0.9) +
        geom_label(data = b, aes(x = 50, y = y, label = lab), size = 3.6, fontface = "bold", fill = "white",
                   linewidth = 0, label.padding = unit(0.12, "lines"))
    }
    g
  }))

  output$s3p_24b_caption <- renderUI({
    lc_caption(if (length(pays()$A) == 0) "Każde kliknięcie to jedna gra w każdą z trzech loterii. Kropka to wypłata."
      else "Chmury mają ten sam środek, 50 zł, ale różną szerokość. Włącz klamry: SD mierzy, jak daleko od E(X) typowo ląduje pojedyncza wypłata.")
  })
}
