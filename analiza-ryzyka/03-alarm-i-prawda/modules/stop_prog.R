# Punkty stop rozdziału „Próg reakcji”:
#   a3_stop_matrix_ui — pojęcie „Macierz konsekwencji” (statyczna tablica 2×2),
#   a3_stop_cost_ui   — piaskownica „Koszt reakcji a koszt zaniechania”.
# Serwer: a3_stop_prog_server(input, output, session).

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

a3_cost_q_max <- 0.5

# Wykres bez sesji Shiny: oczekiwany koszt reakcji (c) i braku reakcji (L · q).
a3_cost_plot <- function(cost_c = 100, loss_L = 2000, cases = a3_cost_cases) {
  q_star <- cost_c / loss_L
  y_top <- loss_L * a3_cost_q_max
  pts <- cases
  pts$act <- pts$q > q_star
  lines <- data.frame(
    q = c(0, a3_cost_q_max, 0, a3_cost_q_max),
    cost = c(cost_c, cost_c, 0, y_top),
    option = rep(c("reakcja", "brak"), each = 2)
  )
  y_band <- -0.13 * y_top  # pas pod osią na podpisy punktów (linie go nie przecinają)
  ggplot() +
    annotate("rect", xmin = q_star, xmax = a3_cost_q_max, ymin = 0, ymax = y_top,
             fill = upwr_accent, alpha = 0.06) +
    annotate("segment", x = q_star, xend = q_star, y = 0, yend = y_top,
             linetype = "dashed", colour = upwr_reference, linewidth = 0.8) +
    geom_line(data = lines, aes(q, cost, linetype = option), colour = upwr_secondary,
              linewidth = 1.4) +
    scale_linetype_manual(values = c(reakcja = "solid", brak = "22"), guide = "none") +
    geom_point(data = pts[pts$act, ], aes(q, 0), shape = 21, size = 5.5, stroke = 1.4,
               colour = upwr_accent, fill = upwr_accent) +
    geom_point(data = pts[!pts$act, ], aes(q, 0), shape = 21, size = 5.5, stroke = 1.4,
               colour = upwr_reference, fill = "white") +
    geom_text(data = pts, aes(q, y_band, label = label,
                              colour = ifelse(act, upwr_accent, upwr_reference)),
              size = 5.2, fontface = "bold") +
    scale_colour_identity() +
    scale_x_continuous(limits = c(0, a3_cost_q_max), breaks = seq(0, 0.5, 0.1),
                       expand = expansion(mult = c(0.03, 0.02))) +
    scale_y_continuous(limits = c(1.25 * y_band, y_top), breaks = function(l) {
                         b <- scales::extended_breaks()(c(0, y_top)); b[b >= 0]
                       }, expand = expansion(mult = c(0, 0.04)),
                       labels = function(x) format(x, big.mark = " ", scientific = FALSE,
                                                   trim = TRUE)) +
    coord_cartesian(clip = "off") +
    labs(x = "Posterior q = P(awaria | alarm)", y = "Oczekiwany koszt (zł)") +
    theme_upwr()
}

a3_stop_cost_ui <- risk_widget_panel("Decyzja", "Koszt reakcji a koszt zaniechania",
  tagList(
    lc_slider("a3_cost_c", "Koszt reakcji c (zł)", 20, 500, 100, 10),
    lc_slider("a3_cost_L", "Strata przy zignorowanej awarii L (zł)", 1000, 10000, 2000, 100)
  ),
  plot_id = "a3_cost", stats_id = "a3_cost_read", ratio = "1.9/1",
  note = "Linia ciągła: koszt reakcji c. Linia przerywana: oczekiwany koszt braku reakcji L · q. Punkty: sytuacje z przykładu 3.8."
)

a3_stop_prog_server <- function(input, output, session) {
  cost_c <- reactive(input$a3_cost_c %||% 100)
  loss_L <- reactive(input$a3_cost_L %||% 2000)

  zoom_plot_server("a3_cost", reactive(a3_cost_plot(cost_c(), loss_L())),
    alt = "Oczekiwany koszt reakcji (pozioma linia) i braku reakcji (rosnąca linia) względem posterioru; pionowa linia w progu q* = c/L, punkty trzech sytuacji z przykładu 3.8.")

  output$a3_cost_read <- renderUI({
    q_star <- cost_c() / loss_L()
    q_hall <- a3_cost_cases$q[a3_cost_cases$label == "hala 0.01"]
    tagList(
      lc_readout("Próg q* = c/L", risk_fmt_p(q_star), color = upwr_reference),
      lc_readout(sprintf("Hala 0.01 (q = %.3f)", q_hall),
        if (q_hall > q_star) "reagować" else "nie reagować",
        color = if (q_hall > q_star) upwr_accent else upwr_reference)
    )
  })
}
