# RYCINY — rozdział 5 -----------------------------------------------------------
#   fig_5a_ui — Ryc. 5.1 scena „Pracownia dokupuje ankiety” (otwiera rozdział).
#               Puenta: każde zmniejszenie marginesu o połowę kosztuje cztery razy więcej ankiet.
#   fig_D1_ui — Ryc. 5.5 domknięcie „Anatomia przedziału” (przed ostatnim akapitem rozdziału).
#               Puenta: cały wykład w jednym przedziale; przecina próg, więc nie rozstrzyga.
# Serwer: fig_ch5_server(input, output, session).

# Pomocnik: margines błędu dla proporcji przy p(1 − p) = 0.25 (z* = 1.96).
fig_5a_me <- function(n) 1.96 * sqrt(0.25 / n)

# ============================================================================
# (5a) Ryc. 5.1 „PRACOWNIA DOKUPUJE ANKIETY”
# ============================================================================

# Górny wykres: bieżący przedział 50% ± ME (gruby) i poprzednie jako ślady (max 12).
fig_5a_ci_fn <- function(ns) {
  n_now <- tail(ns, 1)
  hist_n <- head(rev(head(ns, -1)), 12)
  d_now <- data.frame(y = 0, me = fig_5a_me(n_now))
  d_hist <- data.frame(y = -seq_along(hist_n), me = fig_5a_me(hist_n))
  ggplot() +
    geom_vline(xintercept = 0.5, linetype = "dotted", colour = upwr_reference) +
    { if (nrow(d_hist)) geom_segment(data = d_hist,
        aes(x = 0.5 - me, xend = 0.5 + me, y = y, yend = y),
        colour = col_ci, alpha = 0.22, linewidth = 1.6) } +
    geom_segment(data = d_now, aes(x = 0.5 - me, xend = 0.5 + me, y = y, yend = y),
                 colour = col_ci, linewidth = 3.4) +
    geom_point(data = d_now, aes(x = 0.5, y = y), colour = col_estimate, size = 3) +
    coord_cartesian(xlim = c(0.3, 0.7), ylim = c(-length(hist_n) - 0.7, 0.7)) +
    scale_x_continuous(labels = function(v) paste0(lc_fmt(100 * v, 0), "%")) +
    labs(x = "Poparcie (%)", y = NULL) +
    theme_upwr(grid = "x") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

# Dolny wykres: ścieżka kolejnych zakupów (koszt w tys. zł, ME w pkt proc.).
fig_5a_path_fn <- function(ns) {
  d <- data.frame(cost = 20 * ns / 1000, me = 100 * fig_5a_me(ns))
  last <- d[nrow(d), , drop = FALSE]
  xmax <- max(5, max(d$cost) * 1.08)
  ggplot(d, aes(cost, me)) +
    geom_hline(yintercept = 3, linetype = "dotted", colour = upwr_reference) +
    annotate("text", x = 0, y = 3, label = "±3 pkt: typowy sondaż", hjust = 0,
             vjust = -0.5, size = 3.6, colour = upwr_reference) +
    { if (nrow(d) > 1) geom_path(colour = col_ci, linewidth = 1) } +
    geom_point(colour = col_ci, size = 2.2) +
    geom_point(data = last, colour = upwr_accent, size = 4) +
    coord_cartesian(xlim = c(0, xmax), ylim = c(0, 11)) +
    labs(x = "Koszt badania (tys. zł)", y = "Margines błędu (pkt proc.)") +
    theme_upwr(grid = "y")
}

fig_5a_ui <- fig_panel("5a", "Ryc. 5.1", "Pracownia dokupuje ankiety", width_mode = "wide",
  lc_toolbar(
    lc_action("fig_5a_dokup", "Dokup 100 ankiet", variant = "solid"),
    lc_action("fig_5a_double", "Podwój próbę", variant = "outline"),
    lc_action("fig_5a_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
    lc_readouts(uiOutput("fig_5a_read"))
  ),
  lc_ec_plot_ui("fig_5a_ci", ratio = "3.2/1"),
  lc_ec_plot_ui("fig_5a_path", ratio = "2.6/1"),
  uiOutput("fig_5a_caption")
)

# ============================================================================
# (D1) DOMKNIĘCIE „ANATOMIA PRZEDZIAŁU” — kroki lc_step_widget
# ============================================================================

# Kroki dokładają warstwy: 1 estymata, 2 ±1 SE, 3 95%, 4 warianty, 5 próg 26.
fig_D1_plot_fn <- function(k) {
  xbar <- 28.5; s <- 8; n <- 40
  se <- s / sqrt(n)
  ci_w <- function(se_v, df, p) xbar + c(-1, 1) * qt(p, df) * se_v
  main <- ci_w(se, n - 1, 0.975)
  variants <- list(
    list(y = -1, lab = "n × 4", r = ci_w(s / sqrt(4 * n), 4 * n - 1, 0.975)),
    list(y = -2, lab = "s / 2", r = ci_w((s / 2) / sqrt(n), n - 1, 0.975)),
    list(y = -3, lab = "90%", r = ci_w(se, n - 1, 0.95))
  )
  p <- ggplot() + theme_upwr(grid = "x") +
    coord_cartesian(xlim = c(21.5, 35), ylim = c(-3.7, 1)) +
    scale_x_continuous(breaks = seq(22, 34, by = 2)) +
    labs(x = "Czas dojazdu (min)", y = NULL) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  if (k >= 5) p <- p + annotate("rect", xmin = 26, xmax = 35, ymin = -3.7, ymax = 1,
                                fill = upwr_accent, alpha = 0.07)
  if (k >= 4) for (v in variants) {
    p <- p +
      annotate("segment", x = v$r[1], xend = v$r[2], y = v$y, yend = v$y,
               colour = col_ci, alpha = 0.6, linewidth = 1.2) +
      annotate("text", x = 21.7, y = v$y, label = v$lab, hjust = 0, size = 3.6,
               colour = upwr_ink_soft)
  }
  if (k >= 2) p <- p + annotate("segment", x = xbar - se, xend = xbar + se, y = 0, yend = 0,
                                colour = col_ci, alpha = 0.45, linewidth = 7)
  if (k >= 3) p <- p +
    annotate("segment", x = main[1], xend = main[2], y = 0, yend = 0,
             colour = col_ci, linewidth = 1.6) +
    annotate("segment", x = main[1], xend = main[1], y = -0.18, yend = 0.18,
             colour = col_ci, linewidth = 1.6) +
    annotate("segment", x = main[2], xend = main[2], y = -0.18, yend = 0.18,
             colour = col_ci, linewidth = 1.6) +
    annotate("text", x = 21.7, y = 0, label = "95%", hjust = 0, size = 3.6,
             colour = upwr_ink_soft)
  if (k >= 1) p <- p + annotate("point", x = xbar, y = 0, shape = 18, size = 6,
                                colour = col_estimate)
  if (k >= 5) p <- p +
    geom_vline(xintercept = 26, linetype = "22", colour = upwr_secondary) +
    annotate("text", x = 26, y = 0.75, label = "próg 26 min", hjust = -0.08, size = 3.6,
             colour = upwr_secondary)
  p
}

fig_D1_ui <- fig_panel("D1", "Ryc. 5.5", NULL, width_mode = "wide",
  lc_step_widget("fig_D1_step",
    title = "Anatomia przedziału",
    steps = c("Estymata", "Błąd standardowy", "Przedział", "Co go zwęża", "Odczyt"),
    toolbar = lc_toolbar(lc_chart_zoom_button()),
    body = lc_ec_plot_ui("fig_D1_plot", ratio = "2.6/1", zoom = FALSE)
  )
)

# ============================================================================
# SERWER
# ============================================================================

fig_ch5_server <- function(input, output, session) {

  # --- 5a: ścieżka ankiet (wszystkie n od resetu; ostatnie = bieżące) ---
  fig_5a_ns <- reactiveVal(100)
  observeEvent(input$fig_5a_dokup, fig_5a_ns(c(fig_5a_ns(), tail(fig_5a_ns(), 1) + 100)))
  observeEvent(input$fig_5a_double, fig_5a_ns(c(fig_5a_ns(), 2 * tail(fig_5a_ns(), 1))))
  observeEvent(input$fig_5a_reset, fig_5a_ns(100))

  output$fig_5a_read <- renderUI({
    n <- tail(fig_5a_ns(), 1)
    tagList(
      lc_readout("Ankiet", lc_fmt(n, 0, " ")),
      lc_readout("Margines błędu", paste0("± ", lc_fmt(100 * fig_5a_me(n), 1), " pkt proc.")),
      lc_readout("Koszt", paste(lc_fmt(20 * n, 0, " "), "zł"))
    )
  })

  output$fig_5a_caption <- renderUI({
    lc_caption("Połowa marginesu kosztuje cztery razy więcej ankiet.")
  })

  lc_ec_plot_server("fig_5a_ci", reactive(fig_5a_ci_fn(fig_5a_ns())),
    alt = "Przedziały 50% ± margines błędu dla sondażu, najnowszy na górze, poprzednie blade")
  lc_ec_plot_server("fig_5a_path", reactive(fig_5a_path_fn(fig_5a_ns())),
    alt = "Margines błędu w pkt proc. w zależności od kosztu badania w tys. zł")

  # --- D1: domknięcie krokowe ---
  step <- lc_step_server("fig_D1_step", input)$step
  fig_step_text(output, "fig_D1_step", step, list(
    "Rozdz. 1: średnia z 40 pomiarów wynosi x̄ = 28.5 min. To najlepsza pojedyncza wartość, ale nie mówi, jak bardzo może się mylić.",
    "Rozdz. 1: pasek to ±1 SE = 8/√40 = 1.26 min. Tyle średnia typowo odchodzi od prawdziwej wartości.",
    "Rozdz. 2–3: x̄ ± t*·SE, z t* = 2.023 dla 95% i 39 stopni swobody. Przedział to 25.9–31.1 min.",
    "Rozdz. 5: większa próba (n × 4), mniejsze s (s / 2) i niższa ufność (90%) zawężają przedział. Dwa pierwsze działają przez SE, trzeci przez t*.",
    "Rozdz. 5: próg 26 min leży wewnątrz przedziału, więc dane tego nie rozstrzygają. Wykład 05 wróci do takich pytań."
  ))
  lc_ec_plot_server("fig_D1_plot", reactive(fig_D1_plot_fn(step())),
    alt = "Przedział ufności czasu dojazdu 28.5 min z progiem 26 min w środku")
}
