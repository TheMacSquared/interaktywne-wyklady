# PROTOTYPY PUNKTÓW STOP — rozdział 5 (2026-10-30) ---------------------------
#   s4p_5a_ui — Scena „Pracownia dokupuje ankiety” (po akapicie o sondażach na ~1000 osób)
#               Puenta: każde zmniejszenie marginesu o połowę kosztuje cztery razy więcej ankiet.
#   s4p_5b_ui — Galeria „Schody precyzji” (zaraz po 5a)
#               Puenta: 4× więcej ankiet to połowa marginesu, za 4× wyższą cenę.
#   s4p_5c_ui — Symulacja „Poziom dobrany po fakcie” (przed Zasadą)
#               Puenta: kto dobiera poziom ufności po obejrzeniu wyniku, ogłasza „przekracza”
#               kilka razy częściej, niż obiecuje, nawet gdy prawda leży na progu.
#   s4p_D1_ui — Domknięcie „Anatomia przedziału” (przed ostatnim akapitem rozdziału)
#               Puenta: cały wykład w jednym przedziale; przecina próg, więc nie rozstrzyga.
# Serwer: s4p_ch5_server(input, output, session).

# Pomocnik: margines błędu dla proporcji przy p(1 − p) = 0.25 (z* = 1.96).
s4p_5a_me <- function(n) 1.96 * sqrt(0.25 / n)

# ============================================================================
# (5a) SCENA „PRACOWNIA DOKUPUJE ANKIETY”
# ============================================================================

# Górny wykres: bieżący przedział 50% ± ME (gruby) i poprzednie jako ślady (max 12).
s4p_5a_ci_fn <- function(ns) {
  n_now <- tail(ns, 1)
  hist_n <- head(rev(head(ns, -1)), 12)
  d_now <- data.frame(y = 0, me = s4p_5a_me(n_now))
  d_hist <- data.frame(y = -seq_along(hist_n), me = s4p_5a_me(hist_n))
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
s4p_5a_path_fn <- function(ns) {
  d <- data.frame(cost = 20 * ns / 1000, me = 100 * s4p_5a_me(ns))
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

s4p_5a_ui <- s4p_panel("5a", "Scena", "Pracownia dokupuje ankiety", width_mode = "wide",
  lc_toolbar(
    lc_action("s4p_5a_dokup", "Dokup 100 ankiet", variant = "solid"),
    lc_action("s4p_5a_double", "Podwój próbę", variant = "outline"),
    lc_action("s4p_5a_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
    lc_readouts(uiOutput("s4p_5a_read"))
  ),
  lc_plot("s4p_5a_ci", ratio = "3.2/1"),
  lc_plot("s4p_5a_path", ratio = "2.6/1"),
  uiOutput("s4p_5a_caption")
)

# ============================================================================
# (5b) GALERIA „SCHODY PRECYZJI”
# ============================================================================

s4p_5b_fn <- function() {
  p <- 0.54
  d <- data.frame(n = c(100, 400, 1600, 6400))
  d$me <- 1.96 * sqrt(p * (1 - p) / d$n)
  d$lo <- p - d$me
  d$hi <- p + d$me
  d$row <- factor(paste0("n = ", d$n), levels = rev(paste0("n = ", d$n)))
  d$label <- paste0("± ", lc_fmt(100 * d$me, 1), " pkt · ", lc_fmt(20 * d$n, 0, " "), " zł")
  ggplot(d, aes(y = row)) +
    geom_vline(xintercept = 0.5, linetype = "22", colour = upwr_secondary) +
    geom_segment(aes(x = lo, xend = hi, y = row, yend = row), colour = col_ci, linewidth = 2.2) +
    geom_point(aes(x = p, y = row), colour = col_estimate, size = 3.6) +
    geom_text(aes(x = 0.8, label = label), hjust = 1, vjust = -1.1, family = lc_mono_family,
              size = 3.2, colour = upwr_ink) +
    scale_x_continuous(limits = c(0.3, 0.8), breaks = seq(0.3, 0.8, by = 0.1),
                       labels = function(v) paste0(lc_fmt(100 * v, 0), "%")) +
    labs(x = "Poparcie (%)", y = NULL) +
    theme_upwr(grid = "x")
}

s4p_5b_ui <- s4p_panel("5b", "Galeria", "Schody precyzji", width_mode = "text",
  lc_plot("s4p_5b_plot", ratio = "2/1")
)

# ============================================================================
# (5c) SYMULACJA „POZIOM DOBRANY PO FAKCIE”
# ============================================================================

# Świat: średni dojazd dokładnie 26 min, σ = 8; badanie: n = 40.
s4p_5c_levels <- c(0.99, 0.95, 0.90, 0.80)
s4p_5c_lab <- c("99%", "95%", "90%", "80%")
s4p_5c_tq <- qt((1 + s4p_5c_levels) / 2, df = 39)

# k badań naraz. Zwraca liczby „przekracza” (uczciwy, elastyczny) i ostatnie badanie.
s4p_5c_run <- function(k) {
  x <- matrix(rnorm(k * 40, mean = 26, sd = 8), nrow = k)
  xbar <- rowMeans(x)
  s <- sqrt(rowSums((x - xbar)^2) / 39)
  se <- s / sqrt(40)
  lo <- outer(xbar, rep(1, 4)) - outer(se, s4p_5c_tq)
  hi <- outer(xbar, rep(1, 4)) + outer(se, s4p_5c_tq)
  list(
    honest = sum(lo[, 2] > 26),
    flex = sum(rowSums(lo > 26) > 0),
    last = list(xbar = xbar[k], lo = lo[k, ], hi = hi[k, ])
  )
}

s4p_5c_last_fn <- function(st) {
  if (is.null(st$last)) {
    return(ggplot() +
      annotate("text", x = 26, y = 2.5, label = "Uruchom pierwsze badanie",
               colour = upwr_reference, size = 5) +
      coord_cartesian(xlim = c(14, 38), ylim = c(0.4, 4.6)) +
      labs(x = "Czas dojazdu (min)", y = NULL) +
      theme_upwr(grid = "x") +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()))
  }
  l <- st$last
  d <- data.frame(y = 4:1, lo = l$lo, hi = l$hi, xbar = l$xbar,
                  col = ifelse(l$lo > 26, col_miss, col_ci))
  ggplot(d) +
    geom_vline(xintercept = 26, linetype = "22", colour = upwr_secondary) +
    geom_segment(aes(x = lo, xend = hi, y = y, yend = y, colour = col), linewidth = 2.6) +
    geom_point(aes(x = xbar, y = y), colour = col_estimate, shape = 18, size = 4) +
    scale_colour_identity() +
    scale_y_continuous(breaks = 1:4, labels = rev(s4p_5c_lab)) +
    coord_cartesian(xlim = c(14, 38), ylim = c(0.4, 4.6)) +
    labs(x = "Czas dojazdu (min)", y = NULL) +
    theme_upwr(grid = "x")
}

s4p_5c_bars_fn <- function(st) {
  pu <- if (st$n) st$honest / st$n else 0
  pf <- if (st$n) st$flex / st$n else 0
  d <- data.frame(who = factor(c("Elastyczny", "Uczciwy"), levels = c("Elastyczny", "Uczciwy")),
                  v = 100 * c(pf, pu))
  ggplot(d, aes(x = v, y = who)) +
    geom_vline(xintercept = 2.5, linetype = "dotted", colour = upwr_secondary) +
    geom_col(fill = col_miss, width = 0.55) +
    geom_text(aes(label = paste0(lc_fmt(v, 1), "%")), hjust = -0.15,
              family = lc_mono_family, size = 4, colour = upwr_ink) +
    coord_cartesian(xlim = c(0, max(15, max(d$v) * 1.25))) +
    labs(x = "Odsetek fałszywych „przekracza” (%)", y = NULL) +
    theme_upwr(grid = "x")
}

s4p_5c_ui <- s4p_panel("5c", "Symulacja", "Poziom dobrany po fakcie", width_mode = "wide",
  lc_toolbar(
    lc_action_group(s4p_5c_add1 = "+1", s4p_5c_add100 = "+100", s4p_5c_add1000 = "+1000",
                    label = "Badania"),
    lc_action("s4p_5c_reset", icon = "reset", variant = "ghost", aria_label = "Od nowa"),
    lc_readouts(uiOutput("s4p_5c_read"))
  ),
  lc_plot("s4p_5c_last", ratio = "2.6/1", max_height = "300px"),
  lc_plot("s4p_5c_bars", ratio = "3.4/1", max_height = "200px"),
  uiOutput("s4p_5c_caption")
)

# ============================================================================
# (D1) DOMKNIĘCIE „ANATOMIA PRZEDZIAŁU” — kroki lc_step_widget
# ============================================================================

# Kroki dokładają warstwy: 1 estymata, 2 ±1 SE, 3 95%, 4 warianty, 5 próg 26.
s4p_D1_plot_fn <- function(k) {
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

s4p_D1_ui <- s4p_panel("D1", "Domknięcie", NULL, width_mode = "wide",
  lc_step_widget("s4p_D1_step",
    title = "Anatomia przedziału",
    steps = c("Estymata", "Błąd standardowy", "Przedział", "Co go zwęża", "Odczyt"),
    plot_id = "s4p_D1_plot",
    ratio = "2.6/1"
  )
)

# ============================================================================
# SERWER
# ============================================================================

s4p_ch5_server <- function(input, output, session) {

  # --- 5a: ścieżka ankiet (wszystkie n od resetu; ostatnie = bieżące) ---
  s4p_5a_ns <- reactiveVal(100)
  observeEvent(input$s4p_5a_dokup, s4p_5a_ns(c(s4p_5a_ns(), tail(s4p_5a_ns(), 1) + 100)))
  observeEvent(input$s4p_5a_double, s4p_5a_ns(c(s4p_5a_ns(), 2 * tail(s4p_5a_ns(), 1))))
  observeEvent(input$s4p_5a_reset, s4p_5a_ns(100))

  output$s4p_5a_read <- renderUI({
    n <- tail(s4p_5a_ns(), 1)
    tagList(
      lc_readout("Ankiet", lc_fmt(n, 0, " ")),
      lc_readout("Margines błędu", paste0("± ", lc_fmt(100 * s4p_5a_me(n), 1), " pkt proc.")),
      lc_readout("Koszt", paste(lc_fmt(20 * n, 0, " "), "zł"))
    )
  })

  output$s4p_5a_caption <- renderUI({
    lc_caption("Połowa marginesu kosztuje cztery razy więcej ankiet.")
  })

  zoom_plot_server("s4p_5a_ci", reactive(s4p_5a_ci_fn(s4p_5a_ns())),
    alt = "Przedziały 50% ± margines błędu dla sondażu, najnowszy na górze, poprzednie blade")
  zoom_plot_server("s4p_5a_path", reactive(s4p_5a_path_fn(s4p_5a_ns())),
    alt = "Margines błędu w pkt proc. w zależności od kosztu badania w tys. zł")

  # --- 5b: statyczna galeria ---
  zoom_plot_server("s4p_5b_plot", reactive(s4p_5b_fn()),
    alt = "Cztery przedziały dla poparcia 54% przy n = 100, 400, 1600 i 6400")

  # --- 5c: symulacja „poziom dobrany po fakcie” ---
  s4p_5c_state <- reactiveVal(list(n = 0, honest = 0, flex = 0, last = NULL))
  s4p_5c_add <- function(k) {
    r <- s4p_5c_run(k)
    st <- s4p_5c_state()
    s4p_5c_state(list(n = st$n + k, honest = st$honest + r$honest,
                      flex = st$flex + r$flex, last = r$last))
  }
  observeEvent(input$s4p_5c_add1, s4p_5c_add(1))
  observeEvent(input$s4p_5c_add100, s4p_5c_add(100))
  observeEvent(input$s4p_5c_add1000, s4p_5c_add(1000))
  observeEvent(input$s4p_5c_reset, s4p_5c_state(list(n = 0, honest = 0, flex = 0, last = NULL)))

  output$s4p_5c_read <- renderUI({
    st <- s4p_5c_state()
    tagList(
      lc_readout("Badań", lc_fmt(st$n, 0, " ")),
      lc_readout("Uczciwy", if (st$n) s4p_pct(st$honest / st$n, 1) else "–",
                 color = "var(--upwr-ink)"),
      lc_readout("Elastyczny", if (st$n) s4p_pct(st$flex / st$n, 1) else "–",
                 color = col_miss, swatch = TRUE)
    )
  })

  output$s4p_5c_caption <- renderUI({
    lc_caption("Prawda: średni dojazd to dokładnie 26 min, więc każde „przekracza” jest fałszywe.")
  })

  zoom_plot_server("s4p_5c_last", reactive(s4p_5c_last_fn(s4p_5c_state())),
    alt = "Cztery przedziały (99, 95, 90, 80%) ostatniego badania z progiem 26 min")
  zoom_plot_server("s4p_5c_bars", reactive(s4p_5c_bars_fn(s4p_5c_state())),
    alt = "Odsetek fałszywych „przekracza” dla analityka uczciwego i elastycznego")

  # --- D1: domknięcie krokowe ---
  step <- lc_step_server("s4p_D1_step", input)$step
  s4p_step_text(output, "s4p_D1_step", step, list(
    "Rozdz. 1: średnia z 40 pomiarów wynosi x̄ = 28.5 min. To najlepsza pojedyncza wartość, ale nie mówi, jak bardzo może się mylić.",
    "Rozdz. 1: pasek to ±1 SE = 8/√40 = 1.26 min. Tyle średnia typowo odchodzi od prawdziwej wartości.",
    "Rozdz. 2–3: x̄ ± t*·SE, z t* = 2.023 dla 95% i 39 stopni swobody. Przedział to 25.9–31.1 min.",
    "Rozdz. 5: większa próba (n × 4), mniejsze s (s / 2) i niższa ufność (90%) zawężają przedział. Dwa pierwsze działają przez SE, trzeci przez t*.",
    "Rozdz. 5: próg 26 min leży wewnątrz przedziału, więc dane tego nie rozstrzygają. Wykład 05 wróci do takich pytań."
  ))
  zoom_plot_server("s4p_D1_plot", reactive(s4p_D1_plot_fn(step())),
    alt = "Przedział ufności czasu dojazdu 28.5 min z progiem 26 min w środku")
}
