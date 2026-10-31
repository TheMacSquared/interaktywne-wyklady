# PROTOTYPY PUNKTÓW STOP — rozdział 4 (2026-10-30) ---------------------------
#   s4p_4a_ui — Prototyp 4a · Piaskownica „Zero braków na 50”
#       Puenta: przy kilku brakach przedział Walda wychodzi poniżej zera,
#       a przy zerze braków ma zerową szerokość; Wilson zostaje rozsądny.
#   s4p_4b_ui — Prototyp 4b · Piaskownica „Ile naprawdę daje 95%?”
#       Puenta: Wald obiecuje 95%, ale przy p blisko 0 albo 1 trafia rzadziej;
#       Wilson trzyma się blisko 95%.
# Serwer: s4p_ch4_server(input, output, session).

# ============================================================================
# FUNKCJE WSPÓLNE (przedziały i pokrycie)
# ============================================================================

s4p_ch4_z <- 1.96

# Przedział Walda dla x sukcesów w n próbach (95%).
s4p_ch4_wald <- function(x, n, z = s4p_ch4_z) {
  p <- x / n
  me <- z * sqrt(p * (1 - p) / n)
  c(lo = p - me, hi = p + me)
}

# Przedział Wilsona dla x sukcesów w n próbach (95%).
s4p_ch4_wilson <- function(x, n, z = s4p_ch4_z) {
  p <- x / n
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  me <- (z / denom) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  c(lo = center - me, hi = center + me)
}

# Procent z jedną cyfrą po kropce; zero bez znaku minus.
s4p_ch4_pct <- function(x) {
  v <- round(100 * x, 1)
  v[v == 0] <- 0
  sprintf("%.1f%%", v)
}

# Dokładne pokrycie przedziału przy prawdziwym p: suma P(X = x) po x,
# dla których p leży w przedziale. Bez symulacji; p może być wektorem.
s4p_ch4_coverage <- function(n, p, method = c("wald", "wilson")) {
  method <- match.arg(method)
  f <- if (method == "wald") s4p_ch4_wald else s4p_ch4_wilson
  x <- 0:n
  lo <- vapply(x, function(xi) f(xi, n)[["lo"]], numeric(1))
  hi <- vapply(x, function(xi) f(xi, n)[["hi"]], numeric(1))
  inside <- outer(p, lo, ">=") & outer(p, hi, "<=")
  prob <- outer(p, x, function(pp, xx) dbinom(xx, n, pp))
  rowSums(inside * prob)
}

# ============================================================================
# (4a) PIASKOWNICA „ZERO BRAKÓW NA 50”
# ============================================================================

s4p_4a_ui <- s4p_panel("4a", "Piaskownica", "Zero braków na 50",
  lc_toolbar(
    lc_segmented("s4p_4a_n", "Sprawdzone sztuki n",
                 choices = c("20" = "20", "50" = "50", "200" = "200"), selected = "50"),
    lc_slider("s4p_4a_x", "Wadliwe sztuki x", min = 0, max = 15, value = 2, step = 1),
    lc_readouts(uiOutput("s4p_4a_read"))
  ),
  lc_plot("s4p_4a_plot", ratio = "2.4/1"),
  uiOutput("s4p_4a_caption")
)

# Odcinki przedziałów: Walda dzielony na część poniżej 0 (terakota) i resztę
# (niebo); Wilson cały w szałwii. Odcinek o długości 0 nic nie rysuje.
s4p_4a_plot_fn <- function(n, x) {
  x <- min(x, n)
  p <- x / n
  wald <- s4p_ch4_wald(x, n)
  wilson <- s4p_ch4_wilson(x, n)

  seg <- data.frame(
    y    = c(2, 2, 1),
    x    = c(min(wald[["lo"]], 0), max(wald[["lo"]], 0), max(wilson[["lo"]], 0)),
    xend = c(min(wald[["hi"]], 0), wald[["hi"]], wilson[["hi"]]),
    col  = c(col_miss, col_ci, col_hit)
  )
  pt <- data.frame(y = c(2, 1), x = p)

  x_lo <- -0.10
  x_hi <- max(0.40, ceiling(max(wald[["hi"]], wilson[["hi"]]) * 20) / 20)

  ggplot() +
    annotate("rect", xmin = x_lo, xmax = 0, ymin = 0.4, ymax = 2.6,
             fill = "grey60", alpha = 0.18) +
    geom_vline(xintercept = 0, colour = upwr_secondary, linewidth = 0.5) +
    geom_segment(data = seg, aes(x = x, xend = xend, y = y, yend = y, colour = col),
                 linewidth = 3.4, lineend = "butt") +
    geom_point(data = pt, aes(x = x, y = y), colour = col_estimate, size = 3.6) +
    scale_colour_identity(guide = "none") +
    scale_x_continuous(breaks = seq(-0.10, x_hi, by = 0.10),
                       labels = function(v) paste0(round(100 * v), "%")) +
    scale_y_continuous(breaks = c(2, 1), labels = c("Wald", "Wilson"),
                       limits = c(0.4, 2.6)) +
    coord_cartesian(xlim = c(x_lo, x_hi)) +
    labs(x = "Odsetek wadliwych sztuk", y = NULL) +
    theme_upwr(grid = "x")
}

# ============================================================================
# (4b) PIASKOWNICA „ILE NAPRAWDĘ DAJE 95%?”
# ============================================================================

s4p_4b_grid <- seq(0.005, 0.995, by = 0.005)

s4p_4b_ui <- s4p_panel("4b", "Piaskownica", "Ile naprawdę daje 95%?",
  lc_toolbar(
    lc_segmented("s4p_4b_n", "Liczebność próby n",
                 choices = c("20" = "20", "50" = "50", "200" = "200"), selected = "50"),
    lc_slider("s4p_4b_p", "Prawdziwy odsetek p", min = 0.01, max = 0.99,
              value = 0.08, step = 0.01, digits = 2),
    lc_readouts(uiOutput("s4p_4b_read"))
  ),
  lc_plot("s4p_4b_plot", ratio = "2.4/1")
)

s4p_4b_plot_fn <- function(n, p) {
  g <- s4p_4b_grid
  cov_w <- s4p_ch4_coverage(n, g, "wald")
  cov_l <- s4p_ch4_coverage(n, g, "wilson")
  curve_w <- data.frame(p = g, cov = cov_w)
  curve_l <- data.frame(p = g, cov = cov_l)
  now <- data.frame(
    p = p,
    wald = s4p_ch4_coverage(n, p, "wald"),
    wilson = s4p_ch4_coverage(n, p, "wilson")
  )

  ggplot() +
    geom_hline(yintercept = 0.95, linetype = "22", colour = upwr_secondary, linewidth = 0.5) +
    geom_vline(xintercept = p, colour = upwr_secondary, linewidth = 0.4, alpha = 0.5) +
    geom_line(data = curve_w, aes(p, cov), colour = col_miss, linewidth = 1.1) +
    geom_line(data = curve_l, aes(p, cov), colour = col_hit, linewidth = 1.1) +
    geom_point(data = now, aes(p, wald), colour = col_miss, size = 3.4) +
    geom_point(data = now, aes(p, wilson), colour = col_hit, size = 3.4) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2),
                       labels = function(v) paste0(round(100 * v), "%")) +
    scale_y_continuous(breaks = seq(0.5, 1, by = 0.1),
                       labels = function(v) paste0(round(100 * v), "%")) +
    coord_cartesian(ylim = c(0.5, 1)) +
    labs(x = "Prawdziwy odsetek p", y = "Pokrycie przedziału") +
    theme_upwr(grid = "both")
}

# ============================================================================
# SERWER
# ============================================================================

s4p_ch4_server <- function(input, output, session) {

  # --- 4a -------------------------------------------------------------------
  n_4a <- reactive(as.numeric(input$s4p_4a_n %||% "50"))
  x_4a <- reactive(min(input$s4p_4a_x %||% 4, n_4a()))

  output$s4p_4a_read <- renderUI({
    n <- n_4a()
    x <- x_4a()
    wald <- s4p_ch4_wald(x, n)
    wilson <- s4p_ch4_wilson(x, n)
    tagList(
      lc_readout("p̂", s4p_ch4_pct(x / n), color = col_estimate),
      lc_readout("Wald", paste0("od ", s4p_ch4_pct(wald[["lo"]]), " do ", s4p_ch4_pct(wald[["hi"]]))),
      lc_readout("Wilson", paste0("od ", s4p_ch4_pct(wilson[["lo"]]), " do ", s4p_ch4_pct(wilson[["hi"]])))
    )
  })

  zoom_plot_server("s4p_4a_plot", reactive(s4p_4a_plot_fn(n_4a(), x_4a())),
    alt = "Dwa przedziały ufności dla odsetka wadliwych sztuk, Walda i Wilsona, na osi od -10% do 40%")

  output$s4p_4a_caption <- renderUI({
    n <- n_4a()
    x <- x_4a()
    wald <- s4p_ch4_wald(x, n)
    msg <- if (x == 0) {
      "Wald: przedział o zerowej szerokości — jakby braków na pewno nie było."
    } else if (wald[["lo"]] < 0) {
      "Wald: dolna granica poniżej zera, czyli przedział wychodzi poza możliwe odsetki."
    } else {
      "Wald: dolna granica jeszcze nie wychodzi poniżej zera."
    }
    lc_caption(msg)
  })

  # --- 4b -------------------------------------------------------------------
  n_4b <- reactive(as.numeric(input$s4p_4b_n %||% "50"))
  p_4b <- reactive(input$s4p_4b_p %||% 0.08)

  output$s4p_4b_read <- renderUI({
    n <- n_4b()
    p <- p_4b()
    tagList(
      lc_readout("Pokrycie Walda", s4p_ch4_pct(s4p_ch4_coverage(n, p, "wald")), color = col_miss),
      lc_readout("Pokrycie Wilsona", s4p_ch4_pct(s4p_ch4_coverage(n, p, "wilson")), color = col_hit)
    )
  })

  zoom_plot_server("s4p_4b_plot", reactive(s4p_4b_plot_fn(n_4b(), p_4b())),
    alt = "Pokrycie przedziałów Walda i Wilsona w funkcji prawdziwego odsetka p, z linią 95%")
}
