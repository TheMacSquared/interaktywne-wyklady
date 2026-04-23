# ============================================================================
# FUNKCJE POMOCNICZE
# generate_population_sample(), get_population_params(), dist_names_pl -> R/shared.R
# ============================================================================

# Kolory semantyczne dla typow rozkladow
col_discrete   <- "#3498db"    # niebieski - rozklady dyskretne
col_continuous <- "#27ae60"    # zielony - rozklady ciagle
col_normal     <- "#9b59b6"    # fioletowy - rozklad normalny
col_binomial   <- "#e67e22"    # pomaranczowy - dwumianowy
col_poisson    <- "#1abc9c"    # morski - Poissona
col_uniform    <- "#3498db"    # niebieski - jednostajny
col_exponential <- "#e74c3c"   # czerwony - wykladniczy
col_geometric  <- "#8e44ad"    # ciemny fiolet - geometryczny
col_t_student  <- "#c0392b"    # ciemny czerwony - t-Studenta
col_chi_sq     <- "#d35400"    # ciemny pomaranczowy - chi-kwadrat
col_lognormal  <- "#16a085"    # ciemny turkusowy - log-normalny

col_scenario <- c("#3498db", "#e74c3c", "#27ae60", "#f39c12", "#9b59b6")

# Rysowanie PMF rozkladu dyskretnego
plot_pmf <- function(x_vals, probs, fill_color = "#3498db",
                     title = "", xlab = "x", ylab = "P(X = x)",
                     show_mean = FALSE, show_sd = FALSE, mu = NULL, sigma = NULL) {
  df <- data.frame(x = x_vals, prob = probs)

  p <- ggplot(df, aes(x = x, y = prob)) +
    geom_col(fill = fill_color, color = "white", alpha = 0.85, width = 0.7) +
    geom_text(aes(label = round(prob, 3)), vjust = -0.5, size = 3.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = xlab, y = ylab) +
    theme_educational()

  if (show_mean && !is.null(mu)) {
    p <- p + geom_vline(xintercept = mu, color = "#e74c3c", linewidth = 1.2, linetype = "dashed")
  }
  if (show_sd && !is.null(mu) && !is.null(sigma)) {
    p <- p +
      annotate("rect", xmin = mu - sigma, xmax = mu + sigma,
               ymin = 0, ymax = Inf, fill = "#e74c3c", alpha = 0.1)
  }
  p
}

# Rysowanie PDF rozkladu ciaglego
plot_pdf <- function(density_fn, xlim, fill_color = "#27ae60",
                     title = "", xlab = "x", ylab = "f(x)",
                     shade_from = NULL, shade_to = NULL, n_points = 500) {
  x_seq <- seq(xlim[1], xlim[2], length.out = n_points)
  y_seq <- density_fn(x_seq)
  df <- data.frame(x = x_seq, y = y_seq)

  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line(color = fill_color, linewidth = 1.2) +
    labs(title = title, x = xlab, y = ylab) +
    theme_educational()

  if (!is.null(shade_from) && !is.null(shade_to)) {
    shade_x <- seq(max(xlim[1], shade_from), min(xlim[2], shade_to), length.out = 300)
    shade_y <- density_fn(shade_x)
    shade_df <- data.frame(x = shade_x, y = shade_y)
    p <- p +
      geom_area(data = shade_df, aes(x = x, y = y),
                fill = fill_color, alpha = 0.3)
  }
  p
}
