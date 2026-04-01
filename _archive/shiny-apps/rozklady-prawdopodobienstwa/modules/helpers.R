# ============================================================================
# FUNKCJE POMOCNICZE
# ============================================================================

# Generowanie proby z wybranego rozkladu
generate_population_sample <- function(dist_type, n) {
  switch(dist_type,
    "uniform"     = runif(n, min = 0, max = 10),
    "exponential" = rexp(n, rate = 0.5),
    "bimodal"     = {
      k <- rbinom(n, 1, 0.5)
      ifelse(k == 1, rnorm(n, mean = 3, sd = 0.8), rnorm(n, mean = 7, sd = 0.8))
    },
    "u_shape"     = rbeta(n, 0.5, 0.5) * 10,
    "skewed_left" = 10 - rgamma(n, shape = 2, scale = 1.5),
    "die"         = sample(1:6, n, replace = TRUE),
    rnorm(n)
  )
}

# Parametry populacji dla roznych rozkladow
get_population_params <- function(dist_type) {
  switch(dist_type,
    "uniform"     = list(mu = 5, sigma = sqrt(100/12)),
    "exponential" = list(mu = 2, sigma = 2),
    "bimodal"     = list(mu = 5, sigma = sqrt(0.8^2 + 4)),
    "u_shape"     = list(mu = 5, sigma = sqrt(10^2 / 4)),
    "skewed_left" = list(mu = 10 - 2*1.5, sigma = sqrt(2) * 1.5),
    "die"         = list(mu = 3.5, sigma = sqrt(35/12)),
    list(mu = 0, sigma = 1)
  )
}

# Nazwy rozkladow po polsku
dist_names_pl <- c(
  "uniform"     = "Jednostajny",
  "exponential" = "Wyk\u0142adniczy (prawosko\u015bny)",
  "bimodal"     = "Dwumodalny",
  "u_shape"     = "U-kszta\u0142tny (Beta)",
  "skewed_left" = "Lewosko\u015bny",
  "die"         = "Kostka (dyskretny)"
)

# Wspolny theme dla wykresow
theme_prob <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "#7f8c8d"),
      panel.grid.minor = element_blank()
    )
}

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
    theme_prob()

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
    theme_prob()

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
