# ============================================================================
# FUNKCJE POMOCNICZE
# generate_population_sample(), get_population_params(), dist_names_pl -> R/shared.R
# ============================================================================

# Kolory semantyczne dla typow rozkladow — z palety upwr
col_discrete    <- unname(upwr_cat["niebo"])      # rozklady dyskretne
col_continuous  <- unname(upwr_cat["szalwia"])    # rozklady ciagle
col_normal      <- unname(upwr_cat["wrzos"])      # rozklad normalny
col_binomial    <- unname(upwr_cat["bursztyn"])   # dwumianowy
col_poisson     <- unname(upwr_cat["szalwia"])    # Poissona
col_uniform     <- unname(upwr_cat["niebo"])      # jednostajny
col_exponential <- unname(upwr_cat["terakota"])   # wykladniczy
col_geometric   <- unname(upwr_cat["indygo"])     # geometryczny
col_t_student   <- upwr_accent                    # t-Studenta (burgund)
col_chi_sq      <- unname(upwr_cat["kurkuma"])    # chi-kwadrat
col_lognormal   <- unname(upwr_cat["szalwia"])    # log-normalny


# Rysowanie PMF rozkladu dyskretnego
plot_pmf <- function(x_vals, probs, fill_color = unname(upwr_cat["niebo"]),
                     title = "", xlab = "x", ylab = "P(X = x)",
                     show_mean = FALSE, show_sd = FALSE, mu = NULL, sigma = NULL) {
  df <- data.frame(x = x_vals, prob = probs)

  p <- ggplot(df, aes(x = x, y = prob)) +
    geom_col(fill = fill_color, color = "white", alpha = 0.85, width = 0.7) +
    geom_text(aes(label = round(prob, 3)), vjust = -0.5, size = 3.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = xlab, y = ylab) +
    theme_upwr()

  if (show_mean && !is.null(mu)) {
    p <- p + geom_vline(xintercept = mu, color = upwr_accent, linewidth = 1.2, linetype = "dashed")
  }
  if (show_sd && !is.null(mu) && !is.null(sigma)) {
    p <- p +
      annotate("rect", xmin = mu - sigma, xmax = mu + sigma,
               ymin = 0, ymax = Inf, fill = upwr_accent, alpha = 0.1)
  }
  p
}

# Rysowanie PDF rozkladu ciaglego
plot_pdf <- function(density_fn, xlim, fill_color = unname(upwr_cat["szalwia"]),
                     title = "", xlab = "x", ylab = "f(x)",
                     shade_from = NULL, shade_to = NULL, n_points = 500) {
  x_seq <- seq(xlim[1], xlim[2], length.out = n_points)
  y_seq <- density_fn(x_seq)
  df <- data.frame(x = x_seq, y = y_seq)

  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line(color = fill_color, linewidth = 1.2) +
    labs(x = xlab, y = ylab) +
    theme_upwr()

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


# ============================================================================
# Sceny (scenes.js, PROTOTYPY 2026-10-08): doświadczenie z życia → zmienna → rozkład
# Wzorzec jak scene_widget() w statystyce 00; obok exp_widget() (experiment.js).
# ============================================================================

# Widget krokowy ze sceną SVG rysowaną w scenes.js. Konfiguracja trafia do JS jako JSON.
# labels: podpisy głównego przycisku w kolejnych krokach.
# options: lista list(name, label, values, selected, from = 1), przełączniki opcji.
# more_from: od którego kroku aktywne są przyciski +10, +100, +1000.
scene_widget <- function(id, title, steps, config, labels, options = NULL,
                         more_from = 3, more = c("+10" = "m10", "+100" = "m100", "+1000" = "m1000")) {
  lc_step_widget(id,
    title = title,
    steps = steps,
    toolbar = lc_toolbar(
      lapply(options, function(o) {
        lc_step_from(o$from %||% 1, lc_group(o$label,
          tags$div(class = "lc-seg", role = "group", `aria-label` = o$label,
            lapply(seq_along(o$values), function(i) {
              v <- unname(o$values[[i]])
              lab <- names(o$values)[[i]] %||% v
              if (is.null(lab) || !nzchar(lab)) lab <- v
              tags$button(type = "button", `data-sc-opt` = paste0(o$name, ":", v),
                `aria-pressed` = if (v == o$selected) "true" else "false", lab)
            })
          )
        ))
      }),
      tags$button(type = "button", class = "lc-action is-solid", `data-sc-act` = "go",
        `data-labels` = jsonlite::toJSON(labels),
        lc_icon("shuffle"), tags$span(labels[[1]])),
      if (!is.null(more)) lc_step_from(more_from,
        tags$div(class = "lc-seg", role = "group", `aria-label` = "Więcej powtórzeń",
          lapply(seq_along(more), function(i) tags$button(type = "button",
            `data-sc-act` = unname(more[[i]]), names(more)[[i]]))
        )
      )
    ),
    body = tags$div(class = "lc-sc",
      `data-config` = jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA))
  )
}

# Teksty kroków: lista fragmentów HTML, po jednym na krok.
scene_texts <- function(input, output, id, texts) {
  step <- lc_step_server(id, input)$step
  output[[paste0(id, "_text")]] <- renderUI(texts[[step()]])
}

# Światy scen: te same liczby w JS (config) i w tekstach kroków.
# Czas dojazdu na uczelnię (min): 5 + Gamma(kształt 2, skala 10), prawoskośny.
scene_commute <- list(shape = 2, scale = 10, shift = 5)
scene_commute$mu    <- scene_commute$shift + scene_commute$shape * scene_commute$scale
scene_commute$sigma <- sqrt(scene_commute$shape) * scene_commute$scale

# Zdrapka z kiosku (rozdz. 2) i dwa losy kontrastowe o tej samej E(X) = 4 zł.
scene_tickets <- list(
  main  = list(name = "ZDRAPKA", prizes = c(0, 2, 10, 100), probs = c(0.65, 0.25, 0.08, 0.02),
               foot = "do wygrania: 2, 10 lub 100 zł"),
  sure  = list(name = "LOS PEWNY", prizes = 4, probs = 1, foot = "wygrana: zawsze 4 zł"),
  risky = list(name = "LOS RYZYKOWNY", prizes = c(0, 40), probs = c(0.9, 0.1),
               foot = "do wygrania: 40 zł")
)
scene_ticket_price <- 5
scene_ticket_ev <- function(t) sum(t$prizes * t$probs)
scene_ticket_sd <- function(t) sqrt(sum(t$prizes^2 * t$probs) - scene_ticket_ev(t)^2)
