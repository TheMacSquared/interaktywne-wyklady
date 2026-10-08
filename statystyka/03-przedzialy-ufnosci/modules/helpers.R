# ============================================================================
# FUNKCJE POMOCNICZE - Przedzialy ufnosci
# generate_population_sample(), get_population_params(), dist_names_pl -> R/shared.R
# ============================================================================

# Semantyczne kolory wykładu — z palety upwr_cat.
# Używane w chapterach 1–7 do plotów CI/estymacji.
col_ci       <- unname(upwr_cat["niebo"])      # przedział ufności
col_miss     <- unname(upwr_cat["terakota"])   # przedział nie trafił
col_hit      <- unname(upwr_cat["szalwia"])    # przedział trafił
col_estimate <- unname(upwr_cat["bursztyn"])   # estymata punktowa
col_true     <- unname(upwr_cat["wrzos"])      # prawdziwy parametr

# Symulacja pokrycia dla proporcji
simulate_coverage_prop <- function(true_p, n, conf_level, n_sims = 100,
                                    method = "wald") {
  results <- lapply(seq_len(n_sims), function(i) {
    x <- rbinom(1, n, true_p)
    phat <- x / n

    if (method == "wald") {
      z_star <- qnorm(1 - (1 - conf_level) / 2)
      me <- z_star * sqrt(phat * (1 - phat) / n)
      lower <- phat - me
      upper <- phat + me
    } else {
      # Wilson
      z_star <- qnorm(1 - (1 - conf_level) / 2)
      denom <- 1 + z_star^2 / n
      center <- (phat + z_star^2 / (2 * n)) / denom
      me <- (z_star / denom) * sqrt(phat * (1 - phat) / n + z_star^2 / (4 * n^2))
      lower <- center - me
      upper <- center + me
    }

    data.frame(
      sim = i,
      phat = phat,
      lower = lower,
      upper = upper,
      covers = (lower <= true_p) & (true_p <= upper)
    )
  })

  do.call(rbind, results)
}


# ============================================================================
# Sceny (scenes.js): konkretne doświadczenie → przedział → częstość trafień
# Skopiowane ze statystyki 00 (modules/helpers.R).
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

# Sceny z grupką spod sali (rozdz. 1–3): świat wykładu — wzrost, populacja normalna.
# n = 25 w scenach „Zmierz grupkę” i „Zarzuć siatkę”, n = 5 w „Za mała siatka”.
net_world <- get_population_params("normal")          # μ = 170, σ = 10
net_n     <- 25L
net_tq    <- setNames(as.list(round(qt(0.975, net_n - 1), 3)), net_n)   # t* dla 95%
