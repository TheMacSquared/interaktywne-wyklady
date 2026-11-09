# ============================================================================
# FUNKCJE POMOCNICZE - Zalozenia testow
# ============================================================================

# Kolory domenowe dla założeń testów. Wartości pochodzą z palety UPWr.
col_ok   <- unname(upwr_cat["szalwia"])   # założenie spełnione
col_fail <- upwr_accent                    # założenie naruszone
col_test <- unname(upwr_cat["niebo"])      # dane/test
col_alt  <- unname(upwr_cat["wrzos"])      # alternatywa

# Generowanie danych o roznych rozkladach
generate_test_data <- function(n = 50, dist = "normal") {
  set.seed(NULL)
  switch(dist,
    "normal"     = rnorm(n, 170, 10),
    "skewed"     = rgamma(n, shape = 2, scale = 5) + 150,
    "heavy_tail" = 170 + 10 * rt(n, df = 3),
    "bimodal"    = c(rnorm(n/2, 160, 4), rnorm(n/2, 180, 4)),
    "uniform"    = runif(n, 150, 190),
    rnorm(n, 170, 10)
  )
}

# Generowanie danych do 2 grup o roznej wariancji
generate_two_groups <- function(n1 = 30, n2 = 30, sd1 = 10, sd2 = 10,
                                 mean1 = 170, mean2 = 175) {
  set.seed(NULL)
  data.frame(
    value = c(rnorm(n1, mean1, sd1), rnorm(n2, mean2, sd2)),
    group = factor(c(rep("A", n1), rep("B", n2)))
  )
}

# Nazwy rozkladow
dist_names_pl <- c(
  "normal"     = "Normalny",
  "skewed"     = "Prawoskośny (Gamma)",
  "heavy_tail" = "Ciężkie ogony (t)",
  "bimodal"    = "Dwumodalny",
  "uniform"    = "Jednostajny"
)

# ============================================================================
# SCENY (kopia wzorca z wykładu 00; silnik w modules/scenes.js)
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

# ============================================================================
# PROTOTYPY PUNKTÓW STOP 6B–6E (2026-11-09; 6A odrzucony)
# ============================================================================
# Złota plakietka „Prototyp <nr> · <rodzaj>”, jak w wykładzie 04. Lista
# propozycji i kolejka decyzji: PUNKTY-STOP-PROPOZYCJA.md, KOLEJKA-ZATWIERDZANIA.md.

p6_panel <- function(num, kind, title = NULL, ..., width_mode = "text") {
  figure_panel(label = paste0("Prototyp ", num, " · ", kind), title = title,
    color = upwr_single_alt, width_mode = width_mode, ...)
}

# 6B: kwantyle teoretyczne i pierwsza grupa 20 osób (stała, żeby kadry były powtarzalne).
p6_qq_config <- function(n = 20, mu = 172, sd = 9, seed = 64) {
  set.seed(seed)
  list(kind = "qq", q = round(qnorm(ppoints(n)), 4), mu = mu, sd = sd,
       heights = round(rnorm(n, mu, sd), 1), height = 470,
       aria = "Dwadzieścia osób staje w kolejce według wzrostu, obok konturów idealnej kolejki z rozkładu normalnego; pary wzrost i miejsce w kolejce tworzą wykres kwantyl-kwantyl")
}

# 6C: pula losowań masy jabłek (lekko prawoskośny rozkład) dla n = 20 i n = 2000,
# z wynikiem testu Shapiro-Wilka i prostą przez kwartyle (jak qqline). Dla n = 2000
# na wykres idzie 150 równomiernie wybranych punktów z posortowanej próby.
p6_sw_pool <- function(k = 30, seed = 2611, shape = 10, scale = 6, shift = 120) {
  set.seed(seed)
  one <- function(n) {
    x <- sort(shift + scale * rgamma(n, shape))
    q <- qnorm(ppoints(n))
    idx <- if (n > 200) unique(round(seq(1, n, length.out = 150))) else seq_len(n)
    sw <- shapiro.test(x)
    qy <- quantile(x, c(0.25, 0.75), names = FALSE); qx <- qnorm(c(0.25, 0.75))
    b <- diff(qy) / diff(qx)
    list(n = n, q = round(q[idx], 4), y = round(x[idx], 2),
         W = unname(sw$statistic), p = sw$p.value, a = qy[1] - b * qx[1], b = b)
  }
  lapply(seq_len(k), function(i) list(small = one(20), big = one(2000)))
}
