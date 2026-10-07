# ============================================================================
# Helpery — wykład 00: Dane i populacja
# Populacja syntetyczna: wszyscy studenci jednego wydziału. Parametry są znane,
# więc widgety mogą pokazać, jak statystyki z prób wypadają wokół nich.
# ============================================================================

col_pop    <- unname(upwr_cat["niebo"])     # populacja, tło
col_sample <- upwr_accent                   # wylosowana próba
col_param  <- unname(upwr_cat["wrzos"])     # parametr populacji
col_stat   <- unname(upwr_cat["bursztyn"])  # statystyka z próby

# Wydział: N = 2400 studentów, siatka 60 × 40 na wykresie.
pop_grid_cols <- 60L

make_faculty_population <- function(N = 2400L, seed = 2026L) {
  set.seed(seed)
  rok <- sample(1:5, N, replace = TRUE, prob = c(0.25, 0.22, 0.20, 0.18, 0.15))
  rok <- sort(rok)
  # Akademik: częściej na pierwszych latach.
  akademik <- rbinom(N, 1, c(0.32, 0.24, 0.18, 0.12, 0.08)[rok]) == 1
  # Czas dojazdu (min): z akademika kilka minut, z miasta i okolic rozkład skośny.
  dojazd <- ifelse(akademik,
                   round(runif(N, 3, 15)),
                   round(5 + rgamma(N, shape = 2.6, scale = 12)))
  # Praca zarobkowa: rośnie z rokiem studiów, rzadsza w akademiku.
  p_praca <- c(0.18, 0.28, 0.40, 0.52, 0.62)[rok] - ifelse(akademik, 0.08, 0)
  praca <- rbinom(N, 1, p_praca) == 1
  data.frame(
    id       = seq_len(N),
    rok      = rok,
    akademik = akademik,
    dojazd   = dojazd,
    praca    = praca,
    gx       = (seq_len(N) - 1L) %% pop_grid_cols + 1L,
    gy       = (seq_len(N) - 1L) %/% pop_grid_cols + 1L
  )
}

faculty <- make_faculty_population()

# Wynik egzaminu ze statystyki (rozdz. 2): zdawalność rośnie z rokiem studiów.
faculty$zdal <- local({
  set.seed(99)
  rbinom(nrow(faculty), 1, c(0.52, 0.60, 0.68, 0.76, 0.84)[faculty$rok]) == 1
})

pop_N     <- nrow(faculty)
pop_mu    <- mean(faculty$dojazd)          # parametr: średni czas dojazdu
pop_p     <- mean(faculty$praca)           # parametr: odsetek pracujących
pop_zdal  <- mean(faculty$zdal)            # parametr: odsetek, który zdał egzamin

# Mały przykład do rozdziału 1: pięć osób, trzy dni pomiaru dojazdu.
commute_people <- data.frame(
  osoba    = c("Ania", "Bartek", "Celina", "Darek", "Ewa"),
  kierunek = c("Biologia", "Ekonomia", "Biologia", "Informatyka", "Ekonomia"),
  pon      = c(25, 40, 12, 55, 18),
  wt       = c(28, 35, 10, 61, 20),
  sr       = c(22, 45, 11, 50, 35),
  stringsAsFactors = FALSE
)

commute_long <- local({
  days <- c(pon = "pon", wt = "wt", sr = "śr")
  rows <- lapply(seq_len(nrow(commute_people)), function(i) {
    data.frame(
      osoba    = commute_people$osoba[i],
      kierunek = commute_people$kierunek[i],
      dzien    = unname(days),
      czas     = unname(unlist(commute_people[i, names(days)])),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
})


# ============================================================================
# Sceny (scenes.js): konkretne doświadczenie → statystyka → rozkład wyników
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
