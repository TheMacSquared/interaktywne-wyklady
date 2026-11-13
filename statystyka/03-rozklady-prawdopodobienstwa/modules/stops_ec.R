# RYCINY ECHARTS Z DANYMI Z R ----------------------------------------------------
#   s3p_4aa_ui — Ryc. 4.2 „Ile osób ma dokładnie 170 cm?” (lupa)
#   s3p_e2_ui  — Ryc. 5.7 „Zaznacz przedział”: histogram masy jabłek, zaznaczanie
#                przedziału myszą i przybliżanie ogonów.
#   s3p_e1_ui  — Ryc. 6.5 „Mapa zbieżności”: heatmapa gęstości średniej dla
#                n = 1…40 i histogram 2000 wylosowanych średnich dla wybranego n.
# Wszystkie dane liczy R przy budowie strony (data-config); widgety rysuje
# ec_charts.js w przeglądarce, więc działają też w wersji offline bez serwera.
# Wymaga: stops_common.R (s3p_panel), lecture_layout.R.

# Ziarno tylko dla tych danych; globalny strumień losowy zostaje bez zmian.
s3p_ec_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had) assign(".Random.seed", old, envir = .GlobalEnv)
          else if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv))
  set.seed(seed)
  force(expr)
}

# ============================================================================
# (E1) GALERIA „MAPA ZBIEŻNOŚCI”
# ============================================================================

S3P_E1_N <- 40      # wiersze mapy: n = 1 … 40
S3P_E1_CELLS <- 100 # komórki w poziomie
S3P_E1_SIMS  <- 2000  # wylosowanych średnich na każde n (histogram pod mapą)
S3P_E1_BINS  <- 35

# Gęstość średniej z n obserwacji na siatce środków komórek.
# Wykładniczy: dokładnie Gamma(n, n/μ). Pozostałe: splot gęstości na drobnej
# siatce (suma n obserwacji), potem f_średnia(m) = n · f_suma(n · m).
s3p_e1_rows <- function(base, xs) {
  if (base$key == "exp") {
    return(lapply(seq_len(S3P_E1_N), function(n) dgamma(xs, shape = n, rate = n / base$mu)))
  }
  h <- base$h
  grid <- seq(base$lo, base$hi, by = h)
  f1 <- base$pdf(grid)
  f1 <- f1 / (sum(f1) * h)
  fk <- f1
  lapply(seq_len(S3P_E1_N), function(n) {
    if (n > 1) fk <<- pmax(stats::convolve(fk, rev(f1), type = "open") * h, 0)
    sums <- n * base$lo + (seq_along(fk) - 1) * h
    n * stats::approx(sums, fk, xout = n * xs, yleft = 0, yright = 0)$y
  })
}

s3p_e1_bases <- list(
  list(key = "exp", short = "Infolinia", unit = "min", axis = "Średni czas rozmowy",
       story = "Czas rozmowy na infolinii: większość rozmów jest krótka, nieliczne trwają bardzo długo. Rozkład wykładniczy, średnio 4 min.",
       one = "czas jednej rozmowy", many = "rozmów",
       draw = function(k) rexp(k, rate = 1 / 4),
       mu = 4, sd = 4, skew = 2, xlo = 0, xhi = 14),
  list(key = "mix", short = "Plon z krzaka", unit = "kg", axis = "Średni plon z krzaka",
       story = "Plon z krzaka porzeczki na plantacji częściowo porażonej: 30% krzaków daje około 0.7 kg, zdrowe około 2.3 kg. Rozkład dwumodalny.",
       one = "plon jednego krzaka", many = "krzaków",
       pdf = function(x) 0.3 * dnorm(x, 0.7, 0.2) + 0.7 * dnorm(x, 2.3, 0.35),
       draw = function(k) ifelse(runif(k) < 0.3, rnorm(k, 0.7, 0.2), rnorm(k, 2.3, 0.35)),
       lo = -0.4, hi = 4, h = 0.004, xlo = 0, xhi = 3.4),
  list(key = "unif", short = "Zaokrąglenie wagi", unit = "g", axis = "Średni błąd zaokrąglenia",
       story = "Błąd zaokrąglenia wagi sklepowej do pełnych gramów: każda wartość od −0.5 do 0.5 g jest równie częsta. Rozkład jednostajny.",
       one = "błąd jednego ważenia", many = "ważeń",
       pdf = function(x) as.numeric(x >= -0.5 & x <= 0.5),
       draw = function(k) runif(k, -0.5, 0.5),
       lo = -0.5, hi = 0.5, h = 0.0025, mu = 0, sd = sqrt(1 / 12), skew = 0, xlo = -0.6, xhi = 0.6)
)

s3p_e1_config <- local({
  lapply(s3p_e1_bases, function(b) {
    if (b$key == "mix") {
      # Momenty mieszaniny liczone numerycznie na drobnej siatce.
      g <- seq(b$lo, b$hi, by = b$h); f <- b$pdf(g); f <- f / sum(f)
      b$mu <- sum(g * f); b$sd <- sqrt(sum((g - b$mu)^2 * f))
      b$skew <- sum(((g - b$mu) / b$sd)^3 * f)
    }
    w <- (b$xhi - b$xlo) / S3P_E1_CELLS
    xs <- b$xlo + (seq_len(S3P_E1_CELLS) - 0.5) * w
    rows <- s3p_e1_rows(b, xs)
    # Rozbieżność z dzwonem: odległość całkowitej zmienności ½∫|f − φ| na siatce.
    tv <- vapply(seq_len(S3P_E1_N), function(n) {
      0.5 * sum(abs(rows[[n]] - dnorm(xs, b$mu, b$sd / sqrt(n)))) * w
    }, numeric(1))
    # Histogram 2000 wylosowanych średnich dla każdego n (liczności w przedziałach
    # osi; średnie poza zakresem rysunku pomijamy — przy n = 1 dla infolinii ok. 3%).
    edges <- seq(b$xlo, b$xhi, length.out = S3P_E1_BINS + 1)
    counts <- s3p_ec_seed(20261106, lapply(seq_len(S3P_E1_N), function(n) {
      m <- rowMeans(matrix(b$draw(n * S3P_E1_SIMS), nrow = S3P_E1_SIMS))
      as.integer(table(cut(m, edges, include.lowest = TRUE)))
    }))
    list(key = b$key, short = b$short, unit = b$unit, axis = b$axis, story = b$story,
         one = b$one, many = b$many, sims = S3P_E1_SIMS, bins = S3P_E1_BINS, counts = counts,
         mu = b$mu, sd = b$sd, skew = b$skew, xlo = b$xlo, xhi = b$xhi,
         rows = lapply(rows, function(r) signif(r, 4)), tv = round(tv, 4))
  })
})

s3p_e1_json <- jsonlite::toJSON(s3p_e1_config, auto_unbox = TRUE, digits = NA)
s3p_e1_dist_group <- function() {
  lc_group("Rozkład pojedynczej obserwacji", tags$div(class = "lc-seg", role = "group",
    lapply(seq_along(s3p_e1_config), function(i) {
      tags$button(type = "button", `data-m-dist` = i - 1,
        `aria-pressed` = if (i == 1) "true" else "false", s3p_e1_config[[i]]$short)
    })))
}

# Mapa (heatmapa) z podpisami pierwszego i ostatniego wiersza.
s3p_e1_ui <- s3p_panel("Ryc. 6.5", "Mapa zbieżności",
  tags$div(class = "lc-ecmap",
    `data-config` = s3p_e1_json,
    lc_toolbar(
      s3p_e1_dist_group(),
      lc_readouts(
        lc_readout("n", tags$span(`data-role` = "n", "1")),
        lc_readout("Skośność średniej", tags$span(`data-role` = "skew", "–")),
        lc_readout("Rozbieżność z dzwonem", tags$span(`data-role` = "tv", "–"),
                   color = "var(--upwr-accent)")
      ),
      lc_chart_zoom_button()
    ),
    tags$p(class = "lc-ecmap-story", `data-role` = "story"),
    tags$div(class = "lc-ecmap-canvas",
      tags$div(class = "lc-ec", `data-role` = "map", role = "img",
               `aria-label` = "Mapa gęstości średniej dla n od 1 do 40 i krzywa dla wybranego n na tle przybliżenia normalnego")),
    lc_caption("Każdy wiersz to osobny eksperyment: wiele razy losujemy n obserwacji i liczymy ich średnią; wiersz pokazuje z góry histogram tych średnich (ciemniej = częściej, względem najczęstszej wartości w wierszu). Wiersze nie kumulują się. Najedź na wiersz albo użyj Tab i strzałek ↑/↓: pod mapą histogram 2000 wylosowanych średnich dla tego n, a linia to dzwon przewidziany przez CTG.")
  )
)


# ============================================================================
# (E2) PIASKOWNICA „ZAZNACZ PRZEDZIAŁ”
# ============================================================================

s3p_e2_config <- s3p_ec_seed(20261105, {
  x <- round(rlnorm(2000, log(175), 0.14), 1)
  list(values = x, mean = round(mean(x), 2), sd = round(sd(x), 2),
       lo = 90, hi = 300, bin = 5, a = 160, b = 200)
})

s3p_e2_ui <- s3p_panel("Ryc. 5.7", "Zaznacz przedział",
  tags$div(class = "lc-brushx",
    `data-config` = jsonlite::toJSON(s3p_e2_config, auto_unbox = TRUE, digits = NA),
    lc_toolbar(
      lc_group("Gotowe przedziały", tags$div(class = "lc-seg", role = "group",
          tags$button(type = "button", `data-b-preset` = "mid", `aria-pressed` = "true", "160–200 g"),
          tags$button(type = "button", `data-b-preset` = "big", `aria-pressed` = "false", "Duże > 250 g"),
          tags$button(type = "button", `data-b-preset` = "small", `aria-pressed` = "false", "Małe < 120 g"))),
      lc_readouts(
        lc_readout("Przedział", tags$span(`data-role` = "range", "–")),
        lc_readout("W danych", tags$span(`data-role` = "data", "–")),
        lc_readout("Model normalny", tags$span(`data-role` = "model", "–"), color = "var(--upwr-accent)"),
        lc_readout("Dane / model", tags$span(`data-role` = "ratio", "–"))
      ),
      lc_chart_zoom_button()
    ),
    tags$div(class = "lc-brushx-canvas",
      tags$div(class = "lc-ec", `data-role` = "hist", role = "img",
               `aria-label` = "Histogram masy 2000 jabłek z dopasowaną krzywą normalną i zaznaczonym przedziałem")),
    lc_caption("Przeciągnij po wykresie, żeby zaznaczyć przedział, albo przesuń granice Tabem i strzałkami. Kółko myszy lub suwak pod wykresem przybliżają fragment osi.")
  )
)

s3p_ec_css <- tags$style(HTML("
  .lc-ecmap-canvas { position: relative; aspect-ratio: 1.25 / 1; }
  .lc-ecmap[data-lc-chart-large='true'] .lc-ecmap-canvas { aspect-ratio: 1 / 1; }
  .lc-ecmap-story { margin: .2em 0 .4em; color: var(--upwr-ink-soft); }
  .lc-brushx-canvas { position: relative; aspect-ratio: 1.9 / 1; touch-action: none; }
  .lc-brushx[data-lc-chart-large='true'] .lc-brushx-canvas { aspect-ratio: 1.4 / 1; }
  @media (max-width: 640px) {
    .lc-ecmap-canvas { aspect-ratio: 0.8 / 1; }
    .lc-brushx-canvas { aspect-ratio: 1.1 / 1; }
  }
"))

# ============================================================================
# RYC. 4.2 „ILE OSÓB MA DOKŁADNIE 170 CM?” — lupa (zaakceptowana 2026-11-05)
# ============================================================================
# Ta sama populacja co Ryc. 4.2 (s3p_4a_pop, milion osób z N(170, 8)).
# Oś x przybliża się do 170 cm, pas ma stałą szerokość na ekranie; liczba
# osób w pasie maleje dziesięciokrotnie z każdym krokiem dokładności.

s3p_4ax_config <- local({
  steps <- list(list(key = "cm", lab = "1 cm", d = 1), list(key = "mm", lab = "1 mm", d = 0.1),
                list(key = "mm01", lab = "0.1 mm", d = 0.01), list(key = "mm001", lab = "0.01 mm", d = 0.001),
                list(key = "ideal", lab = "Idealny pomiar", d = 0))
  lapply(steps, function(s) {
    s$count <- if (s$d > 0) sum(abs(s3p_4a_pop - 170) < s$d / 2) else 0L
    s
  })
})

s3p_4ax_seg <- function() {
  lc_group("Dokładność pomiaru", tags$div(class = "lc-seg", role = "group",
    lapply(seq_along(s3p_4ax_config), function(i) tags$button(type = "button", `data-p-step` = i - 1,
      `aria-pressed` = if (i == 1) "true" else "false", s3p_4ax_config[[i]]$lab))))
}
s3p_4ax_read <- function() {
  lc_readouts(
    lc_readout("Osób z wynikiem „170”", tags$span(`data-role` = "count", "–"), color = "var(--upwr-accent)"),
    lc_readout("Odsetek", tags$span(`data-role` = "pct", "–")))
}

s3p_4aa_ui <- s3p_panel("Ryc. 4.2", "Ile osób ma dokładnie 170 cm?",
  tags$div(class = "lc-zoom170", `data-config` = jsonlite::toJSON(s3p_4ax_config, auto_unbox = TRUE),
    lc_toolbar(s3p_4ax_seg(), s3p_4ax_read(), lc_chart_zoom_button()),
    tags$div(class = "lc-zoom170-canvas",
      tags$div(class = "lc-ec", `data-role` = "zoom", role = "img",
               `aria-label` = "Gęstość wzrostu N(170, 8) w coraz większym przybliżeniu wokół 170 cm i pas osób z wynikiem 170")),
    lc_caption("Każdy krok przybliża oś dziesięciokrotnie: pas „170 cm” wygląda tak samo, ale obejmuje dziesięć razy mniej osób. Miniatura u góry pokazuje, jaki wycinek całego rozkładu oglądamy.")
  )
)

s3p_4ax_css <- tags$style(HTML("
  .lc-zoom170-canvas { position: relative; aspect-ratio: 1.8 / 1; }
  .lc-zoom170[data-lc-chart-large='true'] .lc-zoom170-canvas { aspect-ratio: 1.4 / 1; }
  @media (max-width: 640px) {
    .lc-zoom170-canvas { aspect-ratio: 1 / 1; }
  }
"))
