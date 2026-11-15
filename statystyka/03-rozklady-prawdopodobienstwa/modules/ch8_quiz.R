# ============================================================================
# CHAPTER 8: Quiz - dopasuj rozklad do zjawiska
# ============================================================================

# Wczytaj pytania z JSON
.load_quiz_questions <- function() {
  json_path <- file.path(app_dir, "modules", "quiz_rozklady.json")
  jsonlite::fromJSON(json_path, simplifyDataFrame = FALSE)$questions
}

# Opcje (labele) dla rozkladow
QUIZ_OPTIONS <- list(
  dwumianowy   = "Rozkład dwumianowy",
  poissona     = "Rozkład Poissona",
  normalny     = "Rozkład normalny",
  geometryczny = "Rozkład geometryczny",
  lognormalny  = "Rozkład log-normalny",
  wykladniczy  = "Rozkład wykładniczy"
)

QUIZ_MAX_QUESTIONS <- 10
QUIZ_EXAMPLE_N <- 500

# Przykładowa realizacja po odpowiedzi: 500 losowań z modelu pytania (pole
# "example" w quiz_rozklady.json), histogram odsetków i rozkład teoretyczny
# w tych samych przedziałach. Wykres rysuje quiz_chart.js (ECharts).
quiz_example_config <- function(ex, n = QUIZ_EXAMPLE_N) {
  f <- function(v) lc_fmt(v, if (abs(v) >= 100) 0 else if (abs(v) >= 1) 2 else 3)
  # Ułamek tylko tam, gdzie zapis dziesiętny byłby nieskończony (1/6, 1/13), nie dla 0.05.
  frac <- function(p) if (abs(1 / p - round(1 / p)) < 1e-6 && abs(p - round(p, 4)) > 1e-9) paste0("1/", round(1 / p)) else f(p)
  spec <- switch(ex$dist,
    binom = list(r = function(k) rbinom(k, ex$n, ex$p), q = function(u) qbinom(u, ex$n, ex$p),
                 cdf = function(x) pbinom(x, ex$n, ex$p), disc = TRUE,
                 model = sprintf("B(%d, %s)", ex$n, frac(ex$p))),
    pois  = list(r = function(k) rpois(k, ex$lambda), q = function(u) qpois(u, ex$lambda),
                 cdf = function(x) ppois(x, ex$lambda), disc = TRUE,
                 model = sprintf("Pois(λ = %s)", f(ex$lambda))),
    # Geometryczny jak w wykładzie: liczba prób do pierwszego sukcesu (1, 2, …).
    geom  = list(r = function(k) rgeom(k, ex$p) + 1, q = function(u) qgeom(u, ex$p) + 1,
                 cdf = function(x) pgeom(x - 1, ex$p), disc = TRUE,
                 model = sprintf("Geom(p = %s)", frac(ex$p))),
    norm  = list(r = function(k) rnorm(k, ex$mean, ex$sd), q = function(u) qnorm(u, ex$mean, ex$sd),
                 cdf = function(x) pnorm(x, ex$mean, ex$sd), disc = FALSE,
                 model = sprintf("N(%s, %s)", f(ex$mean), f(ex$sd))),
    lnorm = list(r = function(k) rlnorm(k, log(ex$median), ex$sdlog), q = function(u) qlnorm(u, log(ex$median), ex$sdlog),
                 cdf = function(x) plnorm(x, log(ex$median), ex$sdlog), disc = FALSE,
                 model = sprintf("LogN(mediana %s, σ log = %s)", f(ex$median), f(ex$sdlog))),
    exp   = list(r = function(k) rexp(k, 1 / ex$mean), q = function(u) qexp(u, 1 / ex$mean),
                 cdf = function(x) pexp(x, 1 / ex$mean), disc = FALSE,
                 model = sprintf("Exp(średnia %s)", f(ex$mean))))
  x <- spec$r(n)
  lo <- spec$q(0.001); hi <- spec$q(if (spec$disc) 0.999 else 0.995)
  if (spec$disc && hi - lo <= 40) {
    # Każda wartość osobno: słupki odsetków i punkty P(X = k).
    ks <- seq(lo, hi)
    edges <- c(ks - 0.5, hi + 0.5)
    labels <- as.character(ks)
  } else {
    edges <- pretty(c(lo, hi), n = 28)
    if (spec$disc) edges <- unique(floor(edges)) - 0.5
    mids <- (head(edges, -1) + tail(edges, -1)) / 2
    digits <- if (all(abs(mids - round(mids)) < 1e-9)) 0 else if (diff(edges)[1] >= 1) 1 else 2
    labels <- vapply(mids, function(v) lc_fmt(v, digits), "")
  }
  inside <- x >= min(edges) & x < max(edges)
  emp <- as.numeric(table(cut(x[inside], edges, right = FALSE))) / n
  # Dyskretne: krawędzie k − 0.5, więc floor daje P(X ≤ k) − P(X ≤ k − 1).
  theo <- diff(spec$cdf(if (spec$disc) floor(edges) else edges))
  shown <- head(x, 8)
  # Przykładowe wyniki ciągłe: ok. trzech cyfr znaczących, ta sama liczba miejsc
  # po przecinku dla wszystkich (np. 37.5 min, 4.01, 204 cm).
  digits <- max(0, min(3, 2 - floor(log10(max(abs(median(x)), 1e-9)))))
  list(model = spec$model, x = ex$x, discrete = spec$disc, perValue = spec$disc && hi - lo <= 40,
       labels = labels, emp = round(emp, 4), theo = round(theo, 5), n = n,
       outside = sum(!inside),
       sample = vapply(shown, function(v) if (spec$disc) as.character(v) else formatC(v, format = "f", digits = digits), ""))
}

# Blok „przykładowa realizacja” pokazywany po odpowiedzi: model, pierwsze wyniki,
# histogram i podpis. Losowanie ma stałe ziarno (numer pytania), więc strona
# buduje się zawsze tak samo.
quiz_example_block <- function(q) {
  ex <- q$example
  if (is.null(ex)) return(NULL)
  old_seed <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old_seed)) rm(".Random.seed", envir = globalenv()) else
    assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  set.seed(2026L + as.integer(q$id))
  cfg <- quiz_example_config(ex)
  tags$div(class = "lc-quizex",
    `data-config` = jsonlite::toJSON(cfg, auto_unbox = TRUE, digits = NA),
    lc_readouts(lc_readout("Model", cfg$model, color = upwr_accent)),
    tags$p(class = "lc-quizex-sample", "Pierwsze wylosowane wyniki: ",
           tags$span(paste(c(cfg$sample, "…"), collapse = ", "))),
    tags$div(class = "lc-quizex-canvas",
      tags$div(class = "lc-ec", `data-role` = "hist", role = "img",
        `aria-label` = sprintf("Histogram %d losowań z rozkładu %s i rozkład teoretyczny", cfg$n, cfg$model))),
    lc_caption(sprintf("%d przykładowych realizacji: słupki to odsetek %s, %s to rozkład teoretyczny %s.%s",
      cfg$n, if (cfg$perValue) "każdego wyniku" else "wyników w każdym przedziale",
      if (cfg$discrete) "punkty" else "linia", cfg$model,
      if (cfg$outside > 0) sprintf(" Pominięto %d skrajnych wyników spoza osi.", cfg$outside) else ""))
  )
}

# ============================================================================
# UI
# ============================================================================

ch8_ui <- list(
  id = "ch-quiz", num = "07", title = "Quiz",
  content = tagList(

    lc_chapter_hero(
      kicker = "Rozdział 07 · Rozkłady prawdopodobieństwa",
      num    = "07",
      title  = "Quiz.",
      lead   = "Ćwiczenie: rozpoznawanie rozkładów w praktyce."
    ),

    lc_h2("ch8-quiz", "Quiz: dopasuj rozkład do zjawiska"),

    tagList(
      p("Przeczytaj opis zjawiska i wybierz rozkład, który najlepiej je modeluje.
        Każde pytanie ma ", tags$b("3 opcje"),
        " — jedną poprawną i dwie losowo wybrane błędne."),
      p("Quiz losuje ", tags$b("10 pytań"), " z puli 60. Można go powtarzać
        wielokrotnie — za każdym razem pojawia się inny zestaw.")
    ),

    figure_panel(
      label = "Quiz",
      title = "Dopasuj rozkład do zjawiska",
      full_width = TRUE,

      # Quiz prowadzi przeglądarka (shared/lc_quiz.js): losowanie pytań i dwóch
      # błędnych odpowiedzi, ocena, wynik. Przykładowe realizacje liczy R przy
      # budowaniu strony (stałe ziarno), wykres rysuje quiz_chart.js.
      lc_tile_quiz("ch8", .load_quiz_questions(), n = QUIZ_MAX_QUESTIONS,
        options = lapply(names(QUIZ_OPTIONS), function(v) list(value = v, label = QUIZ_OPTIONS[[v]])),
        option_count = 3, tiles_class = "quiz-cols-3",
        extra = quiz_example_block,
        labels = list(question = "Pytanie", score = "Wynik", result = "Wynik",
                      correct = "Poprawne odpowiedzi"),
        colors = list(letter = NULL, question = upwr_secondary, correct = upwr_secondary,
          score_levels = list(start = upwr_secondary, ok = unname(upwr_cat["szalwia"]),
                              mid = unname(upwr_cat["bursztyn"]), low = unname(upwr_cat["terakota"]))),
        summary = c(
          "90" = "Ćwiczenie zakończone celująco!",
          "70" = "Dobry wynik!",
          "50" = "Nieźle, ale warto powtórzyć.",
          "0"  = "Powtórz materiał z wcześniejszych rozdziałów."
        ))
    ),

    lc_chapter_next(
      num       = "08",
      title     = "Ćwiczenia",
      lead      = "zadania z rozkładów prawdopodobieństwa do samodzielnego rozwiązania.",
      target_id = "ch-cwiczenia"
    )
  )
)
