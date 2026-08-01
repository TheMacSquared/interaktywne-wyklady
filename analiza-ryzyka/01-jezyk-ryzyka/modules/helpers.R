# ==========================================================================
# FUNKCJE CZYSTE I WSPÓLNE DLA WYKŁADU 01
# ==========================================================================

risk_term_labels <- c(
  hazard = "Zagrożenie",
  exposure = "Ekspozycja",
  event = "Zdarzenie",
  consequence = "Skutek",
  safeguard = "Zabezpieczenie"
)

risk_scenario_items <- data.frame(
  id = c("peel", "traffic", "slip", "injury", "cleanup"),
  text = c(
    "Skórka od banana leży na przejściu.",
    "Pracownik przechodzi tędy z dokumentami zasłaniającymi część pola widzenia.",
    "But traci przyczepność, a pracownik upada.",
    "Upadek kończy się złamaniem nadgarstka.",
    "Przejście jest kontrolowane i sprzątane co 30 minut."
  ),
  correct = c("hazard", "exposure", "event", "consequence", "safeguard"),
  explanation = c(
    "Skórka jest źródłem możliwości szkody, ale sama nie jest jeszcze wypadkiem.",
    "Ekspozycja opisuje kontakt człowieka z zagrożeniem w określonych warunkach.",
    "Zdarzenie mówi, co faktycznie zaszło: utrata przyczepności i upadek.",
    "Skutek opisuje następstwo zdarzenia, tutaj uraz pracownika.",
    "Kontrola i sprzątanie mają przerwać drogę od zagrożenia do zdarzenia."
  ),
  stringsAsFactors = FALSE
)

score_risk_classification <- function(answers) {
  expected_ids <- risk_scenario_items$id
  if (is.null(names(answers)) || !all(expected_ids %in% names(answers))) {
    stop("Odpowiedzi muszą być nazwane identyfikatorami wszystkich elementów.",
         call. = FALSE)
  }
  selected <- unname(answers[expected_ids])
  correct <- selected == risk_scenario_items$correct
  correct[is.na(correct)] <- FALSE
  list(
    correct = correct,
    score = sum(correct),
    total = length(correct)
  )
}

append_bernoulli_history <- function(history, n, probability, draws = NULL) {
  if (length(n) != 1 || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("n musi być dodatnią liczbą całkowitą.", call. = FALSE)
  }
  if (length(probability) != 1 || is.na(probability) ||
      probability < 0 || probability > 1) {
    stop("Prawdopodobieństwo musi należeć do przedziału [0, 1].", call. = FALSE)
  }

  if (is.null(draws)) {
    draws <- stats::rbinom(as.integer(n), size = 1, prob = probability)
  } else {
    if (length(draws) != n || any(!draws %in% c(0, 1))) {
      stop("draws musi zawierać n wartości 0/1.", call. = FALSE)
    }
    draws <- as.integer(draws)
  }

  c(as.integer(history), draws)
}

cumulative_frequency <- function(history) {
  if (length(history) == 0) {
    return(data.frame(trial = integer(), frequency = numeric()))
  }
  if (any(!history %in% c(0, 1))) {
    stop("Historia musi składać się z wartości 0/1.", call. = FALSE)
  }
  data.frame(
    trial = seq_along(history),
    frequency = cumsum(history) / seq_along(history)
  )
}

classical_probability <- function(favourable, total) {
  values <- c(favourable, total)
  if (any(lengths(list(favourable, total)) != 1) || any(is.na(values)) ||
      any(values != as.integer(values)) || total <= 0 || favourable < 0 ||
      favourable > total) {
    stop("Wymagane są całkowite liczby 0 ≤ favourable ≤ total.", call. = FALSE)
  }
  favourable / total
}

build_pallet_grid <- function(favourable, total = 24L, columns = 6L) {
  classical_probability(favourable, total)
  if (columns < 1 || columns != as.integer(columns)) {
    stop("columns musi być dodatnią liczbą całkowitą.", call. = FALSE)
  }
  ids <- seq_len(total)
  data.frame(
    id = ids,
    column = (ids - 1L) %% columns + 1L,
    row = (ids - 1L) %/% columns + 1L,
    favourable = ids <= favourable
  )
}

event_set_counts <- function(total, n_a, n_b, overlap) {
  values <- c(total, n_a, n_b, overlap)
  if (any(is.na(values)) || any(values != as.integer(values)) || total <= 0 ||
      n_a < 0 || n_b < 0 || n_a > total || n_b > total) {
    stop("Liczebności zdarzeń muszą być całkowite i należeć do przestrzeni.",
         call. = FALSE)
  }

  min_overlap <- max(0L, n_a + n_b - total)
  max_overlap <- min(n_a, n_b)
  if (overlap < min_overlap || overlap > max_overlap) {
    stop("Część wspólna jest niezgodna z liczebnościami A i B.", call. = FALSE)
  }

  c(
    "A i B" = overlap,
    "Tylko A" = n_a - overlap,
    "Tylko B" = n_b - overlap,
    "Ani A, ani B" = total - (n_a + n_b - overlap)
  )
}

build_event_grid <- function(total, n_a, n_b, overlap, columns = 10L) {
  counts <- event_set_counts(total, n_a, n_b, overlap)
  if (columns < 1 || columns != as.integer(columns)) {
    stop("columns musi być dodatnią liczbą całkowitą.", call. = FALSE)
  }

  status <- rep(names(counts), times = unname(counts))
  ids <- seq_len(total)
  data.frame(
    id = ids,
    column = (ids - 1L) %% columns + 1L,
    row = (ids - 1L) %/% columns + 1L,
    status = factor(status, levels = names(counts))
  )
}

format_probability_pl <- function(probability, digits = 3L) {
  if (length(probability) != 1 || is.na(probability) ||
      probability < 0 || probability > 1) {
    stop("Prawdopodobieństwo musi należeć do przedziału [0, 1].", call. = FALSE)
  }
  decimal <- formatC(probability, format = "f", digits = digits)
  paste0(gsub("\\.", ",", decimal), " (", round(100 * probability, 1), "%)")
}

quiz_questions <- jsonlite::fromJSON(
  file.path(app_dir, "modules", "quiz_questions.json"),
  simplifyVector = FALSE
)

quiz_choices <- function(question) {
  option_codes <- names(question$options)
  option_labels <- unlist(question$options, use.names = FALSE)
  stats::setNames(option_codes, option_labels)
}
