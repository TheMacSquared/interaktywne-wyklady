# ==========================================================================
# BANANPOL — WSPÓLNY MODEL PRZYPADKU
# Wszystkie liczby są fikcyjne i służą wyłącznie celom dydaktycznym.
# ==========================================================================

bananpol <- list(
  company = list(
    name = "Bananpol",
    profile = "Importer, dojrzewalnia i dystrybutor bananów",
    fictional = TRUE
  ),
  locations = c(
    "Rampa rozładunkowa",
    "Dojrzewalnia",
    "Chłodnia",
    "Magazyn wysokiego składowania",
    "Linia sortowania i pakowania",
    "Korytarz przy sortowni"
  ),
  events = list(
    corridor_slip = list(
      label = "Co najmniej jedno poślizgnięcie w korytarzu przy sortowni",
      unit = "dzień pracy korytarza",
      horizon = "jedna 8-godzinna zmiana",
      illustrative_probability = 0.08,
      source = "fikcyjny parametr dydaktyczny; nie jest estymacją rzeczywistego ryzyka"
    ),
    damaged_pallet = list(
      label = "Losowo wybrana paleta ma uszkodzone zabezpieczenie ładunku",
      unit = "wylosowana paleta",
      horizon = "jedna kontrola przyjęcia dostawy",
      source = "fikcyjny przykład definicji klasycznej"
    ),
    overheating_incident = list(
      label = "Incydent związany z przegrzaniem łożyska wentylatora",
      unit = "8-godzinna zmiana pracy wentylatora",
      horizon = "jedna zmiana",
      illustrative_probability = 0.012,
      source = "fikcyjny parametr dydaktyczny"
    ),
    uncontrolled_fire = list(
      label = "Nieopanowany pożar magazynu Bananpolu",
      unit = "rok pracy magazynu",
      horizon = "jeden rok",
      source = "fikcyjne zdarzenie szczytowe do ćwiczeń FTA"
    )
  ),
  conditional = list(
    population = 1000L,
    overheating_share = 0.10,
    incident_given_overheating = 0.12,
    incident_given_normal = 0.005,
    common_power_failure = 0.01,
    unit = "zmiana robocza",
    horizon = "1000 porównywalnych zmian",
    source = "fikcyjne parametry dydaktyczne"
  ),
  detector = list(
    prevalence = 0.01,
    sensitivity = 0.95,
    false_positive_rate = 0.05,
    population = 10000L,
    unit = "zmiana pracy dojrzewalni",
    horizon = "10 000 porównywalnych zmian",
    source = "fikcyjne parametry dydaktyczne"
  ),
  inspection = list(
    valve_failure_probability = 0.02,
    batch_size = 100L,
    pallet_defect_probability = 0.10,
    target_defects = 3L,
    unit = "kontrola jednego elementu",
    horizon = "jedna partia kontrolna",
    source = "fikcyjne parametry dydaktyczne"
  ),
  bearing = list(
    temperature_mean = 82,
    temperature_sd = 3,
    internal_threshold = 85,
    unit = "°C",
    horizon = "pomiar podczas ustalonego trybu pracy",
    source = "fikcyjny wewnętrzny próg demonstracyjny; nie jest normą techniczną"
  ),
  lifetime = list(
    exponential_mttf = 1500,
    weibull_shape = 2,
    weibull_scale = 1700,
    unit = "godzina pracy",
    horizon = "czas do awarii wentylatora",
    source = "fikcyjne parametry dydaktyczne"
  ),
  system = list(
    mission_time = 1000,
    fan_reliability = 0.92,
    detector_reliability = 0.95,
    power_reliability = 0.98,
    common_power_failure = 0.01,
    unit = "planowany czas misji",
    horizon = "1000 godzin",
    source = "fikcyjne parametry dydaktyczne"
  ),
  fta = list(
    initiation = 0.005,
    detection_failure = 0.05,
    suppression_failure = 0.08,
    unit = "rok pracy magazynu",
    horizon = "jeden rok",
    source = "fikcyjne parametry dydaktyczne"
  ),
  interventions = data.frame(
    id = c("detector", "inspection", "power", "fan"),
    label = c(
      "Lepszy czujnik", "Częstszy przegląd", "Niezależne zasilanie",
      "Dodatkowy wentylator"
    ),
    relative_reduction = c(0.35, 0.20, 0.55, 0.30),
    cost_index = c(2, 1, 4, 3),
    feasibility = c("wysoka", "wysoka", "średnia", "średnia"),
    stringsAsFactors = FALSE
  )
)

bananpol_meta_fields <- c("unit", "horizon", "source")

# Rejestr liczbowych parametrów używanych w blokach 02–10. Lista jest
# celowo płaska: testy i prowadzący mogą szybko sprawdzić jednostkę, horyzont,
# fikcyjność oraz dopuszczalny zakres każdej liczby.
bananpol_parameters <- data.frame(
  id = c(
    "conditional.population", "conditional.overheating_share",
    "conditional.incident_given_overheating", "conditional.incident_given_normal",
    "conditional.common_power_failure", "detector.prevalence",
    "detector.sensitivity", "detector.false_positive_rate", "detector.population",
    "inspection.valve_failure_probability", "inspection.batch_size",
    "inspection.pallet_defect_probability", "inspection.target_defects",
    "bearing.temperature_mean", "bearing.temperature_sd", "bearing.internal_threshold",
    "lifetime.exponential_mttf", "lifetime.weibull_shape", "lifetime.weibull_scale",
    "system.mission_time", "system.fan_reliability", "system.detector_reliability",
    "system.power_reliability", "system.common_power_failure", "fta.initiation",
    "fta.detection_failure", "fta.suppression_failure"
  ),
  value = c(
    1000, .10, .12, .005, .01, .01, .95, .05, 10000, .02, 100, .10, 3,
    82, 3, 85, 1500, 2, 1700, 1000, .92, .95, .98, .01, .005, .05, .08
  ),
  unit = c(
    "zmiana", rep("udział zmian", 4), "udział zmian",
    rep("prawdopodobieństwo warunkowe", 2), "zmiana",
    "udział elementów", "element", "udział palet", "wykrycie",
    rep("°C", 3), "godzina", "bezwymiarowy", "godzina", "godzina",
    rep("prawdopodobieństwo misji", 4), rep("prawdopodobieństwo roczne", 3)
  ),
  horizon = c(
    rep("1000 porównywalnych zmian", 5), rep("10 000 porównywalnych zmian", 4),
    rep("jedna partia kontrolna", 4), rep("ustalony tryb pracy", 3),
    rep("czas do awarii wentylatora", 3), rep("misja 1000 godzin", 5),
    rep("jeden rok pracy magazynu", 3)
  ),
  source = rep("fikcyjny parametr dydaktyczny Bananpolu", 27),
  fictional = rep(TRUE, 27),
  minimum = c(1, rep(0, 7), 1, 0, 1, 0, 1, -Inf, 0, -Inf, 0, 0, 0, 0, rep(0, 7)),
  maximum = c(Inf, rep(1, 7), Inf, 1, Inf, 1, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, rep(1, 7)),
  stringsAsFactors = FALSE
)

bananpol_validate_parameters <- function(parameters = bananpol_parameters) {
  required <- c(
    "id", "value", "unit", "horizon", "source", "fictional",
    "minimum", "maximum"
  )
  if (length(setdiff(required, names(parameters)))) {
    stop("Rejestr parametrów nie zawiera wszystkich wymaganych pól.", call. = FALSE)
  }
  if (anyDuplicated(parameters$id) || any(!nzchar(parameters$unit)) ||
    any(!nzchar(parameters$horizon)) || any(!nzchar(parameters$source)) ||
    any(!parameters$fictional) || any(parameters$value < parameters$minimum) ||
    any(parameters$value > parameters$maximum)) {
    stop("Rejestr parametrów Bananpolu jest niespójny.", call. = FALSE)
  }
  invisible(TRUE)
}

bananpol_validate_case <- function(case) {
  missing <- setdiff(bananpol_meta_fields, names(case))
  if (length(missing)) {
    stop(sprintf("Brak metadanych przypadku: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

bananpol_event_meta <- function(event_id) {
  event <- bananpol$events[[event_id]]
  if (is.null(event)) {
    stop(sprintf("Nieznane zdarzenie Bananpolu: %s", event_id), call. = FALSE)
  }
  event
}
