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
    )
  )
)

bananpol_event_meta <- function(event_id) {
  event <- bananpol$events[[event_id]]
  if (is.null(event)) {
    stop(sprintf("Nieznane zdarzenie Bananpolu: %s", event_id), call. = FALSE)
  }
  event
}
