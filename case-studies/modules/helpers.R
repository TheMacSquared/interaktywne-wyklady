# ============================================================================
# FUNKCJE POMOCNICZE - Case studies
# ============================================================================


# Formatowanie wyniku testu
format_decision <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) {
    list(text = "Odrzucamy H\u2080", color = "#e74c3c", icon = "\u2717")
  } else {
    list(text = "Brak podstaw do odrzucenia H\u2080", color = "#27ae60", icon = "\u2713")
  }
}
