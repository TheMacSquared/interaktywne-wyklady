# ============================================================================
# FUNKCJE POMOCNICZE - Case studies
# ============================================================================

# Theme dla wykresow
theme_case <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "#7f8c8d"),
      panel.grid.minor = element_blank()
    )
}

# Formatowanie wyniku testu
format_decision <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) {
    list(text = "Odrzucamy H\u2080", color = "#e74c3c", icon = "\u2717")
  } else {
    list(text = "Brak podstaw do odrzucenia H\u2080", color = "#27ae60", icon = "\u2713")
  }
}
