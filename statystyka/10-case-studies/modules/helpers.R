# ============================================================================
# FUNKCJE POMOCNICZE - Case studies
# ============================================================================

case_explore   <- unname(upwr_cat["niebo"])
case_test      <- unname(upwr_cat["wrzos"])
case_model     <- unname(upwr_cat["szalwia"])
case_conclude  <- unname(upwr_cat["bursztyn"])
case_highlight <- upwr_accent
case_reference <- upwr_secondary
case_muted     <- upwr_reference


# Formatowanie wyniku testu
format_decision <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) {
    list(text = "Odrzucamy H₀", color = case_highlight, icon = "✗")
  } else {
    list(text = "Brak podstaw do odrzucenia H₀", color = case_model, icon = "✓")
  }
}
