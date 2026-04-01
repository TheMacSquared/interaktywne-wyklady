# Wspólne funkcje, kolory i theme dla wszystkich wykładów
# Importowane w każdym .qmd przez: source("_common.R") w context: setup

library(ggplot2)
library(dplyr)

# ============================================================================
# KOLORY
# ============================================================================

col_primary    <- "#3498db"
col_secondary  <- "#e74c3c"
col_success    <- "#27ae60"
col_warning    <- "#f39c12"
col_dark       <- "#2c3e50"
col_purple     <- "#9b59b6"
col_teal       <- "#1abc9c"

# Kolory specyficzne
col_ci         <- "#3498db"
col_miss       <- "#e74c3c"
col_hit        <- "#27ae60"
col_estimate   <- "#f39c12"
col_true       <- "#9b59b6"

# Kolory rozkładów
col_uniform    <- "#3498db"
col_binomial   <- "#e74c3c"
col_poisson    <- "#1abc9c"
col_geometric  <- "#f39c12"
col_normal     <- "#3498db"
col_exponential <- "#e74c3c"
col_t_student  <- "#9b59b6"
col_chi_sq     <- "#e67e22"
col_lognormal  <- "#2ecc71"

# ============================================================================
# THEME
# ============================================================================

theme_lecture <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "#7f8c8d"),
      panel.grid.minor = element_blank()
    )
}

# Alias
theme_ci   <- theme_lecture
theme_prob <- theme_lecture

# ============================================================================
# HELPER: stat-box HTML
# ============================================================================

stat_box <- function(text, color = col_primary) {
  tags$span(
    style = paste0(
      "display: inline-block; padding: 8px 16px; margin: 4px;",
      "border-radius: 6px; font-weight: bold; font-size: 16px;",
      "color: white; min-width: 100px; text-align: center;",
      "background: ", color, ";"
    ),
    text
  )
}
