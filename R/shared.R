# ============================================================================
# WSPOLNE FUNKCJE POMOCNICZE
# Zrodlowane przez kazdy app.R po ustaleniu app_dir i project_root
# ============================================================================

# Theme dla wszystkich wykresow w projekcie
theme_educational <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = "#7f8c8d"),
      panel.grid.minor = element_blank()
    )
}
