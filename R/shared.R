# ============================================================================
# WSPOLNE FUNKCJE POMOCNICZE
# Zrodlowane przez kazdy app.R po ustaleniu app_dir i project_root
# ============================================================================

# Theme dla wszystkich wykresow w projekcie.
# base_family = "" -> ggplot uzyje domyslnego fontu systemowego (Arial/sans).
# Jesli uzytkownik zarejestruje Atkinson Hyperlegible przez showtext, mozna
# wywolac theme_educational(base_family = "Atkinson Hyperlegible").
theme_educational <- function(base_size = 14, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title        = element_text(face = "bold", size = base_size + 2,
                                       color = "#2c3e50"),
      plot.title.position = "plot",
      plot.subtitle     = element_text(color = "#7f8c8d", size = base_size - 1),
      plot.caption      = element_text(color = "#95a5a6", size = base_size - 2),
      axis.title        = element_text(color = "#34495e"),
      axis.text         = element_text(color = "#5d6d7e"),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "#ecf0f1"),
      strip.background  = element_blank(),
      strip.text        = element_text(face = "bold", color = "#34495e"),
      legend.position   = "bottom",
      legend.title      = element_text(face = "bold")
    )
}

# Paleta semantyczna projektu (kolory zgodne z shared_styles.css)
lecture_palette <- c(
  primary   = "#3498db",  # niebieski
  secondary = "#e74c3c",  # czerwony
  success   = "#27ae60",  # zielony
  warning   = "#f39c12",  # pomaranczowy
  dark      = "#2c3e50",  # ciemny
  purple    = "#9b59b6",
  teal      = "#1abc9c"
)

scale_color_lecture <- function(...) {
  ggplot2::scale_color_manual(values = unname(lecture_palette), ...)
}
scale_fill_lecture <- function(...) {
  ggplot2::scale_fill_manual(values = unname(lecture_palette), ...)
}
