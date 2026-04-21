# ============================================================================
# WSPOLNE FUNKCJE POMOCNICZE
# Zrodlowane przez kazdy app.R po ustaleniu app_dir i project_root
# ============================================================================

# Kolory bazowe projektu (zgodne z shared_styles.css)
col_primary    <- "#3498db"    # niebieski
col_secondary  <- "#e74c3c"    # czerwony
col_success    <- "#27ae60"    # zielony
col_warning    <- "#f39c12"    # pomaranczowy
col_dark       <- "#2c3e50"    # ciemny
col_purple     <- "#9b59b6"    # fioletowy
col_teal       <- "#1abc9c"    # morski

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

# ============================================================================
# WSPOLNE FUNKCJE GENEROWANIA DANYCH
# Uzywane przez: przedzialy-ufnosci, rozklady-prawdopodobienstwa
# ============================================================================

# Generowanie proby z wybranego rozkladu (superset wszystkich aplikacji)
generate_population_sample <- function(dist_type, n) {
  switch(dist_type,
    "normal"      = rnorm(n, mean = 170, sd = 10),
    "exponential" = rexp(n, rate = 0.5),
    "uniform"     = runif(n, min = 0, max = 10),
    "bimodal"     = {
      k <- rbinom(n, 1, 0.5)
      ifelse(k == 1, rnorm(n, mean = 3, sd = 0.8), rnorm(n, mean = 7, sd = 0.8))
    },
    "skewed"      = rgamma(n, shape = 2, scale = 1.5),
    "u_shape"     = rbeta(n, 0.5, 0.5) * 10,
    "skewed_left" = 10 - rgamma(n, shape = 2, scale = 1.5),
    "die"         = sample(1:6, n, replace = TRUE),
    rnorm(n)
  )
}

# Parametry populacji dla wybranego rozkladu
get_population_params <- function(dist_type) {
  switch(dist_type,
    "normal"      = list(mu = 170, sigma = 10),
    "exponential" = list(mu = 2, sigma = 2),
    "uniform"     = list(mu = 5, sigma = sqrt(100/12)),
    "bimodal"     = list(mu = 5, sigma = sqrt(0.8^2 + 4)),
    "skewed"      = list(mu = 3, sigma = sqrt(2) * 1.5),
    "u_shape"     = list(mu = 5, sigma = sqrt(10^2 / 4)),
    "skewed_left" = list(mu = 10 - 2*1.5, sigma = sqrt(2) * 1.5),
    "die"         = list(mu = 3.5, sigma = sqrt(35/12)),
    list(mu = 0, sigma = 1)
  )
}

# Nazwy rozkladow po polsku (superset wszystkich aplikacji)
dist_names_pl <- c(
  "normal"      = "Normalny (wzrost)",
  "exponential" = "Wykładniczy (prawoskośny)",
  "uniform"     = "Jednostajny",
  "bimodal"     = "Dwumodalny",
  "skewed"      = "Prawoskosńny (Gamma)",
  "u_shape"     = "U-kształtny (Beta)",
  "skewed_left" = "Lewoskośny",
  "die"         = "Kostka (dyskretny)"
)
