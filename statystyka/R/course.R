# course.R
# Konfiguracja kursu dla wspólnego rdzenia wykładów (shared/).
# Sourcowany w app.R przed plikami z shared/; obecność tego pliku oznacza
# też katalog przedmiotu (hub, .find_app_dir()).

.LC_COURSE <- list(
  title     = "Statystyka",
  logo_mark = "Σ",

  # Moduły w górnym pasku
  modules = list(
    list(num = "0",    slug = "wstep",      title = "Dane i populacja",            short = "Wstęp",        href = "#"),
    list(num = "I",    slug = "opisowa",    title = "Statystyka opisowa",          short = "Opisowa",      href = "#"),
    list(num = "II",   slug = "zwiazki",    title = "Związki dwóch zmiennych",     short = "Związki",      href = "#"),
    list(num = "III",  slug = "rozklady",   title = "Rozkłady prawdopodobieństwa", short = "Rozkłady",     href = "#"),
    list(num = "IV",   slug = "przedzialy", title = "Przedziały ufności",          short = "Przedziały",   href = "#"),
    list(num = "V",    slug = "testowanie", title = "Testowanie",                  short = "Testowanie",   href = "#"),
    list(num = "VI",   slug = "zalozenia",  title = "Założenia testów",            short = "Założenia",    href = "#"),
    list(num = "VII",  slug = "regresja",   title = "Regresja",                    short = "Regresja",     href = "#"),
    list(num = "VIII", slug = "dane",       title = "Dobre dane",                  short = "Dane",         href = "#"),
    list(num = "IX",   slug = "case",       title = "Case studies",                short = "Case studies", href = "#"),
    list(num = "X",    slug = "projekt",    title = "Projekt badawczy",            short = "Projekt",      href = "#")
  ),

  # Mapowanie lecture_id → slug modułu
  lecture_module = list(
    "dane-i-populacja"            = "wstep",
    "typy-danych"                 = "opisowa",
    "zwiazki-zmiennych"           = "zwiazki",
    "rozklady-prawdopodobienstwa" = "rozklady",
    "przedzialy-ufnosci"          = "przedzialy",
    "wnioskowanie-statystyczne"   = "testowanie",
    "zalozenia-testow"            = "zalozenia",
    "regresja"                    = "regresja",
    "dobre-dane"                  = "dane",
    "case-studies"                = "case",
    "projekt-badawczy"            = "projekt"
  ),

  # chapter_mode "single": rozdział wstawiany do strony przy wejściu.
  # plot_scaling FALSE: wykresy R w rozmiarze z theme_upwr(), bez skalowania
  # czcionek w zoom_plot_server().
  chapter_mode = "single",
  plot_scaling = FALSE,

  # Arkusze kursu z <kurs>/R/, ładowane po shared/shared_styles.css.
  styles = character(0)
)
