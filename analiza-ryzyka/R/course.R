# course.R
# Konfiguracja kursu dla wspólnego rdzenia wykładów (shared/).
# Sourcowany w app.R przed plikami z shared/; obecność tego pliku oznacza
# też katalog przedmiotu (hub, .find_app_dir()).

.LC_COURSE <- list(
  title     = "Analiza ryzyka",
  logo_mark = "R",

  # Moduły w górnym pasku
  modules = list(
    list(num = "01", slug = "jezyk",       title = "Język ryzyka",         short = "Język",      href = "#"),
    list(num = "02", slug = "warunki",     title = "Warunki",              short = "Warunki",    href = "#"),
    list(num = "03", slug = "alarm",       title = "Alarm i prawda",       short = "Bayes",      href = "#"),
    list(num = "04", slug = "proby",       title = "Wiele prób",           short = "Próby",      href = "#"),
    list(num = "05", slug = "oczekiwanie", title = "Do zdarzenia",         short = "Czekanie",   href = "#"),
    list(num = "06", slug = "prog",        title = "Przekroczenie progu",  short = "Próg",       href = "#"),
    list(num = "07", slug = "czas-zycia",  title = "Czas życia",           short = "Czas życia", href = "#"),
    list(num = "08", slug = "systemy",     title = "Niezawodność systemu", short = "Systemy",    href = "#"),
    list(num = "09", slug = "fta",         title = "Drzewo błędów",        short = "FTA",        href = "#"),
    list(num = "10", slug = "decyzja",     title = "Od modelu do decyzji", short = "Decyzja",    href = "#")
  ),

  # Mapowanie lecture_id → slug modułu
  lecture_module = list(
    "jezyk-ryzyka"          = "jezyk",
    "warunki"               = "warunki",
    "alarm-i-prawda"        = "alarm",
    "wiele-prob"            = "proby",
    "do-zdarzenia"          = "oczekiwanie",
    "zmiennosc-i-prog"      = "prog",
    "czas-zycia"            = "czas-zycia",
    "niezawodnosc-systemu"  = "systemy",
    "drzewo-bledow"         = "fta",
    "model-do-decyzji"      = "decyzja"
  ),

  # chapter_mode "all": wszystkie rozdziały zamontowane od razu, nieaktywne
  # ukryte (kontrolki zachowują wartości).
  # plot_scaling TRUE: mniejsza baza theme_upwr() i skalowanie czcionek
  # w zoom_plot_server() (1.1x w tekście, 1.5x w oknie powiększenia).
  chapter_mode = "all",
  plot_scaling = TRUE,

  # Arkusze kursu z <kurs>/R/, ładowane po shared/shared_styles.css.
  styles = "course_styles.css"
)
