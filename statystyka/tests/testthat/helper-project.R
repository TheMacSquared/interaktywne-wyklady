stat_root <- Sys.getenv("STATYSTYKA_ROOT", unset = "")
if (!nzchar(stat_root)) {
  stat_root <- normalizePath(file.path(testthat::test_path(), "..", ".."),
                             mustWork = TRUE)
}

load_module_helpers <- function(relative_path) {
  env <- new.env(parent = globalenv())
  sys.source(file.path(stat_root, "R", "palette.R"), envir = env)
  sys.source(file.path(stat_root, relative_path), envir = env)
  env
}

expected_apps <- c(
  "00-dane-i-populacja",
  "01-typy-danych",
  "02-zwiazki-zmiennych",
  "03-rozklady-prawdopodobienstwa",
  "04-przedzialy-ufnosci",
  "05-wnioskowanie-statystyczne",
  "06-zalozenia-testow",
  "07-regresja",
  "08-dobre-dane",
  "09-case-studies",
  "10-projekt-badawczy"
)
