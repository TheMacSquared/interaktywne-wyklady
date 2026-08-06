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
  "01-typy-danych",
  "02-rozklady-prawdopodobienstwa",
  "03-przedzialy-ufnosci",
  "04-wnioskowanie-statystyczne",
  "05-zalozenia-testow",
  "06-regresja",
  "07-symulacje-statystyczne",
  "08-metody-bayesowskie",
  "09-dobre-dane",
  "10-case-studies",
  "11-kierunkowe",
  "12-projekt-badawczy",
  "szeregi-czasowe"
)
