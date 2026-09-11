stat_root <- Sys.getenv("STATYSTYKA2_ROOT", unset = "")
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
  "01-symulacje-statystyczne",
  "02-metody-bayesowskie",
  "03-kierunkowe",
  "04-szeregi-czasowe"
)
