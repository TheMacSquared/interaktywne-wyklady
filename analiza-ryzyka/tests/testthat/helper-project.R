risk_root <- Sys.getenv("ANALIZA_RYZYKA_ROOT", unset = "")
if (!nzchar(risk_root)) {
  risk_root <- normalizePath(file.path(testthat::test_path(), "..", ".."),
    mustWork = TRUE
  )
}

app_dir <- file.path(risk_root, "01-jezyk-ryzyka")

expected_apps <- c(
  "01-jezyk-ryzyka" = 8L,
  "02-warunki" = 8L,
  "03-alarm-i-prawda" = 8L,
  "04-wiele-prob" = 8L,
  "05-do-zdarzenia" = 8L,
  "06-zmiennosc-i-prog" = 10L,
  "07-czas-zycia" = 11L,
  "08-niezawodnosc-systemu" = 12L,
  "09-drzewo-bledow" = 12L,
  "10-model-do-decyzji" = 12L
)

load_lecture_helpers <- function() {
  env <- new.env(parent = globalenv())
  env$app_dir <- app_dir
  sys.source(file.path(risk_root, "R", "palette.R"), envir = env)
  sys.source(file.path(app_dir, "modules", "helpers.R"), envir = env)
  env
}
