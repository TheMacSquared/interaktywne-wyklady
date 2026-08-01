#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
quick <- "--quick" %in% args

script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

scripts_dir <- script_dir()
stat_root <- normalizePath(file.path(scripts_dir, ".."), mustWork = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")

run_check <- function(label, script, extra_args = character()) {
  cat(sprintf("\n== %s ==\n", label))
  status <- system2(rscript, c(script, extra_args))
  if (!identical(status, 0L)) {
    stop(sprintf("Kontrola '%s' zakończyła się błędem (status %s).",
                 label, status), call. = FALSE)
  }
}

run_check("Zależności", file.path(scripts_dir, "check_dependencies.R"))
run_check("Kontrakt designu", file.path(scripts_dir, "check_design_contract.R"),
          "--strict")

cat(sprintf("\n== Testy testthat (%s) ==\n", if (quick) "quick" else "pełne"))
Sys.setenv(
  STATYSTYKA_ROOT = stat_root,
  STATYSTYKA_SKIP_APP_SMOKE = if (quick) "true" else "false"
)

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Pakiet 'testthat' jest wymagany. Zainstaluj: install.packages('testthat')",
       call. = FALSE)
}

testthat::test_dir(
  file.path(stat_root, "tests", "testthat"),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

cat("\nWszystkie kontrole zakończone powodzeniem.\n")
