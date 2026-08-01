#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
tests_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE))
} else {
  normalizePath("statystyka/tests", mustWork = TRUE)
}

stat_root <- normalizePath(file.path(tests_dir, ".."), mustWork = TRUE)
Sys.setenv(STATYSTYKA_ROOT = stat_root)

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Pakiet 'testthat' jest wymagany do uruchomienia testów.", call. = FALSE)
}

testthat::test_dir(
  file.path(tests_dir, "testthat"),
  reporter = "progress",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)
