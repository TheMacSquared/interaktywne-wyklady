#!/usr/bin/env Rscript

# Statycznie zbiera zależności używane przez aplikacje statystyczne i sprawdza,
# czy są zainstalowane. Nie uruchamia aplikacji ani ich kodu serwerowego.

script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

subject_root <- normalizePath(file.path(script_dir(), ".."), mustWork = TRUE)
app_dirs <- sort(list.dirs(subject_root, recursive = FALSE, full.names = TRUE))
app_dirs <- app_dirs[file.exists(file.path(app_dirs, "app.R"))]
shared_files <- list.files(file.path(subject_root, "R"), pattern = "[.]R$",
                           full.names = TRUE)

scan_expr <- function(expr, required = character(), optional = character()) {
  if (!is.call(expr) && !is.expression(expr) && !is.pairlist(expr)) {
    return(list(required = required, optional = optional))
  }

  if (is.call(expr)) {
    head <- if (is.symbol(expr[[1]])) as.character(expr[[1]]) else ""
    if (head %in% c("library", "require") && length(expr) >= 2) {
      required <- c(required, as.character(expr[[2]]))
    } else if (head %in% c("::", ":::") && length(expr) >= 2) {
      required <- c(required, as.character(expr[[2]]))
    } else if (head == "requireNamespace" && length(expr) >= 2) {
      optional <- c(optional, as.character(expr[[2]]))
    }
  }

  for (i in seq_along(expr)) {
    # Wywołania takie jak foo(x, optional_arg = ) zawierają pusty symbol.
    if (identical(expr[[i]], quote(expr = ))) next
    found <- scan_expr(expr[[i]], required, optional)
    required <- found$required
    optional <- found$optional
  }
  list(required = required, optional = optional)
}

scan_files <- function(files) {
  result <- list(required = character(), optional = character())
  for (file in files) {
    parsed <- tryCatch(parse(file, keep.source = FALSE), error = function(e) {
      stop(sprintf("Nie można sparsować %s: %s", file, conditionMessage(e)),
           call. = FALSE)
    })
    found <- scan_expr(parsed)
    result$required <- c(result$required, found$required)
    result$optional <- c(result$optional, found$optional)
  }
  result$required <- sort(unique(result$required[nzchar(result$required)]))
  result$optional <- sort(unique(result$optional[nzchar(result$optional)]))
  result
}

shared <- scan_files(shared_files)
missing_any <- FALSE

cat("Dependency check\n")
cat(sprintf("R version: %s\n", getRversion()))
if (getRversion() < "4.1.0") {
  cat("ERROR: wymagany jest R >= 4.1.0 (projekt używa operatora |>).\n")
  missing_any <- TRUE
}

for (app_dir in app_dirs) {
  files <- c(file.path(app_dir, "app.R"),
             list.files(file.path(app_dir, "modules"), pattern = "[.]R$",
                        recursive = TRUE, full.names = TRUE))
  deps <- scan_files(files[file.exists(files)])
  required <- sort(unique(c(shared$required, deps$required)))
  optional <- sort(unique(c(shared$optional, deps$optional)))
  # Pakiet sprawdzany przez requireNamespace() ma w kodzie świadomy fallback.
  # Samo późniejsze użycie pkg::fun() wewnątrz tej gałęzi nie czyni go wymaganym.
  required <- setdiff(required, optional)

  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  missing_optional <- optional[
    !vapply(optional, requireNamespace, logical(1), quietly = TRUE)
  ]

  status <- if (length(missing)) "MISSING" else "OK"
  cat(sprintf("%-32s %s\n", basename(app_dir), status))
  if (length(missing)) {
    cat("  wymagane: ", paste(missing, collapse = ", "), "\n", sep = "")
    missing_any <- TRUE
  }
  if (length(missing_optional)) {
    cat("  opcjonalne (jest fallback): ",
        paste(missing_optional, collapse = ", "), "\n", sep = "")
  }
}

if (missing_any) quit(status = 1)
cat("All required dependencies are available.\n")
