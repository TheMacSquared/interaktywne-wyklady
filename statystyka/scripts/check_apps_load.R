#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

`%||%` <- function(a, b) if (!is.null(a)) a else b

script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

subject_root <- normalizePath(file.path(script_dir(), ".."), mustWork = TRUE)

arg_value <- function(prefix, default = NULL) {
  hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^", prefix, "="), "", hit[1]) else default
}

only_app <- arg_value("--app")

app_files <- list.files(subject_root, pattern = "^app[.]R$", recursive = TRUE, full.names = TRUE)
if (!is.null(only_app)) {
  app_files <- app_files[grepl(only_app, app_files, fixed = TRUE)]
}

app_files <- sort(app_files)

cat("App load smoke test\n")
cat("Subject root:", subject_root, "\n")
cat("Apps found:", length(app_files), "\n\n")

if (!length(app_files)) {
  cat("No app.R files matched.\n")
  quit(status = 1)
}

load_app <- function(file) {
  code <- sprintf(
    "env <- new.env(parent = globalenv()); suppressPackageStartupMessages(source(%s, local = env, chdir = FALSE)); invisible(TRUE)",
    encodeString(normalizePath(file, mustWork = TRUE), quote = "\"")
  )

  output <- tryCatch(
    suppressWarnings(
      system2("Rscript", c("-e", shQuote(code)), stdout = TRUE, stderr = TRUE)
    ),
    error = function(e) structure(conditionMessage(e), status = 1)
  )
  status <- attr(output, "status") %||% 0

  if (identical(status, 0)) {
    list(ok = TRUE, message = "")
  } else {
    msg <- paste(output, collapse = "\n")
    list(ok = FALSE, message = msg)
  }
}

results <- lapply(app_files, load_app)
ok <- vapply(results, `[[`, logical(1), "ok")

for (i in seq_along(app_files)) {
  rel <- sub(paste0("^", subject_root, "/?"), "", app_files[i])
  if (ok[i]) {
    cat(sprintf("OK   %s\n", rel))
  } else {
    cat(sprintf("FAIL %s\n     %s\n", rel, results[[i]]$message))
  }
}

cat("\nSummary:", sum(ok), "OK,", sum(!ok), "failed.\n")
quit(status = if (all(ok)) 0 else 1)
