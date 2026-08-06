#!/usr/bin/env Rscript

script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

root <- script_dir()
source(file.path(root, "narzedzia", "graph_core.R"), local = TRUE)

if (!requireNamespace("yaml", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Wymagane są pakiety R: yaml i jsonlite.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
graph_id <- if (length(args)) args[[1]] else NULL
results <- compile_index(root, graph_id = graph_id, write_output = TRUE)

failed <- FALSE
for (result in results) {
  meta <- result$graph$meta
  if (result$ok) {
    cat(sprintf(
      "OK  %s — węzłów: %d, krawędzi: %d -> %s\n",
      meta$id, meta$wezlow, meta$krawedzi, result$output
    ))
    report <- result$graph$raport
    sections <- list(
      "Nieomawiane w wykładach" = report$nieomawiane,
      "Pytania bez odpowiedzi" = report$pytania_bez_odpowiedzi,
      "Twierdzenia bez źródeł" = report$twierdzenia_bez_zrodel,
      "Węzły odłączone" = report$wezly_odlaczone
    )
    for (label in names(sections)) {
      values <- sections[[label]]
      if (length(values)) {
        cat("  DECYZJA — ", label, ": ", paste(values, collapse = ", "), "\n", sep = "")
      }
    }
  } else {
    failed <- TRUE
    cat(sprintf("BŁĘDY  %s (%d)\n", meta$id, length(result$errors)))
    for (error in result$errors) cat(" - ", error, "\n", sep = "")
  }
}

if (failed) quit(status = 1L)
