#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
strict <- "--strict" %in% args

script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

project_root <- normalizePath(file.path(script_dir(), ".."), mustWork = TRUE)
lecture_dirs <- sprintf("%02d-%s", 1:10, c(
  "jezyk-ryzyka", "warunki", "alarm-i-prawda", "wiele-prob", "do-zdarzenia",
  "zmiennosc-i-prog", "czas-zycia", "niezawodnosc-systemu", "drzewo-bledow",
  "model-do-decyzji"
))

rules <- c(
  "\\bfluidPage\\s*\\(" = "Użyj lecture_page(), nie fluidPage().",
  "\\bnavbarPage\\s*\\(" = "Użyj lecture_page(), nie navbarPage().",
  "\\bsidebarLayout\\s*\\(" = "Użyj komponentów lecture_layout, nie sidebarLayout().",
  "\\btabPanel\\s*\\(" = "Rozdziały mają być obiektami lecture_chapter().",
  "\\blibrary\\s*\\(\\s*bslib\\s*\\)|\\bbs_theme\\s*\\(" = "Nie buduj shell aplikacji przez bslib.",
  "class\\s*=\\s*['\"](section-title|chapter-title|widget-block|narrative)" = "Użyj komponentów lc_*.",
  "(?<!lc-)\\bbtn-(primary|secondary|success|warning|danger|info|default|outline|sm|lg)\\b" = "Użyj klas lc-btn-*.",
  "class\\s*=\\s*['\"]table(\\s|['\"])" = "Użyj klas lc-table*.",
  "\\btheme_(educational|minimal)\\s*\\(" = "Użyj theme_upwr()."
)

files <- unlist(lapply(file.path(project_root, lecture_dirs), function(path) {
  list.files(path, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
}), use.names = FALSE)

matches <- list()
for (file in files) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  for (pattern in names(rules)) {
    hits <- grep(pattern, lines, perl = TRUE)
    if (length(hits)) {
      matches[[length(matches) + 1L]] <- data.frame(
        file = sub(paste0("^", project_root, "/?"), "", file),
        line = hits,
        message = unname(rules[[pattern]]),
        text = trimws(lines[hits]),
        stringsAsFactors = FALSE
      )
    }
  }
}

cat("Design contract scan — analiza ryzyka\n")
cat("Files scanned:", length(files), "\n")

if (!length(matches)) {
  cat("No violations found.\n")
  quit(status = 0)
}

matches <- do.call(rbind, matches)
for (i in seq_len(nrow(matches))) {
  cat(sprintf("%s:%d %s\n    %s\n", matches$file[[i]], matches$line[[i]],
              matches$message[[i]], matches$text[[i]]))
}

if (strict) quit(status = 1)
cat("Informational mode: use --strict to fail.\n")
