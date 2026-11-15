#!/usr/bin/env Rscript

# Spis wykładów dla stron statycznych: <out>/index.html (oba kursy) oraz
# <out>/<kurs>/index.html. Lista powstaje z katalogów kursów (app.R: numer
# i tytuł wykładu); odnośnik dostaje każdy wykład, którego strona jest już
# zbudowana przez scripts/build_static.R, pozostałe są oznaczone jako niedostępne.
#
# Użycie (z katalogu repozytorium):
#   Rscript scripts/build_index.R              # dist/
#   Rscript scripts/build_index.R --out=dist

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1]]) else default
}
script_path <- local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(file_arg)) stop("Uruchom przez Rscript scripts/build_index.R.", call. = FALSE)
  normalizePath(sub("^--file=", "", file_arg[[1]]))
})
repo_root <- normalizePath(file.path(dirname(script_path), ".."))
out_root <- opt("out", file.path(repo_root, "dist"))
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# Kursy przechodzące na strony statyczne (statystyka 2 zostaje na Shiny).
courses <- c(statystyka = "Statystyka", `analiza-ryzyka` = "Analiza ryzyka")

read_text <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
app_field <- function(txt, field) {
  hit <- regmatches(txt, regexpr(paste0(field, "\\s*=\\s*\"[^\"]*\""), txt))
  if (length(hit)) sub("^[^\"]*\"([^\"]*)\"$", "\\1", hit) else NA_character_
}
esc <- function(x) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)

lectures_of <- function(course) {
  dirs <- sort(list.dirs(file.path(repo_root, course), recursive = FALSE, full.names = FALSE))
  dirs <- dirs[grepl("^[0-9]{2}-", dirs) & file.exists(file.path(repo_root, course, dirs, "app.R"))]
  do.call(rbind, lapply(dirs, function(d) {
    txt <- read_text(file.path(repo_root, course, d, "app.R"))
    num <- app_field(txt, "lecture_num")
    data.frame(dir = d, num = if (is.na(num)) substr(d, 1, 2) else num, title = app_field(txt, "lecture_title"),
               built = file.exists(file.path(out_root, course, paste0(d, ".html"))), stringsAsFactors = FALSE)
  }))
}

# Barwy i kroje jak w wykładach (shared/palette.R); strona jest samodzielna, bez plików obok.
palette <- new.env()
sys.source(file.path(repo_root, "shared", "palette.R"), envir = palette)
css <- sprintf('
  :root { --accent: %s; --ink: %s; --soft: #5b6466; --rule: #e3dccd; --paper: #f4efe4; --surface: #fff; }
  * { box-sizing: border-box; }
  body { margin: 0; background: var(--paper); color: var(--ink); font: 17px/1.5 "IBM Plex Sans", system-ui, -apple-system, "Segoe UI", sans-serif; }
  main { max-width: 46em; margin: 0 auto; padding: 2.4em 1.2em 4em; }
  h1 { font-size: 1.9em; margin: 0 0 .2em; }
  h2 { font-size: 1.25em; margin: 2em 0 .5em; padding-bottom: .3em; border-bottom: 3px solid var(--accent); }
  p.lead { color: var(--soft); margin: 0 0 1em; }
  ol { list-style: none; margin: 0; padding: 0; background: var(--surface); border: 1px solid var(--rule); }
  li { display: flex; gap: 1em; align-items: baseline; padding: .65em 1em; border-top: 1px solid var(--rule); }
  li:first-child { border-top: 0; }
  .num { font: 600 .85em/1 "IBM Plex Mono", ui-monospace, monospace; color: var(--accent); min-width: 2em; }
  a { color: var(--ink); text-decoration: none; font-weight: 600; }
  a:hover, a:focus { color: var(--accent); text-decoration: underline; }
  .off { color: var(--soft); }
  .off small { display: block; font-size: .8em; }
  nav { margin-bottom: 1.2em; font-size: .9em; }
', palette$upwr_accent, palette$upwr_ink)

course_list <- function(course, prefix = "") {
  lec <- lectures_of(course)
  items <- vapply(seq_len(nrow(lec)), function(i) {
    label <- esc(lec$title[i])
    body <- if (lec$built[i]) {
      sprintf('<a href="%s%s.html">%s</a>', prefix, lec$dir[i], label)
    } else {
      sprintf('<span class="off">%s<small>wersja bez serwera w przygotowaniu — wykład działa na razie pod Shiny</small></span>', label)
    }
    sprintf('<li><span class="num">%s</span>%s</li>', esc(lec$num[i]), body)
  }, "")
  paste0("<ol>", paste(items, collapse = ""), "</ol>")
}
page <- function(title, body) {
  paste0('<!doctype html>\n<html lang="pl"><head><meta charset="utf-8">',
         '<meta name="viewport" content="width=device-width, initial-scale=1">',
         "<title>", esc(title), "</title><style>", css, "</style></head><body><main>", body, "</main></body></html>\n")
}

for (course in names(courses)) {
  dir.create(file.path(out_root, course), showWarnings = FALSE)
  body <- paste0('<nav><a href="../index.html">← wszystkie kursy</a></nav><h1>', esc(courses[[course]]), "</h1>",
                 '<p class="lead">Wykłady interaktywne. Każdy wykład to jeden plik HTML: działa bez serwera, także z dysku.</p>',
                 course_list(course))
  writeLines(page(paste(courses[[course]], "— wykłady"), body), file.path(out_root, course, "index.html"), useBytes = TRUE)
}
body <- paste0("<h1>Interaktywne wykłady</h1>",
  '<p class="lead">Wykłady interaktywne. Każdy wykład to jeden plik HTML: działa bez serwera, także z dysku.</p>',
  paste(vapply(names(courses), function(course) {
    paste0("<h2>", esc(courses[[course]]), "</h2>", course_list(course, prefix = paste0(course, "/")))
  }, ""), collapse = ""))
writeLines(page("Interaktywne wykłady", body), file.path(out_root, "index.html"), useBytes = TRUE)
cat("Spis wykładów:", file.path(out_root, "index.html"), "\n")
