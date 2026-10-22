#!/usr/bin/env Rscript
# Statyczny zrzut wykładów (Statystyka, Analiza ryzyka) do plików HTML działających offline.
#
# Każdy wykład jest uruchamiany lokalnie, przeglądarka bez okna (chromote)
# przechodzi przez rozdziały, a wynik trafia do jednego pliku HTML z wykresami
# jako obrazkami. Sceny JS zostają interaktywne; kontrolki liczone w R są
# zamrożone w stanie domyślnym. Ściągi, quizy i ćwiczenia są pomijane.
#
# Użycie (z katalogu głównego repozytorium):
#   Rscript statystyka-offline/build/build_offline.R
#   Rscript statystyka-offline/build/build_offline.R --src=/kopia/statystyki --only=01,03
#   Rscript statystyka-offline/build/build_offline.R --course=analiza-ryzyka
#
# Opcje:
#   --course=X   przedmiot: statystyka (domyślnie) albo analiza-ryzyka
#   --src=DIR    katalog przedmiotu (domyślnie ../<course> względem statystyka-offline)
#   --out=DIR    katalog wynikowy (domyślnie <course>-offline/wyklady)
#   --only=NN,.. tylko wybrane wykłady (numery z nazw folderów)
#   --port=N     port startowy aplikacji (domyślnie 7951)
#   --scale=N    gęstość pikseli wykresów (domyślnie 1.5)
#
# Wymaga pakietów chromote, processx, jsonlite oraz przeglądarki opartej na
# Chromium (zmienna CHROMOTE_CHROME, jeśli chromote jej nie znajdzie).

suppressPackageStartupMessages({
  library(chromote)
  library(processx)
  library(jsonlite)
})

# ---------------------------------------------------------------------------
# Argumenty i ścieżki
# ---------------------------------------------------------------------------

.script_dir <- local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f[[1]])) else normalizePath("statystyka-offline/build")
})
offline_root <- dirname(.script_dir)

arg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), commandArgs(TRUE), value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1]]) else default
}

course   <- arg("course", "statystyka")
course_name <- c(statystyka = "Statystyka", "analiza-ryzyka" = "Analiza ryzyka")[[course]]
src_dir  <- normalizePath(arg("src", file.path(dirname(offline_root), course)), mustWork = TRUE)
out_dir  <- arg("out", file.path(dirname(offline_root), paste0(course, "-offline"), "wyklady"))
only     <- arg("only")
port0    <- as.integer(arg("port", "7951"))
px_scale <- as.numeric(arg("scale", "1.5"))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- normalizePath(out_dir)
log_dir <- file.path(tempdir(), paste0(course, "-offline-logs"))
dir.create(log_dir, showWarnings = FALSE)

lectures <- sort(list.dirs(src_dir, recursive = FALSE, full.names = FALSE))
lectures <- lectures[grepl("^[0-9]{2}-", lectures) &
                     file.exists(file.path(src_dir, lectures, "app.R"))]
all_lectures <- lectures
if (!is.null(only)) {
  wanted <- strsplit(only, ",")[[1]]
  lectures <- lectures[substr(lectures, 1, 2) %in% wanted]
}
tab_hrefs <- paste0(all_lectures, ".html")

read_text <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
capture_js <- read_text(file.path(.script_dir, "capture.js"))
runtime_js <- read_text(file.path(.script_dir, "runtime.js"))
offline_css <- read_text(file.path(.script_dir, "offline.css"))

# ---------------------------------------------------------------------------
# Fonty: lokalne kopie IBM Plex zamiast Google Fonts
# ---------------------------------------------------------------------------

font_src <- file.path(src_dir, "R", "fonts")
dir.create(file.path(out_dir, "fonts"), showWarnings = FALSE)
invisible(file.copy(list.files(font_src, full.names = TRUE), file.path(out_dir, "fonts"),
                    overwrite = TRUE))

font_face <- function(family, file, weight, style) {
  sprintf("@font-face{font-family:'%s';src:url('fonts/%s') format('truetype');font-weight:%s;font-style:%s;font-display:swap;}",
          family, file, weight, style)
}
fonts_css <- paste(c(
  font_face("IBM Plex Sans", "IBMPlexSans-Regular.ttf",    "100 500", "normal"),
  font_face("IBM Plex Sans", "IBMPlexSans-Bold.ttf",       "600 900", "normal"),
  font_face("IBM Plex Sans", "IBMPlexSans-Italic.ttf",     "100 500", "italic"),
  font_face("IBM Plex Sans", "IBMPlexSans-BoldItalic.ttf", "600 900", "italic"),
  font_face("IBM Plex Mono", "IBMPlexMono-Regular.ttf",    "100 500", "normal"),
  font_face("IBM Plex Mono", "IBMPlexMono-Bold.ttf",       "600 900", "normal"),
  font_face("IBM Plex Mono", "IBMPlexMono-Italic.ttf",     "100 500", "italic"),
  font_face("IBM Plex Mono", "IBMPlexMono-BoldItalic.ttf", "600 900", "italic")
), collapse = "\n")

# ---------------------------------------------------------------------------
# Pomocnicze
# ---------------------------------------------------------------------------

port_busy <- function(port) {
  tryCatch({
    con <- suppressWarnings(socketConnection("127.0.0.1", port, open = "r+", timeout = 1))
    close(con); TRUE
  }, error = function(e) FALSE)
}

# Inne sesje mogą mieć na tych portach własne aplikacje.
free_port <- function(start) {
  p <- start
  while (port_busy(p)) p <- p + 1L
  p
}

wait_port <- function(port, proc, timeout = 120) {
  t0 <- Sys.time()
  repeat {
    ok <- tryCatch({
      con <- suppressWarnings(socketConnection("127.0.0.1", port, open = "r+", timeout = 1))
      close(con); TRUE
    }, error = function(e) FALSE)
    if (ok) return(TRUE)
    if (!proc$is_alive()) return(FALSE)
    if (difftime(Sys.time(), t0, units = "secs") > timeout) return(FALSE)
    Sys.sleep(0.5)
  }
}

# Skrypt w <script> nie może zawierać dosłownego "</script".
safe_script <- function(x) gsub("</script", "<\\\\/script", x, ignore.case = TRUE)
safe_style  <- function(x) gsub("</style",  "<\\\\/style",  x, ignore.case = TRUE)

build_head <- function(items, title) {
  parts <- c(
    '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    sprintf("<title>%s</title>", title),
    sprintf("<script>\n%s\n</script>", safe_script(runtime_js)),
    sprintf("<style>\n%s\n</style>", fonts_css)
  )
  dropped <- character(0)
  for (it in items) {
    if (it$kind == "style") {
      parts <- c(parts, sprintf("<style>\n%s\n</style>", safe_style(it$text)))
    # risk_script.js: pytania kontrolne analizy ryzyka działają bez serwera.
    } else if (it$kind == "script-src" && grepl("jquery|risk_script", it$src, ignore.case = TRUE)) {
      parts <- c(parts, sprintf("<script>\n%s\n</script>", safe_script(it$text)))
    } else if (it$kind == "script") {
      parts <- c(parts, sprintf("<script>\n%s\n</script>", safe_script(it$text)))
    } else {
      dropped <- c(dropped, it$src)
    }
  }
  parts <- c(parts, sprintf("<style>\n%s\n</style>", offline_css))
  list(html = paste(parts, collapse = "\n"), dropped = dropped)
}

# ---------------------------------------------------------------------------
# Zrzut jednego wykładu
# ---------------------------------------------------------------------------

snapshot_lecture <- function(lecture, port) {
  port <- free_port(port)
  app_dir <- file.path(src_dir, lecture)
  log_file <- file.path(log_dir, paste0(lecture, ".log"))
  message(sprintf("== %s (port %d)", lecture, port))

  proc <- process$new(
    "Rscript",
    c("-e", sprintf("shiny::runApp('%s', port = %d, launch.browser = FALSE)", app_dir, port)),
    wd = app_dir, stdout = log_file, stderr = "2>&1", cleanup = TRUE
  )
  on.exit(if (proc$is_alive()) proc$kill(), add = TRUE)
  if (!wait_port(port, proc)) stop("Aplikacja nie wystartowała, log: ", log_file)

  b <- ChromoteSession$new(width = 1440, height = 1000)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  b$Emulation$setDeviceMetricsOverride(width = 1440, height = 1000,
                                       deviceScaleFactor = px_scale, mobile = FALSE)

  ev <- function(expr, timeout = 60) {
    res <- b$Runtime$evaluate(expr, awaitPromise = TRUE, returnByValue = TRUE,
                              timeout_ = timeout)
    if (!is.null(res$exceptionDetails))
      stop("JS: ", res$exceptionDetails$exception$description %||% res$exceptionDetails$text)
    res$result$value
  }

  b$Page$navigate(sprintf("http://127.0.0.1:%d/", port), wait_ = FALSE)
  t0 <- Sys.time()
  repeat {
    ready <- tryCatch(ev("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected() &&
                             document.querySelector('section[data-lc-chapter-content]'))"),
                      error = function(e) FALSE)
    if (isTRUE(ready)) break
    if (difftime(Sys.time(), t0, units = "secs") > 120) stop("Strona nie załadowała się")
    Sys.sleep(0.5)
  }

  ev(capture_js)
  ev("__off.idle()", timeout = 150)
  ev("__off.initMath()", timeout = 120)

  chapters <- ev("__off.chapters()")
  keep <- Filter(function(ch) isTRUE(ch$keep), chapters)
  skipped <- vapply(Filter(function(ch) !isTRUE(ch$keep), chapters), `[[`, "", "title")
  if (length(skipped)) message("   pominięte: ", paste(skipped, collapse = ", "))

  report <- list()
  for (ch in keep) {
    r <- ev(sprintf("__off.capture(%s)", toJSON(ch$id, auto_unbox = TRUE)), timeout = 300)
    message(sprintf("   %-28s wykresy=%-3s canvas=%-2s zamrożone=%-3s%s",
                    ch$id, r$plots, r$canvas, r$frozen,
                    if (length(r$errors)) paste0("  BŁĘDY: ", length(r$errors)) else ""))
    for (e in r$errors) message("      ! ", e)
    report[[ch$id]] <- r
  }

  keep_ids <- vapply(keep, `[[`, "", "id")
  lecture_title <- ev("(document.querySelector('.lc-nav-module-title')||{}).textContent || ''")
  head_items <- ev("__off.head()", timeout = 120)
  body <- ev(sprintf("__off.shell(%s, %s)",
                     toJSON(keep_ids), toJSON(tab_hrefs)), timeout = 120)

  title <- sprintf("%s · %s — %s (offline)", substr(lecture, 1, 2), trimws(lecture_title), course_name)
  head <- build_head(head_items, title)

  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"pl\">\n<head>\n", head$html, "\n</head>\n<body>\n",
    body, "\n</body>\n</html>\n"
  )
  out_file <- file.path(out_dir, paste0(lecture, ".html"))
  writeLines(html, out_file, useBytes = TRUE)
  message(sprintf("   → %s (%.1f MB); pominięte zasoby: %s",
                  basename(out_file), file.size(out_file) / 1e6,
                  paste(unique(basename(head$dropped)), collapse = ", ")))

  list(lecture = lecture, title = trimws(lecture_title), chapters = keep,
       errors = sum(vapply(report, function(r) length(r$errors), 0)))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Spis wykładów
# ---------------------------------------------------------------------------

write_index <- function() {
  files <- sort(list.files(out_dir, pattern = "^[0-9]{2}-.*\\.html$"))
  titles <- vapply(files, function(f) {
    x <- readLines(file.path(out_dir, f), n = 6, warn = FALSE, encoding = "UTF-8")
    t <- sub(".*<title>(.*)</title>.*", "\\1", grep("<title>", x, value = TRUE)[1])
    sub(paste0(" — ", course_name, " \\(offline\\)$"), "", t)
  }, "")
  items <- sprintf('<li><a href="%s">%s</a></li>', files, titles)
  html <- c(
    "<!DOCTYPE html>", '<html lang="pl">', "<head>", '<meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    sprintf("<title>%s — wykłady offline</title>", course_name),
    "<style>", fonts_css,
    "body{font-family:'IBM Plex Sans',sans-serif;max-width:40rem;margin:3rem auto;padding:0 1rem;color:#2b2b2b;background:#fbfaf7;line-height:1.5}",
    "h1{font-size:1.6rem;margin-bottom:.25rem}p{color:#666}ol{padding:0;list-style:none}",
    "li{margin:.4rem 0}a{color:#6b1a26;text-decoration:none;font-size:1.1rem}a:hover{text-decoration:underline}",
    "</style>", "</head>", "<body>",
    sprintf("<h1>%s — wykłady</h1>", course_name),
    "<p>Wersja offline. Otwórz wykład w przeglądarce; nie trzeba nic instalować. Wykresy pokazują ustawienia domyślne.</p>",
    "<ol>", items, "</ol>", "</body>", "</html>"
  )
  writeLines(html, file.path(out_dir, "index.html"), useBytes = TRUE)
}

# ---------------------------------------------------------------------------
# Uruchomienie
# ---------------------------------------------------------------------------

message("Źródło: ", src_dir)
message("Wynik:  ", out_dir)
results <- list()
for (i in seq_along(lectures)) {
  res <- tryCatch(snapshot_lecture(lectures[[i]], port0 + 10L * (i - 1L)),
                  error = function(e) { message("   BŁĄD: ", conditionMessage(e)); NULL })
  results[[lectures[[i]]]] <- res
}
write_index()

failed <- names(Filter(is.null, results))
message("\nGotowe: ", sum(!vapply(results, is.null, TRUE)), "/", length(lectures), " wykładów.")
if (length(failed)) message("Nieudane: ", paste(failed, collapse = ", "), " (logi: ", log_dir, ")")
