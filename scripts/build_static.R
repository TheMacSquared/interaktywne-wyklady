#!/usr/bin/env Rscript

# Buduje statyczne strony wykładów: jeden plik HTML na wykład, bez serwera R.
# Treść i układ pochodzą z app.R wykładu (ui <- lecture_page(...); wykłady nie mają
# serwera), style i skrypty są wbudowane w plik, wzory złożone przy budowaniu jako SVG.
#
# Użycie (z katalogu repozytorium):
#   Rscript scripts/build_static.R analiza-ryzyka/02-warunki
#   Rscript scripts/build_static.R analiza-ryzyka/02-warunki statystyka/02-zwiazki-zmiennych
#   Rscript scripts/build_static.R --out=dist analiza-ryzyka/02-warunki
#   Rscript scripts/build_static.R --no-math analiza-ryzyka/02-warunki   # wzory składa MathJax z sieci
#
# Wynik: <out>/<kurs>/<wykład>.html — jeden samodzielny plik (kroje IBM Plex w środku).
# Wymagania: pakiety wykładu, htmltools, jsonlite; do wzorów chromote
# i przeglądarka Chromium (zmienna CHROMOTE_CHROME albo Chromium z Playwright).
# Biblioteki z sieci (np. ECharts) są pobierane raz do <out>/.cache/.
#
# Buduje się tylko wykłady bez zależności od serwera; sprawdza to
# scripts/check_static_ready.R.

args <- commandArgs(trailingOnly = TRUE)
if (nzchar(Sys.getenv("LC_DEBUG"))) options(error = function() { traceback(3); quit(status = 1) })
flag <- function(name) paste0("--", name) %in% args
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1]]) else default
}

script_path <- local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(file_arg)) stop("Uruchom przez Rscript scripts/build_static.R.", call. = FALSE)
  normalizePath(sub("^--file=", "", file_arg[[1]]))
})
repo_root <- normalizePath(file.path(dirname(script_path), ".."))
out_root <- opt("out", file.path(repo_root, "dist"))
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
out_root <- normalizePath(out_root)
use_math <- !flag("no-math")
lectures <- args[!grepl("^--", args)]
if (!length(lectures)) stop("Podaj wykład, np. analiza-ryzyka/02-warunki.", call. = FALSE)

read_text <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# Plik jako base64 w jednej linii (jsonlite łamie wynik co 76 znaków, a url() tego nie przyjmuje).
base64_file <- function(path) {
  gsub("\\s", "", jsonlite::base64_enc(readBin(path, "raw", file.size(path))))
}

# lecture_id, numer i tytuł z wywołania lecture_page() w app.R (bez uruchamiania kodu).
app_field <- function(app_file, field) {
  hit <- regmatches(txt <- read_text(app_file),
                    regexpr(paste0(field, "\\s*=\\s*\"[^\"]*\""), txt))
  if (length(hit)) sub("^[^\"]*\"([^\"]*)\"$", "\\1", hit) else NA_character_
}

lecture_dirs <- vapply(lectures, function(l) {
  dir <- file.path(repo_root, l)
  if (!file.exists(file.path(dir, "app.R"))) stop("Brak app.R w ", l, call. = FALSE)
  normalizePath(dir)
}, character(1))

# ============================================================================
# Tryb roboczy: jeden wykład w osobnym procesie R (czyste środowisko app.R)
# ============================================================================

build_one <- function(app_dir) {
  suppressPackageStartupMessages({ library(shiny); library(htmltools) })
  course_dir <- dirname(app_dir)
  course <- basename(course_dir)
  folder <- basename(app_dir)
  out_dir <- file.path(out_root, course)
  cache_dir <- file.path(out_root, ".cache")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Wykłady kursu dostępne jako strony statyczne: budowany teraz i zbudowane wcześniej.
  # Nazwy = katalog wykładu, wartości = lecture_id (dla zakładek górnego paska).
  siblings <- dirname(Sys.glob(file.path(course_dir, "[0-9]*", "app.R")))
  built <- siblings[basename(siblings) == folder |
                    file.exists(file.path(out_dir, paste0(basename(siblings), ".html")))]
  built_ids <- stats::setNames(
    vapply(file.path(built, "app.R"), app_field, character(1), field = "lecture_id"),
    basename(built)
  )
  options(lc.static = TRUE, lc.static.lectures = built_ids[!is.na(built_ids)])

  env <- new.env(parent = globalenv())
  owd <- setwd(app_dir)
  on.exit(setwd(owd), add = TRUE)
  suppressWarnings(suppressMessages(source("app.R", local = env)))
  rendered <- htmltools::renderTags(env$ui)

  # ---- Zależności HTML (Bootstrap, jQuery, suwak, zasoby kursu) wbudowane w plik
  skip_js <- c("shiny-javascript", "shiny-autoreload", "busy-indicators")
  dep_dir <- function(d) {
    if (!is.null(d$package)) system.file(d$src$file, package = d$package) else d$src$file
  }
  dep_files <- function(x) {
    if (is.null(x)) return(character())
    unname(vapply(as.list(x), function(s) if (is.list(s)) s$src else s, character(1)))
  }
  dep_html <- unlist(lapply(htmltools::resolveDependencies(rendered$dependencies), function(d) {
    base <- dep_dir(d)
    css <- vapply(file.path(base, dep_files(d$stylesheet)), read_text, character(1))
    js <- if (d$name %in% skip_js) character() else
      vapply(file.path(base, dep_files(d$script)), read_text, character(1))
    c(sprintf("<!-- %s %s -->", d$name, d$version),
      sprintf("<style>\n%s\n</style>", css),
      sprintf("<script>\n%s\n</script>", gsub("</script", "<\\/script", js, fixed = TRUE)),
      d$head)
  }))

  # Arkusz Shiny dokłada serwer przy wysyłaniu strony, więc nie ma go wśród
  # zależności. Kontrolki (suwak, radio, pola wyboru) zakładają jego reguły.
  dep_html <- c(dep_html, "<!-- shiny css -->", sprintf("<style>\n%s\n</style>", read_text(
    system.file("www", "shared", "shiny.min.css", package = "shiny"))))

  # ---- Nagłówek strony: zasoby z sieci wbudowane albo usunięte
  head <- rendered$head
  mathjax_src <- regmatches(head, regexpr("https?://[^\"']*MathJax\\.js[^\"']*", head))
  mathjax_config <- regmatches(
    head, regexpr("(?s)<script type=\"text/x-mathjax-config\">.*?</script>", head, perl = TRUE))

  # Kroje: zamiast Google Fonts pliki z shared/fonts/ wbudowane w stronę jako dane,
  # żeby wykład był jednym plikiem (ok. 3 MB na dwanaście odmian IBM Plex: te same
  # wagi 400–700, które wersja Shiny pobiera z Google Fonts).
  head <- gsub("<link[^>]*fonts\\.googleapis\\.com[^>]*>", "", head, perl = TRUE)
  head <- gsub("@import url\\(['\"]https://fonts\\.googleapis\\.com[^)]*\\);?", "", head, perl = TRUE)
  font_src <- file.path(repo_root, "shared", "fonts")
  face <- function(family, file, weight, style) {
    path <- file.path(font_src, file)
    sprintf(
      "@font-face{font-family:'%s';src:url(data:font/ttf;base64,%s) format('truetype');font-weight:%s;font-style:%s;font-display:swap}",
      family, base64_file(path), weight, style)
  }
  fonts_css <- paste(c(
    face("IBM Plex Sans", "IBMPlexSans-Regular.ttf", 400, "normal"),
    face("IBM Plex Sans", "IBMPlexSans-Italic.ttf", 400, "italic"),
    face("IBM Plex Sans", "IBMPlexSans-Medium.ttf", 500, "normal"),
    face("IBM Plex Sans", "IBMPlexSans-SemiBold.ttf", 600, "normal"),
    face("IBM Plex Sans", "IBMPlexSans-Bold.ttf", 700, "normal"),
    face("IBM Plex Sans", "IBMPlexSans-BoldItalic.ttf", 700, "italic"),
    face("IBM Plex Mono", "IBMPlexMono-Regular.ttf", 400, "normal"),
    face("IBM Plex Mono", "IBMPlexMono-Italic.ttf", 400, "italic"),
    face("IBM Plex Mono", "IBMPlexMono-Medium.ttf", 500, "normal"),
    face("IBM Plex Mono", "IBMPlexMono-SemiBold.ttf", 600, "normal"),
    face("IBM Plex Mono", "IBMPlexMono-Bold.ttf", 700, "normal"),
    face("IBM Plex Mono", "IBMPlexMono-BoldItalic.ttf", 700, "italic")
  ), collapse = "\n")

  # Arkusze dołączane przez @import z sieci (np. ikony): pobrane raz i wbudowane,
  # a ich kroje (woff2) zapisane w arkuszu jako dane.
  fetch <- function(url) {
    cached <- file.path(cache_dir, gsub("[^A-Za-z0-9._-]", "_", url))
    if (!file.exists(cached)) {
      message("  pobieram ", url)
      utils::download.file(url, cached, quiet = TRUE, mode = "wb")
    }
    cached
  }
  repeat {
    m <- regexpr("@import url\\(['\"]?https?://[^)'\"]+['\"]?\\);?", head, perl = TRUE)
    if (m < 0) break
    url <- sub("^@import url\\(['\"]?([^)'\"]+)['\"]?\\);?$", "\\1", regmatches(head, m))
    css <- read_text(fetch(url))
    refs <- unique(regmatches(css, gregexpr("url\\([^)]+\\)", css))[[1]])
    for (ref in refs) {
      path <- gsub("^url\\(['\"]?|['\"]?\\)$", "", ref)
      if (grepl("^data:", path)) next
      if (!grepl("\\.woff2(\\?|$)", path)) next
      font_url <- paste0(dirname(url), "/", sub("^\\./", "", path))
      css <- gsub(ref, sprintf("url(data:font/woff2;base64,%s)", base64_file(fetch(sub("\\?.*$", "", font_url)))),
                  css, fixed = TRUE)
    }
    regmatches(head, m) <- paste0("/* ", url, " */\n", css)
  }

  # Skrypty z sieci (poza MathJax): pobrane raz i wbudowane.
  repeat {
    m <- regexpr("<script[^>]*\\ssrc=\"https?://[^\"]+\"[^>]*></script>", head, perl = TRUE)
    if (m < 0) break
    tag <- regmatches(head, m)
    url <- sub(".*\\ssrc=\"([^\"]+)\".*", "\\1", tag)
    if (grepl("MathJax", url, fixed = TRUE)) {
      regmatches(head, m) <- "<!--LC-MATHJAX-->"
      next
    }
    code <- gsub("</script", "<\\/script", read_text(fetch(url)), fixed = TRUE)
    regmatches(head, m) <- paste0("<script>/* ", url, " */\n", code, "\n</script>")
  }

  body <- rendered$html

  # Obrazki z katalogów zarejestrowanych przez addResourcePath() (np. assets/)
  # wbudowujemy w plik jako dane.
  resource_dirs <- shiny::resourcePaths()
  mime <- c(png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg", gif = "image/gif",
            svg = "image/svg+xml", webp = "image/webp")
  refs <- unique(regmatches(body, gregexpr("src=\"[^\"/:]+/[^\"]+\"", body))[[1]])
  for (ref in refs) {
    path <- sub("^src=\"(.*)\"$", "\\1", ref)
    prefix <- sub("/.*$", "", path)
    ext <- tolower(tools::file_ext(path))
    if (!prefix %in% names(resource_dirs) || !ext %in% names(mime)) next
    file <- file.path(resource_dirs[[prefix]], sub("^[^/]+/", "", path))
    if (!file.exists(file)) stop("Brak pliku obrazka: ", path, call. = FALSE)
    body <- gsub(ref, sprintf("src=\"data:%s;base64,%s\"", mime[[ext]], base64_file(file)), body, fixed = TRUE)
  }
  # withMathJax() dokłada po treści skrypt ponownego składu; strona statyczna go nie potrzebuje.
  requeue <- "<script>if (window.MathJax) MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>"

  math_css <- ""
  if (use_math && length(mathjax_src)) {
    body <- gsub(requeue, "", body, fixed = TRUE)
    head_nomath <- sub("<!--LC-MATHJAX-->", "", head, fixed = TRUE)
    if (length(mathjax_config)) head_nomath <- sub(mathjax_config, "", head_nomath, fixed = TRUE)
    typeset <- typeset_math(body, c(dep_html, head_nomath, sprintf("<style>%s</style>", fonts_css)),
                            mathjax_src, mathjax_config, out_dir)
    body <- typeset$body
    math_css <- typeset$css
    head <- head_nomath
    # Skrypty wykładu czekają na MathJax (np. strzałki wzoru ze znacznikami):
    # wzory są już złożone, więc kolejka wykonuje funkcje od razu.
    head <- paste0(
      "<script>window.MathJax={Hub:{Register:{MessageHook:function(){},StartupHook:function(){}},",
      "Queue:function(){for(var i=0;i<arguments.length;i++)if(typeof arguments[i]==='function')",
      "setTimeout(arguments[i],0);},Config:function(){},Typeset:function(){}}};</script>\n", head)
  } else {
    head <- sub("<!--LC-MATHJAX-->", sprintf("<script src=\"%s\"></script>", mathjax_src), head, fixed = TRUE)
  }

  num <- app_field(file.path(app_dir, "app.R"), "lecture_num")
  title <- app_field(file.path(app_dir, "app.R"), "lecture_title")
  page_title <- paste(stats::na.omit(c(num, title)), collapse = " · ")

  # Skrypty przenosimy z nagłówka na koniec dokumentu, w tej samej kolejności;
  # w nagłówku zostają skrypty oznaczone data-lc-early (tryb i rozmiar tekstu
  # przed pierwszym rysowaniem) oraz dane.
  head_all <- paste(c(dep_html, head), collapse = "\n")
  late <- character()
  kept <- character()
  repeat {
    m <- regexpr("(?s)<script\\b[^>]*>.*?</script>", head_all, perl = TRUE)
    if (m < 0) break
    tag <- regmatches(head_all, m)
    open <- sub("(?s)^(<script\\b[^>]*>).*$", "\\1", tag, perl = TRUE)
    if (grepl("data-lc-early|type=\"(application/json|text/x-mathjax-config)\"", open)) {
      kept <- c(kept, tag)
      regmatches(head_all, m) <- sprintf("<!--LC-KEPT-%d-->", length(kept))
    } else {
      late <- c(late, tag)
      regmatches(head_all, m) <- ""
    }
  }
  for (i in seq_along(kept)) {
    head_all <- sub(sprintf("<!--LC-KEPT-%d-->", i), kept[[i]], head_all, fixed = TRUE)
  }

  # Treść rozdziałów czeka w <template> i trafia do dokumentu jednym ruchem, gdy
  # wszystkie skrypty są już wczytane — tak jak w wersji Shiny, gdzie rozdział
  # wstawia renderUI. Dzięki temu widgety nie zależą od kolejności skryptów
  # (skrypt uruchomiony od razu nie widzi treści, zanim wczyta się skrypt, którego używa).
  main_open <- regexpr("<main id=\"lc-main\"[^>]*>", body, perl = TRUE)
  main_close <- max(gregexpr("</main>", body, fixed = TRUE)[[1]])
  if (main_open < 0 || main_close < 0) stop("Nie znaleziono <main id=\"lc-main\">.", call. = FALSE)
  inner_start <- main_open + attr(main_open, "match.length")
  body <- paste0(
    substr(body, 1, inner_start - 1),
    "<template id=\"lc-static-content\">", substr(body, inner_start, main_close - 1), "</template>",
    substr(body, main_close, nchar(body))
  )
  late <- c(late, paste0(
    "<script>(function(){var t=document.getElementById('lc-static-content');",
    "if(t)t.parentNode.replaceChild(t.content,t);})();</script>"))

  html <- paste(c(
    "<!DOCTYPE html>",
    "<html lang=\"pl\" data-lc-static=\"true\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    # Strony nie trafiają do wyszukiwarek (publikacja na GitHub Pages bez indeksowania).
    "<meta name=\"robots\" content=\"noindex, nofollow\">",
    sprintf("<title>%s</title>", htmltools::htmlEscape(page_title)),
    sprintf("<style>\n%s\n</style>", fonts_css),
    head_all,
    if (nzchar(math_css)) sprintf("<style>\n%s\n</style>", math_css),
    "</head>",
    "<body>",
    body,
    late,
    "</body>",
    "</html>"
  ), collapse = "\n")

  out_file <- file.path(out_dir, paste0(folder, ".html"))
  writeLines(enc2utf8(html), out_file, useBytes = TRUE)
  cat(sprintf("%s/%s -> %s (%.1f MB)\n", course, folder, out_file,
              file.size(out_file) / 1024^2))
}

# ============================================================================
# Wzory: MathJax składa je w Chromium bez okna jako samodzielne SVG.
# Skrypty wykładu są na ten czas wyjęte z dokumentu (znaczniki-komentarze),
# więc do pliku trafia treść nietknięta przez widgety — zmienia się tylko zapis wzorów.
# ============================================================================

find_chrome <- function() {
  env <- Sys.getenv("CHROMOTE_CHROME", "")
  if (nzchar(env) && file.exists(env)) return(env)
  found <- suppressMessages(tryCatch(chromote::find_chrome(), error = function(e) NULL))
  if (!is.null(found) && nzchar(found)) return(found)
  pw <- sort(Sys.glob(c("~/.cache/ms-playwright/chromium-*/chrome-linux*/chrome",
                        "~/Library/Caches/ms-playwright/chromium-*/chrome-mac*/Chromium.app/Contents/MacOS/Chromium")),
             decreasing = TRUE)
  if (length(pw)) return(path.expand(pw[[1]]))
  stop("Nie znaleziono Chromium. Ustaw CHROMOTE_CHROME albo zbuduj z --no-math.", call. = FALSE)
}

typeset_math <- function(body, head_parts, mathjax_src, mathjax_config, out_dir) {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    stop("Do składu wzorów potrzebny jest pakiet chromote (albo --no-math).", call. = FALSE)
  }
  # Wszystkie skrypty treści (także dane JSON) zastępujemy komentarzami i przywracamy po składzie.
  held <- character()
  repeat {
    m <- regexpr("(?s)<script\\b[^>]*>.*?</script>", body, perl = TRUE)
    if (m < 0) break
    held <- c(held, regmatches(body, m))
    regmatches(body, m) <- sprintf("<!--LC-HELD-%d-->", length(held))
  }
  # W nagłówku zostają same style: skrypty widgetów nie mogą zmienić treści.
  head_css <- gsub("(?s)<script\\b[^>]*>.*?</script>", "", paste(head_parts, collapse = "\n"), perl = TRUE)
  src <- sub("\\?config=.*$", "", mathjax_src)
  page <- paste(c(
    "<!DOCTYPE html><html lang=\"pl\"><head><meta charset=\"utf-8\">",
    head_css,
    # Skład potrzebuje widocznych elementów: MathJax mierzy krój otoczenia.
    "<style>section[data-lc-static-chapter][hidden]{display:block!important}</style>",
    mathjax_config,
    "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({",
    "  jax: ['input/TeX', 'output/SVG'], showMathMenu: false, messageStyle: 'none',",
    "  'fast-preview': { disabled: true },",
    "  SVG: { useGlobalCache: false, mtextFontInherit: true, font: 'TeX' }",
    "});</script>",
    sprintf("<script src=\"%s?config=TeX-AMS_SVG\"></script>", src),
    "</head><body>", body, "</body></html>"
  ), collapse = "\n")
  tmp <- file.path(out_dir, ".math-pass.html")
  writeLines(enc2utf8(page), tmp, useBytes = TRUE)
  on.exit(unlink(tmp), add = TRUE)

  Sys.setenv(CHROMOTE_CHROME = find_chrome())
  b <- chromote::ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  b$Page$navigate(paste0("file://", normalizePath(tmp)), wait_ = FALSE)
  b$Page$loadEventFired(timeout_ = 60)
  js <- "
    (async function () {
      for (var i = 0; i < 300 && !(window.MathJax && MathJax.Hub && MathJax.isReady); i++) {
        await new Promise(function (r) { setTimeout(r, 100); });
      }
      if (!(window.MathJax && MathJax.Hub)) return JSON.stringify({ error: 'MathJax nie wczytał się (sieć?)' });
      var closed = Array.prototype.filter.call(document.querySelectorAll('details'), function (d) { return !d.open; });
      closed.forEach(function (d) { d.open = true; });
      await new Promise(function (r) { MathJax.Hub.Queue(['Typeset', MathJax.Hub], r); });
      closed.forEach(function (d) { d.open = false; });
      ['MathJax_Message', 'MathJax_Hidden', 'MathJax_Font_Test', 'MathJax_SVG_Hidden'].forEach(function (id) {
        var el = document.getElementById(id);
        if (el) (el.id === 'MathJax_Hidden' || el.id === 'MathJax_SVG_Hidden' ? el.parentNode : el).remove();
      });
      var css = Array.prototype.map.call(document.querySelectorAll('head style'), function (s) { return s.textContent; })
        .filter(function (t) { return /MathJax|MJX/.test(t) && t.indexOf('data-lc-static-chapter') < 0; }).join('\\n');
      return JSON.stringify({
        body: document.body.innerHTML, css: css,
        count: document.querySelectorAll('.MathJax_SVG').length,
        errors: document.querySelectorAll('.MathJax_Error, .noError, merror').length
      });
    })()"
  res <- b$Runtime$evaluate(js, awaitPromise = TRUE, returnByValue = TRUE, timeout_ = 120)
  out <- jsonlite::fromJSON(res$result$value)
  if (!is.null(out[["error"]])) stop(out[["error"]], call. = FALSE)
  if (out[["errors"]] > 0) warning(sprintf("Wzory z błędem składu: %d", out$errors), call. = FALSE)
  message(sprintf("  wzory złożone jako SVG: %d", out$count))

  typeset_body <- out$body
  for (i in seq_along(held)) {
    typeset_body <- sub(sprintf("<!--LC-HELD-%d-->", i), held[[i]], typeset_body, fixed = TRUE)
  }
  if (grepl("<!--LC-HELD-", typeset_body, fixed = TRUE)) {
    stop("Nie udało się przywrócić wszystkich skryptów treści po składzie wzorów.", call. = FALSE)
  }
  list(body = typeset_body, css = out$css)
}

# ============================================================================
# Start
# ============================================================================

if (flag("worker")) {
  build_one(lecture_dirs[[1]])
} else {
  rscript <- file.path(R.home("bin"), "Rscript")
  status <- 0L
  for (i in seq_along(lectures)) {
    code <- system2(rscript, c(shQuote(script_path), "--worker",
                               paste0("--out=", shQuote(out_root)),
                               if (!use_math) "--no-math", shQuote(lectures[[i]])))
    if (!identical(code, 0L)) {
      status <- 1L
      message("Błąd budowania: ", lectures[[i]])
    }
  }
  quit(status = status)
}
