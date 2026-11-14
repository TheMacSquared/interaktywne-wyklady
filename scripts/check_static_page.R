#!/usr/bin/env Rscript

# Sprawdza zbudowaną stronę statyczną wykładu w Chromium bez okna:
# brak błędów konsoli i żądań sieciowych, nawigacja między rozdziałami,
# uruchomione suwaki, działający quiz, brak niezłożonych wzorów i brak
# poziomego przewijania na telefonie.
#
# Użycie (z katalogu repozytorium):
#   Rscript scripts/check_static_page.R dist/analiza-ryzyka/02-warunki.html
#   Rscript scripts/check_static_page.R --shots=katalog dist/analiza-ryzyka/02-warunki.html
#
# Wymaga pakietu chromote i Chromium (CHROMOTE_CHROME albo Chromium z Playwright).
# Status wyjścia 1, gdy którakolwiek kontrola zawiedzie.

args <- commandArgs(trailingOnly = TRUE)
shots <- sub("^--shots=", "", grep("^--shots=", args, value = TRUE))
files <- args[!grepl("^--", args)]
if (!length(files)) stop("Podaj plik HTML zbudowany przez scripts/build_static.R.", call. = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

find_chrome <- function() {
  env <- Sys.getenv("CHROMOTE_CHROME", "")
  if (nzchar(env) && file.exists(env)) return(env)
  found <- suppressMessages(tryCatch(chromote::find_chrome(), error = function(e) NULL))
  if (!is.null(found) && nzchar(found)) return(found)
  pw <- sort(Sys.glob(c("~/.cache/ms-playwright/chromium-*/chrome-linux*/chrome",
                        "~/Library/Caches/ms-playwright/chromium-*/chrome-mac*/Chromium.app/Contents/MacOS/Chromium")),
             decreasing = TRUE)
  if (length(pw)) return(path.expand(pw[[1]]))
  stop("Nie znaleziono Chromium. Ustaw CHROMOTE_CHROME.", call. = FALSE)
}
Sys.setenv(CHROMOTE_CHROME = find_chrome())

check_page <- function(file) {
  problems <- character()
  note <- function(ok, text) {
    cat(sprintf("  [%s] %s\n", if (ok) "ok" else "BŁĄD", text))
    if (!ok) problems <<- c(problems, text)
  }
  b <- chromote::ChromoteSession$new(width = 1440, height = 900)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  errors <- character(); requests <- character()
  b$Runtime$enable(); b$Network$enable(); b$Log$enable()
  b$Runtime$exceptionThrown(function(e) {
    errors <<- c(errors, e$exceptionDetails$exception$description %||% e$exceptionDetails$text)
  })
  b$Runtime$consoleAPICalled(function(e) {
    if (identical(e$type, "error")) {
      errors <<- c(errors, paste(vapply(e$args, function(a) as.character(a$value %||% a$description %||% ""), ""),
                                 collapse = " "))
    }
  })
  b$Log$entryAdded(function(e) {
    if (identical(e$entry$level, "error")) errors <<- c(errors, paste(e$entry$text, e$entry$url %||% ""))
  })
  b$Network$requestWillBeSent(function(e) {
    if (!grepl("^(file|data|blob):", e$request$url)) requests <<- c(requests, e$request$url)
  })
  ev <- function(js) b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = TRUE)$result$value
  shot <- function(name) {
    if (length(shots)) {
      dir.create(shots, recursive = TRUE, showWarnings = FALSE)
      b$screenshot(file.path(shots, paste0(tools::file_path_sans_ext(basename(file)), "-", name, ".png")),
                   show = FALSE)
    }
  }

  b$Page$navigate(paste0("file://", normalizePath(file)), wait_ = FALSE)
  b$Page$loadEventFired(timeout_ = 60)
  Sys.sleep(2)

  note(identical(ev("typeof window.Shiny"), "undefined"), "strona nie ładuje Shiny")
  ids <- jsonlite::fromJSON(ev(
    "JSON.stringify(Array.from(document.querySelectorAll('section[data-lc-static-chapter]')).map(s => s.id))"))
  note(length(ids) > 0, sprintf("rozdziały w dokumencie: %d", length(ids)))

  for (i in seq_along(ids)) {
    ev(sprintf("document.querySelectorAll('.lc-nav-chapter-btn')[%d].click()", i - 1L))
    Sys.sleep(0.8)
    state <- jsonlite::fromJSON(ev("JSON.stringify({
      visible: Array.from(document.querySelectorAll('section[data-lc-static-chapter]')).filter(s => !s.hidden).map(s => s.id),
      tex: (document.querySelector('section[data-lc-static-chapter]:not([hidden])').innerText.match(/\\\\\\(|\\$\\$|\\\\frac|\\\\text\\{/g) || []).length,
      emptyCharts: Array.from(document.querySelectorAll('section[data-lc-static-chapter]:not([hidden]) [_echarts_instance_]'))
        .filter(e => e.offsetParent !== null && (e.clientWidth === 0 || e.clientHeight === 0)).length
    })"))
    note(identical(state$visible, ids[[i]]), sprintf("rozdział %d: widoczny tylko %s", i, ids[[i]]))
    if (state$tex > 0) note(FALSE, sprintf("rozdział %d: niezłożony TeX w tekście (%d)", i, state$tex))
    if (state$emptyCharts > 0) note(FALSE, sprintf("rozdział %d: widoczne wykresy o zerowym rozmiarze (%d)", i, state$emptyCharts))
    shot(sprintf("rozdzial-%02d", i))
  }

  # Kroje wbudowane w plik muszą się wczytać (inaczej tekst składa krój systemowy).
  fonts <- jsonlite::fromJSON(ev("document.fonts.ready.then(function(){ return JSON.stringify({
    sans: document.fonts.check('16px \"IBM Plex Sans\"'), mono: document.fonts.check('16px \"IBM Plex Mono\"'),
    loaded: Array.from(document.fonts).filter(f => f.status === 'loaded').length,
    failed: Array.from(document.fonts).filter(f => f.status === 'error').map(f => f.family + ' ' + f.weight + ' ' + f.style) }); })"))
  note(isTRUE(fonts$sans) && isTRUE(fonts$mono) && fonts$loaded > 0 && !length(fonts$failed),
       sprintf("kroje IBM Plex wczytane z pliku (%d odmian w użyciu)", fonts$loaded))

  sliders <- jsonlite::fromJSON(ev("JSON.stringify(Array.from(document.querySelectorAll('.js-range-slider')).map(
    el => !!(window.jQuery && window.jQuery(el).data('ionRangeSlider'))))"))
  if (length(sliders)) note(all(sliders), sprintf("suwaki uruchomione: %d z %d", sum(sliders), length(sliders)))

  quiz <- ev("(function(){ var btn = document.querySelector('[data-lc-quiz-check]'); if (!btn) return 'brak';
    var id = btn.getAttribute('data-lc-quiz-check');
    var data = JSON.parse(document.querySelector('script[data-lc-quiz-data=\"' + id + '\"]').textContent);
    data.forEach(function(q){ var r = document.querySelector('input[name=\"' + q.name + '\"][value=\"' + q.correct + '\"]');
      if (r) { r.checked = true; r.dispatchEvent(new Event('change', {bubbles: true})); } });
    btn.click();
    var out = document.getElementById(id + '_quiz_feedback');
    return JSON.stringify({ n: data.length, items: out.querySelectorAll('ol > li').length,
      verdict: (out.querySelector('.lc-status') || {}).textContent || '' }); })()")
  if (!identical(quiz, "brak")) {
    q <- jsonlite::fromJSON(quiz)
    note(q$items == q$n && grepl(sprintf("Wynik: %d/%d.", q$n, q$n), q$verdict, fixed = TRUE),
         sprintf("quiz: poprawne odpowiedzi dają „%s”", trimws(q$verdict)))
  }

  # Głosowanie: bez wyboru prośba o zaznaczenie, po wyborze komentarz do odpowiedzi.
  vote <- ev("(function(){ var btn = document.querySelector('[data-lc-vote-check]'); if (!btn) return 'brak';
    var id = btn.getAttribute('data-lc-vote-check');
    var cfg = JSON.parse(document.querySelector('script[data-lc-vote-data=\"' + id + '\"]').textContent);
    var out = document.getElementById(cfg.output);
    btn.click(); var none = out.textContent.trim();
    var radios = Array.from(document.querySelectorAll('input[type=\"radio\"][name=\"' + id + '\"]'));
    var texts = radios.map(function(r){ r.checked = true; r.dispatchEvent(new Event('change', {bubbles: true})); return out.textContent.trim(); });
    return JSON.stringify({ none: none, texts: texts }); })()")
  if (!identical(vote, "brak")) {
    v <- jsonlite::fromJSON(vote)
    note(nzchar(v$none) && all(nzchar(v$texts)),
         sprintf("głosowanie: komentarz do każdej z %d odpowiedzi", length(v$texts)))
  }

  b$Emulation$setDeviceMetricsOverride(width = 390, height = 844, deviceScaleFactor = 2, mobile = TRUE)
  Sys.sleep(1)
  shot("telefon")
  note(!isTRUE(ev("document.documentElement.scrollWidth > window.innerWidth + 1")),
       "brak poziomego przewijania przy 390 px")

  # Osadzone filmy (iframe YouTube) są treścią wykładu i wymagają sieci z założenia.
  embeds <- grepl("^https://www\\.youtube(-nocookie)?\\.com/embed/", requests)
  if (any(embeds)) cat(sprintf("  [ok] osadzone filmy z sieci: %d\n", length(unique(requests[embeds]))))
  requests <- unique(requests[!embeds]); errors <- unique(errors)
  note(!length(requests), if (length(requests)) paste("żądania sieciowe:", paste(requests, collapse = ", ")) else "brak żądań sieciowych")
  note(!length(errors), if (length(errors)) paste("błędy konsoli:", paste(errors, collapse = " | ")) else "brak błędów konsoli")
  problems
}

status <- 0L
for (file in files) {
  cat(file, "\n")
  problems <- check_page(file)
  if (length(problems)) status <- 1L
}
quit(status = status)
