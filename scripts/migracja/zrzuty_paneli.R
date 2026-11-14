# Zrzuty tych samych paneli w wersji Shiny i na stronie statycznej, sklejone obok
# siebie (po lewej Shiny, po prawej strona statyczna) do porównania wzrokowego.
# Użycie:
#   Rscript scripts/migracja/zrzuty_paneli.R <katalog aplikacji Shiny> <plik.html> <katalog wyjściowy> <rozdział#nr> ...
# Klucz „ch-ciagla#3” to trzeci widoczny panel rozdziału ch-ciagla (jak w porownaj_panele.R).
# Opcjonalnie po kluczu „::js” — kod wykonywany w obu wersjach przed zrzutem (np. zmiana kontrolki).
args <- commandArgs(TRUE)
app_dir <- normalizePath(args[[1]]); static <- normalizePath(args[[2]]); out <- args[[3]]; keys <- args[-(1:3)]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
port <- 7748L
p <- processx::process$new(file.path(R.home("bin"), "Rscript"),
  c("-e", sprintf("shiny::runApp('%s', port = %d, host = '127.0.0.1', launch.browser = FALSE)", app_dir, port)),
  stdout = "|", stderr = "|")
for (i in 1:60) { ok <- tryCatch({ close(url(sprintf("http://127.0.0.1:%d", port), open = "r")); TRUE }, error = function(e) FALSE, warning = function(w) FALSE); if (ok) break; Sys.sleep(0.5) }
chrome <- Sys.getenv("CHROMOTE_CHROME", "")
if (!nzchar(chrome)) chrome <- path.expand(sort(Sys.glob("~/.cache/ms-playwright/chromium-*/chrome-linux*/chrome"), decreasing = TRUE)[[1]])
Sys.setenv(CHROMOTE_CHROME = chrome)
suppressMessages(library(chromote))

shoot <- function(url, wait, tag) {
  b <- ChromoteSession$new(width = 1100, height = 1000)
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  b$Page$navigate(url, wait_ = FALSE); b$Page$loadEventFired(timeout_ = 60); Sys.sleep(wait)
  ev <- function(js) b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = TRUE)$result$value
  files <- character()
  for (key in keys) {
    js <- if (grepl("::", key, fixed = TRUE)) sub("^.*?::", "", key) else NULL
    k <- sub("::.*$", "", key); ch <- sub("#.*$", "", k); idx <- as.integer(sub("^.*#", "", k))
    ev(sprintf("(window.lcGoToChapter || function(id){ Shiny.setInputValue('lc__switch_chapter', id, {priority: 'event'}); })('%s')", ch))
    Sys.sleep(2.5)
    pick <- sprintf("Array.from(document.getElementById('%s').querySelectorAll('.lc-figure-panel')).filter(p => p.getClientRects().length > 0)[%d]", ch, idx - 1L)
    ev(sprintf("(function(){ var p = %s; if (p) p.scrollIntoView({block: 'start'}); })()", pick))
    if (!is.null(js)) { ev(sprintf("(function(){ var panel = %s; %s })()", pick, js)); }
    Sys.sleep(2.2)
    rect <- jsonlite::fromJSON(ev(sprintf("(function(){ var p = %s; if (!p) return 'null'; var r = p.getBoundingClientRect(); return JSON.stringify({x: r.left, y: r.top, w: r.width, h: Math.min(r.height, 980 - Math.max(0, r.top))}); })()", pick)))
    f <- file.path(out, sprintf("%s_%s.png", gsub("[^A-Za-z0-9]+", "-", k), tag))
    if (is.null(rect)) { cat("brak panelu", k, "w", tag, "\n"); next }
    b$screenshot(f, cliprect = c(rect$x, rect$y, rect$w, rect$h), show = FALSE)
    files <- c(files, f)
  }
  files
}
old <- shoot(sprintf("http://127.0.0.1:%d", port), 6, "shiny")
p$kill()
new <- shoot(paste0("file://", static), 3, "static")
# Sklejenie: Shiny po lewej, strona statyczna po prawej.
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])))
source(file.path(script_dir, "sklej_zrzuty.R"))
sklej_zrzuty(out)
