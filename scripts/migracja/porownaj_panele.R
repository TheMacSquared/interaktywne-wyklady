# Porównanie paneli wersji Shiny i strony statycznej jednego wykładu.
# Użycie: Rscript parity.R <katalog wykładu> <plik statyczny>
args <- commandArgs(trailingOnly = TRUE)
app_dir <- normalizePath(args[[1]]); static <- normalizePath(args[[2]]); port <- 7742L
p <- processx::process$new(file.path(R.home("bin"), "Rscript"),
  c("-e", sprintf("shiny::runApp('%s', port = %d, host = '127.0.0.1', launch.browser = FALSE)", app_dir, port)),
  stdout = "|", stderr = "|")
on.exit(p$kill(), add = TRUE)
for (i in 1:40) { ok <- tryCatch({ close(url(sprintf("http://127.0.0.1:%d", port), open = "r")); TRUE }, error = function(e) FALSE, warning = function(w) FALSE); if (ok) break; Sys.sleep(0.5) }
chrome <- sort(Sys.glob("~/.cache/ms-playwright/chromium-*/chrome-linux*/chrome"), decreasing = TRUE)[[1]]
Sys.setenv(CHROMOTE_CHROME = path.expand(chrome))
suppressMessages(library(chromote))

collect <- function(url, wait) {
  b <- ChromoteSession$new(width = 1440, height = 900)
  on.exit(b$close(), add = TRUE)
  b$Page$navigate(url, wait_ = FALSE); b$Page$loadEventFired(timeout_ = 60); Sys.sleep(wait)
  ev <- function(js) b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = TRUE)$result$value
  ids <- jsonlite::fromJSON(ev("JSON.stringify(Array.from(document.querySelectorAll('[data-lc-chapter]')).map(e => e.getAttribute('data-lc-chapter')))"))
  out <- list()
  for (id in ids) {
    ev(sprintf("(window.lcGoToChapter || function(id){ Shiny.setInputValue('lc__switch_chapter', id, {priority: 'event'}); })('%s')", id)); Sys.sleep(2.5)
    res <- jsonlite::fromJSON(ev(sprintf("JSON.stringify(Array.from(document.getElementById('%s').querySelectorAll('.lc-figure-panel')).filter(function(p){ return p.getClientRects().length > 0; }).map(function(p, i){
      var vis = function(sel){ return Array.from(p.querySelectorAll(sel)).filter(e => e.getClientRects().length > 0).length; };
      var txt = p.innerText.replace(/\\s+/g, ' ').trim();
      return { key: '%s#' + (i + 1), svg: vis('svg:not(.MathJax_SVG svg):not(mjx-container svg)'), canvas: vis('canvas'), echarts: vis('[_echarts_instance_]'),
        buttons: vis('button'), inputs: vis('input, select'), h: Math.round(p.getBoundingClientRect().height / 20) * 20,
        letters: txt.replace(/[^A-Za-zĄĆĘŁŃÓŚŹŻąćęłńóśźż]/g, '').length, head: txt.slice(0, 60) }; }))", id, id)), simplifyDataFrame = TRUE)
    if (is.data.frame(res)) out[[id]] <- res
  }
  do.call(rbind, out)
}
shiny <- collect(sprintf("http://127.0.0.1:%d", port), 6)
stat <- collect(paste0("file://", static), 3)
cat("paneli: Shiny", nrow(shiny), "| statyczna", nrow(stat), "\n")
m <- merge(shiny, stat, by = "key", suffixes = c(".shiny", ".static"), all = TRUE)
diffcol <- function(n) !mapply(identical, m[[paste0(n, ".shiny")]], m[[paste0(n, ".static")]])
m$diff <- ""
for (n in c("svg", "canvas", "echarts", "buttons", "inputs")) m$diff <- ifelse(diffcol(n), paste(m$diff, n), m$diff)
m$diff <- ifelse(abs(m$letters.shiny - m$letters.static) > pmax(15, 0.05 * m$letters.shiny), paste(m$diff, "tekst"), m$diff)
m$diff <- ifelse(abs(m$h.shiny - m$h.static) > pmax(40, 0.08 * m$h.shiny), paste(m$diff, "wysokość"), m$diff)
bad <- m[nzchar(m$diff) | is.na(m$svg.shiny) | is.na(m$svg.static), ]
cat("paneli z różnicą:", nrow(bad), "\n")
if (nrow(bad)) {
  options(width = 250)
  print(bad[, c("key", "diff", "svg.shiny", "svg.static", "echarts.shiny", "echarts.static", "buttons.shiny", "buttons.static",
                "inputs.shiny", "inputs.static", "letters.shiny", "letters.static", "h.shiny", "h.static", "head.static")], row.names = FALSE)
}
