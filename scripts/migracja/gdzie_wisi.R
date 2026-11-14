# Strona zawiesza przeglądarkę albo gubi błędy? Zbiera wyjątki JS (także złapane
# przez try/catch, np. błąd ryciny ponawianej w pętli) z miejscem w pliku, a na
# końcu zatrzymuje wykonanie i wypisuje stos wywołań tego, co właśnie działa.
# Użycie: Rscript scripts/migracja/gdzie_wisi.R <plik.html> [sekundy czekania]
args <- commandArgs(TRUE)
wait <- if (length(args) > 1) as.numeric(args[[2]]) else 6
chrome <- Sys.getenv("CHROMOTE_CHROME", "")
if (!nzchar(chrome)) chrome <- path.expand(sort(Sys.glob("~/.cache/ms-playwright/chromium-*/chrome-linux*/chrome"), decreasing = TRUE)[[1]])
Sys.setenv(CHROMOTE_CHROME = chrome)
suppressMessages(library(chromote))
lines <- readLines(args[[1]], warn = FALSE)
frame_text <- function(f) {
  ln <- f$location$lineNumber + 1
  sprintf("%s, linia %d: %s", if (nzchar(f$functionName)) f$functionName else "(anonimowa)", ln,
          substr(trimws(lines[ln]), 1, 140))
}
b <- ChromoteSession$new(width = 1440, height = 900)
seen <- character(); final <- NULL; collecting <- TRUE
b$Debugger$enable()
b$Debugger$setPauseOnExceptions(state = "all")
b$Debugger$paused(function(e) {
  if (collecting && identical(e$reason, "exception")) {
    seen <<- c(seen, paste0(substr(gsub("\\s+", " ", e$data$description %||% ""), 1, 160), "\n      w: ",
                            frame_text(e$callFrames[[1]])))
    b$Debugger$resume(wait_ = FALSE)
  } else {
    final <<- e
  }
})
`%||%` <- function(a, b) if (is.null(a)) b else a
b$Page$navigate(paste0("file://", normalizePath(args[[1]])), wait_ = FALSE)
t0 <- Sys.time()
while (as.numeric(Sys.time() - t0, units = "secs") < wait) later::run_now(0.2)
collecting <- FALSE
tab <- sort(table(seen), decreasing = TRUE)
cat(sprintf("Wyjątki w ciągu %g s (różnych: %d):\n", wait, length(tab)))
for (i in seq_along(utils::head(tab, 8))) cat(sprintf("  %d× %s\n", tab[[i]], names(tab)[[i]]))
b$Debugger$pause(wait_ = FALSE)
for (i in 1:30) { if (!is.null(final)) break; later::run_now(0.2) }
if (is.null(final)) {
  cat("Strona nie wisi: nic nie działa w tej chwili.\n")
} else {
  cat("Teraz wykonuje się:\n")
  for (f in utils::head(final$callFrames, 8)) cat("  ", frame_text(f), "\n")
}
try(b$close(wait_ = FALSE), silent = TRUE)
