# Wykonuje JS na stronie i wypisuje wynik. Użycie: Rscript dbg.R <plik|URL> <js> [<js>...]
`%||%` <- function(a, b) if (is.null(a)) b else a
args <- commandArgs(TRUE)
chrome <- sort(Sys.glob("~/.cache/ms-playwright/chromium-*/chrome-linux*/chrome"), decreasing = TRUE)[[1]]
Sys.setenv(CHROMOTE_CHROME = path.expand(chrome)); suppressMessages(library(chromote))
b <- ChromoteSession$new(width = 1440, height = 900)
errors <- character(); b$Runtime$enable()
b$Runtime$exceptionThrown(function(e) errors <<- c(errors, e$exceptionDetails$exception$description))
http <- grepl("^http", args[[1]])
b$Page$navigate(if (http) args[[1]] else paste0("file://", normalizePath(args[[1]])), wait_ = FALSE)
b$Page$loadEventFired(timeout_ = 60); Sys.sleep(if (http) 6 else 2)
for (js in args[-1]) {
  if (grepl("^shot:", js)) { invisible(b$screenshot(sub("^shot:", "", js), show = FALSE)); next }
  if (grepl("^wait:", js)) { Sys.sleep(as.numeric(sub("^wait:", "", js))); next }
  res <- b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = TRUE)
  cat("> ", substr(js, 1, 70), "\n  ", format(res$result$value %||% res$exceptionDetails$exception$description %||% "undefined"), "\n")
}
`%||%` <- function(a, b) if (is.null(a)) b else a
cat("wyjątki:", if (length(errors)) paste(errors, collapse = " || ") else "brak", "\n")
invisible(b$close())
