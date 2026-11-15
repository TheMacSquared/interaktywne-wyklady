# Zrzuty wybranych miejsc strony. Użycie:
#   Rscript shot_at.R <plik.html lub URL> <katalog> <nazwa=js zwracający element>...
# Przed zrzutem przełącza na rozdział zawierający element i przewija do niego.
args <- commandArgs(trailingOnly = TRUE)
target <- args[[1]]; shots <- args[[2]]; specs <- args[-(1:2)]
dir.create(shots, recursive = TRUE, showWarnings = FALSE)
chrome <- sort(Sys.glob("~/.cache/ms-playwright/chromium-*/chrome-linux*/chrome"), decreasing = TRUE)[[1]]
Sys.setenv(CHROMOTE_CHROME = path.expand(chrome))
suppressMessages(library(chromote))
width <- as.integer(Sys.getenv("SHOT_W", "1440"))
b <- ChromoteSession$new(width = width, height = 900)
url <- if (grepl("^https?://", target)) target else paste0("file://", normalizePath(target))
b$Page$navigate(url, wait_ = FALSE); b$Page$loadEventFired(timeout_ = 60)
Sys.sleep(as.numeric(Sys.getenv("SHOT_WAIT", "2")))
ev <- function(js) b$Runtime$evaluate(js, returnByValue = TRUE, awaitPromise = TRUE)$result$value
for (spec in specs) {
  name <- sub("=.*$", "", spec); js <- sub("^[^=]*=", "", spec)
  res <- ev(sprintf("(function(){ var el = (function(){ %s })(); if (!el) return 'brak elementu';
    var sec = el.closest('section.lc-chapter');
    if (sec && sec.offsetParent === null && window.lcGoToChapter) { lcGoToChapter(sec.id); return 'przełączam'; }
    el.scrollIntoView({block: 'start'}); var m = document.getElementById('lc-main'); if (m) m.scrollTop -= 20; return 'ok'; })()", js))
  Sys.sleep(1.2)
  invisible(b$screenshot(file.path(shots, paste0(name, ".png")), show = FALSE))
  cat(name, ":", res, "\n")
}
invisible(b$close())
