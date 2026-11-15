#!/usr/bin/env Rscript
# Faza 8 przejścia na strony statyczne: usuwa kod serwera Shiny z wykładów statystyki
# i analizy ryzyka. W app.R wycina definicję server i shinyApp(); w modułach i plikach
# kursów usuwa definicje funkcji, których nazwa kończy się na „server” (są puste od
# przeniesienia rycin do przeglądarki), razem z komentarzami bezpośrednio nad nimi,
# oraz pomocnik scene_texts(). Jednorazowe; uruchamiane z katalogu repozytorium:
#   Rscript scripts/migracja/usun_serwer.R
lectures <- Sys.glob(c("statystyka/[0-9]*", "analiza-ryzyka/[0-9]*"))
files <- c(unlist(lapply(lectures, function(d) Sys.glob(file.path(d, c("modules/*.R", "*.R"))))),
           Sys.glob(c("statystyka/R/*.R", "analiza-ryzyka/R/*.R")))
files <- files[basename(files) != "app.R"]

drop_ranges <- function(lines, ranges) {
  keep <- rep(TRUE, length(lines))
  for (r in ranges) keep[r[1]:r[2]] <- FALSE
  out <- lines[keep]
  # Najwyżej jeden pusty wiersz z rzędu i bez pustych wierszy na końcu pliku.
  blank <- !nzchar(trimws(out))
  out <- out[!(blank & c(FALSE, utils::head(blank, -1)))]
  while (length(out) && !nzchar(trimws(out[length(out)]))) out <- out[-length(out)]
  out
}
# Komentarze (i nagłówki sekcji) bezpośrednio nad wierszem `first`.
with_comments <- function(lines, first) {
  k <- first
  while (k > 1 && grepl("^\\s*#", lines[k - 1])) k <- k - 1
  k
}

removed <- 0L
for (f in files) {
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  ex <- parse(f, keep.source = TRUE, encoding = "UTF-8")
  refs <- attr(ex, "srcref")
  ranges <- list()
  for (i in seq_along(ex)) {
    e <- ex[[i]]
    if (!(is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]))) next
    name <- as.character(e[[2]])
    is_fun <- is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))
    if (is_fun && (grepl("server$", name) || name == "scene_texts")) {
      ranges[[length(ranges) + 1L]] <- c(with_comments(lines, refs[[i]][1]), refs[[i]][3])
      removed <- removed + 1L
    }
  }
  if (length(ranges)) {
    out <- drop_ranges(lines, ranges)
    # Osierocony nagłówek sekcji SERVER na końcu pliku.
    n <- length(out)
    if (n >= 3 && grepl("^#\\s*=+", out[n]) && grepl("SERVER", out[n - 1], ignore.case = TRUE) && grepl("^#\\s*=+", out[n - 2])) {
      out <- drop_ranges(out, list(c(n - 2, n)))
    }
    writeLines(out, f, useBytes = TRUE)
    cat(f, ":", length(ranges), "\n")
  }
}

# app.R: od definicji server (z komentarzami nad nią) do końca pliku.
for (d in lectures) {
  f <- file.path(d, "app.R")
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  at <- grep("^server <- function", lines)
  stopifnot(length(at) == 1)
  out <- drop_ranges(lines, list(c(with_comments(lines, at), length(lines))))
  n <- length(out)
  if (n >= 3 && grepl("^#\\s*=+", out[n]) && grepl("SERVER", out[n - 1], ignore.case = TRUE)) out <- drop_ranges(out, list(c(n - 2, n)))
  writeLines(out, f, useBytes = TRUE)
}
cat("Usunięte funkcje serwera w modułach:", removed, "; app.R:", length(lectures), "\n")
