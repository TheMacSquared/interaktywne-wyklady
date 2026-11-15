#!/usr/bin/env Rscript

# Licznik zależności wykładów od serwera Shiny (migracja na strony statyczne).
# Parsuje app.R i modules/ każdego wykładu (bez uruchamiania kodu) i liczy
# wywołania, które wymagają działającej sesji R. Wykład jest gotowy do
# budowania statycznego, gdy wszystkie liczniki poza „quiz” wynoszą zero.
# Kolumna „wspolne” liczy wywołania funkcji z <kurs>/R/ i shared/, które same
# używają serwera (np. risk_assessment_server() obsługujące quiz).
#
# Użycie (z katalogu repozytorium):
#   Rscript scripts/check_static_ready.R                 # statystyka i analiza ryzyka
#   Rscript scripts/check_static_ready.R analiza-ryzyka  # jeden kurs
#   Rscript scripts/check_static_ready.R --details 02-warunki   # wywołania w wykładzie
#   Rscript scripts/check_static_ready.R --strict        # status 1, gdy coś zostało

args <- commandArgs(trailingOnly = TRUE)
strict  <- "--strict" %in% args
details <- "--details" %in% args
args <- setdiff(args, c("--strict", "--details"))
options(width = 200)

script_dir <- local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg)) dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))) else getwd()
})
repo_root <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)

all_courses <- c("statystyka", "analiza-ryzyka")
courses <- intersect(args, all_courses)
if (!length(courses)) courses <- all_courses
only <- setdiff(args, all_courses)

# Grupy wywołań. Nazwa grupy = kolumna raportu.
groups <- list(
  wykresy = c("zoom_plot_server", "renderPlot", "renderImage", "renderCachedPlot"),
  teksty  = c("renderUI", "renderText", "renderPrint", "renderTable", "renderDataTable"),
  reakcje = c("observeEvent", "observe", "reactive", "reactiveVal", "reactiveValues",
              "eventReactive", "reactiveTimer", "invalidateLater"),
  komunikaty = c("sendCustomMessage", "sendInputMessage", "updateSliderInput",
                 "updateSelectInput", "updateRadioButtons", "updateCheckboxInput",
                 "updateNumericInput", "updateTextInput", "updateActionButton",
                 "lc_update_step", "showModal", "insertUI", "removeUI")
)
call_group <- unlist(lapply(names(groups), function(g) {
  stats::setNames(rep(g, length(groups[[g]])), groups[[g]])
}))

# Nazwa wywoływanej funkcji: foo(...), pkg::foo(...), obj$foo(...).
call_name <- function(expr) {
  head <- expr[[1]]
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && length(head) == 3 && is.symbol(head[[1]]) &&
      as.character(head[[1]]) %in% c("::", ":::", "$") && is.symbol(head[[3]])) {
    return(as.character(head[[3]]))
  }
  ""
}

# Funkcje z <kurs>/R/ i shared/, których ciało używa serwera. Powłoka
# (lecture_server) znika razem z przejściem na stronę statyczną, a zoom_plot_server
# jest liczony w grupie „wykresy”.
helper_names <- character()
find_helpers <- function(files) {
  found <- character()
  for (file in files) {
    for (expr in parse(file, keep.source = FALSE, encoding = "UTF-8")) {
      if (!is.call(expr) || !as.character(expr[[1]]) %in% c("<-", "=") ||
          !is.symbol(expr[[2]]) || !is.call(expr[[3]]) ||
          !identical(expr[[3]][[1]], quote(`function`))) next
      acc <- walk(expr[[3]], list(calls = character(), inputs = character()))
      if (length(acc$calls)) found <- c(found, as.character(expr[[2]]))
    }
  }
  setdiff(found, c("lecture_server", names(call_group)))
}

# Zbiera wywołania z grup oraz nazwy wejść czytanych przez input$x / input[["x"]].
walk <- function(expr, acc) {
  if (is.call(expr)) {
    name <- call_name(expr)
    if (name %in% names(call_group) || name %in% helper_names) acc$calls <- c(acc$calls, name)
    if (name %in% c("$", "[[") && length(expr) == 3 && identical(expr[[2]], quote(input))) {
      key <- expr[[3]]
      acc$inputs <- c(acc$inputs, if (is.symbol(key) || is.character(key)) as.character(key) else "<dynamiczne>")
    }
  }
  if (is.call(expr) || is.expression(expr) || is.pairlist(expr)) {
    for (i in seq_along(expr)) {
      if (identical(expr[[i]], quote(expr = ))) next
      acc <- walk(expr[[i]], acc)
    }
  }
  acc
}

scan_r <- function(files) {
  acc <- list(calls = character(), inputs = character(), where = character())
  for (file in files) {
    parsed <- parse(file, keep.source = FALSE, encoding = "UTF-8")
    before <- length(acc$calls)
    acc <- walk(parsed, acc)
    n <- length(acc$calls) - before
    if (n > 0) acc$where <- c(acc$where, rep(basename(file), n))
  }
  acc
}

# Skrypty wykładu, które rozmawiają z serwerem.
scan_js <- function(files) {
  n <- 0L
  for (file in files) {
    txt <- readLines(file, warn = FALSE, encoding = "UTF-8")
    n <- n + sum(grepl("Shiny\\.(setInputValue|onInputChange|addCustomMessageHandler)", txt))
  }
  n
}

is_quiz_input <- function(x) grepl("quiz|vote|_check$|_reveal|answer", x)

rows <- list()
shared_r <- list.files(file.path(repo_root, "shared"), pattern = "[.]R$", full.names = TRUE)
for (course in courses) {
  helper_names <- character()
  helper_names <- find_helpers(c(
    shared_r, list.files(file.path(repo_root, course, "R"), pattern = "[.]R$", full.names = TRUE)
  ))
  lectures <- sort(dirname(Sys.glob(file.path(repo_root, course, "[0-9]*", "app.R"))))
  for (dir in lectures) {
    if (length(only) && !basename(dir) %in% only) next
    r_files <- c(file.path(dir, "app.R"),
                 list.files(file.path(dir, "modules"), pattern = "[.]R$",
                            recursive = TRUE, full.names = TRUE))
    js_files <- list.files(file.path(dir, "modules"), pattern = "[.]js$",
                           recursive = TRUE, full.names = TRUE)
    acc <- scan_r(r_files)
    # lecture_server() to powłoka: znika razem z przejściem na stronę statyczną.
    # Inne render* (np. visNetwork::renderVisNetwork, renderLeaflet): wyjścia htmlwidgets z serwera.
    grp <- call_group[acc$calls]
    grp[is.na(grp) & grepl("^render[A-Z]", acc$calls)] <- "wykresy"
    counts <- table(factor(grp, levels = names(groups)))
    inputs <- unique(acc$inputs)
    rows[[length(rows) + 1]] <- data.frame(
      kurs = course, wyklad = basename(dir),
      wykresy = counts[["wykresy"]], teksty = counts[["teksty"]],
      reakcje = counts[["reakcje"]], komunikaty = counts[["komunikaty"]],
      wspolne = sum(acc$calls %in% helper_names),
      wejscia = sum(!is_quiz_input(inputs)), quiz = sum(is_quiz_input(inputs)),
      js_shiny = scan_js(js_files),
      stringsAsFactors = FALSE
    )
    if (details) {
      cat(sprintf("\n== %s/%s ==\n", course, basename(dir)))
      if (length(acc$calls)) {
        tab <- as.data.frame(table(plik = acc$where, wywolanie = acc$calls),
                             stringsAsFactors = FALSE)
        tab <- tab[tab$Freq > 0, ]
        print(tab[order(tab$plik, -tab$Freq), ], row.names = FALSE)
      }
      if (length(inputs)) cat("wejścia:", paste(sort(inputs), collapse = ", "), "\n")
    }
  }
}

report <- do.call(rbind, rows)
if (is.null(report)) stop("Nie znaleziono wykładów.", call. = FALSE)
blocking <- c("wykresy", "teksty", "reakcje", "komunikaty", "wspolne", "wejscia", "js_shiny")
report$zostalo <- rowSums(report[, blocking])
report$status <- ifelse(report$zostalo == 0, "gotowy", "")

cat("\nZależność wykładów od serwera Shiny\n")
cat("wykresy: renderPlot/zoom_plot_server · teksty: renderUI/Text/Table ·",
    "reakcje: observe/reactive ·\nkomunikaty: sendCustomMessage/update*/showModal ·",
    "wspolne: funkcje serwerowe z <kurs>/R/ i shared/ ·\nwejscia: input$… poza quizami · quiz: input$… quizów i głosowań ·",
    "js_shiny: wywołania Shiny.* w skryptach wykładu\n\n")
print(report, row.names = FALSE)

totals <- colSums(report[, c(blocking, "quiz")])
cat("\nRazem: ", paste(names(totals), totals, sep = " ", collapse = " · "), "\n", sep = "")
cat(sprintf("Gotowe: %d z %d wykładów.\n", sum(report$zostalo == 0), nrow(report)))

if (strict && any(report$zostalo > 0)) quit(status = 1)
