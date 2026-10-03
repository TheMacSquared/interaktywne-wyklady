#!/usr/bin/env Rscript
# Układy fluidRow(column(...)) w panelach → komponenty v2 (etap 3 migracji).
#
# Użycie:
#   Rscript statystyka/scripts/migrate_v2_columns.R report <pliki...>
#   Rscript statystyka/scripts/migrate_v2_columns.R apply  <pliki...>
#
# Obsługiwane wzorce (reszta trafia do raportu jako „ręcznie”):
#   1. kolumna z kontrolkami + kolumna z wykresem/wyjściami
#      → lc_toolbar(kontrolki), potem zawartość drugiej kolumny; uiOutput()
#        z kolumny kontrolek (statusy) przechodzi pod zawartość, hr() znika;
#   2. każda kolumna to jeden wykres → lc_plots(...).
# W kolumnie kontrolek lc_stack(...) jest rozpakowywany do paska (bez hr/br),
# a helpText(...) przechodzi pod zawartość jako lc_caption(...).
# zoom_plot_ui(id, height = "Npx") → lc_plot(id, max_height = "Npx").

args <- commandArgs(trailingOnly = TRUE)
mode <- args[1]; files <- args[-1]
stopifnot(mode %in% c("report", "apply"))

controls <- c("selectInput", "lc_slider", "sliderInput", "lc_segmented", "lc_action",
              "checkboxInput", "checkboxGroupInput", "numericInput", "radioButtons",
              "actionButton", "textInput", "lc_chips", "lc_step_nav", "lc_group")
plots <- c("zoom_plot_ui", "lc_plot", "plotOutput")

kids_of <- function(pd, id) { k <- pd[pd$parent == id, ]; k[order(k$line1, k$col1), ] }
fn_of <- function(pd, id) {
  k <- kids_of(pd, id); if (!nrow(k) || k$token[1] != "expr") return(NA)
  s <- pd[pd$parent == k$id[1] & pd$token == "SYMBOL_FUNCTION_CALL", "text"]
  if (length(s)) return(s[1])
  kk <- kids_of(pd, k$id[1]); if (nrow(kk) == 3 && kk$token[2] == "'$'") return(paste0("tags$", kk$text[3]))
  NA
}
call_args <- function(pd, id) {
  kids <- kids_of(pd, id); out <- list(); pending <- NA
  for (i in seq_len(nrow(kids))[-1]) { k <- kids[i, ]
    if (k$token == "SYMBOL_SUB") pending <- k$text
    else if (k$token == "expr") { out[[length(out)+1]] <- list(name = pending, id = k$id, text = getParseText(pd, k$id), fn = fn_of(pd, k$id)); pending <- NA } }
  out
}
fix_text <- function(t) {
  t <- gsub('zoom_plot_ui\\("([A-Za-z0-9_]+)", height = "(\\d+)px"\\)', 'lc_plot("\\1", max_height = "\\2px")', t)
  # etykieta stoi nad kontrolką: bez dwukropka na końcu
  sub('^((?:selectInput|numericInput|checkboxGroupInput|radioButtons|textInput)\\("[^"]+", "[^"]*?)\\s*:"', '\\1"', t, perl = TRUE)
}
for (f in files) {
  src <- readLines(f, encoding = "UTF-8", warn = FALSE)
  pd <- getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"))
  rows <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "fluidRow", ]
  edits <- list()
  for (r in seq_len(nrow(rows))) {
    rid <- pd$parent[pd$id == rows$parent[r]]; node <- pd[pd$id == rid, ]
    cols <- call_args(pd, rid)
    tag <- sprintf("%s:%d", basename(f), node$line1)
    if (!length(cols) || !all(vapply(cols, function(x) identical(x$fn, "column"), logical(1)))) { cat("RĘCZNIE", tag, "(nie same column)\n"); next }
    content <- lapply(cols, function(cl) Filter(function(x) is.na(x$name), call_args(pd, cl$id))[-1])
    # lc_stack w kolumnie: jego dzieci (bez hr/br) wchodzą na miejsce stosu
    content <- lapply(content, function(cc) unlist(lapply(cc, function(x) {
      if (identical(x$fn, "lc_stack")) Filter(function(y) is.na(y$name) && !(y$fn %in% c("hr", "br")), call_args(pd, x$id))
      else list(x)
    }), recursive = FALSE))
    fns <- lapply(content, function(cc) vapply(cc, function(x) if (is.na(x$fn)) "?" else x$fn, ""))
    ind <- strrep(" ", node$col1 - 1); ind2 <- paste0(ind, "  ")
    new <- NULL
    if (all(vapply(fns, function(v) length(v) == 1 && v %in% plots, logical(1)))) {
      items <- vapply(content, function(cc) fix_text(cc[[1]]$text), "")
      new <- paste0("lc_plots(\n", ind2, paste(items, collapse = paste0(",\n", ind2)), "\n", ind, ")")
      kind <- "lc_plots"
    } else if (length(cols) == 2 && all(fns[[1]] %in% c(controls, "hr", "br", "uiOutput", "helpText")) &&
               any(fns[[1]] %in% controls) && !any(fns[[2]] %in% controls)) {
      ctrl <- content[[1]][fns[[1]] %in% controls]
      outs <- content[[1]][fns[[1]] == "uiOutput"]
      tb <- paste0("lc_toolbar(\n", ind2, paste(vapply(ctrl, function(x) fix_text(x$text), ""), collapse = paste0(",\n", ind2)), "\n", ind, ")")
      helps <- content[[1]][fns[[1]] == "helpText"]
      rest <- c(vapply(content[[2]], function(x) fix_text(x$text), ""), vapply(outs, `[[`, "", "text"),
                vapply(helps, function(x) sub("^helpText\\(", "lc_caption(", x$text), ""))
      new <- paste(c(tb, rest), collapse = paste0(",\n", ind))
      kind <- "toolbar"
    } else { cat("RĘCZNIE", tag, paste(vapply(fns, paste, "", collapse = "+"), collapse = " | "), "\n"); next }
    cat("OK", tag, kind, "\n")
    edits[[length(edits)+1]] <- list(l1 = node$line1, c1 = node$col1, l2 = node$line2, c2 = node$col2, text = new)
  }
  if (mode == "apply" && length(edits)) {
    ord <- order(-sapply(edits, `[[`, "l1"), -sapply(edits, `[[`, "c1"))
    for (e in edits[ord]) {
      before <- substr(src[e$l1], 1, e$c1 - 1); after <- substr(src[e$l2], e$c2 + 1, nchar(src[e$l2]))
      src <- c(src[seq_len(e$l1 - 1)], strsplit(paste0(before, e$text, after), "\n", fixed = TRUE)[[1]],
               if (e$l2 < length(src)) src[(e$l2 + 1):length(src)])
    }
    writeLines(src, f, useBytes = TRUE)
  }
}
