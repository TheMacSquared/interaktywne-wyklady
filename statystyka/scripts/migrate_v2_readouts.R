#!/usr/bin/env Rscript
# Pudełka lc_stat_box() → odczyty lc_readout() w pasku (etap 3 migracji).
#
# Użycie:
#   Rscript statystyka/scripts/migrate_v2_readouts.R report <pliki...>
#   Rscript statystyka/scripts/migrate_v2_readouts.R apply  <pliki...>
#
# Krok 1: lc_stat_box(etykieta, wartość[, jednostka][, color =]) →
#   lc_readout(etykieta, wartość[ + jednostka][, color =]); opakowania
#   lc_center()/lc_stat_grid() z samymi pudełkami → tagList(). Pudełka
#   z caption = i wywołania w UI (statyczne) trafiają do raportu.
# Krok 2 (osobne uruchomienie po kroku 1, tryb apply2): uiOutput(<wyjście
#   z odczytami>) stojący bezpośrednio w figure_panel() trafia do jego
#   lc_toolbar() jako lc_readouts(uiOutput(...)); bez paska — owinięty.

args <- commandArgs(trailingOnly = TRUE)
mode <- args[1]; files <- args[-1]
stopifnot(mode %in% c("report", "apply", "apply2"))

kids_of <- function(pd, id) { k <- pd[pd$parent == id, ]; k[order(k$line1, k$col1), ] }
fn_of <- function(pd, id) {
  k <- kids_of(pd, id); if (!nrow(k) || k$token[1] != "expr") return(NA)
  s <- pd[pd$parent == k$id[1] & pd$token == "SYMBOL_FUNCTION_CALL", "text"]; if (length(s)) s[1] else NA
}
call_args <- function(pd, id) {
  kids <- kids_of(pd, id); out <- list(); pending <- NA
  for (i in seq_len(nrow(kids))[-1]) { k <- kids[i, ]
    if (k$token == "SYMBOL_SUB") pending <- k$text
    else if (k$token == "expr") { out[[length(out)+1]] <- list(name = pending, id = k$id, text = getParseText(pd, k$id), node = k, fn = fn_of(pd, k$id)); pending <- NA } }
  out
}
output_of <- function(pd, id) {
  repeat { id <- pd$parent[pd$id == id]; if (!length(id) || id <= 0) return(NA)
    t <- getParseText(pd, id); m <- regmatches(t, regexpr("^output\\$[A-Za-z0-9_]+", t)); if (length(m)) return(sub("output\\$", "", m)) }
}
apply_edits <- function(src, edits) {
  ord <- order(-sapply(edits, `[[`, "l1"), -sapply(edits, `[[`, "c1"))
  for (e in edits[ord]) {
    before <- substr(src[e$l1], 1, e$c1 - 1); after <- substr(src[e$l2], e$c2 + 1, nchar(src[e$l2]))
    src <- c(src[seq_len(e$l1 - 1)], strsplit(paste0(before, e$text, after), "\n", fixed = TRUE)[[1]],
             if (e$l2 < length(src)) src[(e$l2 + 1):length(src)])
  }
  src
}
for (f in files) {
  src <- readLines(f, encoding = "UTF-8", warn = FALSE)
  pd <- getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"))
  edits <- list()
  if (mode %in% c("report", "apply")) {
    boxes <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "lc_stat_box", ]
    for (i in seq_len(nrow(boxes))) {
      bid <- pd$parent[pd$id == boxes$parent[i]]; node <- pd[pd$id == bid, ]
      tag <- sprintf("%s:%d", basename(f), node$line1)
      if (is.na(output_of(pd, bid))) { cat("RĘCZNIE", tag, "(pudełko w UI)\n"); next }
      a <- call_args(pd, bid)
      pos <- Filter(function(x) is.na(x$name), a); nm <- Filter(function(x) !is.na(x$name), a)
      names(nm) <- vapply(nm, `[[`, "", "name")
      if (!is.null(nm$caption)) { cat("RĘCZNIE", tag, "(caption)\n"); next }
      if (length(pos) < 2) { cat("RĘCZNIE", tag, "(argumenty)\n"); next }
      val <- if (length(pos) >= 3) paste0("paste0(", pos[[2]]$text, ", ", pos[[3]]$text, ")") else pos[[2]]$text
      new <- paste0("lc_readout(", pos[[1]]$text, ", ", val, if (!is.null(nm$color)) paste0(", color = ", nm$color$text), ")")
      edits[[length(edits)+1]] <- list(l1 = node$line1, c1 = node$col1, l2 = node$line2, c2 = node$col2, text = new)
      cat("OK", tag, "\n")
    }
    # opakowania z samymi pudełkami
    wr <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% c("lc_center", "lc_stat_grid"), ]
    for (i in seq_len(nrow(wr))) {
      wid <- pd$parent[pd$id == wr$parent[i]]; node <- pd[pd$id == wid, ]
      a <- call_args(pd, wid)
      pos <- Filter(function(x) is.na(x$name), a)
      if (length(pos) && all(vapply(pos, function(x) identical(x$fn, "lc_stat_box"), logical(1))) && !is.na(output_of(pd, wid))) {
        # tylko nazwa funkcji: lc_center( → tagList(; argument columns = usuwamy
        fnode <- kids_of(pd, kids_of(pd, wid)$id[1])[1, ]
        edits[[length(edits)+1]] <- list(l1 = fnode$line1, c1 = fnode$col1, l2 = fnode$line2, c2 = fnode$col2, text = "tagList")
        for (x in Filter(function(x) identical(x$name, "columns"), a)) {
          # usuń ", columns = N" razem z poprzedzającym przecinkiem: zastępujemy cały argument pustym NULL
          edits[[length(edits)+1]] <- list(l1 = x$node$line1, c1 = x$node$col1, l2 = x$node$line2, c2 = x$node$col2, text = "NULL")
        }
      }
    }
    if (mode == "apply" && length(edits)) {
      src <- apply_edits(src, edits)
      src <- gsub(",\\s*columns = NULL", "", src)
      writeLines(src, f, useBytes = TRUE)
    }
  } else {
    rs <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "lc_readout", ]
    outs <- unique(na.omit(vapply(rs$parent, function(id) output_of(pd, id), "")))
    for (o in outs) {
      st <- pd[pd$token == "STR_CONST" & pd$text == paste0('"', o, '"'), ]
      for (j in seq_len(nrow(st))) {
        uo <- pd$parent[pd$id == pd$parent[pd$id == st$id[j]]]
        if (!identical(fn_of(pd, uo), "uiOutput")) next
        parent <- pd$parent[pd$id == uo]
        pfn <- fn_of(pd, parent)
        if (identical(pfn, "lc_readouts")) next
        uon <- pd[pd$id == uo, ]
        if (identical(pfn, "figure_panel")) {
          args2 <- call_args(pd, parent)
          tb <- Filter(function(x) identical(x$fn, "lc_toolbar"), args2)
          if (length(tb)) {
            tbn <- pd[pd$id == tb[[1]]$id, ]
            # wstaw przed zamykającym nawiasem paska
            close <- kids_of(pd, tb[[1]]$id); close <- close[nrow(close), ]
            edits[[length(edits)+1]] <- list(l1 = close$line1, c1 = close$col1, l2 = close$line1, c2 = close$col1 - 1,
              text = paste0(",\n", strrep(" ", tbn$col1 + 1), "lc_readouts(", getParseText(pd, uo), ")\n", strrep(" ", tbn$col1 - 1)))
            # usuń uiOutput z panelu (z przecinkiem przed nim)
            edits[[length(edits)+1]] <- list(l1 = uon$line1, c1 = uon$col1, l2 = uon$line2, c2 = uon$col2, text = "NULL")
            cat("DO PASKA", basename(f), o, "\n"); next
          }
        }
        edits[[length(edits)+1]] <- list(l1 = uon$line1, c1 = uon$col1, l2 = uon$line2, c2 = uon$col2,
                                         text = paste0("lc_readouts(", getParseText(pd, uo), ")"))
        cat("OWINIĘTE", basename(f), o, "\n")
      }
    }
    if (length(edits)) {
      src <- apply_edits(src, edits)
      src <- gsub(",\\s*NULL(\\s*\\))", "\\1", src)
      txt <- paste(src, collapse = "\n"); txt <- gsub(",\\n(\\s*)NULL,", ",", txt); txt <- gsub("\\n\\s*NULL,\\n", "\n", txt)
      txt <- gsub("\\n\\s*,\\n(\\s*lc_readouts\\()", ",\n\\1", txt, perl = TRUE)
      txt <- gsub(",\\n\\s*NULL(\\n\\s*\\))", "\\1", txt, perl = TRUE)
      writeLines(strsplit(txt, "\n", fixed = TRUE)[[1]], f, useBytes = TRUE)
    }
  }
}
