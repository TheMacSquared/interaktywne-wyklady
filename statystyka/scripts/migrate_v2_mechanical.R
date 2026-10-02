#!/usr/bin/env Rscript
# Mechaniczne zamiany kontrolek na komponenty v2 (etap 2 migracji).
#
# Użycie:
#   Rscript statystyka/scripts/migrate_v2_mechanical.R report <rodzaje> <pliki...>
#   Rscript statystyka/scripts/migrate_v2_mechanical.R apply  <rodzaje> <pliki...>
# rodzaje: lista po przecinku z slider,radio,button,plot
#
# Skrypt czyta drzewo składni R (getParseData), więc argumenty przenosi
# dosłownie, także wieloliniowe. Pomija przypadki, których nie da się
# zamienić 1:1, i wypisuje powód. Raport idzie na stdout jako TSV.

args <- commandArgs(trailingOnly = TRUE)
mode <- args[1]
kinds <- strsplit(args[2], ",", fixed = TRUE)[[1]]
files <- args[-(1:2)]
stopifnot(mode %in% c("report", "apply"))

formals_of <- list(
  sliderInput = c("inputId", "label", "min", "max", "value", "step", "round",
                  "ticks", "animate", "width", "sep", "pre", "post",
                  "timeFormat", "timezone", "dragRange"),
  radioButtons = c("inputId", "label", "choices", "selected", "inline", "width",
                   "choiceNames", "choiceValues"),
  actionButton = c("inputId", "label", "icon", "width", "disabled"),
  zoom_plot_ui = c("id", "height", "width")
)
kind_of <- c(sliderInput = "slider", radioButtons = "radio",
             actionButton = "button", zoom_plot_ui = "plot")

is_string <- function(x) grepl('^"[^"]*"$', x) || grepl("^'[^']*'$", x)
unquote <- function(x) substr(x, 2, nchar(x) - 1)
# Etykieta stoi nad kontrolką, więc końcowy dwukropek jest zbędny.
strip_colon <- function(x) if (is_string(x)) sub("\\s*:\\s*([\"'])$", "\\1", x) else x

# Argumenty wywołania: lista list(name, text) w kolejności z kodu.
call_args <- function(pd, call_id) {
  kids <- pd[pd$parent == call_id, ]
  kids <- kids[order(kids$line1, kids$col1), ]
  out <- list(); pending <- NA_character_
  for (i in seq_len(nrow(kids))[-1]) {
    k <- kids[i, ]
    if (k$token == "SYMBOL_SUB") pending <- k$text
    else if (k$token == "expr") {
      out[[length(out) + 1]] <- list(name = pending, text = getParseText(pd, k$id))
      pending <- NA_character_
    }
  }
  out
}

match_args <- function(a, formal_names) {
  res <- list(); pos <- formal_names
  for (x in a) {
    if (!is.na(x$name)) {
      res[[x$name]] <- x$text
      pos <- setdiff(pos, x$name)
    }
  }
  for (x in a) {
    if (is.na(x$name)) {
      if (!length(pos)) return(NULL)
      res[[pos[1]]] <- x$text
      pos <- pos[-1]
    }
  }
  res
}

ancestor_calls <- function(pd, id) {
  out <- character(0)
  repeat {
    id <- pd$parent[pd$id == id]
    if (!length(id) || id == 0) break
    kids <- pd[pd$parent == id, ]
    fn <- kids[kids$token == "expr", ][1, ]
    if (!is.na(fn$id)) {
      sym <- pd[pd$parent == fn$id & pd$token == "SYMBOL_FUNCTION_CALL", "text"]
      if (length(sym)) out <- c(out, sym)
    }
  }
  out
}

convert <- function(fun, m, ancestors) {
  if (fun == "sliderInput") {
    if (is.null(m$label) || m$label == "NULL") return(list(skip = "brak etykiety"))
    if (!is.null(m$animate) && m$animate != "FALSE") return(list(skip = "animate"))
    if (!is.null(m$pre)) return(list(skip = "pre"))
    if (!is.null(m$timeFormat) || !is.null(m$dragRange)) return(list(skip = "czas/zakres"))
    if (grepl("^c\\(", m$value)) return(list(skip = "suwak zakresowy"))
    if (!is.null(m$post) && !is_string(m$post)) return(list(skip = "post nie jest tekstem"))
    parts <- c(m$inputId, strip_colon(m$label), m$min, m$max, m$value,
               if (!is.null(m$step)) m$step,
               if (!is.null(m$post)) paste0("suffix = ", m$post))
    return(list(text = paste0("lc_slider(", paste(parts, collapse = ", "), ")")))
  }
  if (fun == "radioButtons") {
    if (!is.null(m$choiceNames) || is.null(m$choices)) return(list(skip = "choiceNames"))
    if (!is.null(m$selected) && grepl("character\\(0\\)", m$selected)) return(list(skip = "brak wyboru"))
    ch <- tryCatch(eval(parse(text = m$choices), envir = baseenv()), error = function(e) NULL)
    if (is.null(ch) || !is.character(ch)) return(list(skip = "choices nie są literałem"))
    labels <- if (is.null(names(ch))) ch else ifelse(nzchar(names(ch)), names(ch), ch)
    if (length(ch) > 4) return(list(skip = paste(length(ch), "opcji")))
    if (max(nchar(labels)) > 24) return(list(skip = "długie etykiety"))
    parts <- c(m$inputId, if (is.null(m$label)) "NULL" else strip_colon(m$label),
               paste0("choices = ", m$choices),
               if (!is.null(m$selected)) paste0("selected = ", m$selected))
    return(list(text = paste0("lc_segmented(", paste(parts, collapse = ", "), ")")))
  }
  if (fun == "actionButton") {
    extra <- setdiff(names(m), c("inputId", "label", "class", "width"))
    if (length(extra)) return(list(skip = paste("argumenty:", paste(extra, collapse = ","))))
    if (is.null(m$class) || !is_string(m$class) || !grepl("lc-btn", m$class)) {
      return(list(skip = "bez klasy lc-btn"))
    }
    cls <- unquote(m$class)
    variant <- if (grepl("lc-btn-(primary|ok|danger|warning)", cls)) "solid" else "outline"
    label_txt <- if (is_string(m$label)) unquote(m$label) else NA
    if (!is.na(label_txt) && tolower(trimws(label_txt)) %in% c("reset", "resetuj", "wyzeruj")) {
      return(list(text = sprintf('lc_action(%s, icon = "reset", variant = "ghost", aria_label = %s)',
                                 m$inputId, m$label)))
    }
    icon <- if (!is.na(label_txt) && grepl("^losuj", tolower(label_txt))) ', icon = "shuffle"' else ""
    return(list(text = sprintf('lc_action(%s, %s%s, variant = "%s")',
                               m$inputId, m$label, icon, variant)))
  }
  if (fun == "zoom_plot_ui") {
    if (any(ancestors %in% c("column", "splitLayout", "lc_widget_layout"))) return(list(skip = "w kolumnie"))
    if (!is.null(m$width)) return(list(skip = "width"))
    if (is.null(m$height) || !grepl('^"[0-9]+px"$', m$height)) return(list(skip = "wysokość nie w px"))
    h <- as.numeric(gsub("[^0-9]", "", m$height))
    ratio <- round(620 / h, 1)
    parts <- c(m$id,
               if (abs(ratio - 2.2) > 0.15) sprintf('ratio = "%s/1"', ratio),
               if (h != 440) sprintf('max_height = "%dpx"', as.integer(h)))
    return(list(text = paste0("lc_plot(", paste(parts, collapse = ", "), ")")))
  }
}

report <- list()
for (f in files) {
  src <- readLines(f, encoding = "UTF-8", warn = FALSE)
  pd <- getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"))
  syms <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% names(kind_of), ]
  syms <- syms[kind_of[syms$text] %in% kinds, ]
  edits <- list()
  for (i in seq_len(nrow(syms))) {
    fun <- syms$text[i]
    fn_expr <- syms$parent[i]
    call_id <- pd$parent[pd$id == fn_expr]
    node <- pd[pd$id == call_id, ]
    m <- match_args(call_args(pd, call_id), formals_of[[fun]])
    if (fun == "actionButton" && is.null(m)) m <- NULL
    res <- if (is.null(m)) list(skip = "nie udało się dopasować argumentów")
           else convert(fun, m, ancestor_calls(pd, call_id))
    report[[length(report) + 1]] <- data.frame(
      file = f, line = node$line1, kind = kind_of[[fun]],
      status = if (!is.null(res$text)) "zamiana" else "pominięte",
      detail = gsub("[\t\n ]+", " ", if (!is.null(res$text)) res$text else res$skip))
    if (!is.null(res$text)) {
      edits[[length(edits) + 1]] <- list(l1 = node$line1, c1 = node$col1,
        l2 = node$line2, c2 = node$col2, text = res$text)
    }
  }
  if (mode == "apply" && length(edits)) {
    ord <- order(-sapply(edits, `[[`, "l1"), -sapply(edits, `[[`, "c1"))
    for (e in edits[ord]) {
      before <- substr(src[e$l1], 1, e$c1 - 1)
      after <- substr(src[e$l2], e$c2 + 1, nchar(src[e$l2]))
      new_lines <- strsplit(paste0(before, e$text, after), "\n", fixed = TRUE)[[1]]
      src <- c(src[seq_len(e$l1 - 1)], new_lines,
               if (e$l2 < length(src)) src[(e$l2 + 1):length(src)])
    }
    writeLines(src, f, useBytes = TRUE)
  }
}
out <- do.call(rbind, report)
if (!is.null(out)) utils::write.table(out, stdout(), sep = "\t", row.names = FALSE, quote = FALSE)
