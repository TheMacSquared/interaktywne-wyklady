# ==============================================================================
# Wykrywanie wykładów — źródło listy dla huba
# ==============================================================================
#
# Zasada: wykładem jest każdy katalog <przedmiot>/<wyklad>/ zawierający app.R,
# którego przedmiot ma własne R/lecture_layout.R (ten sam warunek, którego
# używa .find_app_dir() w każdym app.R).
#
# Dzięki temu nowy wykład wystarczy dodać jako katalog — hub zobaczy go sam,
# bez edycji kodu huba. Głębokość globa (przedmiot/wykład) świadomie pomija
# materiały źródłowe leżące głębiej, np. statystyka/materialy-zrodlowe/...
#
# ==============================================================================


# ---- Odczyt metadanych z app.R -----------------------------------------------

# Wyciąga wartość argumentu lecture_page() metodą tekstową, bez parsowania R.
# Ten sam trik, którego używa analiza-ryzyka/scripts/wyklad (title_of()).
.hub_field <- function(txt, field) {
  pattern <- sprintf('%s[ \\t]*=[ \\t]*"[^"]*"', field)
  hit <- regmatches(txt, regexpr(pattern, txt, perl = TRUE))
  if (length(hit) == 0) return(NA_character_)
  value <- sub('^[^"]*"', "", hit[[1]])
  sub('"$', "", value)
}

.hub_read_app <- function(path) {
  con <- file(path, encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  paste(readLines(con, warn = FALSE), collapse = "\n")
}


# ---- Liczba rozdziałów -------------------------------------------------------
#
# Rozdziały są definiowane w repo na trzy sposoby, więc zamiast regexów używamy
# parsera R: czytamy AST bez wykonywania kodu (parse(), nie source()).
#
#   1. .chapters <- list(ch1_ui, ch2_ui)        — statystyka, ekonometria
#   2. .chapters <- <symbol>                    — analiza ryzyka; symbol wskazuje
#      na risk_block_chapters(<blok>), a liczbę rozdziałów ma argument
#      chapters = list(...) w definicji bloku w modules/
#   3. cokolwiek innego                         — zwracamy NA, hub chowa badge
#
# Dzięki parsowaniu nowy wykład w którymkolwiek ze stylów liczy się sam.

# Zbiera przypisania najwyższego poziomu z pliku jako niezewaluowane wyrażenia.
.hub_assignments <- function(path) {
  if (!file.exists(path)) return(list())
  exprs <- tryCatch(parse(path, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(exprs)) return(list())

  out <- list()
  for (e in exprs) {
    if (!is.call(e) || length(e) != 3L) next
    if (!as.character(e[[1]]) %in% c("<-", "=", "<<-")) next
    if (!is.symbol(e[[2]])) next
    out[[as.character(e[[2]])]] <- e[[3]]
  }
  out
}

# Liczba elementów w wywołaniu list(...); NA gdy to nie jest literał listy.
.hub_list_length <- function(expr) {
  if (is.null(expr)) return(NA_integer_)
  if (is.call(expr) && identical(as.character(expr[[1]]), "list")) {
    return(length(expr) - 1L)
  }
  NA_integer_
}

hub_chapter_count <- function(dir) {
  chapters <- .hub_assignments(file.path(dir, "app.R"))[[".chapters"]]
  if (is.null(chapters)) return(NA_integer_)

  # Styl 1: literał listy prosto w app.R.
  n <- .hub_list_length(chapters)
  if (!is.na(n)) return(n)

  # Styl 2: symbol zdefiniowany w modules/ — zbieramy przypisania z modułów.
  if (!is.symbol(chapters)) return(NA_integer_)
  defs <- list()
  for (m in list.files(file.path(dir, "modules"), pattern = "[.][Rr]$", full.names = TRUE)) {
    defs <- utils::modifyList(defs, .hub_assignments(m))
  }

  rhs <- defs[[as.character(chapters)]]
  n <- .hub_list_length(rhs)
  if (!is.na(n)) return(n)

  # np. risk_block_chapters(warunki_block) — schodzimy do definicji bloku.
  if (is.call(rhs) && length(rhs) >= 2L && is.symbol(rhs[[2]])) {
    block <- defs[[as.character(rhs[[2]])]]
    if (is.call(block)) return(.hub_list_length(as.list(block)[["chapters"]]))
  }

  NA_integer_
}


# ---- Etykieta przedmiotu -----------------------------------------------------

hub_subject_label <- function(subject) {
  words <- strsplit(subject, "-", fixed = TRUE)[[1]]
  words[1] <- paste0(toupper(substring(words[1], 1, 1)), substring(words[1], 2))
  paste(words, collapse = " ")
}


# ---- Skan repo ---------------------------------------------------------------

hub_discover <- function(repo_root) {
  paths <- Sys.glob(file.path(repo_root, "*", "*", "app.R"))

  rows <- lapply(paths, function(path) {
    dir     <- dirname(path)
    subject <- basename(dirname(dir))
    folder  <- basename(dir)

    # Przedmiot musi mieć własny system layoutu — inaczej to nie jest wykład.
    if (!file.exists(file.path(dirname(dir), "R", "lecture_layout.R"))) return(NULL)

    txt <- .hub_read_app(path)
    title <- .hub_field(txt, "lecture_title")
    num   <- .hub_field(txt, "lecture_num")

    data.frame(
      key          = paste(subject, folder, sep = "/"),
      subject      = subject,
      folder       = folder,
      dir          = dir,
      lecture_id   = .hub_field(txt, "lecture_id"),
      num          = if (is.na(num)) "" else num,
      title        = if (is.na(title)) folder else title,
      module_label = .hub_field(txt, "module_label"),
      chapters     = hub_chapter_count(dir),
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(
      key = character(0), subject = character(0), folder = character(0),
      dir = character(0), lecture_id = character(0), num = character(0),
      title = character(0), module_label = character(0), chapters = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  catalog <- do.call(rbind, rows)

  # Sortowanie: przedmiot alfabetycznie, wykład po numerze.
  # Wykłady bez numeru lądują na końcu przedmiotu (fallback na nazwę katalogu).
  num_sort <- suppressWarnings(as.numeric(catalog$num))
  num_sort[is.na(num_sort)] <- Inf
  catalog <- catalog[order(catalog$subject, num_sort, catalog$folder), ]
  rownames(catalog) <- NULL

  catalog$subject_label <- vapply(catalog$subject, hub_subject_label, character(1))
  catalog
}
