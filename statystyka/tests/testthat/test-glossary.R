load_glossary <- function() {
  env <- new.env(parent = globalenv())
  env$tags <- shiny::tags
  sys.source(file.path(stat_root, "R", "glossary.R"), envir = env)
  env
}

# Zwraca hasła z wywołań gloss("hasło"[, "forma"]) bez definicji inline.
find_gloss_terms <- function(file) {
  pd <- utils::getParseData(parse(file, keep.source = TRUE))
  calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "gloss", ]
  terms <- character()

  for (i in seq_len(nrow(calls))) {
    call_id <- pd$parent[pd$id == calls$parent[i]]
    children <- pd[pd$parent == call_id, ]
    if (any(children$token == "SYMBOL_SUB" & children$text == "definition")) next
    args <- children[children$token == "expr", ]
    args <- args[order(args$line1, args$col1), ]
    # args[1] to samo `gloss`, args[2] to hasło
    if (nrow(args) < 2) next
    str <- pd[pd$parent == args$id[2] & pd$token == "STR_CONST", "text"]
    if (length(str) == 1) {
      terms <- c(terms, sprintf("%s:%d %s", basename(file), calls$line1[i],
                                eval(parse(text = str))))
    }
  }
  terms
}

testthat::test_that("każdy termin ze słownika ma niepustą definicję", {
  env <- load_glossary()
  defs <- unlist(env$.GLOSSARY)
  testthat::expect_true(all(nzchar(trimws(defs))))
  testthat::expect_false(anyDuplicated(names(env$.GLOSSARY)) > 0)
})

testthat::test_that("każde gloss() w wykładach ma hasło w słowniku", {
  env <- load_glossary()
  files <- list.files(stat_root, pattern = "[.]R$", recursive = TRUE,
                      full.names = TRUE)
  files <- files[!grepl("/(R|tests|robocze)/", files)]

  found <- unlist(lapply(files, find_gloss_terms), use.names = FALSE)
  testthat::expect_true(length(found) > 0)

  term <- sub("^\\S+ ", "", found)
  missing <- found[!term %in% names(env$.GLOSSARY)]
  testthat::expect_equal(missing, character(),
                         info = paste(missing, collapse = "\n"))
})
