testthat::test_that("wszystkie wykłady ładują UI i kompletną listę rozdziałów", {
  testthat::skip_if_not_installed("callr")

  results <- lapply(names(expected_apps), function(app_name) {
    callr::r(
      function(path, expected_count) {
        setwd(path)
        env <- new.env(parent = globalenv())
        sys.source("app.R", envir = env)
        stopifnot(exists("ui", envir = env, inherits = FALSE))
        stopifnot(is.function(env$server))
        stopifnot(length(env$.chapters) == expected_count)
        ids <- vapply(env$.chapters, function(chapter) chapter$id, character(1))
        stopifnot(all(nzchar(ids)), !anyDuplicated(ids))
        extract_ids <- function(html) {
          # Tylko atrybut id, bez końcówek typu aria-invalid="true".
          matches <- regmatches(html, gregexpr('(?<![[:alnum:]_-])id="[^"]+"', html, perl = TRUE))[[1]]
          sub('^id="|"$', "", matches)
        }
        html <- htmltools::renderTags(env$ui)$html
        ui_ids <- extract_ids(html)
        stopifnot(!anyDuplicated(ui_ids))
        # Rozdziały pozostają zamontowane razem; identyfikatory muszą być unikalne
        # także między rozdziałami, nie tylko wewnątrz pojedynczego widoku.
        all_chapter_ids <- character(0)
        h2_ids <- character(0)
        for (chapter in env$.chapters) {
          chapter_html <- htmltools::renderTags(chapter$content)$html
          chapter_ids <- extract_ids(chapter_html)
          all_chapter_ids <- c(all_chapter_ids, chapter_ids)
          if (anyDuplicated(chapter_ids)) {
            stop(sprintf("Powtórzone id w rozdziale %s: %s", chapter$id,
              paste(unique(chapter_ids[duplicated(chapter_ids)]), collapse = ", ")))
          }
          h2 <- regmatches(chapter_html, gregexpr('<h2[^>]*id="[^"]+"', chapter_html))[[1]]
          h2_ids <- c(h2_ids, sub('.*id="([^"]+)"', "\\1", h2))
        }
        stopifnot(!anyDuplicated(c(ui_ids, all_chapter_ids)))
        if (anyDuplicated(h2_ids)) {
          stop(sprintf("Powtórzone id sekcji w wykładzie: %s",
            paste(unique(h2_ids[duplicated(h2_ids)]), collapse = ", ")))
        }
        TRUE
      },
      args = list(
        path = file.path(risk_root, app_name),
        expected_count = unname(expected_apps[[app_name]])
      ),
      timeout = 60,
      spinner = FALSE,
      show = FALSE
    )
  })
  testthat::expect_true(all(vapply(results, isTRUE, logical(1))))
})
