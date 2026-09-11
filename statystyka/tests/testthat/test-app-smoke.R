testthat::test_that("każda aplikacja ładuje UI i kompletną listę rozdziałów", {
  testthat::skip_if(
    identical(Sys.getenv("STATYSTYKA_SKIP_APP_SMOKE"), "true"),
    "Tryb --quick pomija smoke testy aplikacji."
  )
  testthat::skip_if_not_installed("callr")

  results <- lapply(expected_apps, function(app_name) {
    app_dir <- file.path(stat_root, app_name)
    tryCatch(
      callr::r(
        function(path) {
          setwd(path)
          env <- new.env(parent = globalenv())
          sys.source("app.R", envir = env)

          stopifnot(exists("ui", envir = env, inherits = FALSE))
          stopifnot(exists("server", envir = env, inherits = FALSE))
          stopifnot(is.function(env$server))
          stopifnot(exists(".chapters", envir = env, inherits = FALSE))

          chapters <- env$.chapters
          stopifnot(length(chapters) > 0)
          ids <- vapply(chapters, function(ch) ch$id, character(1))
          stopifnot(all(nzchar(ids)), !anyDuplicated(ids))
          # Nawigacja zawiera wyłącznie wykłady należące do tego przedmiotu.
          subject_root <- dirname(path)
          app_dirs <- list.dirs(subject_root, recursive = FALSE, full.names = TRUE)
          app_dirs <- app_dirs[file.exists(file.path(app_dirs, "app.R"))]
          stopifnot(length(env$.LC_MODULES) == length(app_dirs))
          stopifnot(length(env$.LC_LECTURE_MODULE) == length(app_dirs))
          html <- htmltools::renderTags(env$ui)$html
          subject_title <- if (basename(subject_root) == "statystyka-2") "Statystyka 2" else "Statystyka"
          stopifnot(grepl(paste0('class="lc-tabs-logo-title">', subject_title, '</div>'), html, fixed = TRUE))
          TRUE
        },
        args = list(path = app_dir),
        timeout = 60,
        spinner = FALSE,
        show = FALSE
      ),
      error = function(e) conditionMessage(e)
    )
  })
  names(results) <- expected_apps

  failures <- vapply(results, function(x) !isTRUE(x), logical(1))
  testthat::expect_false(
    any(failures),
    info = paste(sprintf("%s: %s", names(results)[failures],
                         unlist(results[failures])), collapse = "\n")
  )
})
