testthat::test_that("kurs zawiera wszystkie oczekiwane aplikacje", {
  app_dirs <- sort(list.dirs(stat_root, recursive = FALSE, full.names = FALSE))
  app_dirs <- app_dirs[file.exists(file.path(stat_root, app_dirs, "app.R"))]
  testthat::expect_setequal(app_dirs, expected_apps)
})

testthat::test_that("wszystkie pliki R przechodzą parsowanie", {
  files <- list.files(stat_root, pattern = "[.]R$", recursive = TRUE,
                      full.names = TRUE)
  failures <- character()

  for (file in files) {
    tryCatch(
      parse(file, keep.source = FALSE),
      error = function(e) {
        failures <<- c(failures, sprintf("%s: %s", file, conditionMessage(e)))
      }
    )
  }

  testthat::expect_equal(failures, character(),
                         info = paste(failures, collapse = "\n"))
})

testthat::test_that("statyczne obrazy mają tekst alternatywny", {
  files <- list.files(stat_root, pattern = "[.]R$", recursive = TRUE,
                      full.names = TRUE)
  image_blocks <- unlist(lapply(files, function(file) {
    text <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    hits <- gregexpr("tags\\$img\\s*\\([^)]*\\)", text, perl = TRUE)[[1]]
    if (identical(hits[1], -1L)) return(character())
    regmatches(text, list(hits))[[1]]
  }), use.names = FALSE)

  testthat::expect_true(length(image_blocks) > 0)
  testthat::expect_true(all(grepl("\\balt\\s*=", image_blocks)),
                        info = paste(image_blocks[!grepl("\\balt\\s*=", image_blocks)],
                                     collapse = "\n---\n"))
})
