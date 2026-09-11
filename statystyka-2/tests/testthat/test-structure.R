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

