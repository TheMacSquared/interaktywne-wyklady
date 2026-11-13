testthat::test_that("widgety Chart.js zwalniają zasoby i wracają po zmianie rozdziału", {
  node <- Sys.which("node")
  testthat::skip_if(!nzchar(node), "Testy JavaScript wymagają Node.js.")
  # Wszystkie pliki *.test.js (wspólna atrapa DOM: tests/js/harness.js).
  files <- list.files(file.path(stat_root, "tests", "js"), pattern = "\\.test\\.js$", full.names = TRUE)
  result <- suppressWarnings(system2(
    node, c("--test", shQuote(files)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(result, "status")
  testthat::expect_true(is.null(status) || identical(status, 0L),
                        info = paste(result, collapse = "\n"))
})
