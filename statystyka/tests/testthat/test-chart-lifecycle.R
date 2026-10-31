testthat::test_that("widgety Chart.js zwalniają zasoby i wracają po zmianie rozdziału", {
  node <- Sys.which("node")
  testthat::skip_if(!nzchar(node), "Testy JavaScript wymagają Node.js.")
  result <- suppressWarnings(system2(
    node, shQuote(file.path(stat_root, "tests", "js", "lc_charts.test.js")),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(result, "status")
  testthat::expect_true(is.null(status) || identical(status, 0L),
                        info = paste(result, collapse = "\n"))
})
