#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
test_dir <- if (length(file_arg)) {
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
} else {
  normalizePath("testy", mustWork = TRUE)
}
root <- normalizePath(file.path(test_dir, ".."), mustWork = TRUE)

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Pakiet testthat jest wymagany.", call. = FALSE)
}
source(file.path(root, "narzedzia", "graph_core.R"), local = TRUE)

testthat::test_that("graf przykładowy przechodzi pełną walidację", {
  results <- compile_index(root, "kurs-analiza-ryzyka", write_output = FALSE)
  testthat::expect_length(results, 1L)
  testthat::expect_true(results[[1]]$ok)
  testthat::expect_equal(results[[1]]$graph$meta$wezlow, 42L)
  testthat::expect_equal(results[[1]]$graph$meta$krawedzi, 80L)
  node_types <- vapply(results[[1]]$graph$nodes, function(node) node$typ, character(1))
  edge_types <- vapply(results[[1]]$graph$edges, function(edge) edge$typ, character(1))
  testthat::expect_equal(sum(node_types == "wyklad"), 10L)
  testthat::expect_equal(sum(node_types == "pytanie"), 10L)
  testthat::expect_equal(sum(edge_types == "poprzedza"), 9L)
  testthat::expect_length(results[[1]]$graph$raport$nieomawiane, 0L)
  testthat::expect_equal(
    results[[1]]$graph$raport$pytania_bez_odpowiedzi,
    "pytanie-decyzja"
  )
  testthat::expect_equal(
    results[[1]]$graph$raport$twierdzenia_bez_zrodel,
    "twierdzenie-bayesa"
  )
  testthat::expect_length(results[[1]]$graph$raport$wezly_odlaczone, 0L)
})

testthat::test_that("ontologia lokalna nie redefiniuje typów bazowych", {
  base <- list(typy_wezlow = list("pojecie"), typy_relacji = list())
  local <- list(typy_wezlow = list("pojecie"), typy_relacji = list())
  testthat::expect_error(merge_ontologies(base, local), "redefiniuje typy")
})

testthat::test_that("wykrywane są cykle relacji acyklicznej", {
  edges <- list(
    list(source = "a", target = "b", typ = "wymaga"),
    list(source = "b", target = "a", typ = "wymaga")
  )
  testthat::expect_true(check_cycle(c("a", "b"), edges, "wymaga"))
  testthat::expect_false(check_cycle(c("a", "b"), edges[1], "wymaga"))
})

testthat::test_that("błędny stan węzła jest raportowany", {
  allowed <- list(zrozumienie = list("zalazek", "rozumiem"))
  errors <- validate_state("x", list(zrozumienie = "nieznany"), allowed, character())
  testthat::expect_match(errors, "niedozwolona wartość")
})
