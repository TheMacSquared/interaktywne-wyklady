testthat::test_that("syntetyczny zbiór satelitarny ma stabilny kontrakt", {
  path <- file.path(stat_root, "dane", "satelitarne_obserwacje.csv")
  testthat::expect_true(file.exists(path))

  d <- read.csv(path, stringsAsFactors = FALSE)
  required <- c(
    "id_lokalizacji", "data_obserwacji", "region", "strefa", "typ_pokrycia",
    "szerokosc_geo", "dlugosc_geo", "wysokosc_m", "zachmurzenie_pct", "ndvi",
    "grunt_temp_c", "sat_temp_c", "roznica_temp_c", "jakosc_pomiaru",
    "pomiar_dostepny"
  )

  testthat::expect_equal(nrow(d), 180)
  testthat::expect_setequal(names(d), required)
  testthat::expect_false(anyDuplicated(d$id_lokalizacji) > 0)
  testthat::expect_true(all(d$zachmurzenie_pct >= 0 & d$zachmurzenie_pct <= 100))
  testthat::expect_true(all(d$ndvi >= -1 & d$ndvi <= 1))
  testthat::expect_true(all(d$pomiar_dostepny %in% c("tak", "nie")))
  testthat::expect_true(all(d$jakosc_pomiaru %in%
                            c("dobra", "ograniczona", "odrzucona")))
  testthat::expect_lt(
    max(abs(d$roznica_temp_c - (d$sat_temp_c - d$grunt_temp_c))),
    0.021
  )
})

testthat::test_that("zbiór niesie zaplanowane sygnały dydaktyczne", {
  d <- read.csv(file.path(stat_root, "dane", "satelitarne_obserwacje.csv"),
                stringsAsFactors = FALSE)

  testthat::expect_gt(mean(d$roznica_temp_c), 0.5)
  testthat::expect_gt(
    mean(d$sat_temp_c[d$strefa == "miejska"]),
    mean(d$sat_temp_c[d$strefa == "zielona"])
  )
  testthat::expect_gt(
    mean(d$ndvi[d$typ_pokrycia == "las"]),
    mean(d$ndvi[d$strefa == "miejska"])
  )
})

testthat::test_that("wariant satelitarny jest podłączony do rdzenia kursu", {
  files <- c(
    "01-typy-danych/modules/ch8_cwiczenia.R",
    "02-rozklady-prawdopodobienstwa/modules/ch9_cwiczenia.R",
    "03-przedzialy-ufnosci/modules/ch7_cwiczenia.R",
    "04-wnioskowanie-statystyczne/modules/ch9_cwiczenia.R",
    "06-regresja/modules/ch7_cwiczenia.R"
  )
  text <- vapply(files, function(file) {
    paste(readLines(file.path(stat_root, file), warn = FALSE, encoding = "UTF-8"),
          collapse = "\n")
  }, character(1))

  testthat::expect_true(all(grepl("sat", text, fixed = TRUE)))
})
