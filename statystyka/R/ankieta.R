# Ankieta studencka (n = 200): wspólne dane wykładów 01 i 02.
# Wczytywana przez source(..., local = TRUE) z app.R wykładu; definiuje
# student_data i variable_meta. Kolejność losowań jest częścią danych:
# nowe kolumny dopisujemy na końcu, żeby nie zmieniać istniejących.

# ============================================================================
# ZBIOR DANYCH: ANKIETA STUDENCKA
# ============================================================================

set.seed(2024)
n <- 200

plec <- sample(c("Kobieta", "Mężczyzna"), n, replace = TRUE, prob = c(0.55, 0.45))

student_data <- data.frame(
  plec = factor(plec),
  kierunek = factor(sample(
    c("Informatyka", "Biologia", "Psychologia", "Ekonomia"),
    n, replace = TRUE, prob = c(0.3, 0.2, 0.25, 0.25)
  )),
  grupa_krwi = factor(sample(
    c("A", "B", "AB", "0"),
    n, replace = TRUE, prob = c(0.35, 0.2, 0.08, 0.37)
  )),
  rok_studiow = factor(
    sample(1:5, n, replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1)),
    levels = 1:5, ordered = TRUE
  ),
  zadowolenie = factor(
    sample(
      c("Bardzo niezadowolony", "Niezadowolony", "Neutralny", "Zadowolony", "Bardzo zadowolony"),
      n, replace = TRUE, prob = c(0.05, 0.15, 0.30, 0.35, 0.15)
    ),
    levels = c("Bardzo niezadowolony", "Niezadowolony", "Neutralny", "Zadowolony", "Bardzo zadowolony"),
    ordered = TRUE
  ),
  liczba_kursow = sample(3:9, n, replace = TRUE),
  liczba_nieobecnosci = rpois(n, lambda = 3),
  wzrost = round(ifelse(
    plec == "Kobieta",
    rnorm(n, mean = 166, sd = 6),
    rnorm(n, mean = 178, sd = 7)
  ), 1),
  srednia_ocen = round(pmin(5.0, pmax(2.0, rnorm(n, mean = 3.8, sd = 0.6))), 2),
  czas_dojazdu = round(rgamma(n, shape = 3, scale = 10) + 5, 1),
  waga = round(ifelse(
    plec == "Kobieta",
    rnorm(n, mean = 62, sd = 8),
    rnorm(n, mean = 80, sd = 10)
  ), 1),
  ocena_wykładowcy = factor(
    sample(1:10, n, replace = TRUE,
           prob = c(0.02, 0.03, 0.05, 0.08, 0.12, 0.15, 0.20, 0.18, 0.12, 0.05)),
    levels = 1:10, ordered = TRUE
  ),
  # Silnie prawoskośna (log-normalna); dopisana na końcu, żeby nie zmieniać
  # losowań wcześniejszych kolumn.
  wydatki = round(rlnorm(n, meanlog = log(800), sdlog = 0.8), -1),
  stringsAsFactors = FALSE
)

# Ankietowa płeć ma cztery odpowiedzi. Część osób przeetykietowujemy dopiero
# tutaj, żeby nie zmieniać losowań wcześniejszych kolumn (wzrost i waga
# zależą od pierwotnego losowania płci).
plec_u <- runif(n)
plec_ankieta <- as.character(student_data$plec)
plec_ankieta[plec_u < 0.04] <- "Niebinarna"
plec_ankieta[plec_u >= 0.04 & plec_u < 0.08] <- "Odmowa odpowiedzi"
student_data$plec <- factor(
  plec_ankieta,
  levels = c("Kobieta", "Mężczyzna", "Niebinarna", "Odmowa odpowiedzi")
)
rm(plec_u, plec_ankieta)

# ============================================================================
# METADANE ZMIENNYCH
# ============================================================================

variable_meta <- list(
  plec = list(label = "Płeć", type = "nominalna"),
  kierunek = list(label = "Kierunek studiów", type = "nominalna"),
  grupa_krwi = list(label = "Grupa krwi", type = "nominalna"),
  rok_studiow = list(label = "Rok studiów", type = "porzadkowa"),
  zadowolenie = list(label = "Zadowolenie ze studiów", type = "porzadkowa"),
  liczba_kursow = list(label = "Liczba kursów", type = "ilosciowa_dyskretna"),
  liczba_nieobecnosci = list(label = "Liczba nieobecności", type = "ilosciowa_dyskretna"),
  wzrost = list(label = "Wzrost (cm)", type = "ilosciowa_ciagla"),
  srednia_ocen = list(label = "Średnia ocen", type = "ilosciowa_ciagla"),
  czas_dojazdu = list(label = "Czas dojazdu (min)", type = "ilosciowa_ciagla"),
  waga = list(label = "Waga (kg)", type = "ilosciowa_ciagla"),
  ocena_wykladowcy = list(label = "Ocena wykładowcy (1-10)", type = "porzadkowa"),
  wydatki = list(label = "Wydatki miesięczne (zł)", type = "ilosciowa_ciagla")
)
