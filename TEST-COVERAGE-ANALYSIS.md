# Analiza pokrycia testami / Test Coverage Analysis

## Stan obecny

**Projekt nie posiada żadnych testów.** Brak:
- Frameworka testowego (testthat, shinytest2)
- Katalogu `tests/`
- Plików testowych
- CI/CD pipeline (GitHub Actions)
- Struktury pakietu R (DESCRIPTION, NAMESPACE)

Projekt składa się z **16 samodzielnych aplikacji Shiny** (`app.R`), z których każda zawiera logikę biznesową zmieszaną z kodem UI i serwerowym.

---

## Identyfikacja logiki testowalnej

### Priorytet 1: Funkcje generowania danych (krytyczne)

Każda aplikacja zawiera czyste funkcje generujące dane, które są idealnymi kandydatami do testów jednostkowych — nie zależą od Shiny.

| Aplikacja | Funkcje | Co testować |
|-----------|---------|-------------|
| `box-plot-builder` | `generate_autobusy_data(n)`, `generate_kac_data(n)`, `generate_sklep_data(n)` | Wymiary wyjścia, zakresy wartości, typy danych |
| `losowanie_spoznienia` | Duplikaty tych samych 3 generatorów | Identyczne jak wyżej (+ deduplikacja kodu) |
| `srednia-vs-mediana` | `generate_data(scenario_name)` z 5 scenariuszami | Rozmiar wyjścia, rozkłady, zakresy per scenariusz |
| `Testowanie-hipotez/test-t-builder` | `generate_data(scenario, n)` | Struktura data.frame (wartosc, grupa), poprawne poziomy czynnika |
| `Testowanie-hipotez/korelacja-builder` | `generate_data(scenario, n)` | Wymiary (n×2), realistyczne zakresy x/y |
| `Testowanie-hipotez/chi-kwadrat-builder` | `generate_data(scenario, n)` | Wymiary macierzy, suma = n, nazwy wierszy/kolumn |
| `regresja-interakcja` | `generate_regression_data(...)`, `fit_models(data)`, `get_model_lines(...)`, `format_equation(...)` | Wymiary data.frame, poprawność modeli, format równań |
| `gra-estymacja` | `generate_round_data(difficulty)` | Zakres 0–100, rozmiar wektora, różnice między trudnościami |
| `zalozenia-testow` | **20+ funkcji** generujących dane dla 4 modułów | Wymiary, typy kolumn, właściwości rozkładów |

**Dlaczego to priorytet:** Te funkcje definiują poprawność danych pedagogicznych. Błąd w generatorze (np. `generate_kac_data` produkujący wartości poza 0–10) zniekształca lekcję statystyki.

### Priorytet 2: Logika obliczeniowa

| Aplikacja | Logika | Co testować |
|-----------|--------|-------------|
| `regresja-interakcja` | `fit_models(data)` — dopasowuje model addytywny i z interakcją, porównuje ANOVA | Zwraca listę z 4 elementami, p-value jest liczbą, modele mają poprawne współczynniki |
| `regresja-interakcja` | `get_model_lines(model, type)` — wyodrębnia intercept/slope per grupa | Poprawna ekstrakcja współczynników dla modelu addytywnego vs interakcji |
| `regresja-interakcja` | `format_equation(intercept, slope, group)` — formatuje równanie regresji | Poprawny format string, obsługa ujemnych nachyleń |
| `gra-estymacja` | Logika punktacji: `max(0, 100 - error * 5)` | Poprawność obliczania punktów, zakres 0–100 per statystyka |
| `zalozenia-testow` | `create_variance_data(seed, n_per_group, means, sds, group_names)` | Poprawna struktura data.frame, liczba obserwacji per grupa |
| `zalozenia-testow` | `get_variance_data(n_groups, equal_var)` — routing do danych | Zwraca poprawny zbiór danych dla każdej kombinacji parametrów |

### Priorytet 3: Stałe zbiory danych (zalozenia-testow)

Aplikacja `zalozenia-testow` zawiera ponad 15 predefiniowanych zbiorów danych (`NORMAL_DATA`, `BIMODAL_DATA`, `VAR_EQUAL_2`, `REG_HOMOSCEDASTIC_DATA`, itd.) wygenerowanych z ustalonymi seedami. Te dane powinny mieć testy regresyjne sprawdzające, że:
- Mają oczekiwane właściwości statystyczne (np. `BIMODAL_DATA` ma dwa mody)
- Nie zmieniają się niezamierzenie przy aktualizacji kodu
- `NORMAL_DATA` przechodzi test Shapiro-Wilka, `HIGHLY_SKEWED_DATA` go nie przechodzi

### Priorytet 4: Testy integracyjne Shiny (shinytest2)

Interakcje użytkownika z UI — wymaga frameworka `shinytest2`:
- `box-plot-builder`: Kliknięcie kolejnych kroków ujawnia poprawne elementy
- `gra-estymacja`: Cykl gry (start → runda → wynik → podsumowanie)
- `srednia-vs-mediana`: Dodawanie outlierów przesuwa średnią bardziej niż medianę

---

## Konkretne rekomendacje

### 1. Wyekstrahować logikę do testowalnych modułów

Obecnie każdy `app.R` zawiera wszystko w jednym pliku. Proponowana struktura:

```
app-name/
├── app.R              # UI + server (cienka warstwa)
├── R/
│   ├── generators.R   # Funkcje generujące dane
│   └── helpers.R      # Funkcje obliczeniowe
└── tests/
    └── testthat/
        ├── test-generators.R
        └── test-helpers.R
```

Minimalnie: wydzielić funkcje z `app.R` do osobnych plików R, które można `source()`'ować zarówno z app jak i z testów.

### 2. Dodać testthat dla funkcji generujących dane

Przykładowe testy dla `box-plot-builder`:

```r
test_that("generate_autobusy_data zwraca wektor odpowiedniej długości", {
  result <- generate_autobusy_data(50)
  expect_length(result, 50)
  expect_type(result, "double")
})

test_that("generate_autobusy_data produkuje wartości w oczekiwanym zakresie", {
  result <- generate_autobusy_data(1000)
  expect_true(all(result >= -2))  # pmax(., -2) w generatorze
})

test_that("generate_kac_data mieści się w zakresie 0-10", {
  result <- generate_kac_data(1000)
  expect_true(all(result >= 0))
  expect_true(all(result <= 10))
})

test_that("generate_sklep_data ma kwoty >= 10", {
  result <- generate_sklep_data(1000)
  expect_true(all(result >= 10))  # pmax(10, .) w generatorze
})
```

### 3. Testy regresyjne dla stałych zbiorów danych

```r
test_that("NORMAL_DATA przechodzi test normalności", {
  p <- shapiro.test(NORMAL_DATA)$p.value
  expect_gt(p, 0.05)
})

test_that("BIMODAL_DATA ma dwa skupiska", {
  expect_equal(length(BIMODAL_DATA), 50)
  below_50 <- sum(BIMODAL_DATA < 50)
  above_50 <- sum(BIMODAL_DATA >= 50)
  expect_equal(below_50, 25)
  expect_equal(above_50, 25)
})

test_that("VAR_EQUAL_2 ma równe wariancje", {
  result <- levene_test(value ~ group, data = VAR_EQUAL_2)
  expect_gt(result$p, 0.05)
})
```

### 4. Testy dla logiki gry estymacyjnej

```r
test_that("punktacja: idealny strzał = 100 punktów", {
  error <- 0
  points <- max(0, 100 - error * 5)
  expect_equal(points, 100)
})

test_that("punktacja: błąd 20+ = 0 punktów", {
  error <- 25
  points <- max(0, 100 - error * 5)
  expect_equal(points, 0)
})

test_that("generate_round_data respektuje zakres 0-100", {
  for (diff in c("easy", "medium", "hard")) {
    data <- generate_round_data(diff)
    expect_true(all(data >= 0 & data <= 100))
  }
})
```

### 5. Testy dla logiki regresji z interakcją

```r
test_that("fit_models zwraca poprawną strukturę", {
  data <- generate_regression_data(30, 2, 1.5, 6, 1.5, 2)
  models <- fit_models(data)
  expect_named(models, c("additive", "interaction", "anova", "interaction_pvalue"))
  expect_s3_class(models$additive, "lm")
  expect_s3_class(models$interaction, "lm")
})

test_that("get_model_lines addytywny ma równe nachylenia", {
  data <- generate_regression_data(50, 2, 1.5, 6, 1.5, 2)
  models <- fit_models(data)
  lines <- get_model_lines(models$additive, "additive")
  expect_equal(lines$slope[1], lines$slope[2])
})

test_that("format_equation obsługuje ujemne nachylenia", {
  result <- format_equation(5, -2.5, "A")
  expect_match(result, "- 2.50")
})
```

### 6. Wyeliminować duplikację kodu

`generate_autobusy_data`, `generate_kac_data`, `generate_sklep_data` są zdefiniowane identycznie w dwóch aplikacjach (`box-plot-builder` i `losowanie_spoznienia`). Należy wydzielić je do wspólnego pliku `R/shared-generators.R` — jeden zestaw testów pokrywa obie aplikacje.

### 7. Dodać CI/CD (GitHub Actions)

```yaml
# .github/workflows/tests.yml
name: R Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
      - name: Install dependencies
        run: Rscript -e 'install.packages(c("testthat", "shiny", "ggplot2", "dplyr", "rstatix", "broom", "lmtest", "MASS"))'
      - name: Run tests
        run: Rscript -e 'testthat::test_dir("tests/testthat")'
```

---

## Podsumowanie priorytetów

| Priorytet | Obszar | Wysiłek | Wpływ |
|-----------|--------|---------|-------|
| **P1** | Testy generatorów danych (zakresy, wymiary, typy) | Niski | Wysoki — łapie błędy w danych pedagogicznych |
| **P2** | Testy logiki obliczeniowej (regresja, punktacja gry) | Niski | Wysoki — weryfikuje poprawność matematyczną |
| **P3** | Testy regresyjne stałych zbiorów danych (zalozenia-testow) | Niski | Średni — chroni przed regresją |
| **P4** | Deduplikacja wspólnych generatorów | Niski | Średni — redukuje ryzyko rozbieżności |
| **P5** | Testy integracyjne Shiny (shinytest2) | Średni | Średni — weryfikuje przepływ UI |
| **P6** | CI/CD pipeline | Niski | Wysoki — automatyzacja zapobiega regresji |

Najwyższy zwrot z inwestycji mają **P1 + P2 + P6**: kilkadziesiąt testów jednostkowych dla czystych funkcji R + automatyczne uruchamianie w CI. Nie wymaga to restrukturyzacji projektu — wystarczy `source("app.R")` w plikach testowych (po opakowaniu `shinyApp()` w `if`), albo wyodrębnienie funkcji do osobnych plików.
