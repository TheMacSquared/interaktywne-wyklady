# Statystyka

Podstawowy kurs statystyki: od opisu danych do samodzielnego wniosku z analizy. Interaktywne aplikacje R Shiny wspierają wykład dla początkujących.

## Cel i zakres

Student potrafi opisać dane, odczytać niepewność wyniku, wybrać podstawową metodę i wyjaśnić wniosek w kontekście pytania. Przykłady budują intuicję; szczegóły metodologiczne pojawiają się tylko tam, gdzie są potrzebne do zrozumienia wyniku.

Symulacje, Bayes, rozszerzenia kierunkowe i szeregi czasowe należą do oddzielnego przedmiotu [Statystyka 2](../statystyka-2/README.md). Nie są wymagane do ukończenia kursu podstawowego.

## Wykłady

| Nr | Aplikacja | Zadanie dydaktyczne |
|---|---|---|
| 01 | [Typy danych](01-typy-danych/) | Rozpoznać zmienne, dobrać opis i wykres. |
| 02 | [Rozkłady prawdopodobieństwa](02-rozklady-prawdopodobienstwa/) | Zrozumieć losowość, wartość oczekiwaną i rozkład średniej. |
| 03 | [Przedziały ufności](03-przedzialy-ufnosci/) | Odczytać oszacowanie i jego niepewność. |
| 04 | [Wnioskowanie statystyczne](04-wnioskowanie-statystyczne/) | Przejść od pytania i hipotezy do testu, siły efektu i wniosku. |
| 05 | [Założenia testów](05-zalozenia-testow/) | Rozpoznać sytuacje wymagające zmiany metody. |
| 06 | [Regresja](06-regresja/) | Opisać związek i zobaczyć, jak uwzględnienie kontekstu zmienia interpretację. |
| 07 | [Dobre dane](07-dobre-dane/) | Ocenić, czy dane odpowiadają na pytanie. |
| 08 | [Case studies](08-case-studies/) | Prześledzić pełną analizę na przykładzie szkół i sytuacji uczniów. |
| 09 | [Projekt badawczy](09-projekt-badawczy/) | Sformułować własne pytanie, zaplanować analizę i przedstawić wniosek. |

Numeracja porządkuje materiały, nie narzuca dziewięciu osobnych spotkań. Przy ograniczonej liczbie godzin fragmenty „Dobrych danych” i „Projektu badawczego” można wplatać we wcześniejsze przykłady. Rozbudowane aplikacje są także materiałem do pracy własnej.

## Jak używać na zajęciach

Najpierw postaw zrozumiałe pytanie, potem poproś o przewidywanie, zmień parametr lub pokaż dane i wróć do odpowiedzi. W CASchools punktem zaczepienia jest pytanie: „Czy dopłaty do obiadów pogarszają oceny?”. Ujemny związek pozwala odkryć, że pomoc częściej trafia do uczniów w trudniejszej sytuacji. Celem jest intuicja dotycząca korelacji i kontekstu.

## Wymagania

- R ≥ 4.1.
- Pakiety zależne od aplikacji; kompletność sprawdza `scripts/check_dependencies.R`.

```r
install.packages(c(
  "shiny", "ggplot2", "dplyr", "e1071", "gridExtra", "rstatix", "broom",
  "tidyr", "knitr", "lmtest", "sandwich", "visNetwork", "DT", "bslib",
  "AER", "palmerpenguins", "ISLR", "fivethirtyeight", "jsonlite"
))
```

Pakiety BayesFactor, rstanarm i pakiety szeregów czasowych nie należą do wymagań tej części kursu.

## Uruchamianie

Z katalogu głównego repozytorium:

```sh
scripts/hub
```

Hub pokazuje „Statystykę” i „Statystykę 2” jako oddzielne przedmioty. Pojedyncza aplikacja:

```r
shiny::runApp("statystyka/01-typy-danych")
```

## Kontrola

Testy wymagają `testthat` i `callr`.

```sh
Rscript statystyka/scripts/run_tests.R
Rscript statystyka/scripts/run_tests.R --quick
```

Pełna kontrola obejmuje zależności, design, testy i wczytanie dziewięciu aplikacji. Tryb `--quick` pomija wczytywanie aplikacji.

## Organizacja kodu

Każdy katalog wykładu zawiera `app.R` i moduły rozdziałów. Wspólne komponenty są w `R/`, narzędzia w `scripts/`, testy w `tests/`. Reguły wyglądu określa [kontrakt designu](R/DESIGN_CONTRACT.md).

Po podziale kursu dawne wykłady 09, 10 i 12 mają numery 07, 08 i 09. Pozostałe cztery bloki przeniesiono do `statystyka-2/`; treść ćwiczeń kierunkowych wewnątrz podstawowych wykładów pozostaje częścią kursu podstawowego.
