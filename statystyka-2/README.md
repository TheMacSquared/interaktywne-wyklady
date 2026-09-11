# Statystyka 2

Rozszerzenie podstawowego kursu statystyki: symulacje, wnioskowanie bayesowskie, zastosowania kierunkowe i szeregi czasowe.

## Cel i wymagania wstępne

Student poznaje sposoby analizy wykraczające poza podstawowe testy i regresję: wykorzystuje resampling, interpretuje posterior, dobiera narzędzie do domeny oraz analizuje zależność w czasie.

Punktem wyjścia są umiejętności z [Statystyki](../statystyka/README.md): opis danych, rozkłady, przedziały ufności, testowanie hipotez i regresja. Jest to oddzielny przedmiot, a nie dodatkowy obowiązkowy zakres kursu podstawowego.

## Bloki

| Nr | Aplikacja | Pytanie przewodnie |
|---|---|---|
| 01 | [Symulacje statystyczne](01-symulacje-statystyczne/) | Jak wykorzystać losowanie do oceny niepewności i działania metody? |
| 02 | [Metody bayesowskie](02-metody-bayesowskie/) | Jak łączyć wiedzę wstępną z danymi i interpretować posterior? |
| 03 | [Materiały kierunkowe](03-kierunkowe/) | Jak dopasować narzędzie do problemu danego kierunku studiów? |
| 04 | [Szeregi czasowe](04-szeregi-czasowe/) | Co zmienia kolejność obserwacji i jak oceniać prognozy? |

Bloki mają odpowiednio 10, 12, 6 i 16 rozdziałów. Nie oznacza to czterech spotkań: prowadzący wybiera zakres odpowiednio do godzin. W materiałach kierunkowych wybiera się rozdział właściwy dla grupy. Bayes i szeregi czasowe mogą stanowić osobne wielospotkaniowe części kursu.

## Wymagania techniczne

- R ≥ 4.1.
- Podstawowe pakiety: `shiny`, `ggplot2`, `dplyr`, `tidyr`, `rstatix`, `broom`.
- Dodatkowe pakiety zależą od bloku; ich dostępność sprawdza skrypt poniżej. Bayes korzysta z `BayesFactor`, a regresja bayesowska z `rstanarm`.

```sh
Rscript statystyka-2/scripts/check_dependencies.R
```

## Uruchamianie

Z katalogu głównego repozytorium:

```sh
scripts/hub
```

Albo bezpośrednio:

```r
shiny::runApp("statystyka-2/01-symulacje-statystyczne")
```

## Kontrola

Testy wymagają `testthat` i `callr`:

```sh
Rscript statystyka-2/scripts/run_tests.R
Rscript statystyka-2/scripts/run_tests.R --quick
```

Kontrole tej części nie są wymagane do uruchomienia „Statystyki”. Pełny przebieg sprawdza zależności, design, funkcje matematyczne, strukturę i wczytanie czterech aplikacji.

## Organizacja i status

Materiały przeniesiono z pierwszej części kursu. Własny katalog `R/` jest świadomą kopią systemu layoutu statystyki; aplikacje korzystają wyłącznie z komponentów swojego przedmiotu. Zasady opisuje [kontrakt designu](R/DESIGN_CONTRACT.md).

Ustalenia 1–8 [audytu z 11 września 2026](../audyty/2026-09-11-statystyka-analiza-ryzyka.md) zostały poprawione. Dane szeregów są jawnie syntetyczne w aplikacji, podpisach wykresów i CSV. Generator wykorzystuje daty scenariusza, a testy sprawdzają odtwarzalność danych oraz skrajne p-wartości symulacyjne. Pozostałe zadania audytu dotyczą szerszego pokrycia interakcji testami i utrzymania.
