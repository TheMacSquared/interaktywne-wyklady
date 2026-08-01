# Interaktywne wykłady z analizy ryzyka

Seria aplikacji R Shiny dla studentów inżynierii bezpieczeństwa. Przypadkiem
przewodnim jest fikcyjny importer bananów Bananpol, a pierwsza aplikacja uczy
języka zdarzeń i podstaw prawdopodobieństwa.

## Status

- plan kanoniczny: `docs/plan-kanoniczny.md`;
- wykład 01: pełna, rozbudowana wersja z ośmioma rozdziałami;
- wykłady 02–10: kompletne, uruchamialne szkice wszystkich rozdziałów z jednym
  kluczowym widgetem na wykład;
- kolejny etap: przegląd globalny, a następnie pogłębianie wybranych rozdziałów.

## Wymagania

- R w wersji co najmniej 4.1;
- pakiety: `shiny`, `ggplot2`, `dplyr`, `jsonlite`;
- do testów: `testthat` i `callr`.

## Uruchamianie

Z katalogu głównego repozytorium:

```r
shiny::runApp("analiza-ryzyka/01-jezyk-ryzyka")
shiny::runApp("analiza-ryzyka/02-warunki")
shiny::runApp("analiza-ryzyka/03-alarm-i-prawda")
shiny::runApp("analiza-ryzyka/04-wiele-prob")
shiny::runApp("analiza-ryzyka/05-do-zdarzenia")
shiny::runApp("analiza-ryzyka/06-zmiennosc-i-prog")
shiny::runApp("analiza-ryzyka/07-czas-zycia")
shiny::runApp("analiza-ryzyka/08-niezawodnosc-systemu")
shiny::runApp("analiza-ryzyka/09-drzewo-bledow")
shiny::runApp("analiza-ryzyka/10-model-do-decyzji")
```

## Kontrola

```sh
Rscript analiza-ryzyka/scripts/check_design_contract.R --strict
Rscript analiza-ryzyka/tests/testthat.R
```

## Struktura

```text
analiza-ryzyka/
├── R/                    # własny snapshot layoutu i model Bananpolu
├── docs/                 # plany źródłowe, ocena i plan kanoniczny
├── scripts/              # kontrole projektu
├── tests/                # testy funkcji oraz smoke test aplikacji
├── 01-jezyk-ryzyka/      # pełna aplikacja referencyjna
└── 02-warunki/ ... 10-model-do-decyzji/
                          # kompletne szkice generowane ze wspólnego katalogu
```

## Konwencje

Interfejs jest po polsku, kod używa angielskich nazw. Wszystkie aplikacje
korzystają z `lecture_page()` oraz komponentów `lc_*`. Dane Bananpolu są
fikcyjne, a ich jednostki i horyzonty czasu znajdują się w `R/bananpol.R`.

Wykłady 02–10 korzystają z `R/course_catalog.R` i `R/course_factory.R`. Katalog
jest jednym źródłem prawdy dla kolejności oraz treści rozdziałów na etapie
przeglądu globalnego.
