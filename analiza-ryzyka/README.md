# Interaktywne wykłady z analizy ryzyka

Seria aplikacji R Shiny dla studentów inżynierii bezpieczeństwa. Przypadkiem
przewodnim jest fikcyjny importer bananów Bananpol, a pierwsza aplikacja uczy
języka zdarzeń i podstaw prawdopodobieństwa.

## Status

- plan kanoniczny: `docs/plan-kanoniczny.md`;
- wykład 01: pełna, rozbudowana wersja z ośmioma rozdziałami;
- wykłady 02–10: pełne bloki z lokalnymi modułami, narracją, głosowaniem,
  interakcjami, decyzją, pułapką, ściągą, pięciopytaniowym quizem i ćwiczeniami;
- bloki 06–10 mają naturalny podział na dwa spotkania po 90 minut;
- kolejny etap: manualna próba tempa zajęć i korekta obciążenia treścią.

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
                          # pełne bloki z lokalnym modules/block.R
```

## Konwencje

Interfejs jest po polsku, kod używa angielskich nazw. Wszystkie aplikacje
korzystają z `lecture_page()` oraz komponentów `lc_*`. Dane Bananpolu są
fikcyjne, a ich jednostki i horyzonty czasu znajdują się w `R/bananpol.R`.

Wykłady 02–10 korzystają ze wspólnych czystych funkcji w `R/risk_math.R` oraz
komponentów w `R/risk_block.R`. `R/course_catalog.R` jest indeksem kolejności i
metadanych; treść każdego bloku pozostaje w jego własnym `modules/block.R`.
