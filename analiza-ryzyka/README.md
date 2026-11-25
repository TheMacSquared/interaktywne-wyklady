# Interaktywne wykłady z analizy ryzyka

Seria aplikacji R Shiny dla studentów inżynierii bezpieczeństwa. Przypadkiem
przewodnim jest fikcyjny importer bananów Bananpol, a pierwsza aplikacja uczy
języka zdarzeń i podstaw prawdopodobieństwa.

## Status

- klucz ćwiczeń i wskazówki prowadzenia: `~/praca/dydaktyka/przedmioty/analiza-ryzyka/listy/przewodnik-prowadzacego.md`;
- korekty spójności: model okresu pracy w 10, warunkowe wejścia FTA, quizy tematyczne
  z objaśnieniami, niepewność parametrów i kryterium decyzji;
- plan kanoniczny: `~/praca/dydaktyka/przedmioty/analiza-ryzyka/plan-kanoniczny.md`;
- graf wiedzy prowadzącego (dawne `graph/`): `~/praca/dydaktyka/przedmioty/analiza-ryzyka/graf/`;
- wykład 01: pełna, rozbudowana wersja z ośmioma rozdziałami;
- wykłady 02–10: pełne bloki z lokalnymi modułami, narracją, głosowaniem,
  interakcjami, decyzją, pułapką, ściągą, pięciopytaniowym quizem i ćwiczeniami;
- rozdziały są jednostkami skryptu: dawne mini-rozdziały weszły jako sekcje
  (`sections` z własnym wzorem, widgetem i pułapką w `R/risk_block.R`);
- plan semestru to 12 tygodni: każdy blok zajmuje jedno spotkanie, tylko 07
  („Część B” od rozdziału Weibulla) rozkłada się na dwa;
- kolejny etap: manualna próba tempa zajęć i korekta obciążenia treścią.

## Wymagania

- R w wersji co najmniej 4.1;
- pakiety: `shiny`, `ggplot2`, `dplyr`, `jsonlite`;
- do testów: `testthat` i `callr`.

## Uruchamianie

Z katalogu głównego repozytorium budujemy stronę wykładu i otwieramy ją w przeglądarce:

```sh
Rscript scripts/build_static.R analiza-ryzyka/01-jezyk-ryzyka   # → dist/analiza-ryzyka/01-jezyk-ryzyka.html
```


## Kontrola

```sh
Rscript analiza-ryzyka/scripts/check_design_contract.R --strict
Rscript analiza-ryzyka/tests/testthat.R
```

## Struktura

```text
analiza-ryzyka/
├── R/                    # course.R, course_styles.css i model Bananpolu
│                         # (layout i style wspólne: ../shared/)
├── docs/                 # plany źródłowe, ocena i plan kanoniczny
├── scripts/              # kontrole projektu
├── tests/                # testy funkcji, rycin i stron
├── 01-jezyk-ryzyka/      # wykład 01 (block.R + helpers.R widżetów)
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

Blok 09 dotyczy pożaru z warunkowymi niepowodzeniami detekcji i tłumienia.
Blok 10 jest osobnym studium utraty ochrony termicznej w jednym okresie pracy: model
czasu życia zasila układ chłodzenia, a interwencje porównujemy w trzech
scenariuszach, z budżetem i demonstracyjnym kryterium pozostałego ryzyka.
