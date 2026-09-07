# Interaktywne wykłady z analizy ryzyka

Seria aplikacji R Shiny dla studentów inżynierii bezpieczeństwa. Przypadkiem
przewodnim jest fikcyjny importer bananów Bananpol, a pierwsza aplikacja uczy
języka zdarzeń i podstaw prawdopodobieństwa.

## Status

- klucz ćwiczeń i wskazówki prowadzenia: `docs/przewodnik-prowadzacego.md`;
- korekty spójności: model misji w 10, warunkowe wejścia FTA, quizy tematyczne
  z objaśnieniami, niepewność parametrów i kryterium decyzji;
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

Najprościej hubem — spis wszystkich wykładów w przeglądarce, bez wracania do
terminala między wykładami (lista wykrywana automatycznie, więc zawsze aktualna):

```bash
scripts/hub          # z katalogu głównego repo; albo dwuklik w Wyklady.command
```

Szczegóły: [hub/README.md](../hub/README.md).

Pojedynczy wykład bezpośrednio — z katalogu głównego repo:

```r
shiny::runApp("analiza-ryzyka/01-jezyk-ryzyka")
```

Albo z katalogu `analiza-ryzyka/`:

```r
shiny::runApp("01-jezyk-ryzyka")
```

Menu wykładów tylko dla tego przedmiotu, w terminalu: `scripts/wyklad`.


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

Blok 09 dotyczy pożaru z warunkowymi niepowodzeniami detekcji i tłumienia.
Blok 10 jest osobnym studium utraty ochrony termicznej w jednej misji: model
czasu życia zasila układ chłodzenia, a interwencje porównujemy w trzech
scenariuszach, z budżetem i demonstracyjnym kryterium pozostałego ryzyka.
