# Interaktywne Wyklady z Ekonometrii

Ten katalog zawiera rownolegly zestaw aplikacji R Shiny do nauczania ekonometrii. Kazdy katalog numerowany (`01-*`, `02-*`, ...) jest osobnym chapterem i osobna aplikacja Shiny, zeby pojedynczy proces nie wczytywal calego kursu naraz.

Framework, layout i komponenty UI sa skopiowane z `statystyka/R/`, z osobna konfiguracja nawigacji dla ekonometrii. Gorna nawigacja miedzy chapterami jest na razie placeholderem lokalnym (`href = "#"`) i ma byc w przyszlosci podmieniona na lacznik serwerowy.

## Struktura

```text
ekonometria/
├── README.md
├── R/                              # wspolne komponenty i style dla chapterow
├── ekonometria-pd000000peks-l4-0561-24.pdf
└── 01-model-ekonometryczny/
    ├── app.R
    └── modules/
```

## Uruchamianie chaptera

Z katalogu glownego repozytorium:

```r
shiny::runApp("ekonometria/01-model-ekonometryczny")
shiny::runApp("ekonometria/02-regresja-liniowa-kmnk")
shiny::runApp("ekonometria/03-estymatory-bledy-standardowe")
shiny::runApp("ekonometria/04-weryfikacja-modelu")
shiny::runApp("ekonometria/05-szeregi-prognozowanie")
shiny::runApp("ekonometria/06-optymalizacja-liniowa")
shiny::runApp("ekonometria/07-simpleks-dualizm")
```

## Zakres z syllabusowego PDF-a

Pierwszy chapter odpowiada pierwszemu punktowi programu:

- pojecie modelu ekonometrycznego,
- klasyfikacja zmiennych w modelu,
- rola skladnika losowego,
- postepowanie ekonometryczne.

Roboczy plan kolejnych osobnych aplikacji/chapterow:

- `02-regresja-liniowa-kmnk` - model regresji liniowej z jedna zmienna, zalozenia KMNK,
- `03-estymatory-bledy-standardowe` - estymatory parametrow i bledy standardowe,
- `04-weryfikacja-modelu` - dopasowanie, istotnosc, weryfikacja merytoryczna i statystyczna,
- `05-szeregi-prognozowanie` - dynamiczne modele ekonometryczne i prognozy,
- `06-optymalizacja-liniowa` - problem decyzyjny, funkcja celu, ograniczenia,
- `07-simpleks-dualizm` - metoda simpleks i dualizm programowania liniowego.
