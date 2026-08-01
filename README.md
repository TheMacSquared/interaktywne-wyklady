# Interaktywne wykłady

Repozytorium na rownolegle zestawy interaktywnych wykladow R Shiny. Numerowany katalog wykladu jest osobna aplikacja/chapterem, uruchamiana lokalnie niezaleznie od pozostalych.

## Struktura

```text
interaktywne-wyklady/
├── statystyka/     # istniejące wykłady ze statystyki
├── ekonometria/    # zestaw wykładów z ekonometrii
└── analiza-ryzyka/ # interaktywne wykłady z analizy ryzyka
```

## Zestawy wykładów

| Folder | Status | Dokumentacja |
|--------|--------|--------------|
| `statystyka/` | gotowy zestaw aplikacji statystycznych | [statystyka/README.md](statystyka/README.md) |
| `ekonometria/` | startowy zestaw wykładów z ekonometrii | [ekonometria/README.md](ekonometria/README.md) |
| `analiza-ryzyka/` | pierwszy pionowy wycinek w realizacji | [analiza-ryzyka/README.md](analiza-ryzyka/README.md) |

## Uruchamianie z katalogu głównego

```r
shiny::runApp("statystyka/01-typy-danych")
shiny::runApp("statystyka/02-rozklady-prawdopodobienstwa")
shiny::runApp("statystyka/03-przedzialy-ufnosci")
shiny::runApp("statystyka/04-wnioskowanie-statystyczne")
shiny::runApp("statystyka/05-zalozenia-testow")
shiny::runApp("statystyka/06-regresja")
shiny::runApp("statystyka/07-symulacje-statystyczne")
shiny::runApp("statystyka/08-metody-bayesowskie")
shiny::runApp("statystyka/09-dobre-dane")
shiny::runApp("statystyka/10-case-studies")
shiny::runApp("statystyka/11-kierunkowe")

shiny::runApp("ekonometria/01-model-ekonometryczny")
shiny::runApp("ekonometria/02-regresja-liniowa-kmnk")
shiny::runApp("ekonometria/03-estymatory-bledy-standardowe")
shiny::runApp("ekonometria/04-weryfikacja-modelu")
shiny::runApp("ekonometria/05-szeregi-prognozowanie")
shiny::runApp("ekonometria/06-optymalizacja-liniowa")
shiny::runApp("ekonometria/07-simpleks-dualizm")

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

Szczegółowe wymagania pakietów i opis aplikacji są w dokumentacji konkretnego zestawu.
