# Interaktywne wykłady

Repozytorium na równoległe zestawy interaktywnych wykładów R Shiny.

## Struktura

```text
interaktywne-wyklady/
├── statystyka/     # istniejące wykłady ze statystyki
└── ekonometria/    # miejsce na nowy zestaw wykładów z ekonometrii
```

## Zestawy wykładów

| Folder | Status | Dokumentacja |
|--------|--------|--------------|
| `statystyka/` | gotowy zestaw aplikacji statystycznych | [statystyka/README.md](statystyka/README.md) |
| `ekonometria/` | nowy projekt, jeszcze bez aplikacji | [ekonometria/README.md](ekonometria/README.md) |

## Uruchamianie z katalogu głównego

```r
shiny::runApp("statystyka/typy-danych")
shiny::runApp("statystyka/rozklady-prawdopodobienstwa")
shiny::runApp("statystyka/przedzialy-ufnosci")
shiny::runApp("statystyka/wnioskowanie-statystyczne")
shiny::runApp("statystyka/regresja")
shiny::runApp("statystyka/zalozenia-testow")
shiny::runApp("statystyka/case-studies")
shiny::runApp("statystyka/dobre-dane")
shiny::runApp("statystyka/symulacje-statystyczne")
shiny::runApp("statystyka/metody-bayesowskie")
```

Szczegółowe wymagania pakietów i opis aplikacji są w dokumentacji konkretnego zestawu.
