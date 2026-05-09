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
shiny::runApp("statystyka/01-typy-danych")
shiny::runApp("statystyka/02-rozklady-prawdopodobienstwa")
shiny::runApp("statystyka/03-przedzialy-ufnosci")
shiny::runApp("statystyka/04-wnioskowanie-statystyczne")
shiny::runApp("statystyka/05-regresja")
shiny::runApp("statystyka/05a-zalozenia-testow")
shiny::runApp("statystyka/06-symulacje-statystyczne")
shiny::runApp("statystyka/07-metody-bayesowskie")
shiny::runApp("statystyka/08-dobre-dane")
shiny::runApp("statystyka/09-case-studies")
```

Szczegółowe wymagania pakietów i opis aplikacji są w dokumentacji konkretnego zestawu.
