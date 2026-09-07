# Interaktywne wykłady

Repozytorium na rownolegle zestawy interaktywnych wykladow R Shiny. Numerowany katalog wykladu jest osobna aplikacja/chapterem, uruchamiana lokalnie niezaleznie od pozostalych.

## Struktura

```text
interaktywne-wyklady/
├── hub/            # launcher: spis wszystkich wykładów w przeglądarce
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

## Uruchamianie

Najprościej hubem — jeden spis wszystkich wykładów w przeglądarce, bez wracania
do terminala między wykładami:

```bash
scripts/hub          # albo dwuklik w Wyklady.command
```

Hub wykrywa wykłady sam, więc lista nigdy się nie rozjeżdża z repo. Uruchomione
wykłady zostają żywe, dzięki czemu powrót do wcześniejszego zachowuje jego stan —
opis w [hub/README.md](hub/README.md).

Pojedynczy wykład można nadal uruchomić bezpośrednio:

```r
shiny::runApp("statystyka/01-typy-danych")
```

Szczegółowe wymagania pakietów i opis aplikacji są w dokumentacji konkretnego zestawu.
