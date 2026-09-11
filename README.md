# Interaktywne wykłady

Interaktywne wykłady R Shiny do nauczania przedmiotów ilościowych. Każdy numerowany katalog wykładu jest osobną aplikacją z rozdziałami, uruchamianą niezależnie od pozostałych.

## Struktura

```text
interaktywne-wyklady/
├── hub/            # launcher: spis wszystkich wykładów w przeglądarce
├── statystyka/     # podstawowy kurs statystyki
├── statystyka-2/   # symulacje, Bayes, kierunkowe, szeregi czasowe
├── analiza-ryzyka/ # interaktywne wykłady z analizy ryzyka
├── audyty/         # ustalenia audytu i stan poprawek
└── deprecated/     # archiwum, pomijane przez hub
    └── ekonometria/
```

## Zestawy wykładów

| Folder | Status | Dokumentacja |
|--------|--------|--------------|
| `statystyka/` | podstawy: 9 aplikacji | [statystyka/README.md](statystyka/README.md) |
| `statystyka-2/` | rozszerzenia: 4 aplikacje | [statystyka-2/README.md](statystyka-2/README.md) |
| `analiza-ryzyka/` | 10 aplikacji z analizy ryzyka | [analiza-ryzyka/README.md](analiza-ryzyka/README.md) |

## Obecny stan — 11 września 2026

Aktywne są 23 aplikacje: 9 w Statystyce, 4 w Statystyce 2 i 10 w analizie ryzyka. Podstawowy kurs statystyki kończy się projektem badawczym; symulacje, Bayes, materiały kierunkowe i szeregi czasowe tworzą oddzielny przedmiot. Siedem aplikacji ekonometrii znajduje się w [archiwum](deprecated/ekonometria/README.md).

Wykonano poprawki ustaleń 1–8 [audytu](audyty/2026-09-11-statystyka-analiza-ryzyka.md): skorygowano interpretacje i obliczenia, oznaczono syntetyczne dane i poprawiono ich generator. CASchools zachowuje intuicyjną narrację dla początkujących. Hub ma powiększone czcionki.

Kontrole po poprawkach: 36 sprawdzeń Statystyki, 73 Statystyki 2 i 472 analizy ryzyka — bez błędów, ostrzeżeń i pominięć. To nie jest pełny test wszystkich interakcji przeglądarkowych. Pozostałe zadania, w tym rozszerzenie pokrycia testami i pilotaż tempa zajęć, są opisane w audycie.

```sh
Rscript statystyka/scripts/run_tests.R
Rscript statystyka-2/scripts/run_tests.R
Rscript analiza-ryzyka/scripts/check_design_contract.R --strict
Rscript analiza-ryzyka/tests/testthat.R
```

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
