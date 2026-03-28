# 📊 Interaktywne Wykłady ze Statystyki

Aplikacje R Shiny w formie interaktywnych skryptów wykładowych. Każda aplikacja to scrollowalny przewodnik z osadzonymi widgetami — student czyta narrację i eksperymentuje z danymi w kontekście.

## 📋 Wymagania

- R (wersja ≥ 4.0)
- Pakiety R: `shiny`, `ggplot2`, `dplyr`, `e1071`, `gridExtra`

## 🚀 Instalacja pakietów

```r
install.packages(c("shiny", "ggplot2", "dplyr", "e1071", "gridExtra"))

# Dodatkowe (dla dobre-dane)
install.packages(c("DT", "tidyr", "AER", "palmerpenguins", "ISLR", "fivethirtyeight"))
```

## ▶️ Uruchamianie

```r
# Z R/RStudio
shiny::runApp("typy-danych")
shiny::runApp("rozklady-prawdopodobienstwa")
shiny::runApp("dobre-dane")
```

## 📚 Aplikacje

| Aplikacja | Temat | Rozdziały / zakres |
|-----------|-------|--------------------|
| [typy-danych](typy-danych/) | Statystyka opisowa | 6 rozdziałów: typy danych, zmienne jakościowe, statystyki położenia, rozrzutu, kształt rozkładu, ściąga |
| [rozklady-prawdopodobienstwa](rozklady-prawdopodobienstwa/) | Rozkłady prawdopodobieństwa | 8 rozdziałów: od danych do prawdopodobieństwa, wartość oczekiwana i wariancja, rozkłady dyskretne, ciągłe, normalny, CTG, dobór rozkładu, ściąga |
| [dobre-dane](dobre-dane/) | Jakość danych | Ocena zbiorów danych do analiz statystycznych (case studies) |

## 📁 Struktura projektu

```
interaktywne-wyklady/
├── typy-danych/                    # Statystyka opisowa
│   ├── app.R                       # Główny plik: dane, kolory, CSS/JS, nawigacja
│   └── modules/                    # Moduły rozdziałów
│       ├── helpers.R               # Funkcje pomocnicze (taksonomia, wykresy good/bad)
│       ├── ch1_typy.R              # 1. Typy danych
│       ├── ch2_jakosciowe.R        # 2. Zmienne jakościowe
│       ├── ch3_polozenie.R         # 3. Statystyki położenia
│       ├── ch4_rozrzut.R           # 4. Statystyki rozrzutu
│       ├── ch5_ksztalt.R           # 5. Kształt rozkładu
│       └── ch6_sciaga.R            # 6. Ściąga
├── rozklady-prawdopodobienstwa/    # Rozkłady prawdopodobieństwa
│   ├── app.R                       # Główny plik: kolory, CSS/JS, nawigacja
│   └── modules/
│       ├── helpers.R               # Funkcje symulacyjne, theme
│       ├── ch1_most.R              # 1. Od danych do prawdopodobieństwa
│       ├── ch2_ev_var.R            # 2. Wartość oczekiwana i wariancja
│       ├── ch3_dyskretne.R         # 3. Rozkłady dyskretne (jednostajny, dwumianowy, Poissona)
│       ├── ch4_ciagle.R            # 4. Rozkłady ciągłe (histogram→PDF, jednostajny, wykładniczy)
│       ├── ch5_normalny.R          # 5. Rozkład normalny (μ/σ, reguła 68-95-99.7, z-score)
│       ├── ch6_ctg.R               # 6. Centralne Twierdzenie Graniczne
│       ├── ch7_dobor.R             # 7. Dobór rozkładu (drzewo decyzyjne, Q-Q plot)
│       └── ch8_sciaga.R            # 8. Ściąga (wzory, tabele, funkcje R)
├── dobre-dane/                     # Jakość danych
│   └── app.R
├── README.md                       # Ten plik
└── CLAUDE.md                       # Instrukcje dla AI
```

## 🏗️ Architektura aplikacji wykładowej

Każda aplikacja wykładowa (np. `typy-danych`) stosuje wzorzec **scrollowalnego skryptu**:

- **navbarPage** z zakładkami = rozdziały wykładu
- **Sticky TOC** — spis treści z auto-podświetlaniem bieżącej sekcji
- **Variable tracker** — student wybiera zmienną w rozdziale 1 i śledzi ją przez cały kurs
- **Osadzone widgety** — interaktywne ćwiczenia wplecione w narrację
- **MathJax** — wzory matematyczne renderowane profesjonalnie
- **Chart.js** — wykresy kołowe/słupkowe w HTML5 Canvas

### Modularyzacja

Kod rozdziałów jest rozbity na osobne pliki w katalogu `modules/` (nie `R/`, bo Shiny automatycznie sourcuje `R/` przed `app.R`). Każdy moduł eksportuje:

```r
# modules/ch3_polozenie.R
ch3_ui <- tabPanel("3. Statystyki położenia", ...)
ch3_server <- function(input, output, session) { ... }
```

Główny `app.R` łączy je:

```r
source(file.path(app_dir, "modules", "ch1_typy.R"), local = TRUE)
# ...
ui <- navbarPage(..., ch1_ui, ch2_ui, ch3_ui, ch4_ui, ch5_ui, ch6_ui)
server <- function(input, output, session) {
  # nawigacja + tracker
  ch1_server(input, output, session)
  # ...
}
```

## 🎓 Użycie dydaktyczne

Aplikacje są zaprojektowane do:

1. **Demonstracji na wykładach** — prowadzący scrolluje i omawia, studenci widzą na projektorze
2. **Samodzielnej eksploracji** — studenci uruchamiają lokalnie i eksperymentują w swoim tempie
3. **Materiałów do powtórki** — ściąga (rozdział 6) jako kompaktowe podsumowanie

## 🛠️ Dodawanie nowej aplikacji

1. Utwórz folder z nazwą w formacie `nazwa-aplikacji/`
2. Utwórz `app.R` zgodnie z konwencjami projektu (patrz `CLAUDE.md`)
3. Dla dużych aplikacji: rozbij na moduły w `modules/`
4. Zaktualizuj ten `README.md`

## 🐛 Rozwiązywanie problemów

### Aplikacja nie uruchamia się

```r
# Sprawdź czy pakiety są zainstalowane
installed.packages()[c("shiny", "ggplot2", "dplyr"), ]

# Jeśli brak któregoś:
install.packages("nazwa_pakietu")
```

### Błąd "cannot open file"

Upewnij się, że uruchamiasz przez `shiny::runApp("typy-danych")` z katalogu nadrzędnego, nie przez `Rscript app.R`.

## 📝 Licencja

Projekt edukacyjny. Wolne do użytku i modyfikacji w celach dydaktycznych.
